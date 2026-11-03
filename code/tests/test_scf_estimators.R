# test_scf_estimators -- hand-checked unit tests for code/03_scf_estimators.R
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U5 of records/plans/2026-10-11_stage1-scf-pipeline.md. Run from the
# project root as
#   Rscript code/tests/test_scf_estimators.R
# It stops at the first failed check, naming it. Every expected value below is
# worked out by hand in the comment beside it; none is computed by the code
# under test.

################################################################################
###                       1. Load packages and the library                   ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here", "data.table")
installed_flag <- vapply(
  required_packages, requireNamespace, logical(1L),
  quietly = TRUE
)
if (!all(installed_flag)) {
  stop(
    "Required package(s) not installed: ",
    paste(required_packages[!installed_flag], collapse = ", "),
    call. = FALSE
  )
}

# 2) Stop unless this file sits at code/tests/ in the project, then source
#    the estimator library (functions only)
here::i_am("code/tests/test_scf_estimators.R")
source(here::here("code", "03_scf_estimators.R"))

# 3) Compare one value with its hand calculation; stop with the label if not
expect_close <- function(actual, expected, label) {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected),
                        tolerance = 1e-10))) {
    stop(
      "FAILED: ", label, ": got ", paste(actual, collapse = ", "),
      ", expected ", paste(expected, collapse = ", "),
      call. = FALSE
    )
  }
  message("PASS: ", label)
}

################################################################################
###                         2. Build the synthetic panel                     ###
################################################################################

# 4) Four cases, five implicates, one year. Case weights 1, 1, 2, 4 (the same
#    in every implicate, total 8); base values 10, 20, 30, 40; implicate i
#    adds i to every value, so implicate 1 holds 11, 21, 31, 41.
case_weight <- c(1, 1, 2, 4)
case_base <- c(10, 20, 30, 40)
synthetic_panel <- data.table::CJ(implicate = 1:5, yy1 = 1:4)
synthetic_panel[, year := 2025L]
synthetic_panel[, y1 := 10L * yy1 + implicate]
synthetic_panel[, wgt := case_weight[yy1]]
synthetic_panel[, value_varying := case_base[yy1] + implicate]
synthetic_panel[, value_constant := case_base[yy1]]
synthetic_panel[, above_25_flag := case_base[yy1] > 25]
synthetic_panel[, group_label := ifelse(yy1 <= 2L, "a", "b")]

# 5) Three replicate weights for the implicate-1 records (key y1 = 10*yy1 + 1)
#    rw_1 = (1, 1, 1, 1), rw_2 = (0, 2, 2, 0), rw_3 = (0, 0, 0, 1)
synthetic_repwgts <- data.table::data.table(
  year = 2025L,
  y1 = 10L * (1:4) + 1L,
  rw_1 = c(1, 1, 1, 1),
  rw_2 = c(0, 2, 2, 0),
  rw_3 = c(0, 0, 0, 1)
)

################################################################################
###                               3. Run checks                              ###
################################################################################

# 6) Mean. Implicate means: (10*1 + 20*1 + 30*2 + 40*4) / 8 + i = 31.25 + i,
#    so 32.25 ... 36.25; their average and the pooled mean are 34.25.
#    B = var(1:5) = 2.5. Replicate means on implicate 1: rw_1 (11+21+31+41)/4
#    = 26; rw_2 (2*21 + 2*31)/4 = 26; rw_3 = 41. Mean 31; squared deviations
#    25 + 25 + 100 = 150; / (3 - 1) = 75. Total variance 75 + 1.2 * 2.5 = 78.
mean_result <- scf_estimate(
  synthetic_panel, synthetic_repwgts, "value_varying", stat = "mean"
)
expect_close(mean_result$estimate, 34.25, "pooled mean")
expect_close(mean_result$se_imputation, sqrt(2.5), "mean: sqrt(B)")
expect_close(mean_result$se_sampling, sqrt(75), "mean: sampling SE")
expect_close(mean_result$se, sqrt(78), "mean: total SE")
expect_close(mean_result$n_unweighted, 4, "mean: implicate-1 case count")
expect_close(mean_result$n_weighted, 40, "mean: weighted count (5 x 8)")

# 7) Median, first value whose cumulative weight share reaches 0.5. Within
#    implicate i: shares 1/8, 2/8, 4/8 -> the third value, 30 + i (a share of
#    exactly 0.5 takes that value, not an average with the next). Implicate
#    medians 31 ... 35, average 33, B = 2.5. Pooled over implicates (total
#    weight 40): values 11-15 and 21-25 carry 10, then 31, 32, ... add 2 each,
#    reaching 20 = half at 35, so the pooled median is 35.
#    Replicate medians: rw_1 shares 1/4, 2/4 -> 21; rw_2 shares 0, 2/4 -> 21;
#    rw_3 -> 41. Mean 83/3; variance ((20/3)^2 * 2 + (40/3)^2) / 2 = 400/3.
median_result <- scf_estimate(
  synthetic_panel, synthetic_repwgts, "value_varying", stat = "median"
)
expect_close(median_result$estimate, 35, "pooled median")
expect_close(median_result$se_imputation, sqrt(2.5), "median: sqrt(B)")
expect_close(median_result$se_sampling, sqrt(400 / 3), "median: sampling SE")
expect_close(median_result$se, sqrt(400 / 3 + 1.2 * 2.5), "median: total SE")

median_average <- scf_estimate(
  synthetic_panel, synthetic_repwgts, "value_varying", stat = "median",
  pooling = "implicate_average"
)
expect_close(median_average$estimate, 33, "average of implicate medians")
expect_close(median_average$se, median_result$se,
             "median: SE does not depend on pooling")

# 8) A value constant across implicates has no imputation variance: B = 0,
#    so the total SE equals the sampling SE. Replicate means of 10, 20, 30,
#    40: rw_1 25, rw_2 25, rw_3 40; variance (25 + 25 + 100) / 2 = 75.
constant_result <- scf_estimate(
  synthetic_panel, synthetic_repwgts, "value_constant", stat = "mean"
)
expect_close(constant_result$se_imputation, 0, "constant: B = 0")
expect_close(constant_result$se, constant_result$se_sampling,
             "constant: total SE = sampling SE")
expect_close(constant_result$se_sampling, sqrt(75), "constant: sampling SE")

# 9) Share of a logical: (2 + 4) / 8 = 0.75, the same in every implicate.
#    Replicates: rw_1 2/4, rw_2 2/4, rw_3 1; variance (1/36 * 2 + 4/36) / 2
#    = 1/12 (mean 2/3; deviations -1/6, -1/6, 1/3).
share_result <- scf_estimate(
  synthetic_panel, synthetic_repwgts, "above_25_flag", stat = "share"
)
expect_close(share_result$estimate, 0.75, "share estimate")
expect_close(share_result$se_imputation, 0, "share: B = 0")
expect_close(share_result$se_sampling, sqrt(1 / 12), "share: sampling SE")

# 10) Groups, subset, and cond. Group "a" (cases 1, 2; weights 1, 1): pooled
#     mean (10 + 20) / 2 + average(i) = 18. Subset "value_constant > 25" with
#     cond "value_constant < 35" leaves case 3 only: pooled mean 30 + 3 = 33.
group_result <- scf_estimate(
  synthetic_panel, NULL, "value_varying", stat = "mean", by = "group_label"
)
expect_close(group_result[group_label == "a", estimate], 18,
             "by group: group a mean")
expect_close(group_result[group_label == "b", estimate],
             (30 * 2 + 40 * 4) / 6 + 3, "by group: group b mean")
expect_close(sum(is.na(group_result$se_sampling)), 2,
             "no replicate weights: sampling SE is NA")
filtered_result <- scf_estimate(
  synthetic_panel, NULL, "value_varying", stat = "mean",
  subset = "value_constant > 25", cond = "value_constant < 35"
)
expect_close(filtered_result$estimate, 33, "subset and cond")

# 11) Missing values are dropped: setting case 4 to NA in every implicate
#     leaves weights 1, 1, 2: mean (10 + 20 + 60) / 4 + 3 = 25.5.
missing_panel <- data.table::copy(synthetic_panel)
missing_panel[yy1 == 4L, value_varying := NA_real_]
missing_result <- scf_estimate(
  missing_panel, NULL, "value_varying", stat = "mean"
)
expect_close(missing_result$estimate, 25.5, "missing values dropped")

# 12) A group with no weight in a replicate: case 4 alone has weight only in
#     rw_1 and rw_3, so rw_2 is skipped with a warning; its replicate means
#     are 41 and 41, so the sampling variance is 0.
empty_warning_text <- NA_character_
empty_result <- withCallingHandlers(
  scf_estimate(synthetic_panel, synthetic_repwgts, "value_varying",
               stat = "mean", subset = "yy1 == 4L"),
  warning = function(w) {
    empty_warning_text <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
expect_close(grepl("empty in 1 replicate", empty_warning_text), 1,
             "empty replicate: warning raised")
expect_close(empty_result$se_sampling, 0, "empty replicate: others used")

# 13) n_replicates_empty counts the replicates with no weight in the group:
#     case 4 alone has weight 0 in rw_2 only, so 1; all four cases together
#     have weight in rw_1, rw_2, and rw_3, so 0; with no replicate weights
#     the count is NA for both groups of group_result
expect_close(empty_result$n_replicates_empty, 1,
             "n_replicates_empty: one empty replicate counted")
expect_close(mean_result$n_replicates_empty, 0,
             "n_replicates_empty: zero when no replicate is empty")
expect_close(sum(is.na(group_result$n_replicates_empty)), 2,
             "n_replicates_empty: NA without replicate weights")
expect_close(empty_result$estimate,
             scf_estimate(synthetic_panel, NULL, "value_varying",
                          stat = "mean", subset = "yy1 == 4L")$estimate,
             "n_replicates_empty: estimate unchanged by replicate weights")

message("All scf_estimate() unit tests passed.")
