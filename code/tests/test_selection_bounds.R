# test_selection_bounds -- hand-checked unit tests for lee_trim_stat()
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U5 of records/plans/2026-10-11_mr4-headship-selection.md. Run from the
# project root as
#   Rscript code/tests/test_selection_bounds.R
# It stops at the first failed check, naming it. Every expected value below
# is worked out by hand in the comment beside it; none is computed by the
# code under test. 11 runs this file first and stops if it fails.

################################################################################
###                       1. Load packages and the function                  ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here")
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
#    the shared functions and the estimator library (functions only)
here::i_am("code/tests/test_selection_bounds.R")
source(here::here("code", "00_functions.R"))
source(here::here("code", "03_scf_estimators.R"))

# 3) Compare values with their hand calculation; stop with the label if not
expect_equal <- function(actual, expected, label) {
  if (length(actual) != length(expected) ||
        any(abs(actual - expected) > 1e-9)) {
    stop("FAILED: ", label, ": got ", paste(actual, collapse = ", "),
         "; expected ", paste(expected, collapse = ", "), call. = FALSE)
  }
  message("ok: ", label)
}

################################################################################
###                                2. Tests                                  ###
################################################################################

# 4) Values 1-10, weight 1 each (total 10)
values <- 1:10
unit_weights <- rep(1, 10L)

# No trimming: mean 5.5; median = first value with cumulative weight >= 5,
# which is 5
expect_equal(lee_trim_stat(values, unit_weights, 0, "drop_top", "mean"),
             5.5, "no trim, mean")
expect_equal(lee_trim_stat(values, unit_weights, 0, "drop_top", "median"),
             5, "no trim, median")
expect_equal(lee_trim_stat(values, unit_weights, 0, "drop_bottom", "mean"),
             5.5, "no trim from the bottom, mean")

# Trim 20 percent from the top: keep 1-8 (8 units); mean 36 / 8 = 4.5;
# median = first value with cumulative kept weight >= 4, which is 4
expect_equal(lee_trim_stat(values, unit_weights, 0.2, "drop_top", "mean"),
             4.5, "drop top 20 percent, mean")
expect_equal(lee_trim_stat(values, unit_weights, 0.2, "drop_top", "median"),
             4, "drop top 20 percent, median")

# Trim 20 percent from the bottom: keep 3-10; mean 52 / 8 = 6.5; median =
# first value whose cumulative kept weight (from 3 up) reaches 4: 3, 4, 5, 6
expect_equal(lee_trim_stat(values, unit_weights, 0.2, "drop_bottom", "mean"),
             6.5, "drop bottom 20 percent, mean")
expect_equal(lee_trim_stat(values, unit_weights, 0.2, "drop_bottom",
                           "median"),
             6, "drop bottom 20 percent, median")

# Fractional boundary: trim 25 percent from the top keeps 7.5 units: 1-7 in
# full and half of 8; mean (28 + 4) / 7.5 = 32 / 7.5
expect_equal(lee_trim_stat(values, unit_weights, 0.25, "drop_top", "mean"),
             32 / 7.5, "drop top 25 percent, fractional boundary, mean")
# Median: cumulative kept weight reaches 3.75 at value 4
expect_equal(lee_trim_stat(values, unit_weights, 0.25, "drop_top", "median"),
             4, "drop top 25 percent, fractional boundary, median")

# 5) Unequal weights: values 1, 2, 3 with weights 1, 1, 2 (total 4). Trim 50
#    percent from the top keeps 2 units: values 1 and 2; mean 1.5; median 1
#    (cumulative 1 reaches half of 2). Trim 50 percent from the bottom keeps
#    the 2 units of value 3; mean 3, median 3
unequal_weights <- c(1, 1, 2)
expect_equal(lee_trim_stat(1:3, unequal_weights, 0.5, "drop_top", "mean"),
             1.5, "unequal weights, drop top, mean")
expect_equal(lee_trim_stat(1:3, unequal_weights, 0.5, "drop_top", "median"),
             1, "unequal weights, drop top, median")
expect_equal(lee_trim_stat(1:3, unequal_weights, 0.5, "drop_bottom", "mean"),
             3, "unequal weights, drop bottom, mean")

# 6) Several weight columns at once, each trimmed by its own total: column 1
#    as in step 4 (drop top 20 percent: mean 4.5); column 2 puts all weight
#    on values 9 and 10 (weights 1, 1), so dropping the top 20 percent keeps
#    1.6 units: 1 of value 9 and 0.6 of value 10; mean (9 + 6) / 1.6
two_columns <- cbind(unit_weights, c(rep(0, 8L), 1, 1))
expect_equal(lee_trim_stat(values, two_columns, 0.2, "drop_top", "mean"),
             c(4.5, 15 / 1.6), "two weight columns, drop top 20 percent")

# 7) With no trimming, the median agrees with 03's replicate median on the
#    same columns (both follow the codebook's convention)
expect_equal(
  lee_trim_stat(values, two_columns, 0, "drop_top", "median"),
  scf_replicate_medians(as.numeric(values), two_columns),
  "no trim agrees with scf_replicate_medians()"
)

# 8) A column with no weight gives NA; a bad trim share stops
empty_result <- lee_trim_stat(values, cbind(unit_weights, 0), 0.1,
                              "drop_top", "mean")
if (!is.na(empty_result[2L])) {
  stop("FAILED: a column with no weight should give NA", call. = FALSE)
}
message("ok: empty column gives NA")
bad_share <- tryCatch(
  lee_trim_stat(values, unit_weights, 1, "drop_top", "mean"),
  error = function(e) "error"
)
if (!identical(bad_share, "error")) {
  stop("FAILED: trim_share = 1 should stop", call. = FALSE)
}
message("ok: trim_share = 1 stops")

message("All selection-bound tests passed.")
