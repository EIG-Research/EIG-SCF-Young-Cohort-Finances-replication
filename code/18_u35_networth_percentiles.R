# 18_u35_networth_percentiles -- under-35 net worth percentiles and band mix
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U2 of records/plans/2026-10-16_draft-v2-replies.md. It reads
#   data/processed/scf_panel_1989_2025.parquet   (written by 02)
#   data/processed/scf_repwgts_1989_2025.parquet (written by 02)
#   output/tables/networth_change_by_age.csv     (written by 12, a check)
#   output/tables/u35_balance_sheet.csv          (written by 12, a check)
# sources the estimator library code/03_scf_estimators.R, and writes, to
# output/tables/:
#   u35_networth_percentiles.csv  percentile (10th-90th): under-35 net worth
#                                 in 2022 and 2025, the change, and the
#                                 percent change with a 95% interval
#   u35_band_composition.csv      year x measure x group: homeownership of
#                                 all under-35 households and of the
#                                 40th-60th band, where owners and renters
#                                 sit relative to the band, and 2025 in the
#                                 2022 dollar band
# Run from the project root as
#   Rscript code/18_u35_networth_percentiles.R
#
# Percentiles follow 03's median rule at other p: the first value at which
# the cumulative WGT share reaches p, over the five pooled implicates. SEs
# follow 03: sampling variance from the 999 replicate weights on implicate
# 1 (variance of the replicate estimates), imputation variance across the
# five implicates, se = sqrt(sampling + (6/5) x imputation). The p50 rows
# must equal 12's medians (step 12 checks).
#
# Caveats that apply to every number written here:
# - Young households EXCLUDE young adults living inside someone else's
#   primary economic unit (PROJECT.md, Additional Context).
# - Dollars are on the project basis `_pce25` (headline PCE price index,
#   2025 dollars).
# - The two waves are independent samples; a percentile's change compares
#   the household at that rank in each wave, not the same households.
# - Band rows hold the band cutoffs fixed, so their SEs are understated
#   (12, step 21).
#
# Exceptions to the EIG R style guide:
# - data.table, not the tidyverse, to match 03's data.table interface.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "data.table", "arrow", "dplyr", "readr",
                       "tidyselect")
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

# data.table column names used unquoted below, declared so that lintr does
# not read them as undefined globals
`:=` <- year <- implicate <- wgt <- young <- networth_pce25 <-
  houses_pce25 <- owner_flag <- cum_share <- cutoff_lo <- cutoff_hi <-
  position <- basis <- age_group <- change <- estimate_2022 <-
  estimate_2025 <- se_2022 <- se_2025 <- se_change <- ci_low <- ci_high <-
  in_2022_band_flag <- below_flag <- in_band_flag <- above_flag <- tenure <-
  measure <- group <- band <- component <- statistic <- NULL

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at code/18_u35_networth_percentiles.R
here::i_am("code/18_u35_networth_percentiles.R")
source(here::here("code", "03_scf_estimators.R"))

path_processed <- here::here("data", "processed")
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
percentile_path <- file.path(path_tables, "u35_networth_percentiles.csv")
composition_path <- file.path(path_tables, "u35_band_composition.csv")

# 3) Waves compared, percentiles (plan O3), 12's band and home-holding rule
#    (a household holds a home when HOUSES is more than 50 cents from zero,
#    12 step 5), and 16's rule for suppressing a percent change: a base
#    that is not positive, or a coefficient of variation above 0.2
compare_years <- c(2022L, 2025L)
percentile_levels <- seq(0.1, 0.9, by = 0.1)
band_p_lo <- 0.40
band_p_hi <- 0.60
holding_threshold <- 0.5
max_base_cv <- 0.2

# 4) Log a timestamped line
log_step <- function(text) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", text)
}

################################################################################
###                              3. Read inputs                              ###
################################################################################

# 5) Under-35 rows of the two waves and their replicate weights (WGT is the
#    SCF weight, registry entry `scf`)
if (!file.exists(panel_path) || !file.exists(repwgts_path)) {
  stop("Panel or replicate-weight parquet missing in ", path_processed,
       "; run code/02_build_scf_panel.R first", call. = FALSE)
}
panel_columns <- c("year", "y1", "yy1", "implicate", "wgt", "young",
                   "networth_pce25", "houses_pce25")
young_panel <- arrow::open_dataset(panel_path) |>
  dplyr::filter(year %in% compare_years) |>
  dplyr::select(tidyselect::all_of(panel_columns)) |>
  dplyr::collect() |>
  data.table::as.data.table()
young_panel <- young_panel[!is.na(young) & young == TRUE]
repwgts <- arrow::open_dataset(repwgts_path) |>
  dplyr::filter(year %in% compare_years) |>
  dplyr::collect() |>
  data.table::as.data.table()
if (anyNA(young_panel$networth_pce25) || anyNA(young_panel$houses_pce25)) {
  stop("Under-35 rows have missing net worth or home value", call. = FALSE)
}
log_step(paste0("Read ", format(nrow(young_panel), big.mark = ","),
                " under-35 household-implicate rows"))

# 6) Match implicate-1 rows to their replicate weights (as 03, step 5a); a
#    missing or negative replicate weight counts as 0 (codebk2025.txt line
#    2305)
rw_cols <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
rw_cols <- rw_cols[order(as.integer(sub("^rw_", "", rw_cols)))]
join_key <- intersect(c("y1", "yy1"), names(repwgts))[1L]
if (length(rw_cols) == 0L || is.na(join_key)) {
  stop("Replicate weights lack rw_ columns or a y1/yy1 key", call. = FALSE)
}

################################################################################
###                        4. Percentiles of net worth                       ###
################################################################################

# 7) For each wave: the pooled percentile (all five implicates), one per
#    implicate (imputation variance), and one per replicate weight on
#    implicate 1 (sampling variance), each the first sorted value at which
#    the cumulative weight share reaches p
percentile_parts <- vector("list", length(compare_years))
for (year_index in seq_along(compare_years)) {
  wave_year <- compare_years[year_index]
  wave_rows <- young_panel[year == wave_year]
  data.table::setorderv(wave_rows, "networth_pce25")

  pooled_share <- cumsum(wave_rows$wgt) / sum(wave_rows$wgt)
  pooled_value <- vapply(
    percentile_levels,
    function(p) wave_rows$networth_pce25[which(pooled_share >= p)[1L]],
    numeric(1L)
  )

  implicate_value <- matrix(NA_real_, nrow = 5L,
                            ncol = length(percentile_levels))
  for (implicate_index in 1L:5L) {
    imp_rows <- wave_rows[implicate == implicate_index]
    imp_share <- cumsum(imp_rows$wgt) / sum(imp_rows$wgt)
    implicate_value[implicate_index, ] <- vapply(
      percentile_levels,
      function(p) imp_rows$networth_pce25[which(imp_share >= p)[1L]],
      numeric(1L)
    )
  }
  imputation_var <- apply(implicate_value, 2L, stats::var)

  first_rows <- wave_rows[implicate == 1L]
  match_index <- repwgts[first_rows, on = c("year", join_key),
                         which = TRUE, mult = "first"]
  if (anyNA(match_index)) {
    stop(sum(is.na(match_index)), " implicate-1 case(s) in ", wave_year,
         " have no replicate weights", call. = FALSE)
  }
  weight_matrix <- as.matrix(repwgts[match_index, rw_cols, with = FALSE])
  weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0
  replicate_share <- apply(weight_matrix, 2L, cumsum)
  replicate_share <- sweep(replicate_share, 2L,
                           replicate_share[nrow(replicate_share), ], "/")
  sampling_var <- vapply(
    percentile_levels,
    function(p) {
      below_count <- colSums(replicate_share < p)
      replicate_value <- first_rows$networth_pce25[
        pmin(below_count + 1L, nrow(first_rows))
      ]
      stats::var(replicate_value)
    },
    numeric(1L)
  )

  percentile_parts[[year_index]] <- data.table::data.table(
    year = wave_year,
    percentile = as.integer(round(100 * percentile_levels)),
    estimate = pooled_value,
    se = sqrt(sampling_var + (6 / 5) * imputation_var),
    se_sampling = sqrt(sampling_var),
    se_imputation = sqrt(imputation_var),
    n_unweighted = nrow(first_rows)
  )
}
percentile_long <- data.table::rbindlist(percentile_parts)
log_step("Estimated the 10th-90th percentiles for 2022 and 2025")

# 8) Check: the 50th percentile and its SE equal 12's under-35 median
age_change <- data.table::as.data.table(readr::read_csv(
  file.path(path_tables, "networth_change_by_age.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  progress = FALSE
))
median_row <- age_change[basis == "pce25" & age_group == "less than 35"]
if (nrow(median_row) != 1L) {
  stop("networth_change_by_age.csv: no single pce25 under-35 row",
       call. = FALSE)
}
for (wave_year in compare_years) {
  ours <- percentile_long[year == wave_year & percentile == 50L]
  theirs_estimate <- as.numeric(median_row[[paste0("estimate_", wave_year)]])
  theirs_se <- as.numeric(median_row[[paste0("se_", wave_year)]])
  if (abs(ours$estimate - theirs_estimate) > 0.01 ||
        abs(ours$se - theirs_se) > 1e-6 * theirs_se) {
    stop("The ", wave_year, " 50th percentile (", ours$estimate, ", SE ",
         ours$se, ") differs from 12's median (", theirs_estimate, ", SE ",
         theirs_se, ")", call. = FALSE)
  }
}
log_step("50th percentiles and SEs match 12's medians")

# 9) Changes, 2022-2025: the level change with its SE and z (independent
#    waves), and the percent change with 12's log-ratio interval, which is
#    suppressed when either base is not positive or has a CV above 0.2
first_wave <- percentile_long[year == compare_years[1L]]
second_wave <- percentile_long[year == compare_years[2L]]
percentile_table <- data.table::data.table(
  percentile = first_wave$percentile,
  estimate_2022 = first_wave$estimate,
  estimate_2025 = second_wave$estimate,
  se_2022 = first_wave$se,
  se_2025 = second_wave$se,
  n_unweighted_2022 = first_wave$n_unweighted,
  n_unweighted_2025 = second_wave$n_unweighted
)
percentile_table[, change := estimate_2025 - estimate_2022]
percentile_table[, se_change := sqrt(se_2022^2 + se_2025^2)]
percentile_table[, z_change := change / se_change]
percentile_table[, pct_suppressed_reason := data.table::fcase(
  estimate_2022 <= 0 | estimate_2025 <= 0, "base not positive",
  se_2022 / estimate_2022 > max_base_cv |
    se_2025 / estimate_2025 > max_base_cv, "base CV above 0.2",
  default = ""
)]
pct_valid_flag <- percentile_table$pct_suppressed_reason == ""
log_ratio <- rep(NA_real_, nrow(percentile_table))
se_log_ratio <- rep(NA_real_, nrow(percentile_table))
log_ratio[pct_valid_flag] <- log(
  percentile_table$estimate_2025[pct_valid_flag] /
    percentile_table$estimate_2022[pct_valid_flag]
)
se_log_ratio[pct_valid_flag] <- sqrt(
  (percentile_table$se_2022[pct_valid_flag] /
     percentile_table$estimate_2022[pct_valid_flag])^2 +
    (percentile_table$se_2025[pct_valid_flag] /
       percentile_table$estimate_2025[pct_valid_flag])^2
)
percentile_table[, pct_change := 100 * (exp(log_ratio) - 1)]
percentile_table[, se_log_ratio := se_log_ratio]
percentile_table[, ci_low := 100 * (exp(log_ratio - 1.96 * se_log_ratio) - 1)]
percentile_table[, ci_high := 100 * (exp(log_ratio + 1.96 * se_log_ratio) - 1)]
percentile_table[, significant_flag := ci_low > 0 | ci_high < 0]
data.table::fwrite(percentile_table, percentile_path)
log_step(paste0("Wrote ", nrow(percentile_table), " rows to ",
                percentile_path))

################################################################################
###                   5. Homeownership around the median band                ###
################################################################################

# 10) Each wave's 40th-60th band cutoffs by 12's rule (pooled implicates)
#     and every household's position: below, in, or above the band
young_panel[, owner_flag := abs(houses_pce25) > holding_threshold]
data.table::setorderv(young_panel, c("year", "networth_pce25"))
young_panel[, cum_share := cumsum(wgt) / sum(wgt), by = year]
band_cutoffs <- young_panel[
  ,
  list(cutoff_lo = networth_pce25[which(cum_share >= band_p_lo)[1L]],
       cutoff_hi = networth_pce25[which(cum_share >= band_p_hi)[1L]]),
  by = year
]
young_panel <- merge(young_panel, band_cutoffs, by = "year", sort = FALSE)
young_panel[, position := data.table::fcase(
  networth_pce25 < cutoff_lo, "below band",
  networth_pce25 > cutoff_hi, "above band",
  default = "in band"
)]

# 11) The 2022 dollar cutoffs applied to both waves (2025 in 2022's band)
cutoffs_2022 <- band_cutoffs[year == compare_years[1L]]
young_panel[, in_2022_band_flag :=
              networth_pce25 >= cutoffs_2022$cutoff_lo &
              networth_pce25 <= cutoffs_2022$cutoff_hi]

# 12) The shares, each by scf_estimate() (03) with fixed cutoffs:
#     homeownership overall, in each position, and in the 2022 dollar band;
#     where owners and where renters sit; and median net worth by tenure
young_panel[, below_flag := position == "below band"]
young_panel[, in_band_flag := position == "in band"]
young_panel[, above_flag := position == "above band"]
young_panel[, tenure := data.table::fifelse(owner_flag, "owners", "renters")]
label_rows <- function(estimate_table, measure, group, note) {
  data.table::set(estimate_table, j = "measure", value = measure)
  data.table::set(estimate_table, j = "group", value = group)
  data.table::set(estimate_table, j = "note", value = note)
  estimate_table
}
composition_parts <- list(
  label_rows(scf_estimate(young_panel, repwgts, "owner_flag", "share"),
             "homeownership rate", "all under-35 households",
             "share owning a home (HOUSES > 0)"),
  label_rows(scf_estimate(young_panel, repwgts, "owner_flag", "share",
                          subset = young_panel$position == "in band"),
             "homeownership rate", "in that year's 40th-60th band",
             "equals u35_balance_sheet.csv home share_holding"),
  label_rows(scf_estimate(young_panel, repwgts, "owner_flag", "share",
                          subset = young_panel$position == "below band"),
             "homeownership rate", "below that year's band", ""),
  label_rows(scf_estimate(young_panel, repwgts, "owner_flag", "share",
                          subset = young_panel$position == "above band"),
             "homeownership rate", "above that year's band", ""),
  label_rows(scf_estimate(young_panel, repwgts, "owner_flag", "share",
                          subset = young_panel$in_2022_band_flag),
             "homeownership rate", "in the 2022 dollar band",
             paste0("net worth between the 2022 cutoffs, ",
                    round(cutoffs_2022$cutoff_lo), " and ",
                    round(cutoffs_2022$cutoff_hi), " (2025 dollars)"))
)
position_labels <- c(below_flag = "below band", in_band_flag = "in band",
                     above_flag = "above band")
for (tenure_group in c("owners", "renters")) {
  tenure_mask <- young_panel$tenure == tenure_group
  for (position_var in names(position_labels)) {
    composition_parts[[length(composition_parts) + 1L]] <- label_rows(
      scf_estimate(young_panel, repwgts, position_var, "share",
                   subset = tenure_mask),
      paste0("share of ", tenure_group), position_labels[[position_var]],
      paste0("share of under-35 ", tenure_group, " whose net worth is ",
             position_labels[[position_var]], " (that year's band)")
    )
  }
  composition_parts[[length(composition_parts) + 1L]] <- label_rows(
    scf_estimate(young_panel, repwgts, "networth_pce25", "median",
                 subset = tenure_mask),
    "median net worth", tenure_group, "2025 dollars (PCE)"
  )
}
composition <- data.table::rbindlist(composition_parts, use.names = TRUE)
composition <- merge(composition, band_cutoffs, by = "year")
composition <- composition[
  ,
  c("year", "measure", "group", "estimate", "se", "se_sampling",
    "se_imputation", "n_unweighted", "n_weighted", "cutoff_lo", "cutoff_hi",
    "note"),
  with = FALSE
]

# 13) Check: the in-band homeownership rate equals 12's home holding share
balance_sheet <- data.table::as.data.table(readr::read_csv(
  file.path(path_tables, "u35_balance_sheet.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  progress = FALSE
))
for (wave_year in compare_years) {
  theirs <- balance_sheet[year == as.character(wave_year) &
                            band == "p40_p60" & component == "home" &
                            statistic == "share_holding"]
  ours <- composition[year == wave_year & measure == "homeownership rate" &
                        group == "in that year's 40th-60th band"]
  if (nrow(theirs) != 1L || nrow(ours) != 1L ||
        abs(ours$estimate - as.numeric(theirs$estimate)) > 1e-9) {
    stop("The ", wave_year, " in-band homeownership rate differs from 12's",
         call. = FALSE)
  }
}
log_step("In-band homeownership matches 12's home holding share")
data.table::fwrite(composition, composition_path)
log_step(paste0("Wrote ", nrow(composition), " rows to ", composition_path))
