# 11_selection_sensitivity -- SCF young-household estimates under selection
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U5 of records/plans/2026-10-11_mr4-headship-selection.md (items H2
# and Q of the U9 review triage), with the bounding method the owner
# approved as O5 (2026-10-11). It reads the SCF panel and replicate weights
# written by 02, 05's young_by_age_band.csv (a wiring check), 08's
# headship_context.csv, 09's headship_cps_age_split.csv, and the CPS person
# checkpoint written by 08, and writes, to output/tables/:
#   selection_sensitivity.csv    SCF wave x band x outcome: the composition
#                                checks (S1, S2) and the Lee bounds (S3)
#   scf_cps_age_split.csv        SCF wave: the share of reference persons
#                                aged 25-34 who are 25-29, SCF against CPS
#                                (Q1, Q3)
#   scf_cps_age_single_year.csv  SCF wave x single year of age 23-36: each
#                                survey's distribution of reference persons
#                                (Q2)
# Run from the project root as
#   Rscript code/11_selection_sensitivity.R
#
# Outcomes: median and mean net worth and median income, on the project's
# PCE 2025-dollar basis (networth_pce25, income_pce25; 02 and 05).
#
# Methods (O5):
# - S1, composition by age: the under-35 estimate beside 25-34 and 30-34
#   alone (and the other SCF bands), from scf_estimate() in 03.
# - S2, CPS age mix: the under-35 estimate with WGT rescaled within each
#   band (under 25, 25-29, 30-34) so that each band's share of the under-35
#   SCF reference persons equals its share of the CPS SCF-rule reference
#   persons in the same wave (08). The under-35 total is unchanged. Each
#   replicate weight is recalibrated the same way, with band factors from
#   that replicate's own band totals on implicate 1 (owner decision on the
#   MR4 review, 2026-10-12), so the SE includes the variability of the SCF
#   band mix. The CPS shares are treated as fixed; their sampling error is
#   not carried into the SE.
# - S3, Lee (2009) trimming bounds on the change in each statistic between
#   a reference wave (1989, 2007) and each other wave, in the cells that
#   match the SCF's age raking cells: under 25, 25-34, and under 35 (owner
#   decision on the MR4 review, 2026-10-12). The SCF rakes 25-34 as one cell
#   (Kennickell 1999, Tables A2a and A12a, documented for 1989-1998), so its
#   25-29 and 30-34 counts are set by the sample and depart from the CPS by
#   up to about 12 percent; bounds for those two bands alone would rest on
#   a headship rate the SCF does not match. ASSUMPTION
#   (monotone selection): anyone who heads an SCF unit in the wave with the
#   lower headship rate would also head one in the wave with the higher
#   rate. ASSUMPTION (comparable populations): the two waves draw young
#   adults from the same distribution of headship type and outcome, which
#   Lee's random assignment supplies and two cross-sections do not; S3 does
#   not adjust for changes in education, nativity, or age mix (S2 adjusts
#   the band mix only). The headship rate is the CPS scf_rule_rp_share for
#   the band (08), treated as fixed. In the higher-headship wave the share
#   (h_high - h_low) / h_high of the weight is trimmed from the top (lower
#   bound on that wave's statistic) or the bottom (upper bound); the other
#   wave is untrimmed. The bounds are on the change for the households that
#   would head a unit in both waves. Weight-basis breaks in the CPS rates
#   (weight_break_flag in 08) carry into the trimming share. Before 1995 the
#   CPS rate counts spouses only as couples (08), so pairs with the 1989
#   reference wave carry partners_identified_both_flag = FALSE.
# - SEs of every estimate follow 03: replicate sampling variance on
#   implicate 1 (999 replicates, divisor 998) plus (6/5) x the variance of
#   the five implicate estimates. The trimmed statistics reuse that method
#   with lee_trim_stat() and lee_trim_estimate() (00_functions.R); with no
#   trimming they must equal scf_estimate() (step 17 checks it). A
#   bound's SE combines the two waves' SEs as independent samples. ci95
#   columns give a conservative interval
#   (lower bound - 1.96 se, upper bound + 1.96 se).
#
# Q (the 25-29/30-34 split): Q1 compares the SCF share of reference persons
# aged 25-34 who are 25-29 (scf_estimate() share, design-based SE) with the
# CPS SCF-rule share and its SE from 09, with a z statistic; Q3 adds the
# CPS share from the ASEC of year t + 1 and the mean of t and t + 1, since
# the SCF is fielded over the survey year and into the next (08 header);
# Q2 tabulates both surveys' reference persons by single year of age.
#
# Caveats: every SCF young-household caveat in 05 applies; these are
# sensitivity analyses under stated assumptions, not corrected estimates.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "data.table", "arrow", "tidyselect")
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

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at code/11_selection_sensitivity.R; run
#    the trimming unit tests in a fresh process and stop if they fail; then
#    source the shared functions (00) and the estimator library (03)
here::i_am("code/11_selection_sensitivity.R")
test_script_path <- here::here("code", "tests", "test_selection_bounds.R")
test_status <- system2(
  file.path(R.home("bin"), "Rscript"), shQuote(test_script_path),
  stdout = FALSE, stderr = FALSE
)
if (!identical(test_status, 0L)) {
  stop("Selection-bound unit tests failed (", test_script_path, "); run ",
       "it alone to see which check", call. = FALSE)
}
message("Selection-bound unit tests passed")
source(here::here("code", "00_functions.R"))
source(here::here("code", "03_scf_estimators.R"))

path_processed <- here::here("data", "processed")
path_tables <- here::here("output", "tables")
panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
persons_path <- file.path(path_processed, "cps_persons_15_44.parquet")
age_table_path <- file.path(path_tables, "young_by_age_band.csv")
headship_path <- file.path(path_tables, "headship_context.csv")
cps_split_path <- file.path(path_tables, "headship_cps_age_split.csv")
sensitivity_output_path <- file.path(path_tables, "selection_sensitivity.csv")
split_output_path <- file.path(path_tables, "scf_cps_age_split.csv")
single_year_output_path <- file.path(
  path_tables, "scf_cps_age_single_year.csv"
)

# 3) SCF bands (reference person's age, no lower bound, as in 02 and 08);
#    the bands with a CPS headship rate in 08 are the Lee cells
scf_bands <- data.table::data.table(
  band = c("under 25", "25-29", "30-34", "under 35", "25-34", "35-44"),
  age_min = c(0L, 25L, 30L, 0L, 25L, 35L),
  age_max = c(24L, 29L, 34L, 34L, 34L, 44L)
)
under_35_parts <- c("under 25", "25-29", "30-34")
lee_bands <- c("under 25", "25-34", "under 35")
partner_first_year <- 1995L
reference_years <- c(1989L, 2007L)

# 4) Outcomes and statistics (PCE 2025 dollars)
outcome_table <- data.table::data.table(
  outcome = c("networth", "networth", "income"),
  var = c("networth_pce25", "networth_pce25", "income_pce25"),
  stat = c("median", "mean", "median")
)

# 5) Q: single years of age tabulated, and the SCF-wave years
single_year_ages <- 23L:36L
ci_multiplier <- 1.96

run_start <- Sys.time()

################################################################################
###                              3. Read inputs                              ###
################################################################################

# 6) SCF panel (needed columns) and replicate weights
panel <- data.table::as.data.table(arrow::read_parquet(
  panel_path,
  col_select = tidyselect::all_of(c(
    "year", "y1", "implicate", "wgt", "age", "networth_pce25",
    "income_pce25"
  ))
))
repwgts <- data.table::as.data.table(arrow::read_parquet(repwgts_path))
replicate_columns <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
replicate_columns <- replicate_columns[
  order(as.integer(sub("^rw_", "", replicate_columns)))
]
wave_years <- sort(unique(panel$year))
message("Read ", format(nrow(panel), big.mark = ","), " panel rows and ",
        length(replicate_columns), " replicate weights")

# 7) CPS: headship rates by band (08), with 25-34 built from 08's 25-29 and
#    30-34 rows (reference persons over persons), and the 25-29 split (09)
headship_context <- data.table::fread(headship_path)
cps_split <- data.table::fread(cps_split_path)
cps_rates <- headship_context[
  scf_wave == TRUE,
  list(year, band = age_band, scf_rule_rp_share,
       cps_rp_weighted = scf_rule_rp_share * population_weighted,
       population_weighted, weight_break_flag)
]
cps_rates_25_34 <- cps_rates[
  band %in% c("25-29", "30-34"),
  list(band = "25-34",
       cps_rp_weighted = sum(cps_rp_weighted),
       population_weighted = sum(population_weighted),
       weight_break_flag = any(weight_break_flag)),
  by = year
]
cps_rates_25_34[, scf_rule_rp_share := cps_rp_weighted / population_weighted]
cps_rates <- data.table::rbindlist(list(cps_rates, cps_rates_25_34),
                                   use.names = TRUE)
if (nrow(cps_rates[band %in% lee_bands]) !=
      length(wave_years) * length(lee_bands)) {
  stop("headship_context.csv lacks a CPS rate for some SCF wave and band",
       call. = FALSE)
}

################################################################################
###                    4. S1: SCF estimates by age band                      ###
################################################################################

# 8) Stack the panel once per band (a row appears in each band it falls in)
band_design_list <- vector("list", nrow(scf_bands))
for (band_index in seq_len(nrow(scf_bands))) {
  band_mask <- panel$age >= scf_bands$age_min[band_index] &
    panel$age <= scf_bands$age_max[band_index]
  band_part <- panel[band_mask]
  band_part[, band := scf_bands$band[band_index]]
  band_design_list[[band_index]] <- band_part
}
band_design <- data.table::rbindlist(band_design_list)
rm(band_design_list)

# 9) One scf_estimate() call per outcome, grouped by year and band
s1_list <- vector("list", nrow(outcome_table))
for (outcome_index in seq_len(nrow(outcome_table))) {
  s1_estimate <- scf_estimate( # nolint: object_usage_linter.
    band_design, repwgts,
    var = outcome_table$var[outcome_index],
    stat = outcome_table$stat[outcome_index], by = "band"
  )
  s1_estimate[, outcome := outcome_table$outcome[outcome_index]]
  s1_estimate[, statistic := outcome_table$stat[outcome_index]]
  s1_list[[outcome_index]] <- s1_estimate
}
s1 <- data.table::rbindlist(s1_list)
message("S1: ", nrow(s1), " band estimates")

# 10) Wiring check: the under-35 rows reproduce 05's young_by_age_band.csv
age_table <- data.table::fread(age_table_path)
wiring <- merge(
  s1[band == "under 35"],
  age_table[age_group == "under 35" & basis == "pce25" &
              outcome %in% outcome_table$outcome,
            list(year, outcome, statistic,
                 estimate_05 = estimate, se_05 = se)],
  by = c("year", "outcome", "statistic")
)
if (nrow(wiring) != 3L * length(wave_years) ||
      any(abs(wiring$estimate - wiring$estimate_05) >
            1e-6 * pmax(1, abs(wiring$estimate_05))) ||
      any(abs(wiring$se - wiring$se_05) > 1e-6 * pmax(1, wiring$se_05))) {
  stop("S1 under-35 estimates do not reproduce young_by_age_band.csv",
       call. = FALSE)
}
message("Check passed: S1 under-35 rows reproduce 05 (", nrow(wiring),
        " estimates)")

################################################################################
###                    5. S2: under 35 at the CPS age mix                    ###
################################################################################

# 11) Band factors by wave: CPS band share of under-35 reference persons
#     over the SCF band share (pooled WGT, all five implicates)
panel[, under_35_part := data.table::fcase(
  age <= 24, "under 25",
  age >= 25 & age <= 29, "25-29",
  age >= 30 & age <= 34, "30-34",
  default = NA_character_
)]
scf_mix <- panel[!is.na(under_35_part),
                 list(scf_rp_weighted = sum(wgt)),
                 by = list(year, band = under_35_part)]
scf_mix[, scf_mix_share := scf_rp_weighted / sum(scf_rp_weighted),
        by = year]
cps_mix <- cps_rates[band %in% under_35_parts,
                     list(year, band, cps_rp_weighted)]
cps_mix[, cps_mix_share := cps_rp_weighted / sum(cps_rp_weighted), by = year]
mix <- merge(scf_mix, cps_mix, by = c("year", "band"))
if (nrow(mix) != length(wave_years) * length(under_35_parts)) {
  stop("S2: a wave lacks an SCF or CPS band share", call. = FALSE)
}
mix[, mix_factor := cps_mix_share / scf_mix_share]

# 12) Rescale WGT on every under-35 row; check that the under-35 total is
#     unchanged
s2_panel <- panel[!is.na(under_35_part)]
s2_panel <- merge(
  s2_panel, mix[, list(year, under_35_part = band, mix_factor)],
  by = c("year", "under_35_part"), sort = FALSE
)
total_before <- s2_panel[, list(total = sum(wgt)), by = year]
s2_panel[, wgt := wgt * mix_factor]
total_after <- s2_panel[, list(total = sum(wgt)), by = year]
if (any(abs(total_after$total / total_before$total - 1) > 1e-9)) {
  stop("S2 rescaling changed the under-35 weighted total", call. = FALSE)
}
# 13) Recalibrate each replicate: within each wave, the band totals of
#     replicate r over the implicate-1 under-35 cases (03 estimates
#     sampling variance on implicate 1; missing or negative replicate
#     weights count as 0, as in 03) give replicate r's own band factors,
#     cps_mix_share / replicate band share. Check that every replicate's
#     under-35 total is unchanged
case_band <- s2_panel[implicate == 1L, list(year, y1, under_35_part)]
s2_repwgts <- merge(repwgts, case_band, by = c("year", "y1"), sort = FALSE)
s2_replicate_matrix <- as.matrix(s2_repwgts[, replicate_columns,
                                            with = FALSE])
s2_replicate_matrix[is.na(s2_replicate_matrix) |
                      s2_replicate_matrix < 0] <- 0
for (wave_year in wave_years) {
  wave_mask <- s2_repwgts$year == wave_year
  wave_band <- s2_repwgts$under_35_part[wave_mask]
  wave_weights <- s2_replicate_matrix[wave_mask, , drop = FALSE]
  band_totals <- rowsum(wave_weights, wave_band)
  band_shares <- sweep(band_totals, 2L, colSums(band_totals), "/")
  wave_mix <- mix[year == wave_year]
  cps_share <- wave_mix$cps_mix_share[
    match(rownames(band_shares), wave_mix$band)
  ]
  replicate_factors <- cps_share / band_shares
  if (anyNA(replicate_factors) || any(!is.finite(replicate_factors))) {
    stop("S2: a replicate has no weight in an under-35 band in ",
         wave_year, call. = FALSE)
  }
  adjusted <- wave_weights *
    replicate_factors[match(wave_band, rownames(band_shares)), ,
                      drop = FALSE]
  if (any(abs(colSums(adjusted) / colSums(wave_weights) - 1) > 1e-9)) {
    stop("S2 recalibration changed a replicate's under-35 total in ",
         wave_year, call. = FALSE)
  }
  s2_replicate_matrix[wave_mask, ] <- adjusted
}
for (column_index in seq_along(replicate_columns)) {
  data.table::set(s2_repwgts, j = replicate_columns[column_index],
                  value = s2_replicate_matrix[, column_index])
}
rm(s2_replicate_matrix)
s2_panel[, band := "under 35"]

# 14) The reweighted under-35 estimates
s2_list <- vector("list", nrow(outcome_table))
for (outcome_index in seq_len(nrow(outcome_table))) {
  s2_estimate <- scf_estimate( # nolint: object_usage_linter.
    s2_panel, s2_repwgts,
    var = outcome_table$var[outcome_index],
    stat = outcome_table$stat[outcome_index], by = "band"
  )
  s2_estimate[, outcome := outcome_table$outcome[outcome_index]]
  s2_estimate[, statistic := outcome_table$stat[outcome_index]]
  s2_list[[outcome_index]] <- s2_estimate
}
s2 <- data.table::rbindlist(s2_list)
s2 <- merge(
  s2,
  mix[, list(scf_25_29_share = scf_mix_share[band == "25-29"] /
               sum(scf_mix_share[band != "under 25"]),
             cps_25_29_share = cps_mix_share[band == "25-29"] /
               sum(cps_mix_share[band != "under 25"])),
      by = year],
  by = "year"
)
rm(s2_repwgts)
message("S2: ", nrow(s2), " reweighted under-35 estimates")

################################################################################
###                     6. S3: Lee bounds on the change                      ###
################################################################################

# 15) Replicate weights keyed by case, for lee_trim_estimate() (00), which
#     gives a trimmed statistic and its SE by the method of 03
replicate_lookup <- repwgts
data.table::setkey(replicate_lookup, year, y1)

# 16) Untrimmed statistics by wave, band, and outcome
lee_rows <- data.table::CJ(
  year = wave_years, band = lee_bands, outcome_index = seq_len(3L)
)
untrimmed_values <- matrix(NA_real_, nrow(lee_rows), 2L)
for (row_index in seq_len(nrow(lee_rows))) {
  band_spec <- scf_bands[band == lee_rows$band[row_index]]
  wave_rows <- panel[year == lee_rows$year[row_index] &
                       age >= band_spec$age_min & age <= band_spec$age_max]
  untrimmed_row <- lee_trim_estimate( # nolint: object_usage_linter.
    wave_rows, replicate_lookup, replicate_columns,
    outcome_table$var[lee_rows$outcome_index[row_index]],
    outcome_table$stat[lee_rows$outcome_index[row_index]], 0, "drop_top"
  )
  untrimmed_values[row_index, ] <- untrimmed_row
}
lee_rows[, `:=`(estimate = untrimmed_values[, 1L],
                se = untrimmed_values[, 2L])]
lee_rows[, outcome := outcome_table$outcome[outcome_index]]
lee_rows[, statistic := outcome_table$stat[outcome_index]]

# 17) Check: with no trimming the method reproduces scf_estimate() (S1)
untrimmed_check <- merge(
  lee_rows, s1,
  by = c("year", "band", "outcome", "statistic"),
  suffixes = c("_lee", "_s1")
)
if (nrow(untrimmed_check) != nrow(lee_rows) ||
      any(abs(untrimmed_check$estimate_lee - untrimmed_check$estimate_s1) >
            1e-6 * pmax(1, abs(untrimmed_check$estimate_s1))) ||
      any(abs(untrimmed_check$se_lee - untrimmed_check$se_s1) >
            1e-6 * pmax(1, untrimmed_check$se_s1))) {
  stop("Untrimmed Lee statistics do not reproduce scf_estimate()",
       call. = FALSE)
}
message("Check passed: untrimmed statistics reproduce scf_estimate() (",
        nrow(untrimmed_check), " cells)")

# 18) For each reference wave, band, outcome, and other wave: the trimming
#     share and the trimmed wave, then the bounds on the change
lee_pairs <- data.table::CJ(
  reference_year = reference_years, year = wave_years, band = lee_bands,
  outcome_index = seq_len(3L)
)[year != reference_year]
lee_pairs <- merge(
  lee_pairs,
  cps_rates[, list(reference_year = year, band,
                   headship_reference = scf_rule_rp_share)],
  by = c("reference_year", "band")
)
lee_pairs <- merge(
  lee_pairs,
  cps_rates[, list(year, band, headship_wave = scf_rule_rp_share)],
  by = c("year", "band")
)
lee_pairs[, trimmed_wave := data.table::fifelse(
  headship_wave <= headship_reference, reference_year, year
)]
lee_pairs[, trim_share := abs(headship_reference - headship_wave) /
            pmax(headship_reference, headship_wave)]
if (any(lee_pairs$trim_share < 0 | lee_pairs$trim_share >= 1)) {
  stop("A Lee trimming share is outside [0, 1)", call. = FALSE)
}
bound_values <- matrix(NA_real_, nrow(lee_pairs), 4L)
for (pair_index in seq_len(nrow(lee_pairs))) {
  band_spec <- scf_bands[band == lee_pairs$band[pair_index]]
  trimmed_rows <- panel[year == lee_pairs$trimmed_wave[pair_index] &
                          age >= band_spec$age_min &
                          age <= band_spec$age_max]
  outcome_index <- lee_pairs$outcome_index[pair_index]
  for (side_index in 1:2) {
    side <- c("drop_top", "drop_bottom")[side_index]
    bound_values[pair_index, c(2L * side_index - 1L, 2L * side_index)] <-
      lee_trim_estimate( # nolint: object_usage_linter.
        trimmed_rows, replicate_lookup, replicate_columns,
        outcome_table$var[outcome_index],
        outcome_table$stat[outcome_index],
        lee_pairs$trim_share[pair_index], side
      )
  }
}
lee_pairs[, `:=`(
  trimmed_low = bound_values[, 1L], se_trimmed_low = bound_values[, 2L],
  trimmed_high = bound_values[, 3L], se_trimmed_high = bound_values[, 4L]
)]

# 19) Join the untrimmed statistic of each wave and form the bounds: when
#     the reference wave is trimmed, change = wave - [high, low] of the
#     reference; when the other wave is trimmed, change = [low, high] of
#     that wave - reference
untrimmed_lookup <- lee_rows[, list(year, band, outcome_index,
                                    estimate, se)]
lee_pairs <- merge(
  lee_pairs, untrimmed_lookup,
  by = c("year", "band", "outcome_index")
)
lee_pairs <- merge(
  lee_pairs,
  untrimmed_lookup[, list(reference_year = year, band, outcome_index,
                          estimate_reference = estimate,
                          se_reference = se)],
  by = c("reference_year", "band", "outcome_index")
)
reference_trimmed_flag <- lee_pairs$trimmed_wave == lee_pairs$reference_year
lee_pairs[, change_unadjusted := estimate - estimate_reference]
lee_pairs[, se_change_unadjusted := sqrt(se^2 + se_reference^2)]
lee_pairs[, lower_bound := data.table::fifelse(
  reference_trimmed_flag, estimate - trimmed_high,
  trimmed_low - estimate_reference
)]
lee_pairs[, upper_bound := data.table::fifelse(
  reference_trimmed_flag, estimate - trimmed_low,
  trimmed_high - estimate_reference
)]
lee_pairs[, se_lower := data.table::fifelse(
  reference_trimmed_flag, sqrt(se^2 + se_trimmed_high^2),
  sqrt(se_trimmed_low^2 + se_reference^2)
)]
lee_pairs[, se_upper := data.table::fifelse(
  reference_trimmed_flag, sqrt(se^2 + se_trimmed_low^2),
  sqrt(se_trimmed_high^2 + se_reference^2)
)]
lee_pairs[, ci95_lower := lower_bound - ci_multiplier * se_lower]
lee_pairs[, ci95_upper := upper_bound + ci_multiplier * se_upper]
lee_pairs[, outcome := outcome_table$outcome[outcome_index]]
lee_pairs[, statistic := outcome_table$stat[outcome_index]]

# 20) Check: lower bound <= upper bound everywhere, and the bounds contain
#     the unadjusted change whenever the trimming share is zero
if (anyNA(lee_pairs$lower_bound) ||
      any(lee_pairs$lower_bound > lee_pairs$upper_bound + 1e-9)) {
  stop("A Lee lower bound is missing or above its upper bound",
       call. = FALSE)
}
zero_trim <- lee_pairs[trim_share == 0]
if (any(abs(zero_trim$lower_bound - zero_trim$change_unadjusted) > 1e-9)) {
  stop("Zero-trim bounds differ from the unadjusted change", call. = FALSE)
}
message("S3: ", nrow(lee_pairs), " bounded changes; trimming shares ",
        round(min(lee_pairs$trim_share), 3), "-",
        round(max(lee_pairs$trim_share), 3))

################################################################################
###                 7. Q: the 25-29/30-34 split, SCF and CPS                 ###
################################################################################

# 21) Q1: SCF share of reference persons aged 25-34 who are 25-29, with the
#     design-based SE of 03
split_design <- panel[age >= 25 & age <= 34]
split_design[, age_25_29_flag := as.numeric(age <= 29)]
split_design[, band := "25-34"]
scf_split <- scf_estimate( # nolint: object_usage_linter.
  split_design, repwgts,
  var = "age_25_29_flag", stat = "share", by = "band"
)

# 22) Q1 and Q3: join the CPS share for ASEC t (09), and for ASEC t + 1 and
#     the mean of t and t + 1; z for the SCF-CPS difference at ASEC t
cps_split_next <- cps_split[, list(
  year = year - 1L, cps_share_next = cps_share_25_29, se_next = se
)]
age_split <- merge(
  scf_split[, list(year, scf_share_25_29 = estimate, scf_se = se,
                   scf_n_unweighted = n_unweighted)],
  cps_split[, list(year, cps_share_25_29, cps_se = se, cps_se_method =
                     se_method)],
  by = "year"
)
age_split <- merge(age_split, cps_split_next, by = "year", all.x = TRUE)
age_split[, difference := scf_share_25_29 - cps_share_25_29]
age_split[, z := difference / sqrt(scf_se^2 + cps_se^2)]
age_split[, cps_share_mean_t_t1 := (cps_share_25_29 + cps_share_next) / 2]
age_split[, difference_vs_mean_t_t1 := scf_share_25_29 - cps_share_mean_t_t1]
if (nrow(age_split) != length(wave_years)) {
  stop("Q1: the SCF and CPS splits do not match one to one by wave",
       call. = FALSE)
}

# 23) Q2: single-year age distributions of reference persons aged 23-36,
#     SCF (pooled WGT) and CPS (SCF-rule reference persons, ASEC t)
scf_single <- panel[age %in% single_year_ages,
                    list(scf_weighted = sum(wgt)), by = list(year, age)]
persons <- data.table::as.data.table(arrow::read_parquet(
  persons_path,
  col_select = c("YEAR", "AGE", "person_weight", "scf_rule_rp_flag")
))
cps_single <- persons[
  scf_rule_rp_flag & YEAR %in% wave_years & AGE %in% single_year_ages,
  list(cps_weighted = sum(person_weight)),
  by = list(year = YEAR, age = AGE)
]
single_year <- merge(scf_single, cps_single, by = c("year", "age"),
                     all = TRUE)
single_year[is.na(scf_weighted), scf_weighted := 0]
single_year[is.na(cps_weighted), cps_weighted := 0]
single_year[, scf_share := scf_weighted / sum(scf_weighted), by = year]
single_year[, cps_share := cps_weighted / sum(cps_weighted), by = year]
single_year[, scf_cps_ratio := scf_share / cps_share]

################################################################################
###                       8. Write the tables and log                        ###
################################################################################

# 24) selection_sensitivity.csv: S1 and S2 levels, then S3 bounds, in one
#     long table (method says which)
s1_rows <- s1[, list(
  year, reference_year = NA_integer_, band, outcome, statistic,
  method = "S1 band estimate", estimate, se, n_unweighted,
  partners_identified_both_flag = NA
)]
s2_rows <- s2[, list(
  year, reference_year = NA_integer_, band, outcome, statistic,
  method = "S2 under 35 at the CPS age mix", estimate, se, n_unweighted,
  scf_25_29_share, cps_25_29_share,
  partners_identified_both_flag = year >= partner_first_year
)]
s3_rows <- lee_pairs[, list(
  year, reference_year, band, outcome, statistic,
  method = "S3 Lee bounds on the change from the reference wave",
  estimate = change_unadjusted, se = se_change_unadjusted,
  lower_bound, upper_bound, se_lower, se_upper, ci95_lower, ci95_upper,
  trimmed_wave, trim_share, headship_reference, headship_wave,
  partners_identified_both_flag = year >= partner_first_year &
    reference_year >= partner_first_year
)]
sensitivity <- data.table::rbindlist(
  list(s1_rows, s2_rows, s3_rows),
  use.names = TRUE, fill = TRUE
)
sensitivity <- merge(
  sensitivity,
  cps_rates[band == "under 35", list(year, weight_break_flag)],
  by = "year", all.x = TRUE, sort = FALSE
)
sensitivity[, basis := "pce25"]
sensitivity[, band := factor(band, levels = scf_bands$band)]
data.table::setorder(sensitivity, method, outcome, statistic, band,
                     reference_year, year, na.last = TRUE)
data.table::fwrite(sensitivity, sensitivity_output_path)
message("Wrote ", nrow(sensitivity), " rows to ", sensitivity_output_path)

# 25) scf_cps_age_split.csv and scf_cps_age_single_year.csv
data.table::setorder(age_split, year)
data.table::fwrite(age_split, split_output_path)
message("Wrote ", nrow(age_split), " rows to ", split_output_path)
data.table::setorder(single_year, year, age)
data.table::fwrite(single_year, single_year_output_path)
message("Wrote ", nrow(single_year), " rows to ", single_year_output_path)

# 26) Diagnostic summary: under-35 median net worth, 1989 to 2025
summary_s3 <- lee_pairs[
  band == "under 35" & outcome == "networth" & statistic == "median" &
    reference_year == 1989L & year == 2025L
]
message(sprintf(
  paste0("Under-35 median net worth, 1989 to 2025: unadjusted change %.0f;",
         " Lee bounds [%.0f, %.0f] (trimmed wave %d, share %.3f)"),
  summary_s3$change_unadjusted, summary_s3$lower_bound,
  summary_s3$upper_bound, summary_s3$trimmed_wave, summary_s3$trim_share
))
message(sprintf(
  "Q1: SCF minus CPS 25-29 split, mean over waves from 1995: %.3f",
  mean(age_split[year >= 1995L, difference])
))
message("Finished in ", round(as.numeric(difftime(
  Sys.time(), run_start, units = "mins"
)), 2), " minutes")
