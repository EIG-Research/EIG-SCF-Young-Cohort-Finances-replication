# 05_young_cohort_descriptives -- diagnostic tables for young households
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U6 (tables) of records/plans/2026-10-11_stage1-scf-pipeline.md, with
# O2 and the amendments. These are diagnostic tables, not a draft. It reads
#   data/processed/scf_panel_1989_2025.parquet   (written by 02)
#   data/processed/scf_repwgts_1989_2025.parquet (written by 02)
# sources the estimator library code/03_scf_estimators.R and the shared
# break table in code/00_functions.R, and writes, to output/tables/:
#   young_by_age_band.csv  year x age group (under 25, 25-29, 30-34,
#                          under 35 pooled, 35-44)
#   young_by_cut.csv       under-35 households, year x each cut
#   cohort_by_age.csv      year x five-year birth band, all birth bands
#                          (age_topcode_flag = TRUE when the band holds a
#                          reference person at the AGE top-code of 95)
#   fig_inputs_young.csv   a small tidy file for the figure step, with the
#                          tiny_cell flag that 06 filters on
# Run from the project root as
#   Rscript code/05_young_cohort_descriptives.R
#
# Caveats that apply to every number written here:
# - The SCF unit is the primary economic unit (PEU), and age, education, and
#   sex describe its reference person. Young adults living inside someone
#   else's PEU (for example with their parents) are not observed as their
#   own household, so "young households" EXCLUDE them and are a selected
#   group whose composition shifts as co-residence changes (PROJECT.md,
#   Additional Context).
# - Race and ethnicity (RACECL4) describe the survey RESPONDENT, not the
#   reference person (2025 bulletin, footnote 11), and 2025 is a coding
#   break (race_2025_break = TRUE on the race rows of 2025; kept for
#   compatibility, and the 1998, 2004, and 2025 race breaks are also in
#   `definition_break`).
# - Some outcomes and cuts change definition across waves. `definition_break`
#   names the documented change on every row of the first wave measured
#   under the new definition (step 12, from scf_definition_breaks() in
#   00_functions.R); NA when there is none. A comparison
#   across that wave and the one before it mixes real change with the
#   measurement change.
# - Every dollar value is on the project basis `_pce25`: headline PCE
#   chain-type price index, 2025 dollars (O1; assets and debts deflated with
#   the survey-year index, income with the prior-year index). The `basis`
#   column says "pce25" for dollars, "share" for proportions (0-1), and
#   "years" for age. These are not the Fed's CPI-U-RS real figures.
#
# Method: every estimate comes from scf_estimate() in 03, with its default
# pooling = "pooled" (WGT-weighted statistic over the five stacked
# implicates) and se^2 = replicate sampling variance on implicate 1 +
# (6/5) x between-implicate variance. n_unweighted is the implicate-1 case
# count; n_weighted is the sum of WGT over all five implicates (households).
# small_cell = TRUE when n_unweighted < 100 (O2); such rows are kept.
# tiny_cell = TRUE when n_unweighted <= 10, the size at which the Fed's tables
# print "*" instead of a value; replicate SEs of such cells are unreliable.
# n_replicates_empty is the number of the 999 replicates in which the cell
# has no weight; its sampling variance uses the other replicates (03).
#
# Design-effect check for shares (MR3 of the 2026-10-11 methodology review):
# se_srs = sqrt(p (1 - p) / n_unweighted) is the simple-random-sampling SE
# implied by the estimate and the implicate-1 case count, and
# se_design_ratio = se / se_srs. The SCF's oversample of wealthy families
# usually puts this ratio near or above 1. A ratio below 0.5 (se_low_flag)
# marks a share whose replicate SE is understated: the weights (X42001) and
# every replicate weight are calibrated to CPS control totals, including
# homeownership, so a share that reproduces a control total, such as the
# under-35 homeownership rate, barely varies across replicates. Its SE leaves
# out the CPS's own sampling error and is not the precision of an
# independent SCF measurement. Rows other than shares have NA in se_srs and
# se_design_ratio and FALSE in se_low_flag.
# A cell with no case (n_unweighted = 0, for example no holder of an item in
# implicate 1) has no standard error; it is dropped and the count logged.
# Any other missing standard error stops the script.
#
# Statistics (column `statistic`):
#   mean, median      over all households in the group (totals, age)
#   share_holding     share of households holding the item (macro flag)
#   median_holders    median value among households holding the item
#   share             homeownership rate
#
# Exceptions to the EIG R style guide:
# - data.table, not the tidyverse, to match 03's data.table interface.
# - Per-wave results are cached as .rds only (section 7, non-tabular list
#   objects), in data/processed/descriptives_cache/ (gitignored). The cache
#   key hashes the inputs and the code of 03 and 05 (step 17).
# - Two short helpers (section 4) only orchestrate calls to scf_estimate();
#   weights and variance stay in 03, and the outcome and sample definitions
#   stay visible in section 3.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "data.table", "arrow", "digest",
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

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at code/05_young_cohort_descriptives.R
here::i_am("code/05_young_cohort_descriptives.R")
source(here::here("code", "03_scf_estimators.R"))
source(here::here("code", "00_functions.R"))

path_processed <- here::here("data", "processed")
path_cache <- file.path(path_processed, "descriptives_cache")
path_tables <- here::here("output", "tables")
dir.create(path_cache, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
age_table_path <- file.path(path_tables, "young_by_age_band.csv")
cut_table_path <- file.path(path_tables, "young_by_cut.csv")
cohort_table_path <- file.path(path_tables, "cohort_by_age.csv")
fig_inputs_path <- file.path(path_tables, "fig_inputs_young.csv")

# 3) Cache version, a label for the log. The cache key (step 17) hashes both
#    input parquet files and the code of 03_scf_estimators.R and this
#    script, so editing either script invalidates every cached wave
script_version <- "05_young_cohort_descriptives v2 2026-10-11"

# 4) Cell-size and SE flags: small_cell (O2) below 100 implicate-1 cases;
#    tiny_cell at 10 or fewer (the Fed's "*" rule, see 04); se_low_flag when
#    a share's SE is below half its simple-random-sampling SE (header)
small_cell_threshold <- 100L
tiny_cell_threshold <- 10L
se_low_ratio_threshold <- 0.5

# 5) AGE is top-coded at 95 in the public data (codebooks 1995-2025, cited in
#    data/docs/scf/scf_2025_demographics.md, AGE; the 1989 and 1992 top-code
#    is unverified there). The panel's maximum AGE is 95 in every wave from
#    1992 and 91 in 1989, so no 1989 band is flagged
age_topcode <- 95L

# 6) Runtime guard (performance-cost-governance.md): stop if a cold run is
#    projected past 60 minutes
max_projected_minutes <- 60

# 7) Wiring check: under-35 median net worth in 2025 on the extract basis
#    (`networth`, 2025 CPI-U-RS dollars) is 32.82 thousand in
#    scf2025_tables_public_real_historical.xlsx, Table 4; the tolerance is
#    half the table's rounding unit of $10
benchmark_median_2025 <- 32820
benchmark_tolerance <- 5

run_start <- Sys.time()

################################################################################
###                      3. Define outcomes and groups                       ###
################################################################################

# 8) Totals (data/docs/scf/scf_2025_balance_sheet.md, "Totals";
#    scf_2025_income.md): mean and median over all households
#    - networth: NETWORTH = ASSET - DEBT (bulletin.macro.txt line 2790)
#    - asset:    ASSET = FIN + NFIN (line 2525)
#    - debt:     DEBT, the sum of MRTHEL, RESDBT, OTHLOC, CCBAL, INSTALL,
#                and ODEBT (line 2784)
#    - income:   INCOME = MAX(0, X5729) (+ PENACCTWD from 2004), prior
#                calendar year (lines 847-913)
total_outcomes <- c("networth", "asset", "debt", "income")
total_spec <- data.frame(
  outcome = rep(total_outcomes, each = 2L),
  statistic = rep(c("mean", "median"), times = length(total_outcomes)),
  var = rep(paste0(total_outcomes, "_pce25"), each = 2L),
  stat = rep(c("mean", "median"), times = length(total_outcomes)),
  cond = NA_character_,
  basis = "pce25",
  stringsAsFactors = FALSE
)

# 9) Components, each with the macro's own holding flag:
#    - houses   primary residence, HOUSES (line 2357);
#               HHOUSES = (HOUSES ^= 0) (line 2361)
#    - retqliq  retirement accounts, RETQLIQ = IRAKH + THRIFT + FUTPEN +
#               CURRPEN (lines 1791-1797); HRETQLIQ = (RETQLIQ > 0)
#               (lines 1789-1815)
#    - fin      financial assets, FIN (line 2246); HFIN = (FIN > 0)
#               (line 2248)
#    - vehic    vehicles, VEHIC (lines 2253-2262); HVEHIC = (VEHIC > 0)
#               (line 2264)
#    - bus      business equity, BUS (lines 2450-2472); HBUS = (X3103 = 1 |
#               X3401 = 1) (line 2497): owns a business, so a holder's
#               equity can be zero or negative
#    - mrthel   mortgages and HELOCs on the primary residence, MRTHEL
#               (lines 2532-2548); HMRTHEL = (MRTHEL > 0) (line 2554)
#    - edn_inst education loans, EDN_INST (lines 2683-2691);
#               HEDN_INST = (EDN_INST > 0) (line 2726)
#    - ccbal    credit card balances, CCBAL (line 2645); HCCBAL = (CCBAL > 0)
#               (line 2664)
#    - veh_inst vehicle loans, VEH_INST (lines 2683-2691);
#               HVEH_INST = (VEH_INST > 0) (line 2725)
component_outcomes <- c(
  "houses", "retqliq", "fin", "vehic", "bus",
  "mrthel", "edn_inst", "ccbal", "veh_inst"
)
component_flags <- paste0("h", component_outcomes)
component_spec <- data.frame(
  outcome = rep(component_outcomes, each = 2L),
  statistic = rep(c("share_holding", "median_holders"),
                  times = length(component_outcomes)),
  var = as.vector(rbind(component_flags,
                        paste0(component_outcomes, "_pce25"))),
  stat = rep(c("share", "median"), times = length(component_outcomes)),
  cond = as.vector(rbind(NA_character_, paste0(component_flags, " == 1"))),
  basis = rep(c("share", "pce25"), times = length(component_outcomes)),
  stringsAsFactors = FALSE
)

# 10) Homeownership rate: `homeowner` = (HOUSECL == 1), built in 02 from
#    HOUSECL (lines 2363-2373; scf_2025_demographics.md); and the reference
#    person's age, AGE (top-coded at 95), for the cohort table's mean age
homeowner_spec <- data.frame(
  outcome = "homeowner", statistic = "share", var = "homeowner",
  stat = "share", cond = NA_character_, basis = "share",
  stringsAsFactors = FALSE
)
age_spec <- data.frame(
  outcome = "age", statistic = "mean", var = "age",
  stat = "mean", cond = NA_character_, basis = "years",
  stringsAsFactors = FALSE
)
young_spec <- rbind(total_spec, component_spec, homeowner_spec)
cohort_spec <- rbind(total_spec, homeowner_spec, age_spec)
young_spec$spec_order <- seq_len(nrow(young_spec))
cohort_spec$spec_order <- seq_len(nrow(cohort_spec))

# 11) Groups. Table 1: the reference person's age bands below 35, the pooled
#     under-35 group (`young` = age < 35), and 35-44 for comparison.
#     Table 2: under-35 households by each cut, in this order.
age_group_order <- c("under 25", "25-29", "30-34", "under 35", "35-44")
band_levels <- c("under 25", "25-29", "30-34", "35-44")
cut_order <- c(
  "race4", "educ4", "sex_cut", "family_structure", "homeowner",
  "income_group"
)
homeowner_labels <- c("homeowner", "not homeowner")

# 12) Cross-wave definition breaks: one row per outcome or cut name and the
#     first wave measured under the new definition, with a note citing
#     data/docs/scf/scf_cross_wave_changes_1989_2025.md. The table, its
#     sources, and its duplicate-key check live in scf_definition_breaks()
#     in code/00_functions.R (step 3), which 09 and 12 also read, so the
#     scripts flag the same breaks. The one-formula totals networth, asset,
#     and debt carry no flag (doc section 8). income is flagged in 2004:
#     it is not one of those totals, and from 2004 the macro adds IRA and
#     pension-account withdrawals (PENACCTWD) to INCOME. The doc and macro
#     say these were "already included in earlier years", so the flag marks
#     a change in how they are collected, not in what INCOME covers; income
#     is flagged again in 2010 (two pension grids dropped from PENACCTWD).
#     fin carries the changes inside FIN (1992, 1998, 2001, 2004, 2010,
#     2016). Rows whose target is an SCF variable 05 does not estimate
#     (liq, obnd, nmmf, othma, oresre, nnresre, resdbt, odebt, install)
#     match no 05 row
definition_breaks <- scf_definition_breaks()
definition_break_key <- paste(definition_breaks$target, definition_breaks$year)

# 13) Panel columns this script reads
estimate_vars <- unique(c(young_spec$var, cohort_spec$var))
panel_columns <- unique(c(
  "year", "y1", "implicate", "wgt", "young", "age_band", "birth_band",
  cut_order, estimate_vars, "networth"
))
design_columns <- c("year", "y1", "implicate", "wgt", estimate_vars)

################################################################################
###                            4. Define helpers                             ###
################################################################################

# 14) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

# 15) Run scf_estimate() once per row of an outcome spec, label the rows,
#     and collect the estimator's warnings (empty replicates or implicates
#     in small groups) so they are counted in the log, not printed one by one
estimate_outcomes <- function(design, wave_repwgts, spec, by_cols) {
  warning_text <- character(0L)
  estimate_list <- vector("list", nrow(spec))
  for (spec_index in seq_len(nrow(spec))) {
    cond_text <- spec$cond[spec_index]
    # scf_estimate() is defined in 03, sourced in section 2
    estimate_one <- withCallingHandlers(
      scf_estimate( # nolint: object_usage_linter.
        design, wave_repwgts,
        var = spec$var[spec_index], stat = spec$stat[spec_index],
        by = by_cols,
        cond = if (is.na(cond_text)) NULL else cond_text
      ),
      warning = function(condition) {
        warning_text <<- c(warning_text, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    data.table::set(estimate_one, j = "outcome",
                    value = spec$outcome[spec_index])
    data.table::set(estimate_one, j = "statistic",
                    value = spec$statistic[spec_index])
    data.table::set(estimate_one, j = "basis", value = spec$basis[spec_index])
    estimate_list[[spec_index]] <- estimate_one
  }
  list(
    estimates = data.table::rbindlist(estimate_list, use.names = TRUE),
    warnings = warning_text
  )
}

################################################################################
###                 5. Read the panel and set the cache key                  ###
################################################################################

# 16) Read only the needed panel columns; replicate weights are read later,
#     and only if a wave is not cached
if (!file.exists(panel_path) || !file.exists(repwgts_path)) {
  stop("Panel or replicate-weight parquet missing in ", path_processed,
       "; run code/02_build_scf_panel.R first", call. = FALSE)
}
panel <- data.table::as.data.table(
  arrow::read_parquet(
    panel_path, col_select = tidyselect::all_of(panel_columns)
  )
)
wave_years <- sort(unique(panel$year))
log_step(paste0("Read ", format(nrow(panel), big.mark = ","),
                " panel rows (", length(wave_years), " waves)"))

# 17) Cache key: MD5 of both input files, of the estimator library 03, and
#     of this script, plus the script version, so a change to the data or
#     to either script's code re-estimates every wave. The break table
#     (00_functions.R) is joined after the cache (step 29), so it is not
#     part of the key
input_md5 <- unname(tools::md5sum(c(panel_path, repwgts_path)))
code_md5 <- unname(tools::md5sum(here::here(
  "code", c("03_scf_estimators.R", "05_young_cohort_descriptives.R")
)))
cache_key <- substr(
  digest::digest(c(input_md5, code_md5, script_version), algo = "sha256"),
  1L, 16L
)
cache_files <- list.files(path_cache, pattern = "^wave_[0-9]{4}_.*\\.rds$",
                          full.names = TRUE)
stale_files <- cache_files[!grepl(paste0("_", cache_key, "\\.rds$"),
                                  cache_files)]
if (length(stale_files) > 0L) {
  file.remove(stale_files)
  log_step(paste("Removed", length(stale_files), "stale cache file(s)"))
}
log_step(paste0("Cache key ", cache_key, " (", script_version, ")"))

################################################################################
###              6. Check the estimator wiring (2025 benchmark)              ###
################################################################################

# 18) Under-35 median net worth in 2025 on the extract basis must reproduce
#     the Fed's table; point estimate only (no replicate weights needed)
benchmark_rows_flag <- panel$year == 2025L & panel$young
benchmark_estimate <- scf_estimate(
  panel, NULL,
  var = "networth", stat = "median", subset = benchmark_rows_flag
)
if (nrow(benchmark_estimate) != 1L ||
      abs(benchmark_estimate$estimate - benchmark_median_2025) >
        benchmark_tolerance) {
  stop("Wiring check failed: 2025 under-35 median networth is ",
       paste(benchmark_estimate$estimate, collapse = ", "),
       "; expected ", benchmark_median_2025, " +/- ", benchmark_tolerance,
       call. = FALSE)
}
log_step(paste0("Wiring check passed: 2025 under-35 median networth ",
                "(extract basis) = ", benchmark_estimate$estimate))

################################################################################
###                   7. Estimate each wave, with a cache                    ###
################################################################################

repwgts <- NULL
wave_results <- list()
computed_rows <- 0
computed_seconds <- 0
uncached_years <- wave_years[!file.exists(file.path(
  path_cache, sprintf("wave_%d_%s.rds", wave_years, cache_key)
))]
log_step(paste0(length(wave_years) - length(uncached_years), " wave(s) ",
                "cached; ", length(uncached_years), " to estimate"))

for (wave_year in wave_years) {
  wave_label <- as.character(wave_year)
  cache_path <- file.path(
    path_cache, sprintf("wave_%d_%s.rds", wave_year, cache_key)
  )

  # 19) Reuse a cached wave
  if (file.exists(cache_path)) {
    wave_results[[wave_label]] <- readRDS(cache_path)
    log_step(paste0(wave_year, ": read from cache"))
    next
  }

  # 20) Read the replicate weights once, at the first uncached wave
  if (is.null(repwgts)) {
    repwgts <- data.table::as.data.table(arrow::read_parquet(repwgts_path))
    log_step(paste0("Read ", format(nrow(repwgts), big.mark = ","),
                    " replicate-weight rows"))
  }
  wave_start <- Sys.time()
  wave_panel <- panel[year == wave_year]
  wave_repwgts <- repwgts[year == wave_year]

  # 21) Stack the rows of every table-1 and table-2 group into one design,
  #     so each outcome is one scf_estimate() call grouped by (cut,
  #     cut_level); rows of a household appear once per group it belongs to
  band_rows_flag <- as.character(wave_panel$age_band) %in% band_levels
  band_part <- wave_panel[band_rows_flag, design_columns, with = FALSE]
  band_part[, `:=`(
    cut = "age_group",
    cut_level = as.character(wave_panel$age_band[band_rows_flag])
  )]
  young_panel <- wave_panel[young == TRUE]
  pooled_part <- young_panel[, design_columns, with = FALSE]
  pooled_part[, `:=`(cut = "age_group", cut_level = "under 35")]
  design_parts <- list(band_part, pooled_part)
  for (cut_name in cut_order) {
    cut_part <- young_panel[, design_columns, with = FALSE]
    cut_value <- young_panel[[cut_name]]
    cut_level <- if (is.logical(cut_value)) {
      ifelse(cut_value, homeowner_labels[1L], homeowner_labels[2L])
    } else {
      as.character(cut_value)
    }
    cut_part[, `:=`(cut = cut_name, cut_level = cut_level)]
    design_parts[[length(design_parts) + 1L]] <- cut_part
  }
  young_design <- data.table::rbindlist(design_parts, use.names = TRUE)

  # 22) Estimate tables 1 and 2 (grouped by cut and level) and table 3
  #     (grouped by birth band, all households in the wave)
  young_run <- estimate_outcomes(
    young_design, wave_repwgts, young_spec, c("cut", "cut_level")
  )
  cohort_design <- wave_panel[, c(design_columns, "birth_band"),
                              with = FALSE]
  cohort_design[, birth_band := as.character(birth_band)]
  cohort_run <- estimate_outcomes(
    cohort_design, wave_repwgts, cohort_spec, "birth_band"
  )

  # 23) Count cells that are absent (no case with a value in any implicate,
  #     for example no holder) or have n_unweighted = 0; both have no SE and
  #     are dropped in section 8
  young_expected <- nrow(unique(young_design[, c("cut", "cut_level")])) *
    nrow(young_spec)
  cohort_expected <- length(unique(cohort_design$birth_band)) *
    nrow(cohort_spec)
  absent_count <- (young_expected - nrow(young_run$estimates)) +
    (cohort_expected - nrow(cohort_run$estimates))

  wave_seconds <- as.numeric(difftime(Sys.time(), wave_start,
                                      units = "secs"))
  wave_results[[wave_label]] <- list(
    young = young_run$estimates,
    cohort = cohort_run$estimates,
    absent_count = absent_count,
    warnings = c(young_run$warnings, cohort_run$warnings),
    seconds = wave_seconds
  )
  saveRDS(wave_results[[wave_label]], cache_path)

  # 24) Log the wave and project the rest of the cold run from the rows
  #     estimated so far; stop before a run would pass the 60-minute limit
  computed_rows <- computed_rows + nrow(wave_panel)
  computed_seconds <- computed_seconds + wave_seconds
  remaining_years <- uncached_years[uncached_years > wave_year]
  remaining_rows <- sum(panel$year %in% remaining_years)
  projected_minutes <- (
    as.numeric(difftime(Sys.time(), run_start, units = "secs")) +
      computed_seconds / computed_rows * remaining_rows
  ) / 60
  log_step(sprintf(
    paste0("%d: estimated in %.1f s; %d absent cell(s); %d estimator ",
           "warning(s); projected total run %.1f min"),
    wave_year, wave_seconds, absent_count,
    length(wave_results[[wave_label]]$warnings), projected_minutes
  ))
  if (projected_minutes > max_projected_minutes) {
    stop("Projected run time ", round(projected_minutes, 1), " min exceeds ",
         max_projected_minutes, " min; completed waves are cached",
         call. = FALSE)
  }
}
rm(repwgts)

################################################################################
###                     8. Assemble and check the tables                     ###
################################################################################

# 25) Stack the waves and report the estimator warnings by kind
young_estimates <- data.table::rbindlist(
  lapply(wave_results, function(result) result$young), use.names = TRUE
)
cohort_estimates <- data.table::rbindlist(
  lapply(wave_results, function(result) result$cohort), use.names = TRUE
)
all_warnings <- unlist(lapply(wave_results, function(result) {
  result$warnings
}))
replicate_warning_count <- sum(grepl("replicate\\(s\\)", all_warnings))
implicate_warning_count <- sum(grepl("five implicates", all_warnings))
log_step(paste0(
  "Estimator warnings: ", replicate_warning_count, " group(s) empty in ",
  "some replicates (variance from the others); ", implicate_warning_count,
  " estimate(s) with a group missing from an implicate; ",
  length(all_warnings) - replicate_warning_count - implicate_warning_count,
  " other"
))

# 26) Drop cells with no implicate-1 case (no SE is possible) and log the
#     count with the absent cells; then stop on any other missing SE
absent_total <- sum(vapply(wave_results, function(result) {
  result$absent_count
}, numeric(1L)))
young_empty_flag <- young_estimates$n_unweighted == 0L
cohort_empty_flag <- cohort_estimates$n_unweighted == 0L
log_step(paste0(
  "Dropped cells with n = 0: ", absent_total, " absent (no case in any ",
  "implicate) and ", sum(young_empty_flag) + sum(cohort_empty_flag),
  " with no implicate-1 case"
))
young_estimates <- young_estimates[!young_empty_flag]
cohort_estimates <- cohort_estimates[!cohort_empty_flag]
for (table_name in c("young", "cohort")) {
  table_estimates <- if (table_name == "young") {
    young_estimates
  } else {
    cohort_estimates
  }
  missing_se_flag <- is.na(table_estimates$se) |
    is.na(table_estimates$estimate)
  if (any(missing_se_flag)) {
    first_missing <- table_estimates[which(missing_se_flag)[1L]]
    stop(sum(missing_se_flag), " ", table_name, " estimate(s) with n > 0 ",
         "lack an estimate or SE; first: ",
         paste(names(first_missing), unlist(first_missing), sep = "=",
               collapse = ", "),
         call. = FALSE)
  }
}

# 27) Common columns: small-cell (O2) and tiny-cell flags, and the outcome
#     order
young_estimates[, small_cell := n_unweighted < small_cell_threshold]
cohort_estimates[, small_cell := n_unweighted < small_cell_threshold]
young_estimates[, tiny_cell := n_unweighted <= tiny_cell_threshold]
cohort_estimates[, tiny_cell := n_unweighted <= tiny_cell_threshold]
young_estimates[, spec_order := young_spec$spec_order[
  match(paste(outcome, statistic),
        paste(young_spec$outcome, young_spec$statistic))
]]
cohort_estimates[, spec_order := cohort_spec$spec_order[
  match(paste(outcome, statistic),
        paste(cohort_spec$outcome, cohort_spec$statistic))
]]

# 28) Design-effect check for shares (header): the simple-random-sampling
#     SE from the estimate and the implicate-1 count, the ratio of the
#     replicate SE to it, and the flag for ratios below 0.5; se_srs is
#     computed on share rows only (fifelse() would evaluate it on the dollar
#     rows too, where estimate * (1 - estimate) is negative)
young_share_flag <- young_estimates$statistic %in% c("share_holding", "share")
young_estimates[, se_srs := NA_real_]
young_estimates[
  young_share_flag, se_srs := sqrt(estimate * (1 - estimate) / n_unweighted)
]
young_estimates[, se_design_ratio := data.table::fifelse(
  !is.na(se_srs) & se_srs > 0, se / se_srs, NA_real_
)]
young_estimates[, se_low_flag := !is.na(se_design_ratio) &
                  se_design_ratio < se_low_ratio_threshold]
cohort_share_flag <- cohort_estimates$statistic %in%
  c("share_holding", "share")
cohort_estimates[, se_srs := NA_real_]
cohort_estimates[
  cohort_share_flag, se_srs := sqrt(estimate * (1 - estimate) / n_unweighted)
]
cohort_estimates[, se_design_ratio := data.table::fifelse(
  !is.na(se_srs) & se_srs > 0, se / se_srs, NA_real_
)]
cohort_estimates[, se_low_flag := !is.na(se_design_ratio) &
                   se_design_ratio < se_low_ratio_threshold]

# 29) Definition-break notes (step 12): the note for the row's outcome in
#     its year and, in the young tables, the note for its cut; two different
#     notes are joined with "; " (homeowner within the homeowner cut matches
#     the same note twice and keeps one)
young_outcome_note <- definition_breaks$note[match(
  paste(young_estimates$outcome, young_estimates$year), definition_break_key
)]
young_cut_note <- definition_breaks$note[match(
  paste(young_estimates$cut, young_estimates$year), definition_break_key
)]
young_estimates[, definition_break := data.table::fifelse(
  is.na(young_outcome_note),
  young_cut_note,
  data.table::fifelse(
    is.na(young_cut_note) | young_cut_note == young_outcome_note,
    young_outcome_note,
    paste(young_outcome_note, young_cut_note, sep = "; ")
  )
)]
cohort_estimates[, definition_break := definition_breaks$note[match(
  paste(outcome, year), definition_break_key
)]]

# 30) Output columns: the original ones in their original order, then the
#     columns added by the 2026-10-11 review fixes
estimate_columns <- c(
  "outcome", "statistic", "estimate", "se", "se_sampling", "se_imputation",
  "n_unweighted", "n_weighted", "small_cell", "basis",
  "definition_break", "n_replicates_empty", "tiny_cell", "se_srs",
  "se_design_ratio", "se_low_flag"
)

# 31) Table 1: year x age group
age_table <- young_estimates[cut == "age_group"]
age_table[, age_group := cut_level]
age_table[, group_order := match(age_group, age_group_order)]
data.table::setorderv(age_table, c("year", "group_order", "spec_order"))
age_table <- age_table[, c("year", "age_group", estimate_columns),
                       with = FALSE]

# 32) Table 2: under-35 households, year x cut, cuts in the set order and
#     levels in each factor's code order; race rows of 2025 flagged
cut_level_order <- data.table::rbindlist(lapply(cut_order, function(name) {
  levels_one <- if (name == "homeowner") {
    homeowner_labels
  } else {
    levels(panel[[name]])
  }
  data.table::data.table(cut = name, cut_level = levels_one,
                         level_order = seq_along(levels_one))
}))
cut_table <- young_estimates[cut != "age_group"]
cut_table <- merge(cut_table, cut_level_order, by = c("cut", "cut_level"),
                   all.x = TRUE, sort = FALSE)
if (anyNA(cut_table$level_order)) {
  stop("young_by_cut: a cut level is not in its factor levels",
       call. = FALSE)
}
cut_table[, cut_order_index := match(cut, cut_order)]
cut_table[, race_2025_break := cut == "race4" & year == 2025L]
data.table::setorderv(
  cut_table, c("cut_order_index", "year", "level_order", "spec_order")
)
cut_table <- cut_table[, c("year", "cut", "cut_level", "race_2025_break",
                           estimate_columns), with = FALSE]

# 33) Table 3: year x birth band (all bands), with the band's mean age of
#     the reference person as a column as well as an outcome row
birth_band_levels <- levels(panel$birth_band)
cohort_table <- data.table::copy(cohort_estimates)
mean_age_lookup <- cohort_table[outcome == "age",
                                list(year, birth_band, mean_age = estimate)]
cohort_table <- merge(cohort_table, mean_age_lookup,
                      by = c("year", "birth_band"), all.x = TRUE,
                      sort = FALSE)
cohort_table[, band_order := match(birth_band, birth_band_levels)]
if (anyNA(cohort_table$band_order)) {
  stop("cohort_by_age: a birth band is not in the panel's levels",
       call. = FALSE)
}

# 34) Flag the birth band x year cells that hold any reference person at the
#     AGE top-code: everyone aged 95 or older is recorded as 95, so the band
#     of birth year (year - 95) absorbs all older reference persons
topcode_lookup <- panel[
  ,
  list(age_topcode_flag = any(age >= age_topcode)),
  by = list(year, birth_band = as.character(birth_band))
]
cohort_rows_before <- nrow(cohort_table)
cohort_table <- merge(cohort_table, topcode_lookup,
                      by = c("year", "birth_band"), all.x = TRUE,
                      sort = FALSE)
if (nrow(cohort_table) != cohort_rows_before ||
      anyNA(cohort_table$age_topcode_flag)) {
  stop("cohort_by_age: the top-code lookup did not match one row per ",
       "birth band and year", call. = FALSE)
}
data.table::setorderv(cohort_table, c("band_order", "year", "spec_order"))
cohort_table <- cohort_table[, c("year", "birth_band", "mean_age",
                                 estimate_columns, "age_topcode_flag"),
                             with = FALSE]

# 35) The table's 2025 under-35 median networth_pce25 must equal the wiring
#     check above, because the 2025 PCE stock factor is exactly 1
table_median_2025 <- age_table[
  year == 2025L & age_group == "under 35" & outcome == "networth" &
    statistic == "median",
  estimate
]
if (length(table_median_2025) != 1L ||
      abs(table_median_2025 - benchmark_estimate$estimate) > 1e-6) {
  stop("young_by_age_band: 2025 under-35 median networth_pce25 (",
       paste(table_median_2025, collapse = ", "), ") differs from the ",
       "wiring check (", benchmark_estimate$estimate, ")", call. = FALSE)
}

# 36) Figure inputs: (a) under 35 and 35-44, mean and median net worth by
#     year; (b) median net worth by birth band x year, 1940-44 to 2000-04.
#     tiny_cell (n_unweighted <= 10, step 27) is appended last, after the
#     original columns, so 06 can leave such cells out (decision D2 of
#     records/plans/2026-10-13_young-bottom-earners-closeout.md)
fig_bands <- birth_band_levels[
  seq(match("1940-44", birth_band_levels),
      match("2000-04", birth_band_levels))
]
fig_age <- age_table[
  age_group %in% c("under 35", "35-44") & outcome == "networth",
  list(panel = "a_age_group", year, group = age_group, outcome, statistic,
       estimate, se, n_unweighted, small_cell, mean_age = NA_real_, basis,
       tiny_cell)
]
fig_cohort <- cohort_table[
  birth_band %in% fig_bands & outcome == "networth" &
    statistic == "median",
  list(panel = "b_birth_band", year, group = birth_band, outcome,
       statistic, estimate, se, n_unweighted, small_cell, mean_age, basis,
       tiny_cell)
]
fig_inputs <- data.table::rbindlist(list(fig_age, fig_cohort),
                                    use.names = TRUE)

################################################################################
###                           9. Write the tables                            ###
################################################################################

# 37) Write the four CSV files and log their row and small-cell counts
output_tables <- list(
  list(path = age_table_path, data = age_table),
  list(path = cut_table_path, data = cut_table),
  list(path = cohort_table_path, data = cohort_table),
  list(path = fig_inputs_path, data = fig_inputs)
)
for (output_table in output_tables) {
  data.table::fwrite(output_table$data, output_table$path)
  log_step(paste0(
    "Wrote ", file.path("output", "tables", basename(output_table$path)),
    ": ", nrow(output_table$data), " rows, ",
    sum(output_table$data$small_cell), " small cell(s)"
  ))
}

# 38) Log the review flags of the three estimate tables
for (output_table in output_tables[1:3]) {
  log_step(paste0(
    basename(output_table$path), ": ",
    sum(output_table$data$tiny_cell), " tiny cell(s), ",
    sum(output_table$data$se_low_flag), " with se_low_flag, ",
    sum(!is.na(output_table$data$definition_break)),
    " at a definition break, ",
    sum(output_table$data$n_replicates_empty > 0L, na.rm = TRUE),
    " with an empty replicate"
  ))
}
log_step(paste0(
  "cohort_by_age.csv: ", sum(cohort_table$age_topcode_flag),
  " row(s) with age_topcode_flag"
))

# 39) Report the run time
elapsed_seconds_num <- round(
  as.numeric(difftime(Sys.time(), run_start, units = "secs")), 1L
)
log_step(paste0("Done in ", elapsed_seconds_num, " seconds"))
