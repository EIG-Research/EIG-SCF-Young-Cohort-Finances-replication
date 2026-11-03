# 12_u35_balance_sheet -- balance sheet of under-35 households near the median
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U1 of records/plans/2026-10-11_u35-networth-sankey.md. It reads
#   data/processed/scf_panel_1989_2025.parquet   (written by 02)
#   data/processed/scf_repwgts_1989_2025.parquet (written by 02)
#   output/tables/validation_benchmarks.csv      (written by 04)
# sources the estimator library code/03_scf_estimators.R and the shared
# break table in code/00_functions.R, and writes, to output/tables/:
#   u35_balance_sheet.csv              year x band x component: band means,
#                                      holding rates, band median income,
#                                      and the under-35 median net worth
#   networth_change_by_age.csv         median net worth by the Fed's age
#                                      groups, 2022 and 2025, and the
#                                      percent change with a 95% interval
#   u35_balance_sheet_sensitivity.csv  year x component: the main band
#                                      (40th-60th) against the narrow
#                                      band (45th-55th)
# Run from the project root as
#   Rscript code/12_u35_balance_sheet.R
#
# Why a band (owner decision, 2026-10-11): medians do not add, so the
# median net worth cannot be split into median assets and median debts.
# The table instead gives the MEAN of each balance-sheet component among
# under-35 households whose net worth lies in a band around the under-35
# median. Those means sum exactly to the band's mean net worth, which sits
# close to the median. Main band: 40th-60th percentile; sensitivity band:
# 45th-55th (O1, changed by the owner on 2026-10-11, because the narrow band
# holds only about 50-130 sample households per wave). Cutoffs are the
# WGT-weighted percentiles of net worth over the five pooled implicates, in
# each year (O2), by the same "first value at which the cumulative weight
# share reaches p" rule as the median in 03.
#
# Caveats that apply to every number written here:
# - Young households EXCLUDE young adults living inside someone else's
#   primary economic unit (PROJECT.md, Additional Context).
# - A band mean blends holders and non-holders (in 2025 about a fifth of the
#   main band owns a home), so `share_holding` rows give the share of the
#   band holding each item.
# - Standard errors re-estimate the band cutoffs in every replicate and
#   implicate (step 21); `se_fixed_cutoffs` keeps the narrower SE that
#   holds the cutoffs fixed.
# - The SCF is a repeated cross-section, so the 2022 and 2025 bands hold
#   different households; changes describe the typical household of each
#   wave, not the same households over time.
# - `definition_break` (last column of both band tables) names a documented
#   SCF change in how the component is measured, in the first wave under
#   the new definition (step 8a); a comparison across it mixes real change
#   with the measurement change.
# - Dollars are on the project basis `_pce25` (headline PCE price index,
#   2025 dollars; income deflated with the prior-year index). These are not
#   the Fed's CPI-U-RS real figures.
#
# Exceptions to the EIG R style guide:
# - data.table, not the tidyverse, to match 03's data.table interface.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "data.table", "arrow", "readr", "tidyselect")
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
`:=` <- year <- networth_pce25 <- wgt <- cum_share <- in_band_flag <-
  band <- comp_business_other <- comp_business <- comp_other_nonfinancial <-
  se_recut <- se_sampling_recut <- se_imputation_recut <- se_fixed_cutoffs <-
  NULL

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at code/12_u35_balance_sheet.R
here::i_am("code/12_u35_balance_sheet.R")
source(here::here("code", "03_scf_estimators.R"))
source(here::here("code", "00_functions.R"))

path_processed <- here::here("data", "processed")
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
benchmark_path <- file.path(path_tables, "validation_benchmarks.csv")
table_path <- file.path(path_tables, "u35_balance_sheet.csv")
sensitivity_path <- file.path(path_tables,
                              "u35_balance_sheet_sensitivity.csv")
age_change_path <- file.path(path_tables, "networth_change_by_age.csv")

# The two survey years compared in the change-by-age table
change_years <- c(2022L, 2025L)

# 3) Bands around the median (O1): name, lower and upper percentile
band_spec <- data.frame(
  band = c("p45_p55", "p40_p60"),
  p_lo = c(0.45, 0.40),
  p_hi = c(0.55, 0.60),
  stringsAsFactors = FALSE
)
# The main band (O1) is p40_p60; 13 sets the same band for the figures

# 4) Identity tolerance, in dollars, for the household- and band-level
#    adding-up checks
identity_tolerance <- 1

# 5) A household "holds" an item when its value is more than 50 cents away
#    from zero: the residual components below are differences of doubles,
#    so a zero balance can come out as a few nanodollars, and business
#    equity and other real estate can be negative and still held (the
#    macro's HBUS counts ownership, not positive equity)
holding_threshold <- 0.5

# 6) small_cell = TRUE when a cell has fewer than 100 implicate-1 cases, the
#    project's small-cell rule (05, O2 of the Stage 1 plan). The 45th-55th
#    band holds roughly 50-130 implicate-1 cases per wave, and the
#    40th-60th band about twice that, so component means carry wide
#    standard errors; rows are kept and flagged
small_cell_threshold <- 100L

# 7) Log a timestamped line
log_step <- function(text) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", text)
}

################################################################################
###                         3. Define the components                         ###
################################################################################

# 8) Twelve components plus net worth (plan, Approach; variable definitions
#    in data/docs/scf/scf_2025_balance_sheet.md). The residual categories
#    make each side add up exactly:
#    - FIN - LIQ - RETQLIQ: CDs, funds, stocks, bonds, savings bonds,
#      cash-value life insurance, other managed and other financial assets
#    - NFIN - HOUSES - VEHIC - BUS: other residential and nonresidential
#      real estate, other nonfinancial assets
#    - DEBT - MRTHEL - EDN_INST - VEH_INST - CCBAL: other residential debt,
#      other lines of credit, other installment loans (BNPL from 2022 is
#      already inside OTH_INST), and other debt
component_spec <- data.frame(
  component = c(
    "liquid", "retirement", "other_financial", "home", "vehicles",
    "business", "other_nonfinancial",
    "mortgage", "education", "vehicle_loans", "credit_card", "other_debt"
  ),
  label = c(
    "Liquid accounts", "Retirement accounts", "Other financial assets",
    "Primary residence", "Vehicles", "Business equity",
    "Other nonfinancial assets",
    "Home mortgage and HELOC", "Education loans", "Vehicle loans",
    "Credit card balances", "Other debt"
  ),
  side = c(rep("asset", 7L), rep("debt", 5L)),
  definition = c(
    "LIQ", "RETQLIQ", "FIN - LIQ - RETQLIQ", "HOUSES", "VEHIC", "BUS",
    "NFIN - HOUSES - VEHIC - BUS",
    "MRTHEL", "EDN_INST", "VEH_INST", "CCBAL",
    "DEBT - MRTHEL - EDN_INST - VEH_INST - CCBAL"
  ),
  stringsAsFactors = FALSE
)
component_spec$var <- paste0("comp_", component_spec$component)

# 8a) Definition breaks (U2 of records/plans/2026-10-12_u35-definition-
#     breaks.md, its O2 mapping, and decision D1 of
#     records/plans/2026-10-13_young-bottom-earners-closeout.md, extended
#     on 2026-10-13 with the macro sweep recorded in 00_functions.R). Each
#     row below gives a component one (target, year) row of the shared
#     break table, scf_definition_breaks() in code/00_functions.R (sources
#     and macro lines there; doc = data/docs/scf/scf_cross_wave_changes_
#     1989_2025.md). A component with several breaks in one year gets the
#     notes joined with "; ". The note is joined on (component, year) in
#     steps 28 and 29 and changes no estimate.
#     - liquid (LIQ): liq 2004 (new savings and money market grid) and fin
#       2016 (prepaid cards enter LIQ, macro lines 1395-1402). The other fin
#       rows describe changes in RETQLIQ, OBND, NMMF, and OTHMA, which are
#       not in LIQ
#     - retirement (RETQLIQ = IRAKH + THRIFT + FUTPEN + CURRPEN, macro lines
#       1782-1797, so IRAKH is inside it): retqliq 2001, 2004, 2010
#     - other_financial (FIN - LIQ - RETQLIQ, which holds CDS, NMMF, STOCKS,
#       BOND, SAVBND, CASHLI, OTHMA, OTHFIN; macro line 2246): obnd 1992,
#       othma 1998 and 2004, nmmf 2004. PREPAID sits in LIQ and CURRPEN in
#       RETQLIQ, so the residual holds neither
#     - vehicles (VEHIC), business (BUS), education (EDN_INST),
#       vehicle_loans (VEH_INST), credit_card (CCBAL), income (INCOME): the
#       component is the target variable itself
#     - other_nonfinancial (NFIN - HOUSES - VEHIC - BUS, which holds ORESRE,
#       NNRESRE, and OTHNFIN; macro line 2517): oresre 2010 and 2013,
#       nnresre 2010
#     - business_other: NOT BUS alone. It is NFIN - HOUSES - VEHIC, that is
#       BUS plus other_nonfinancial, so it carries both components' breaks:
#       bus 2010, oresre 2010 and 2013, nnresre 2010
#     - other_debt (DEBT - MRTHEL - EDN_INST - VEH_INST - CCBAL, which holds
#       RESDBT, OTHLOC, OTH_INST, and ODEBT; macro line 2784): install 1995
#       (X7183) and 2022 (BNPL), both in OTH_INST (macro lines 2683-2724);
#       odebt 1995, 1998, 2004, 2010; resdbt 2010 and 2013
#     Not flagged:
#     - home: HOUSES is built without HOUSECL (macro line 2357); the 1995
#       homeowner change is in HOUSECL (lines 2364-2373)
#     - total_assets, total_debt, networth, networth_nominal: one formula in
#       every wave (doc section 8), as in 05
#     - mortgage (MRTHEL): no year branch in the macro and no documented
#       change
component_break_lookup <- data.table::fread(
  text = "
    component,          target,   year
    liquid,             liq,      2004
    liquid,             fin,      2016
    retirement,         retqliq,  2001
    retirement,         retqliq,  2004
    retirement,         retqliq,  2010
    other_financial,    obnd,     1992
    other_financial,    othma,    1998
    other_financial,    othma,    2004
    other_financial,    nmmf,     2004
    vehicles,           vehic,    1995
    business,           bus,      2010
    other_nonfinancial, oresre,   2010
    other_nonfinancial, nnresre,  2010
    other_nonfinancial, oresre,   2013
    business_other,     bus,      2010
    business_other,     oresre,   2010
    business_other,     nnresre,  2010
    business_other,     oresre,   2013
    education,          edn_inst, 1992
    education,          edn_inst, 1995
    education,          edn_inst, 2016
    education,          edn_inst, 2025
    vehicle_loans,      veh_inst, 1995
    credit_card,        ccbal,    1992
    credit_card,        ccbal,    2010
    credit_card,        ccbal,    2016
    other_debt,         install,  1995
    other_debt,         odebt,    1995
    other_debt,         odebt,    1998
    other_debt,         odebt,    2004
    other_debt,         odebt,    2010
    other_debt,         resdbt,   2010
    other_debt,         resdbt,   2013
    other_debt,         install,  2022
    income,             income,   2004
    income,             income,   2010
  ",
  colClasses = c("character", "character", "integer"),
  strip.white = TRUE
)
component_break_lookup[, lookup_order := seq_len(.N)]
definition_breaks <- scf_definition_breaks()
lookup_breaks <- merge(
  component_break_lookup, definition_breaks, by = c("target", "year"),
  all.x = TRUE
)
if (anyNA(lookup_breaks$note)) {
  missing_rows <- lookup_breaks[is.na(note)]
  stop("Break lookup (target, year) not in scf_definition_breaks(): ",
       paste(missing_rows$target, missing_rows$year, collapse = ", "),
       call. = FALSE)
}
# Every break of a target the lookup uses whole (all but fin, whose 2001
# change sits in RETQLIQ) must reach a component, so a row added to the
# shared table for one of these targets cannot be left out here
whole_targets <- setdiff(unique(component_break_lookup$target), "fin")
unmapped_breaks <- definition_breaks[
  definition_breaks$target %in% whole_targets &
    !paste(definition_breaks$target, definition_breaks$year) %in%
      paste(component_break_lookup$target, component_break_lookup$year)
]
if (nrow(unmapped_breaks) > 0L) {
  stop("Break(s) in scf_definition_breaks() reach no component: ",
       paste(unmapped_breaks$target, unmapped_breaks$year, collapse = ", "),
       call. = FALSE)
}
data.table::setorderv(lookup_breaks, "lookup_order")
component_breaks <- lookup_breaks[
  ,
  list(note = paste(note, collapse = "; ")),
  by = list(component, year)
]
component_break_key <- paste(component_breaks$component,
                             component_breaks$year)

panel_columns <- c(
  "year", "y1", "yy1", "implicate", "wgt", "young",
  "networth_pce25", "networth_nom", "asset_pce25", "fin_pce25",
  "nfin_pce25", "debt_pce25", "liq_pce25", "retqliq_pce25",
  "houses_pce25", "vehic_pce25", "bus_pce25", "mrthel_pce25",
  "edn_inst_pce25", "veh_inst_pce25", "ccbal_pce25", "income_pce25",
  "age_group_fed", "networth"
)

################################################################################
###                   4. Read the panel and build components                 ###
################################################################################

# 9) Read the needed panel columns and keep under-35 households (`young` is
#    AGE < 35 of the reference person, built in 02)
if (!file.exists(panel_path) || !file.exists(repwgts_path)) {
  stop("Panel or replicate-weight parquet missing in ", path_processed,
       "; run code/02_build_scf_panel.R first", call. = FALSE)
}
panel <- data.table::as.data.table(
  arrow::read_parquet(
    panel_path, col_select = tidyselect::all_of(panel_columns)
  )
)
young_panel <- panel[!is.na(panel$young) & panel$young]
log_step(paste0("Read ", format(nrow(panel), big.mark = ","),
                " panel rows; ", format(nrow(young_panel), big.mark = ","),
                " are under-35 household-implicates"))
age_panel <- panel[year %in% change_years]
rm(panel)

# 10) Build the twelve components on the pce25 basis
young_panel[, `:=`(
  comp_liquid = liq_pce25,
  comp_retirement = retqliq_pce25,
  comp_other_financial = fin_pce25 - liq_pce25 - retqliq_pce25,
  comp_home = houses_pce25,
  comp_vehicles = vehic_pce25,
  comp_business = bus_pce25,
  comp_other_nonfinancial = nfin_pce25 - houses_pce25 - vehic_pce25 -
    bus_pce25,
  comp_mortgage = mrthel_pce25,
  comp_education = edn_inst_pce25,
  comp_vehicle_loans = veh_inst_pce25,
  comp_credit_card = ccbal_pce25,
  comp_other_debt = debt_pce25 - mrthel_pce25 - edn_inst_pce25 -
    veh_inst_pce25 - ccbal_pce25
)]

# 11) Household-level identities: ASSET = FIN + NFIN and
#     NETWORTH = ASSET - DEBT (bulletin.macro.txt lines 2525, 2790), so the
#     components add up for every household. The debt sum holds by
#     construction (other_debt is its residual) and guards only against an
#     edit to the component list; the substantive checks are the asset and
#     net-worth identities, and the non-negative residuals below
component_matrix <- as.matrix(young_panel[, component_spec$var, with = FALSE])
if (anyNA(component_matrix)) {
  stop("A balance-sheet component is missing for some under-35 rows",
       call. = FALSE)
}
asset_sum <- rowSums(component_matrix[, component_spec$side == "asset"])
debt_sum <- rowSums(component_matrix[, component_spec$side == "debt"])
identity_gap <- c(
  asset = max(abs(asset_sum - young_panel$asset_pce25)),
  debt = max(abs(debt_sum - young_panel$debt_pce25)),
  networth = max(abs(asset_sum - debt_sum - young_panel$networth_pce25))
)
if (any(identity_gap > identity_tolerance)) {
  stop("Household identity check failed; largest gaps (dollars): ",
       paste(names(identity_gap), signif(identity_gap, 3L), sep = " = ",
             collapse = ", "),
       call. = FALSE)
}
log_step("Household identities hold (assets, debts, net worth)")

# 12) The two residual categories must not be negative: a negative residual
#     would mean a component counted twice (as BNPL would be if added on
#     top of OTH_INST)
residual_min <- c(
  other_financial = min(young_panel$comp_other_financial),
  other_debt = min(young_panel$comp_other_debt)
)
if (any(residual_min < -identity_tolerance)) {
  stop("A residual component is negative; minimum (dollars): ",
       paste(names(residual_min), signif(residual_min, 3L), sep = " = ",
             collapse = ", "),
       call. = FALSE)
}

# 13) Business equity plus other nonfinancial assets, which the Sankey
#     shows as one node; estimated directly so its SE includes the
#     covariance of the two parts
young_panel[, comp_business_other := comp_business + comp_other_nonfinancial]

# 14) Holding flags (0/1) for each component
holding_vars <- paste0("hold_", component_spec$component)
for (component_index in seq_len(nrow(component_spec))) {
  data.table::set(
    young_panel,
    j = holding_vars[component_index],
    value = as.numeric(
      abs(young_panel[[component_spec$var[component_index]]]) >
        holding_threshold
    )
  )
}

################################################################################
###                    5. Flag the bands around the median                   ###
################################################################################

# 15) Weighted cumulative share of net worth within each year, over the
#     five pooled implicates (WGT; registry entry `scf`, weights)
data.table::setorderv(young_panel, c("year", "networth_pce25"))
young_panel[, cum_share := cumsum(wgt) / sum(wgt), by = year]

# 16) For each band, the cutoffs are the first net worth values at which the
#     cumulative share reaches p_lo and p_hi; stack one copy of the band's
#     rows per band, as 05 stacks its groups
band_parts <- vector("list", nrow(band_spec))
cutoff_parts <- vector("list", nrow(band_spec))
for (band_index in seq_len(nrow(band_spec))) {
  p_lo <- band_spec$p_lo[band_index]
  p_hi <- band_spec$p_hi[band_index]
  cutoffs <- young_panel[
    ,
    list(
      cutoff_lo = networth_pce25[which(cum_share >= p_lo)[1L]],
      cutoff_hi = networth_pce25[which(cum_share >= p_hi)[1L]]
    ),
    by = year
  ]
  band_rows <- merge(young_panel, cutoffs, by = "year", sort = FALSE)
  band_rows[, in_band_flag := networth_pce25 >= cutoff_lo &
              networth_pce25 <= cutoff_hi]
  band_rows <- band_rows[in_band_flag == TRUE]
  band_rows[, band := band_spec$band[band_index]]
  band_parts[[band_index]] <- band_rows
  cutoffs[, band := band_spec$band[band_index]]
  cutoff_parts[[band_index]] <- cutoffs
}
band_design <- data.table::rbindlist(band_parts, use.names = TRUE)
band_cutoffs <- data.table::rbindlist(cutoff_parts, use.names = TRUE)

# 17) Every year must have all five implicates in every band
implicate_count <- band_design[, list(n_imp = data.table::uniqueN(implicate)),
                               by = c("year", "band")]
if (nrow(implicate_count) != nrow(band_spec) *
      data.table::uniqueN(young_panel$year) ||
      any(implicate_count$n_imp != 5L)) {
  stop("A year-band cell is empty or lacks one of the five implicates",
       call. = FALSE)
}
log_step(paste0("Flagged ", nrow(band_spec), " bands in ",
                data.table::uniqueN(young_panel$year), " waves"))

################################################################################
###                              6. Estimate                                 ###
################################################################################

repwgts <- data.table::as.data.table(arrow::read_parquet(repwgts_path))
log_step(paste0("Read ", format(nrow(repwgts), big.mark = ","),
                " replicate-weight rows"))

# 18) Run scf_estimate() (03) for each row of a spec, label the rows, and
#     collect the estimator's warnings so they are counted, not printed
estimate_spec <- function(design, spec, by_cols) {
  warning_text <- character(0L)
  estimate_list <- vector("list", nrow(spec))
  for (spec_index in seq_len(nrow(spec))) {
    estimate_one <- withCallingHandlers(
      scf_estimate( # nolint: object_usage_linter.
        design, repwgts,
        var = spec$var[spec_index], stat = spec$stat[spec_index],
        by = by_cols
      ),
      warning = function(condition) {
        warning_text <<- c(warning_text, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    for (label_col in c("component", "side", "statistic")) {
      data.table::set(estimate_one, j = label_col,
                      value = spec[[label_col]][spec_index])
    }
    estimate_list[[spec_index]] <- estimate_one
  }
  if (length(warning_text) > 0L) {
    log_step(paste0(length(warning_text), " estimator warning(s), first: ",
                    warning_text[1L]))
  }
  data.table::rbindlist(estimate_list, use.names = TRUE)
}

# 19) Band means of the components, the band totals, holding rates, and
#     the band's median income
band_estimate_spec <- rbind(
  data.frame(
    component = component_spec$component, side = component_spec$side,
    statistic = "mean", var = component_spec$var, stat = "mean",
    stringsAsFactors = FALSE
  ),
  data.frame(
    component = c("business_other", "total_assets", "total_debt",
                  "networth"),
    side = c("asset_group", "asset_total", "debt_total", "networth"),
    statistic = "mean",
    var = c("comp_business_other", "asset_pce25", "debt_pce25",
            "networth_pce25"),
    stat = "mean",
    stringsAsFactors = FALSE
  ),
  data.frame(
    component = component_spec$component, side = component_spec$side,
    statistic = "share_holding", var = holding_vars, stat = "mean",
    stringsAsFactors = FALSE
  ),
  data.frame(
    component = "income", side = "context", statistic = "median",
    var = "income_pce25", stat = "median",
    stringsAsFactors = FALSE
  )
)
band_estimates <- estimate_spec(band_design, band_estimate_spec, "band")
log_step(paste0("Estimated ", nrow(band_estimates), " band cells"))

# 20) The under-35 median net worth over all under-35 households, on the
#     project basis and on the nominal basis (for the benchmark check)
young_panel[, band := "all_under35"]
median_spec <- data.frame(
  component = c("networth", "networth_nominal"),
  side = "networth", statistic = "median",
  var = c("networth_pce25", "networth_nom"), stat = "median",
  stringsAsFactors = FALSE
)
median_estimates <- estimate_spec(young_panel, median_spec, "band")

# 21) Standard errors that re-estimate the band cutoffs. scf_estimate()
#     above holds each band's cutoffs fixed, which understates the
#     uncertainty of a band statistic: net worth in particular is boxed in
#     by the cutoffs, so its fixed-cutoff SE is far too small (review loop
#     3, 2026-10-11). Here every variance draw re-defines the band before
#     estimating, following 03's method otherwise:
#     - sampling variance: on implicate 1, for each of the 999 replicate
#       weights, recompute the cutoffs, the band, and every band statistic;
#       variance of the 999 estimates, divisor 998;
#     - imputation variance: in each of the five implicates, recompute the
#       cutoffs and band with WGT; variance of the five estimates, divisor 4;
#     - se = sqrt(sampling + (6/5) x imputation).
#     Point estimates are unchanged (pooled cutoffs). The fixed-cutoff SE is
#     kept as `se_fixed_cutoffs`.
recut_mean_vars <- unique(band_estimate_spec$var[band_estimate_spec$stat ==
                                                   "mean"])
recut_median_vars <- unique(band_estimate_spec$var[band_estimate_spec$stat ==
                                                     "median"])
rw_cols <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
rw_cols <- rw_cols[order(as.integer(sub("^rw_", "", rw_cols)))]
join_key <- intersect(c("y1", "yy1"), names(repwgts))[1L]

# Band statistics for one weight vector: cutoffs at the first values whose
# cumulative weight share reaches p_lo and p_hi (values sorted ascending),
# then weighted means and weighted medians over the band
recut_band_stats <- function(value_sorted, weight, mean_matrix,
                             median_matrix, p_lo, p_hi) {
  cum_share <- cumsum(weight) / sum(weight)
  in_band <- value_sorted >= value_sorted[which(cum_share >= p_lo)[1L]] &
    value_sorted <= value_sorted[which(cum_share >= p_hi)[1L]] &
    weight > 0
  band_weight <- weight[in_band]
  means <- colSums(mean_matrix[in_band, , drop = FALSE] * band_weight) /
    sum(band_weight)
  medians <- apply(median_matrix[in_band, , drop = FALSE], 2L,
                   function(column_values) {
                     value_order <- order(column_values)
                     cum_weight <- cumsum(band_weight[value_order])
                     column_values[value_order][
                       which(cum_weight >= 0.5 * sum(band_weight))[1L]
                     ]
                   })
  c(means, medians)
}

recut_parts <- list()
for (wave_year in sort(unique(young_panel$year))) {
  wave_rows <- young_panel[year == wave_year]
  for (band_index in seq_len(nrow(band_spec))) {
    p_lo <- band_spec$p_lo[band_index]
    p_hi <- band_spec$p_hi[band_index]

    # a) Imputation: one estimate per implicate, cutoffs within implicate
    implicate_est <- sapply(1:5, function(implicate_number) {
      imp_rows <- wave_rows[implicate == implicate_number]
      data.table::setorderv(imp_rows, "networth_pce25")
      recut_band_stats(
        imp_rows$networth_pce25, imp_rows$wgt,
        as.matrix(imp_rows[, recut_mean_vars, with = FALSE]),
        as.matrix(imp_rows[, recut_median_vars, with = FALSE]),
        p_lo, p_hi
      )
    })
    imputation_var <- apply(implicate_est, 1L, stats::var)

    # b) Sampling: implicate 1 with each replicate weight (negative or
    #    missing weights set to 0, as in 03)
    imp1_rows <- wave_rows[implicate == 1L]
    data.table::setorderv(imp1_rows, "networth_pce25")
    match_index <- repwgts[imp1_rows, on = c("year", join_key),
                           which = TRUE, mult = "first"]
    if (anyNA(match_index)) {
      stop("Implicate-1 rows without replicate weights in ", wave_year,
           call. = FALSE)
    }
    weight_matrix <- as.matrix(repwgts[match_index, rw_cols, with = FALSE])
    weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0
    mean_matrix <- as.matrix(imp1_rows[, recut_mean_vars, with = FALSE])
    median_matrix <- as.matrix(imp1_rows[, recut_median_vars, with = FALSE])
    replicate_est <- apply(weight_matrix, 2L, function(replicate_weight) {
      recut_band_stats(imp1_rows$networth_pce25, replicate_weight,
                       mean_matrix, median_matrix, p_lo, p_hi)
    })
    sampling_var <- apply(replicate_est, 1L, stats::var)

    recut_parts[[length(recut_parts) + 1L]] <- data.table::data.table(
      year = wave_year, band = band_spec$band[band_index],
      var = c(recut_mean_vars, recut_median_vars),
      stat = c(rep("mean", length(recut_mean_vars)),
               rep("median", length(recut_median_vars))),
      se_sampling_recut = sqrt(sampling_var),
      se_imputation_recut = sqrt(imputation_var)
    )
  }
}
recut_se <- data.table::rbindlist(recut_parts)
recut_se[, se_recut := sqrt(se_sampling_recut^2 +
                              (6 / 5) * se_imputation_recut^2)]

# 22) Replace the band SEs with the re-cut SEs, keeping the fixed-cutoff SE
spec_keys <- data.table::as.data.table(band_estimate_spec)[
  , list(component, statistic, var, stat)
]
recut_se <- merge(recut_se, spec_keys, by = c("var", "stat"),
                  allow.cartesian = TRUE)
rows_before <- nrow(band_estimates)
band_estimates <- merge(
  band_estimates,
  recut_se[, list(year, band, component, statistic, se_recut,
                  se_sampling_recut, se_imputation_recut)],
  by = c("year", "band", "component", "statistic"), all.x = TRUE
)
if (nrow(band_estimates) != rows_before || anyNA(band_estimates$se_recut)) {
  stop("Re-cut SEs do not match the band estimates one to one",
       call. = FALSE)
}
band_estimates[, se_fixed_cutoffs := se]
band_estimates[, `:=`(se = se_recut, se_sampling = se_sampling_recut,
                      se_imputation = se_imputation_recut)]
band_estimates[, c("se_recut", "se_sampling_recut",
                   "se_imputation_recut") := NULL]
log_step("Re-estimated band SEs with cutoffs recomputed in every draw")

################################################################################
###                              7. Check                                    ###
################################################################################

# 23) Band level: component means add up to the band totals, and assets
#     minus debts equal the band's mean net worth, in every year and band
band_means <- band_estimates[statistic == "mean"]
band_wide <- data.table::dcast(band_means, year + band ~ component,
                               value.var = "estimate")
asset_components <- component_spec$component[component_spec$side == "asset"]
debt_components <- component_spec$component[component_spec$side == "debt"]
band_gap <- c(
  asset = max(abs(rowSums(band_wide[, asset_components, with = FALSE]) -
                    band_wide$total_assets)),
  debt = max(abs(rowSums(band_wide[, debt_components, with = FALSE]) -
                   band_wide$total_debt)),
  networth = max(abs(band_wide$total_assets - band_wide$total_debt -
                       band_wide$networth))
)
if (any(band_gap > identity_tolerance)) {
  stop("Band identity check failed; largest gaps (dollars): ",
       paste(names(band_gap), signif(band_gap, 3L), sep = " = ",
             collapse = ", "),
       call. = FALSE)
}
log_step("Band identities hold in every year and band")

# 24) The under-35 nominal median must match the Fed's published nominal
#     value (validation_benchmarks.csv, from 04) within its tolerance
if (!file.exists(benchmark_path)) {
  stop("Benchmark table not found: ", benchmark_path,
       "; run code/04_validate_benchmarks.R first", call. = FALSE)
}
benchmarks <- data.table::as.data.table(readr::read_csv(
  benchmark_path,
  col_types = readr::cols(
    basis = readr::col_character(), year = readr::col_integer(),
    age_group = readr::col_character(), variable = readr::col_character(),
    statistic = readr::col_character(), published = readr::col_double(),
    tolerance = readr::col_double(), .default = readr::col_guess()
  )
))
benchmark_rows <- benchmarks[basis == "nominal" & age_group == "less than 35" &
                               variable == "networth" & statistic == "median"]
nominal_medians <- median_estimates[component == "networth_nominal"]
benchmark_check <- merge(
  nominal_medians[, list(year, ours = estimate)],
  benchmark_rows[, list(year, published, tolerance)],
  by = "year", all = TRUE
)
benchmark_failed_flag <- is.na(benchmark_check$ours) |
  is.na(benchmark_check$published) |
  abs(benchmark_check$ours - benchmark_check$published) >
    benchmark_check$tolerance
if (any(benchmark_failed_flag)) {
  stop("Under-35 median net worth misses the published nominal value in ",
       paste(benchmark_check$year[benchmark_failed_flag], collapse = ", "),
       call. = FALSE)
}
log_step(paste0("Under-35 nominal median matches the Fed in all ",
                nrow(benchmark_check), " waves"))

# 25) Median net worth by the Fed's age groups, 2022 and 2025, for the
#     percent-change figure (owner request of 2026-10-12, a version of the
#     WSJ chart of the bulletin's real changes, with error bars). Two
#     bases: the project basis (pce25) and the extract basis (`networth`,
#     the Fed's CPI-U-RS 2025 dollars, which the published tables and the
#     WSJ chart use). Medians and SEs come from scf_estimate() (03)
age_spec <- data.frame(
  component = c("networth", "networth_extract"),
  side = "networth", statistic = "median",
  var = c("networth_pce25", "networth"), stat = "median",
  stringsAsFactors = FALSE
)
age_estimates <- estimate_spec(age_panel, age_spec, "age_group_fed")
age_estimates[, age_group := as.character(age_group_fed)]
age_estimates[, basis := data.table::fifelse(component == "networth",
                                             "pce25", "extract")]

# 26) The extract-basis medians must match the Fed's published real
#     medians (validation_benchmarks.csv, basis "real") within tolerance,
#     so the percent changes reproduce the published ones
age_check <- merge(
  age_estimates[basis == "extract", list(year, age_group, ours = estimate)],
  benchmarks[basis == "real" & variable == "networth" &
               statistic == "median" & year %in% change_years,
             list(year, age_group, published, tolerance)],
  by = c("year", "age_group"), all = TRUE
)
age_failed_flag <- is.na(age_check$ours) | is.na(age_check$published) |
  abs(age_check$ours - age_check$published) > age_check$tolerance
if (any(age_failed_flag)) {
  stop("Median net worth by age misses the published real value for ",
       paste(age_check$year[age_failed_flag],
             age_check$age_group[age_failed_flag], collapse = "; "),
       call. = FALSE)
}
log_step(paste0("Median net worth by age matches the Fed's real tables in ",
                nrow(age_check), " cells"))

# 27) Percent change from the first to the second year, with a 95%
#     interval on the log-ratio scale (methodology finding 1 HIGH of
#     review-reports/methodology-w3.findings.json, applied as in 16). For
#     two independent medians m1 and m2, log(m2 / m1) has the delta-method
#     SE se_log = sqrt((se1 / m1)^2 + (se2 / m2)^2), and the interval is
#     100 x (exp(log(m2 / m1) +/- 1.96 x se_log) - 1). It stays above -100%
#     and is not symmetric, so it agrees with the test of no change, which
#     significant_flag reports. pct_change is 100 x (m2 / m1 - 1), as
#     before. se_pct_change is the delta-method SE of the percent,
#     100 x (m2 / m1) x se_log, which equals the former ratio form for
#     positive medians; it is for reference, not for the interval. The
#     interval, SE, and flag are NA when either median is zero or below,
#     since a ratio of such values is not a percent change (this also
#     replaces the form that turned negative for a negative end value,
#     code review LOW of review-reports/code-w3.findings.json)
age_wide <- data.table::dcast(
  age_estimates, basis + age_group ~ year,
  value.var = c("estimate", "se", "n_unweighted")
)
first_change_year <- change_years[1L]
second_change_year <- change_years[2L]
median_first <- age_wide[[paste0("estimate_", first_change_year)]]
median_second <- age_wide[[paste0("estimate_", second_change_year)]]
se_first <- age_wide[[paste0("se_", first_change_year)]]
se_second <- age_wide[[paste0("se_", second_change_year)]]
age_wide[, pct_change := 100 * (median_second / median_first - 1)]
positive_ratio_flag <- !is.na(median_first) & !is.na(median_second) &
  median_first > 0 & median_second > 0
log_ratio <- rep(NA_real_, nrow(age_wide))
se_log_ratio <- rep(NA_real_, nrow(age_wide))
log_ratio[positive_ratio_flag] <- log(
  median_second[positive_ratio_flag] / median_first[positive_ratio_flag]
)
se_log_ratio[positive_ratio_flag] <- sqrt(
  (se_first[positive_ratio_flag] / median_first[positive_ratio_flag])^2 +
    (se_second[positive_ratio_flag] / median_second[positive_ratio_flag])^2
)
age_wide[, se_pct_change := 100 * exp(log_ratio) * se_log_ratio]
age_wide[, ci_low := 100 * (exp(log_ratio - 1.96 * se_log_ratio) - 1)]
age_wide[, ci_high := 100 * (exp(log_ratio + 1.96 * se_log_ratio) - 1)]
age_wide[, significant_flag := ci_low > 0 | ci_high < 0]
age_wide[, ci_method := "log ratio"]
age_wide[, age_rank := match(age_group, levels(age_panel$age_group_fed))]
data.table::setorderv(age_wide, c("basis", "age_rank"))
age_wide[, age_rank := NULL]
data.table::fwrite(age_wide, age_change_path)
log_step(paste0("Wrote ", nrow(age_wide), " rows to ", age_change_path))

################################################################################
###                              8. Write                                    ###
################################################################################

# 28) One long table: band rows, then the under-35 medians; labels,
#     cutoffs, and the definition-break note (step 8a; on every band and
#     statistic of the component in that year) attached, and a row order
#     the figure step can rely on
label_lookup <- rbind(
  component_spec[, c("component", "label", "definition")],
  data.frame(
    component = c("business_other", "total_assets", "total_debt",
                  "networth", "networth_nominal", "income"),
    label = c("Business and other nonfinancial", "Total assets",
              "Total debt", "Net worth", "Net worth (nominal dollars)",
              "Income"),
    definition = c("NFIN - HOUSES - VEHIC (= BUS + other nonfinancial)",
                   "ASSET", "DEBT", "NETWORTH = ASSET - DEBT", "NETWORTH",
                   "INCOME (prior calendar year)"),
    stringsAsFactors = FALSE
  )
)
balance_sheet <- data.table::rbindlist(
  list(band_estimates, median_estimates), use.names = TRUE, fill = TRUE
)
balance_sheet <- merge(balance_sheet, label_lookup, by = "component",
                       all.x = TRUE, sort = FALSE)
balance_sheet <- merge(balance_sheet, band_cutoffs, by = c("year", "band"),
                       all.x = TRUE, sort = FALSE)
balance_sheet[, basis := data.table::fifelse(
  statistic == "share_holding", "share",
  data.table::fifelse(component == "networth_nominal", "nominal", "pce25")
)]
component_order <- c(component_spec$component, "business_other",
                     "total_assets", "total_debt", "networth",
                     "networth_nominal", "income")
balance_sheet[, small_cell := n_unweighted < small_cell_threshold]
balance_sheet[, definition_break := component_breaks$note[match(
  paste(component, year), component_break_key
)]]
unmatched_breaks <- setdiff(component_break_key,
                            paste(balance_sheet$component,
                                  balance_sheet$year))
if (length(unmatched_breaks) > 0L) {
  stop("Break lookup: no balance-sheet rows for ",
       paste(unmatched_breaks, collapse = ", "), call. = FALSE)
}
balance_sheet[, component_rank := match(component, component_order)]
data.table::setorderv(balance_sheet,
                      c("band", "statistic", "year", "component_rank"))
balance_sheet[, component_rank := NULL]
output_columns <- c(
  "year", "band", "cutoff_lo", "cutoff_hi", "side", "component", "label",
  "definition", "statistic", "basis", "estimate", "se", "se_sampling",
  "se_imputation", "se_fixed_cutoffs", "n_unweighted", "n_weighted",
  "n_replicates_empty",
  "small_cell", "definition_break"
)
balance_sheet <- balance_sheet[, output_columns, with = FALSE]
data.table::fwrite(balance_sheet, table_path)
log_step(paste0("Wrote ", nrow(balance_sheet), " rows to ", table_path))

# 29) Sensitivity: main band (40th-60th) against the narrow band
#     (45th-55th), component by component; diff = main minus narrow; the
#     definition-break note (step 8a) is the last column
sensitivity <- data.table::dcast(
  band_means[, list(year, band, component, estimate)],
  year + component ~ band, value.var = "estimate"
)
data.table::setnames(sensitivity, c("p45_p55", "p40_p60"),
                     c("estimate_p45_p55", "estimate_p40_p60"))
sensitivity[, diff := estimate_p40_p60 - estimate_p45_p55]
sensitivity[, component_rank := match(component, component_order)]
data.table::setorderv(sensitivity, c("year", "component_rank"))
sensitivity[, component_rank := NULL]
sensitivity[, definition_break := component_breaks$note[match(
  paste(component, year), component_break_key
)]]
data.table::fwrite(sensitivity, sensitivity_path)

# 30) Log how far each band's mean net worth sits from the median
median_pce25 <- median_estimates[component == "networth",
                                 list(year, median = estimate)]
gap_to_median <- merge(
  band_wide[, list(year, band, band_mean = networth)], median_pce25,
  by = "year"
)
gap_to_median[, gap := band_mean - median]
for (band_name in band_spec$band) {
  band_gap_rows <- gap_to_median[band == band_name]
  log_step(paste0(
    band_name, ": band mean minus median net worth ranges from $",
    format(round(min(band_gap_rows$gap)), big.mark = ","), " to $",
    format(round(max(band_gap_rows$gap)), big.mark = ","), " across waves"
  ))
}
log_step(paste0("Wrote ", sensitivity_path))
