# 15_rate_price_test -- price, financing, and holding channels, 2019-2025
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U3 of records/plans/2026-10-12_rate-vs-price-test.md (with its
# Amendments). It reads the checkpoints written by 14
# (data/processed/rate_price_households and rate_price_vehicles), the SCF
# panel and replicate weights written by 02, and 05's young_by_age_band.csv
# (a wiring check), sources 00 and 03, and writes, to output/tables/:
#   rate_price_vehicles.csv     wave x group x measure: vehicle holding,
#                               value by vehicle age, and financing terms
#   rate_price_housing.csv      wave x group x measure: ownership, recent
#                               purchases, prices, and mortgage rates
#   rate_price_changes.csv      every measure's 2019-2022 and 2022-2025
#                               change, with its SE and z
#   rate_price_shift_share.csv  the change in vehicle value per household
#                               split into count, age-mix, and value parts
#                               over single years of vehicle age, in PCE
#                               2025 and nominal dollars; home equity per
#                               household split into ownership and
#                               equity-per-owner parts
# Run from the project root as
#   Rscript code/15_rate_price_test.R
#
# Groups: households whose reference person is under 35; 35-44 (the
# contrast); and under-35 households between the 40th and 60th percentiles
# of under-35 net worth (12's main band, cutoffs by 12's rule). Band SEs
# hold the cutoffs fixed and are understated (12, step 21), so the band rows
# carry se_note and the plan's decision rules use the under-35 rows (O4).
#
# Estimates use scf_estimate() (03): the five implicates pooled with WGT, and
# SEs from the 999 replicate weights plus (6/5) x the imputation variance.
# Vehicle-level measures weight each vehicle by its household's WGT, and
# their n_unweighted counts implicate-1 vehicles. The shift-share parts are
# point decompositions of pooled estimates (shift_share_decompose(), 00);
# their uncertainty is the SE of the inputs and of the total change (owner
# decision of 2026-10-13: no replicate SE for the parts). The vehicle price
# rule is judged in PCE 2025 dollars, with the nominal split beside it.
#
# Caveats for every number written here:
# - Repeated cross-sections: each wave holds different households. Nothing
#   here identifies a cause; "consistent with" is the strongest reading.
# - Young households exclude young adults living in another household's
#   primary economic unit (PROJECT.md), and the 2025 PEU change is open.
# - Vehicle values are NADA guidebook retail values as of the fall of the
#   survey year; home values are the owners' estimates. Dollars are PCE 2025
#   dollars (_pce25) unless the basis column says nominal.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here", "data.table", "arrow")
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
library(data.table)

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at code/15_rate_price_test.R; run the
#    decomposition unit tests in a fresh process and stop if they fail; then
#    source the shared functions (00) and the estimator library (03)
here::i_am("code/15_rate_price_test.R")
test_script_path <- here::here("code", "tests",
                               "test_rate_price_decomposition.R")
test_status <- system2(
  file.path(R.home("bin"), "Rscript"), shQuote(test_script_path),
  stdout = FALSE, stderr = FALSE
)
if (!identical(test_status, 0L)) {
  stop("Decomposition unit tests failed (", test_script_path, "); run it ",
       "alone to see which check", call. = FALSE)
}
message("Decomposition unit tests passed")
source(here::here("code", "00_functions.R"))
source(here::here("code", "03_scf_estimators.R"))
estimator <- scf_estimate

path_processed <- here::here("data", "processed")
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

# 3) Waves, the windows for a recent purchase (O2; U1 Amendment 2), the
#    band (12's main band), and the small-cell threshold (O2)
wave_years <- c(2019L, 2022L, 2025L)
home_recent_years <- 2L
band_p_lo <- 0.40
band_p_hi <- 0.60
small_cell_n <- 50L
band_se_note <- paste(
  "SE holds the band cutoffs fixed and is understated (12, step 21)"
)
group_levels <- c("under 35", "35-44", "under 35, 40th-60th percentile")

# 4) Vehicle age classes, in order (14, step 20), and the single years of
#    age used by the shift-share (20 = 20 or more, the bottom-code)
age_class_levels <- c("0-4 years", "5-9 years", "10-14 years",
                      "15 years or more")
single_age_levels <- 0:20
owner_share_note <- paste(
  "Close to a weighting control (SCF weights are calibrated to CPS",
  "homeownership); not used in the decision rules"
)

################################################################################
###                              3. Read the inputs                          ###
################################################################################

households <- data.table::as.data.table(arrow::read_parquet(
  file.path(path_processed, "rate_price_households.parquet")
))
vehicles <- data.table::as.data.table(arrow::read_parquet(
  file.path(path_processed, "rate_price_vehicles.parquet")
))
panel <- data.table::as.data.table(arrow::read_parquet(
  file.path(path_processed, "scf_panel_1989_2025.parquet"),
  col_select = c("year", "y1", "networth_pce25")
))[year %in% wave_years]
repwgts <- data.table::as.data.table(arrow::read_parquet(
  file.path(path_processed, "scf_repwgts_1989_2025.parquet")
))[year %in% wave_years]
message("Read ", nrow(households), " household and ", nrow(vehicles),
        " vehicle records; ", nrow(repwgts), " replicate-weight rows")

################################################################################
###                       4. Groups and derived measures                     ###
################################################################################

# 5) Under-35 band: the cutoffs are the first net worth values, in ascending
#    order, at which the pooled WGT share reaches 0.40 and 0.60 (12, steps
#    15-16); a household is in the band when its net worth lies between them
households <- merge(households, panel, by = c("year", "y1"),
                    all.x = TRUE)
if (anyNA(households$networth_pce25)) {
  stop("Some 14 records have no panel net worth", call. = FALSE)
}
young <- households[age < 35][order(year, networth_pce25)]
young[, cum_share := cumsum(wgt) / sum(wgt), by = year]
band_cutoffs <- young[, list(
  cutoff_lo = networth_pce25[which(cum_share >= band_p_lo)[1L]],
  cutoff_hi = networth_pce25[which(cum_share >= band_p_hi)[1L]]
), by = year]
households <- merge(households, band_cutoffs, by = "year")
households[, band_flag := age < 35 & networth_pce25 >= cutoff_lo &
             networth_pce25 <= cutoff_hi]

# 6) Household measures: detailed vehicles (rows in 14's vehicle table) and
#    any recent vehicle purchase; a recent home purchase; home equity with
#    no first mortgage counted as 0 owed; loan-to-cost for a purchase
#    mortgage (both nominal, same year); a current rate below 4%
vehicle_household <- vehicles[, list(
  detailed_vehicle_count = .N,
  any_recent_vehicle_flag = any(recent_purchase_flag)
), by = c("year", "y1")]
households <- merge(households, vehicle_household, by = c("year", "y1"),
                    all.x = TRUE)
households[is.na(detailed_vehicle_count), `:=`(
  detailed_vehicle_count = 0L, any_recent_vehicle_flag = FALSE
)]
households[, recent_home_buyer_flag := home_purchased_flag &
             !is.na(home_purchase_year) &
             home_purchase_year >= year - home_recent_years]
households[, recent_purchase_mortgage_flag := purchase_mortgage_flag &
             recent_home_buyer_flag]
households[, mortgage_owed_all_pce25 := data.table::fifelse(
  mortgage_flag, mortgage_owed_pce25, 0
)]
households[, equity_pce25 := data.table::fifelse(
  home_owner_flag, home_value_pce25 - mortgage_owed_all_pce25, 0
)]
households[, loan_to_cost := data.table::fifelse(
  recent_purchase_mortgage_flag & !is.na(home_cost_nom) & home_cost_nom > 0,
  mortgage_borrowed_nom / home_cost_nom, NA_real_
)]
households[, rate_below_4_flag := data.table::fifelse(
  mortgage_flag, rate_current_pct < 4, NA
)]
households[, vehic_holder_flag := vehic_value_pce25 > 0]

# 7) Stack the three groups (a band household is also an under-35 household)
#    and attach each vehicle to its household's groups
group_rows <- list(
  households[age < 35][, group := group_levels[1L]],
  households[age_band == "35-44"][, group := group_levels[2L]],
  households[band_flag == TRUE][, group := group_levels[3L]]
)
hh_long <- data.table::rbindlist(group_rows, use.names = TRUE)
veh_long <- merge(vehicles, hh_long[, c("year", "y1", "group")],
                  by = c("year", "y1"), allow.cartesian = TRUE)
for (class_index in seq_along(age_class_levels)) {
  veh_long[, (paste0("age_class_", class_index, "_flag")) :=
             vehicle_age_class == age_class_levels[class_index]]
}

################################################################################
###                                5. Estimate                               ###
################################################################################

# 8) One call per measure; empty-replicate warnings are counted in the
#    output (n_replicates_empty), so they are muffled here
estimate_measure <- function(data, measure, var, stat, cond = NULL,
                             by = NULL, basis = "pce25") {
  # data.table names used unquoted below, not globals
  class <- `:=` <- NULL
  result <- withCallingHandlers(
    estimator(data, repwgts, var, stat = stat, by = c("group", by),
              cond = cond),
    warning = function(w) invokeRestart("muffleWarning")
  )
  if (is.null(by)) {
    result[, class := "all"]
  } else {
    data.table::setnames(result, by, "class")
  }
  result[, `:=`(measure = measure, statistic = stat, basis = basis,
                among = if (is.null(cond)) "all in group" else cond)]
  result[]
}

# 9) Vehicles: holding, the age mix and value at a given age (V1, V3), and
#    financing of recent purchases (V2)
vehicle_parts <- list(
  estimate_measure(hh_long, "owns a vehicle", "owns_vehicle_flag", "share",
                   basis = "share"),
  estimate_measure(hh_long, "vehicles owned (X2202, top-coded 10)",
                   "vehicle_count", "mean", basis = "count"),
  estimate_measure(hh_long, "detailed vehicles per household",
                   "detailed_vehicle_count", "mean", basis = "count"),
  estimate_measure(hh_long, "vehicle value per household (VEHIC)",
                   "vehic_value_pce25", "mean"),
  estimate_measure(hh_long, "holds any vehicle value (VEHIC > 0)",
                   "vehic_holder_flag", "share", basis = "share"),
  estimate_measure(hh_long, "vehicle value per holder (VEHIC)",
                   "vehic_value_pce25", "median", cond = "vehic_holder_flag"),
  estimate_measure(hh_long, "detailed vehicle value per household",
                   "detailed_value_pce25", "mean"),
  estimate_measure(hh_long, "detailed vehicle value per household",
                   "detailed_value_nom", "mean", basis = "nominal"),
  estimate_measure(hh_long, "bought a vehicle recently",
                   "any_recent_vehicle_flag", "share", basis = "share"),
  estimate_measure(veh_long, "value per vehicle", "value_pce25", "mean"),
  estimate_measure(veh_long, "value per vehicle", "value_pce25", "mean",
                   by = "vehicle_age_class"),
  estimate_measure(veh_long, "value per vehicle", "value_nom", "mean",
                   by = "vehicle_age_class", basis = "nominal"),
  estimate_measure(veh_long, "value per vehicle", "value_pce25", "median",
                   by = "vehicle_age_class"),
  estimate_measure(veh_long, "bought new", "bought_new_flag", "share",
                   basis = "share"),
  estimate_measure(veh_long, "financed", "financed_flag", "share",
                   cond = "recent_purchase_flag", basis = "share"),
  estimate_measure(veh_long, "loan APR (%)", "apr_pct", "mean",
                   cond = "recent_purchase_flag & financed_flag",
                   basis = "percent"),
  estimate_measure(veh_long, "loan APR (%)", "apr_pct", "median",
                   cond = "recent_purchase_flag & financed_flag",
                   basis = "percent"),
  estimate_measure(veh_long, "amount borrowed", "borrowed_pce25", "mean",
                   cond = "recent_purchase_flag & financed_flag"),
  estimate_measure(veh_long, "zero-interest loan", "apr_no_interest_flag",
                   "share", cond = "recent_purchase_flag & financed_flag",
                   basis = "share"),
  estimate_measure(veh_long, "loan APR (%)", "apr_pct", "mean",
                   cond = "financed_flag", basis = "percent")
)
for (class_index in seq_along(age_class_levels)) {
  class_share <- estimate_measure(
    veh_long, "share of vehicles in age class",
    paste0("age_class_", class_index, "_flag"), "share", basis = "share"
  )
  class_share[, class := age_class_levels[class_index]]
  vehicle_parts[[length(vehicle_parts) + 1L]] <- class_share
}
vehicle_estimates <- data.table::rbindlist(vehicle_parts, use.names = TRUE)

# 9a) Mean value per vehicle by single year of vehicle age, in both bases,
#     for the shift-share in section 8; each age's share of vehicles is its
#     pooled weight (n_weighted) over the group's total. Not written out:
#     many single-year cells are thin
single_age <- data.table::rbindlist(list(
  estimate_measure(veh_long, "value per vehicle", "value_pce25", "mean",
                   by = "vehicle_age"),
  estimate_measure(veh_long, "value per vehicle", "value_nom", "mean",
                   by = "vehicle_age", basis = "nominal")
), use.names = TRUE)

# 10) Housing: ownership and recent purchases (H1), purchase prices (H2),
#     mortgage rates (H3), and owners' value, debt, and equity (H4)
recent_buyer_cond <- "recent_home_buyer_flag & !is.na(home_cost_pce25)"
housing_parts <- list(
  estimate_measure(hh_long, "owns the home unit (X716)", "home_owner_flag",
                   "share", basis = "share"),
  estimate_measure(hh_long, "bought the home in the last 3 years",
                   "recent_home_buyer_flag", "share", basis = "share"),
  estimate_measure(hh_long, "home equity per household", "equity_pce25",
                   "mean"),
  estimate_measure(hh_long, "home value", "home_value_pce25", "mean",
                   cond = "home_owner_flag"),
  estimate_measure(hh_long, "first-mortgage balance",
                   "mortgage_owed_all_pce25", "mean", cond = "home_owner_flag"),
  estimate_measure(hh_long, "home equity", "equity_pce25", "mean",
                   cond = "home_owner_flag"),
  estimate_measure(hh_long, "has a first mortgage", "mortgage_flag", "share",
                   cond = "home_owner_flag", basis = "share"),
  estimate_measure(hh_long, "purchase price", "home_cost_pce25", "mean",
                   cond = recent_buyer_cond),
  estimate_measure(hh_long, "purchase price", "home_cost_pce25", "median",
                   cond = recent_buyer_cond),
  estimate_measure(hh_long, "home value", "home_value_pce25", "mean",
                   cond = recent_buyer_cond),
  estimate_measure(hh_long, "mortgage rate when taken out (%)",
                   "rate_origination_pct", "mean",
                   cond = "recent_purchase_mortgage_flag", basis = "percent"),
  estimate_measure(hh_long, "mortgage rate when taken out (%)",
                   "rate_origination_pct", "median",
                   cond = "recent_purchase_mortgage_flag", basis = "percent"),
  estimate_measure(hh_long, "amount borrowed", "mortgage_borrowed_pce25",
                   "mean", cond = "recent_purchase_mortgage_flag"),
  estimate_measure(hh_long, "loan-to-cost ratio", "loan_to_cost", "mean",
                   cond = "!is.na(loan_to_cost)", basis = "ratio"),
  estimate_measure(hh_long, "current mortgage rate (%)", "rate_current_pct",
                   "mean", cond = "mortgage_flag", basis = "percent"),
  estimate_measure(hh_long, "current mortgage rate below 4%",
                   "rate_below_4_flag", "share", cond = "mortgage_flag",
                   basis = "share")
)
housing_estimates <- data.table::rbindlist(housing_parts, use.names = TRUE)

# 11) Common columns, flags, and order
finish_table <- function(estimates) {
  # data.table column names used unquoted below, not globals
  small_cell <- n_unweighted <- se_note <- group <- measure <- statistic <-
    basis <- among <- class <- year <- `:=` <- NULL
  estimates[, small_cell := n_unweighted < small_cell_n]
  estimates[, se_note := data.table::fifelse(group == group_levels[3L],
                                             band_se_note, "")]
  estimates[measure == "owns the home unit (X716)",
            se_note := trimws(paste(se_note, owner_share_note))]
  estimates[, group := factor(group, levels = group_levels)]
  data.table::setorder(estimates, measure, statistic, basis, among, class,
                       group, year)
  estimates[, group := as.character(group)]
  estimates[, c("year", "group", "measure", "class", "statistic", "basis",
                "among", "estimate", "se", "se_sampling", "se_imputation",
                "n_unweighted", "n_weighted", "n_replicates_empty",
                "small_cell", "se_note"), with = FALSE]
}
vehicle_estimates <- finish_table(vehicle_estimates)
housing_estimates <- finish_table(housing_estimates)

################################################################################
###                               6. Checks                                  ###
################################################################################

# 12) Wiring: the under-35 share holding vehicles (VEHIC > 0) and the median
#     VEHIC among holders reproduce 05 (young_by_age_band.csv, outcome
#     vehic: share_holding and median_holders, pce25), within 1e-9 and $1
young_05 <- data.table::fread(file.path(path_tables, "young_by_age_band.csv"))
young_05 <- young_05[age_group == "under 35" & outcome == "vehic"]
wiring <- rbind(
  merge(
    vehicle_estimates[measure == "holds any vehicle value (VEHIC > 0)" &
                        group == group_levels[1L], c("year", "estimate")],
    young_05[statistic == "share_holding", list(year, estimate_05 = estimate)],
    by = "year"
  )[, tolerance := 1e-9],
  merge(
    vehicle_estimates[measure == "vehicle value per holder (VEHIC)" &
                        group == group_levels[1L], c("year", "estimate")],
    young_05[statistic == "median_holders" & basis == "pce25",
             list(year, estimate_05 = estimate)],
    by = "year"
  )[, tolerance := 1]
)
if (nrow(wiring) != 2L * length(wave_years) ||
      any(abs(wiring$estimate - wiring$estimate_05) > wiring$tolerance)) {
  stop("Under-35 VEHIC holding and median do not reproduce ",
       "young_by_age_band.csv", call. = FALSE)
}
message("Check passed: under-35 VEHIC holding and median reproduce 05 (",
        nrow(wiring), " estimates)")

# 13) Each wave's age-class shares sum to 1 in every group
share_sums <- vehicle_estimates[measure == "share of vehicles in age class",
                                list(total = sum(estimate)),
                                by = c("group", "year")]
if (any(abs(share_sums$total - 1) > 1e-9)) {
  stop("Vehicle age-class shares do not sum to 1", call. = FALSE)
}

################################################################################
###                    7. Changes between waves, with z                      ###
################################################################################

# 14) For every measure, the 2019-2022 and 2022-2025 change; the SE treats
#     the waves as independent samples
all_estimates <- data.table::rbindlist(list(
  vehicle_estimates[, section := "vehicles"],
  housing_estimates[, section := "housing"]
), use.names = TRUE)
key_cols <- c("section", "group", "measure", "class", "statistic", "basis",
              "among")
change_parts <- list()
for (period in list(c(2019L, 2022L), c(2022L, 2025L))) {
  change_cols <- c(key_cols, "estimate", "se", "n_unweighted", "small_cell",
                   "se_note")
  first <- all_estimates[year == period[1L], change_cols, with = FALSE]
  second <- all_estimates[year == period[2L], change_cols, with = FALSE]
  paired <- merge(first, second, by = key_cols,
                  suffixes = c("_first", "_second"))
  paired[, `:=`(
    period = paste0(period[1L], "-", period[2L]),
    change = estimate_second - estimate_first,
    se_change = sqrt(se_first^2 + se_second^2)
  )]
  paired[, z := change / se_change]
  paired[, beyond_95_flag := abs(z) > 1.96]
  paired[, small_cell := small_cell_first | small_cell_second]
  paired[, se_note := se_note_first]
  paired[, c("small_cell_first", "small_cell_second", "se_note_first",
             "se_note_second") := NULL]
  change_parts[[length(change_parts) + 1L]] <- paired
}
changes <- data.table::rbindlist(change_parts, use.names = TRUE)
data.table::setcolorder(changes, c("period", key_cols))

################################################################################
###                              8. Shift-share                              ###
################################################################################

# 15) Vehicles: detailed vehicle value per household = vehicles per
#     household x sum over single years of vehicle age of (share x mean
#     value), in PCE 2025 and nominal dollars. With pooled estimates this
#     holds exactly; the check stops on a gap above $1. An age with no
#     vehicles in one wave takes the other wave's value there, so it adds
#     nothing to the value part (owner decision of 2026-10-13)
pick <- function(table, measure_name, group_name, wave, class_name = "all",
                 stat = "mean", basis_name = "pce25") {
  # data.table column names used unquoted below, not globals
  measure <- group <- year <- class <- statistic <- basis <- among <-
    estimate <- NULL
  table[measure == measure_name & group == group_name & year == wave &
          class == class_name & statistic == stat & basis == basis_name &
          among == "all in group", estimate]
}
shift_parts <- list()
filled_age_count <- 0L
for (group_name in group_levels) {
  for (period in list(c(2019L, 2022L), c(2022L, 2025L))) {
    period_label <- paste0(period[1L], "-", period[2L])
    for (basis_name in c("pce25", "nominal")) {
      n_pair <- c(
        pick(vehicle_estimates, "detailed vehicles per household",
             group_name, period[1L], basis_name = "count"),
        pick(vehicle_estimates, "detailed vehicles per household",
             group_name, period[2L], basis_name = "count")
      )
      total_pair <- c(
        pick(vehicle_estimates, "detailed vehicle value per household",
             group_name, period[1L], basis_name = basis_name),
        pick(vehicle_estimates, "detailed vehicle value per household",
             group_name, period[2L], basis_name = basis_name)
      )
      share_matrix <- matrix(0, nrow = length(single_age_levels), ncol = 2L)
      value_matrix <- matrix(NA_real_, nrow = length(single_age_levels),
                             ncol = 2L)
      for (wave_index in 1:2) {
        age_rows <- single_age[group == group_name &
                                 year == period[wave_index] &
                                 basis == basis_name]
        age_index <- match(age_rows$class, single_age_levels)
        if (anyNA(age_index)) {
          stop("A vehicle age outside 0-20 in ", group_name, call. = FALSE)
        }
        share_matrix[age_index, wave_index] <-
          age_rows$n_weighted / sum(age_rows$n_weighted)
        value_matrix[age_index, wave_index] <- age_rows$estimate
      }
      missing_first <- is.na(value_matrix[, 1L])
      missing_second <- is.na(value_matrix[, 2L])
      value_matrix[missing_first, 1L] <- value_matrix[missing_first, 2L]
      value_matrix[missing_second, 2L] <- value_matrix[missing_second, 1L]
      filled_age_count <- filled_age_count + sum(missing_first) +
        sum(missing_second)
      empty_both <- is.na(value_matrix[, 1L])
      value_matrix[empty_both, ] <- 0
      vehicle_split <- shift_share_decompose(
        n_pair[1L], n_pair[2L], share_matrix[, 1L], share_matrix[, 2L],
        value_matrix[, 1L], value_matrix[, 2L]
      )
      if (any(abs(c(vehicle_split[["total_first"]],
                    vehicle_split[["total_second"]]) - total_pair) > 1)) {
        stop("Vehicle shift-share does not reproduce the detailed value per ",
             "household for ", group_name, " (", basis_name, ")",
             call. = FALSE)
      }
      shift_parts[[length(shift_parts) + 1L]] <- data.table::data.table(
        group = group_name, period = period_label,
        total = "vehicle value per household (detailed vehicles)",
        basis = basis_name, classes = "single years of vehicle age, 0-20",
        total_first = vehicle_split[["total_first"]],
        total_second = vehicle_split[["total_second"]],
        change = vehicle_split[["change"]],
        count_or_holding_part = vehicle_split[["count_part"]],
        mix_part = vehicle_split[["mix_part"]],
        value_part = vehicle_split[["value_part"]],
        value_part_home_value = NA_real_, value_part_mortgage = NA_real_
      )
    }

    # 16) Housing: equity per household = share owning x mean equity per
    #     owner (one class); the equity-per-owner change is then split into
    #     the home-value and mortgage-balance changes
    housing_inputs <- lapply(period, function(wave) {
      list(
        own = pick(housing_estimates, "owns the home unit (X716)",
                   group_name, wave, stat = "share", basis_name = "share"),
        equity_owner = housing_estimates[
          measure == "home equity" & group == group_name & year == wave &
            among == "home_owner_flag", estimate
        ],
        value_owner = housing_estimates[
          measure == "home value" & group == group_name & year == wave &
            among == "home_owner_flag", estimate
        ],
        owed_owner = housing_estimates[
          measure == "first-mortgage balance" & group == group_name &
            year == wave & among == "home_owner_flag", estimate
        ],
        total = pick(housing_estimates, "home equity per household",
                     group_name, wave)
      )
    })
    housing_split <- shift_share_decompose(
      housing_inputs[[1L]]$own, housing_inputs[[2L]]$own, 1, 1,
      housing_inputs[[1L]]$equity_owner, housing_inputs[[2L]]$equity_owner
    )
    if (abs(housing_split[["total_first"]] - housing_inputs[[1L]]$total) > 1 ||
          abs(housing_split[["total_second"]] -
                housing_inputs[[2L]]$total) > 1) {
      stop("Housing shift-share does not reproduce equity per household ",
           "for ", group_name, call. = FALSE)
    }
    own_mid <- (housing_inputs[[1L]]$own + housing_inputs[[2L]]$own) / 2
    shift_parts[[length(shift_parts) + 1L]] <- data.table::data.table(
      group = group_name, period = period_label,
      total = "home equity per household", basis = "pce25",
      classes = "owners of the home unit (one class)",
      total_first = housing_split[["total_first"]],
      total_second = housing_split[["total_second"]],
      change = housing_split[["change"]],
      count_or_holding_part = housing_split[["count_part"]],
      mix_part = NA_real_,
      value_part = housing_split[["value_part"]],
      value_part_home_value = own_mid * (
        housing_inputs[[2L]]$value_owner - housing_inputs[[1L]]$value_owner
      ),
      value_part_mortgage = -own_mid * (
        housing_inputs[[2L]]$owed_owner - housing_inputs[[1L]]$owed_owner
      )
    )
  }
}
shift_share <- data.table::rbindlist(shift_parts, use.names = TRUE)
part_sum <- shift_share$count_or_holding_part +
  data.table::fifelse(is.na(shift_share$mix_part), 0, shift_share$mix_part) +
  shift_share$value_part
if (any(abs(part_sum - shift_share$change) > 1e-6)) {
  stop("Shift-share parts do not add up to the change", call. = FALSE)
}
message("Check passed: shift-share parts add up and reproduce the totals (",
        nrow(shift_share), " rows); vehicle ages filled from the other ",
        "wave: ", filled_age_count)

################################################################################
###                                 9. Write                                 ###
################################################################################

data.table::fwrite(vehicle_estimates,
                   file.path(path_tables, "rate_price_vehicles.csv"))
data.table::fwrite(housing_estimates,
                   file.path(path_tables, "rate_price_housing.csv"))
data.table::fwrite(changes, file.path(path_tables, "rate_price_changes.csv"))
data.table::fwrite(shift_share,
                   file.path(path_tables, "rate_price_shift_share.csv"))
message("Wrote rate_price_vehicles.csv (", nrow(vehicle_estimates),
        " rows), rate_price_housing.csv (", nrow(housing_estimates),
        " rows), rate_price_changes.csv (", nrow(changes),
        " rows), rate_price_shift_share.csv (", nrow(shift_share), " rows)")

# 10) The under-35 2022-2025 changes, for the run log
headline <- changes[period == "2022-2025" & group == group_levels[1L] &
                      among != "financed_flag"]
for (row_index in seq_len(nrow(headline))) {
  message(sprintf(
    "  u35 2022-2025 %-38s %-16s %-7s %10.3f (z %5.2f)",
    substr(headline$measure[row_index], 1L, 38L),
    substr(headline$class[row_index], 1L, 16L),
    headline$statistic[row_index], headline$change[row_index],
    headline$z[row_index]
  ))
}
