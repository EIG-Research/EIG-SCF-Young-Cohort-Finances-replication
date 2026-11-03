# 14_rate_price_inputs -- vehicle, home, and mortgage detail for 2019-2025
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U2 of records/plans/2026-10-12_rate-vs-price-test.md (with the U1
# Amendments). It reads the full public SCF files for 2019, 2022, and 2025
# (data/raw/scf/<year>/scf<yy>s.zip, hashes checked against
# data/raw/scf/manifest.csv), the PCE index, and the panel written by 02, and
# writes, to data/processed/:
#   rate_price_households.rds/.parquet  one row per household-implicate:
#                                       vehicle counts, the home, and the
#                                       first mortgage
#   rate_price_vehicles.rds/.parquet    one row per detailed owned vehicle
#                                       (up to four per household)
# Variable definitions, codes, and codebook lines are in
# data/docs/scf/scf_rate_price_variables.md. Every dollar value is kept
# nominal and in PCE 2025 dollars: survey-year values (current home value,
# vehicle values, amounts owed) with the survey-year index, as 02 does for
# stocks; the home's original cost with the purchase-year index; and amounts
# borrowed with the index of the year the loan was taken out.
# Checks (each stops the script before anything is written):
# - every full-file record joins one-to-one to a panel record on Y1;
# - the vehicle items reproduce the panel's VEHIC and VEH_INST, and the home
#   items its HOUSES (bulletin.macro.txt lines 2255-2261, 2684, 2357), on
#   the nominal basis, within $1.
# Run from the project root as
#   Rscript code/14_rate_price_inputs.R
#
# Caveats for every number built here:
# - Young households exclude young adults living in another household's
#   primary economic unit (PROJECT.md, Additional Context), and the 2025 PEU
#   change is an open question (data/docs/scf/scf_2025_demographics.md).
# - Vehicle values are NADA guidebook retail values as of the fall of the
#   survey year, not owners' estimates; the home value is the owner's
#   estimate.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here", "data.table", "haven", "arrow", "digest")
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

# 2) Stop unless this script sits at code/14_rate_price_inputs.R
here::i_am("code/14_rate_price_inputs.R")
path_scf_raw <- here::here("data", "raw", "scf")
path_processed <- here::here("data", "processed")
manifest_path <- file.path(path_scf_raw, "manifest.csv")
panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
households_stem <- file.path(path_processed, "rate_price_households")
vehicles_stem <- file.path(path_processed, "rate_price_vehicles")

# 3) The waves (O1: 2019-2022 is the pre-period)
wave_years <- c(2019L, 2022L, 2025L)

# 4) The four detailed vehicles: one column name per item and vehicle
#    (codebook groups #1-#4; scf_rate_price_variables.md)
vehicle_items <- list(
  model_year = c("x2205", "x2305", "x2405", "x7152"),
  value = c("x8166", "x8167", "x8168", "x8188"),
  new_used = c("x7543", "x7542", "x7541", "x7153"),
  year_got = c("x7540", "x7539", "x7538", "x7154"),
  loan_flag = c("x2206", "x2306", "x2406", "x7155"),
  loan_year = c("x2208", "x2308", "x2408", "x7157"),
  borrowed = c("x2209", "x2309", "x2409", "x7158"),
  owed = c("x2218", "x2318", "x2418", "x7169"),
  apr = c("x2219", "x2319", "x2419", "x7170")
)

# 5) Items outside the four detailed vehicles that VEHIC and VEH_INST add
#    (mop-up and other vehicles), and the HOUSES items
#    (bulletin.macro.txt lines 2255-2261, 2684, 2357)
vehic_other_items <- c("x2422", "x2506", "x2606", "x2623")
veh_inst_other_items <- c("x2424", "x2519", "x2619", "x2625")
houses_items <- c("x604", "x614", "x623", "x507", "x513", "x526")

# 6) Household items: vehicles owned, the home unit, the first mortgage
household_items <- c(
  "x2201", "x2202",
  "x716", "x717", "x718", "x720",
  "x723", "x802", "x804", "x805", "x816", "x820", "x7053", "x7054",
  "x7137", "x7568"
)
keep_columns <- unique(c(
  "y1", "yy1", household_items, unlist(vehicle_items, use.names = FALSE),
  vehic_other_items, veh_inst_other_items, houses_items
))

# 7) Tolerance for the identity checks, in nominal dollars
identity_tol_dollars <- 1

################################################################################
###                              3. Define helpers                           ###
################################################################################

# 8) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

################################################################################
###                 4. Check the inputs and read the PCE index               ###
################################################################################

# 9) Find each wave's full public file and the PCE index in the manifest and
#    stop if a file is missing or its SHA-256 differs
manifest <- utils::read.csv(
  manifest_path,
  colClasses = "character", na.strings = "", check.names = FALSE
)
input_rows <- manifest[
  (manifest$file_role == "full_public" &
     manifest$year %in% as.character(wave_years)) |
    manifest$file_role == "pce_index", ,
  drop = FALSE
]
if (nrow(input_rows) != length(wave_years) + 1L) {
  stop("Expected ", length(wave_years) + 1L, " manifest rows (full public ",
       "files and the PCE index), found ", nrow(input_rows), call. = FALSE)
}
for (row_index in seq_len(nrow(input_rows))) {
  full_path <- here::here(input_rows$local_path[row_index])
  if (!file.exists(full_path) ||
        !identical(digest::digest(file = full_path, algo = "sha256"),
                   input_rows$sha256[row_index])) {
    stop("Missing file or SHA-256 mismatch against the manifest: ",
         full_path, call. = FALSE)
  }
}
log_step(paste(nrow(input_rows), "input files match the manifest hashes"))

# 10) The PCE index rebased to 2025 = 100 (as in 02, section 6); years
#     before the index starts get no PCE value, and their real values are NA
pce_path <- here::here(input_rows$local_path[
  input_rows$file_role == "pce_index"
])
pce_raw <- utils::read.csv(
  pce_path,
  colClasses = c("character", "numeric"), check.names = FALSE
)
if (!identical(names(pce_raw), c("observation_date", "DPCERG3A086NBEA"))) {
  stop("Unexpected columns in ", pce_path, call. = FALSE)
}
pce_index <- data.table(
  pce_year = as.integer(substr(pce_raw$observation_date, 1L, 4L)),
  pce_2017 = pce_raw$DPCERG3A086NBEA
)
pce_index[, pce_factor := 100 / (100 * pce_2017 /
                                   pce_2017[pce_year == 2025L])]
pce_first_year <- min(pce_index$pce_year)

################################################################################
###                    5. Read the panel and the full files                  ###
################################################################################

# 11) The panel records for the three waves, with the totals to reproduce
panel <- data.table::as.data.table(arrow::read_parquet(
  panel_path,
  col_select = c("year", "y1", "yy1", "implicate", "wgt", "age",
                 "age_band", "vehic_nom", "veh_inst_nom", "houses_nom")
))
panel <- panel[year %in% wave_years]
panel[, age_band := as.character(age_band)]

# 12) Unzip each full public file to a temp directory, read the needed
#     columns with haven (labels dropped), and join to the panel on Y1
temp_root <- tempfile("rate_price_")
dir.create(temp_root)
wave_tables <- list()
for (wave_year in wave_years) {
  zip_path <- here::here(input_rows$local_path[
    input_rows$file_role == "full_public" &
      input_rows$year == as.character(wave_year)
  ])
  wave_temp_dir <- file.path(temp_root, wave_year)
  dir.create(wave_temp_dir)
  unzipped <- utils::unzip(zip_path, exdir = wave_temp_dir)
  if (length(unzipped) != 1L || !grepl("\\.dta$", unzipped)) {
    stop(wave_year, " full public zip does not hold exactly one .dta: ",
         paste(basename(unzipped), collapse = ", "), call. = FALSE)
  }
  full <- haven::read_dta(unzipped, col_select = tidyselect::any_of(
    c(keep_columns, toupper(keep_columns))
  ))
  full <- data.table::as.data.table(haven::zap_formats(
    haven::zap_labels(haven::zap_label(full))
  ))
  data.table::setnames(full, tolower(names(full)))
  missing_columns <- setdiff(keep_columns, names(full))
  if (length(missing_columns) > 0L) {
    stop(wave_year, " full public file lacks: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  full[, year := wave_year]

  # 13) One-to-one join on year and Y1; every record must match both ways
  wave_panel <- panel[year == wave_year]
  if (anyDuplicated(full$y1) > 0L || nrow(full) != nrow(wave_panel)) {
    stop(wave_year, ": full file has ", nrow(full), " rows (duplicates: ",
         anyDuplicated(full$y1) > 0L, "); the panel has ", nrow(wave_panel),
         call. = FALSE)
  }
  joined <- merge(wave_panel, full, by = c("year", "y1"),
                  suffixes = c("", "_full"), all = TRUE)
  if (nrow(joined) != nrow(wave_panel) || anyNA(joined$wgt) ||
        anyNA(joined$x2201) || any(joined$yy1 != joined$yy1_full)) {
    stop(wave_year, ": the full file and the panel do not match one to one ",
         "on Y1", call. = FALSE)
  }
  joined[, yy1_full := NULL]
  wave_tables[[as.character(wave_year)]] <- joined
  log_step(paste0(wave_year, ": joined ", nrow(joined), " records on Y1"))
}
unlink(temp_root, recursive = TRUE)
records <- data.table::rbindlist(wave_tables, use.names = TRUE)

################################################################################
###                         6. Reproduce the totals                          ###
################################################################################

# 14) VEHIC, VEH_INST, and HOUSES from their items, nominal dollars, against
#     the panel's _nom columns (02 undoes the extract's 2025 scaling)
vehicle_value_items <- as.matrix(records[, c(
  vehicle_items$value, vehic_other_items
), with = FALSE])
vehicle_value_items[vehicle_value_items < 0] <- 0
vehicle_value_sum <- rowSums(vehicle_value_items)
vehicle_owed_sum <- rowSums(as.matrix(records[, c(
  vehicle_items$owed, veh_inst_other_items
), with = FALSE]))
#     HOUSES caps the farming share X507 at 90% first (macro lines 2314-2316);
#     farm owners have X716 Inap., so the home table below excludes them
farm_share_capped <- pmin(pmax(0, records$x507), 9000)
houses_sum <- rowSums(as.matrix(records[, c("x604", "x614", "x623", "x716"),
                                        with = FALSE])) +
  ((10000 - farm_share_capped) / 10000) * (records$x513 + records$x526)
identity_gaps <- c(
  vehic = max(abs(vehicle_value_sum - records$vehic_nom)),
  veh_inst = max(abs(vehicle_owed_sum - records$veh_inst_nom)),
  houses = max(abs(houses_sum - records$houses_nom))
)
if (any(identity_gaps > identity_tol_dollars)) {
  stop("Full-file items do not reproduce the panel totals; largest gaps ",
       "(nominal $): ", paste(names(identity_gaps), round(identity_gaps, 2),
                              sep = " ", collapse = ", "), call. = FALSE)
}
log_step(paste0(
  "Items reproduce VEHIC, VEH_INST, and HOUSES within $",
  identity_tol_dollars, " (largest gaps $",
  paste(round(identity_gaps, 4), collapse = ", $"), ")"
))

################################################################################
###                        7. Build the household table                      ###
################################################################################

# 15) Survey-year PCE factor for stocks (2025 dollars), as in 02
records <- merge(records, pce_index[, list(year = pce_year,
                                           stock_factor = pce_factor)],
                 by = "year", all.x = TRUE)

# 16) Vehicles owned: X2202 counts them (top-coded at 10); X2201 = 1 owns any.
#     The detailed-vehicle value total is the sum over the first four
records[, owns_vehicle_flag := x2201 == 1L]
records[, vehicle_count := data.table::fifelse(owns_vehicle_flag, x2202, 0)]
detailed_value_items <- as.matrix(records[, vehicle_items$value,
                                          with = FALSE])
detailed_value_items[detailed_value_items < 0] <- 0
records[, detailed_value_nom := rowSums(detailed_value_items)]
records[, vehic_value_nom := vehicle_value_sum]

# 17) The home: an owner of the unit has X716 > 0 or -1 (lifetime tenancy;
#     no value); a purchase (X718 = 5) carries its year X720 and its
#     original cost X717, deflated with the purchase-year index
records[, home_owner_flag := x716 > 0 | x716 == -1]
records[, home_value_nom := data.table::fifelse(x716 > 0, x716, 0)]
records[, home_purchased_flag := home_owner_flag & x718 == 5L]
records[, home_purchase_year := data.table::fifelse(
  home_owner_flag & x720 > 0, x720, NA_real_
)]
records <- merge(
  records,
  pce_index[, list(home_purchase_year = as.numeric(pce_year),
                   purchase_factor = pce_factor)],
  by = "home_purchase_year", all.x = TRUE
)
records[, home_cost_nom := data.table::fifelse(
  home_purchased_flag & x717 > 0, x717, NA_real_
)]

# 18) The first mortgage (X723 = 1 mortgage or 2 land contract). X802 is the
#     year obtained or last modified or refinanced; a purchase mortgage is
#     one taken out in the purchase year (X802 = X720, where X7137 is Inap.).
#     The rate when taken out is X7054 for an adjustable loan whose rate has
#     changed, and otherwise the current rate X816 (U1 Amendment 3). Rates
#     are percent x 100; -1 (no interest) becomes a 0% rate and a flag
records[, mortgage_flag := home_owner_flag & x723 %in% c(1L, 2L)]
records[, mortgage_year := data.table::fifelse(
  mortgage_flag & x802 > 0, x802, NA_real_
)]
records[, purchase_mortgage_flag := mortgage_flag & !is.na(mortgage_year) &
          !is.na(home_purchase_year) & mortgage_year == home_purchase_year]
records[, rate_changed_arm_flag := mortgage_flag & x820 == 1L &
          x7053 == 1L]
records[, rate_current_raw := data.table::fifelse(mortgage_flag, x816,
                                                  NA_real_)]
records[, rate_origination_raw := data.table::fifelse(
  rate_changed_arm_flag, x7054, rate_current_raw
)]
records[, mortgage_no_interest_flag := mortgage_flag &
          rate_origination_raw == -1]
records[, rate_current_pct := pmax(0, rate_current_raw) / 100]
records[, rate_origination_pct := pmax(0, rate_origination_raw) / 100]
records[, mortgage_borrowed_nom := data.table::fifelse(
  mortgage_flag & x804 > 0, x804, NA_real_
)]
records[, mortgage_owed_nom := data.table::fifelse(mortgage_flag, x805,
                                                   NA_real_)]
records <- merge(
  records,
  pce_index[, list(mortgage_year = as.numeric(pce_year),
                   loan_year_factor = pce_factor)],
  by = "mortgage_year", all.x = TRUE
)

# 19) PCE 2025 dollars: stocks with the survey-year factor, the home's cost
#     with the purchase-year factor, the amount borrowed with the loan-year
#     factor (NA when that year precedes the index)
records[, `:=`(
  vehic_value_pce25 = vehic_value_nom * stock_factor,
  detailed_value_pce25 = detailed_value_nom * stock_factor,
  home_value_pce25 = home_value_nom * stock_factor,
  mortgage_owed_pce25 = mortgage_owed_nom * stock_factor,
  home_cost_pce25 = home_cost_nom * purchase_factor,
  mortgage_borrowed_pce25 = mortgage_borrowed_nom * loan_year_factor
)]

households <- records[, list(
  year, y1, yy1, implicate, wgt, age, age_band,
  owns_vehicle_flag, vehicle_count, vehic_value_nom, vehic_value_pce25,
  detailed_value_nom, detailed_value_pce25,
  home_owner_flag, home_purchased_flag, home_purchase_year,
  home_value_nom, home_value_pce25, home_cost_nom, home_cost_pce25,
  mortgage_flag, mortgage_year, purchase_mortgage_flag,
  rate_changed_arm_flag, mortgage_no_interest_flag,
  rate_current_pct, rate_origination_pct,
  mortgage_borrowed_nom, mortgage_borrowed_pce25,
  mortgage_owed_nom, mortgage_owed_pce25
)]
data.table::setorder(households, year, y1)

################################################################################
###                        8. Build the vehicle table                        ###
################################################################################

# 20) One row per detailed vehicle that exists (owns vehicles and the item
#     number is at most X2202), with its age, how it was obtained, and its
#     loan. Vehicle age = survey year - model year; the model year is
#     bottom-coded at survey year - 20, so the oldest class is open-ended
vehicle_list <- list()
for (vehicle_number in 1:4) {
  item_columns <- vapply(vehicle_items, `[`, character(1L), vehicle_number)
  one_vehicle <- records[
    owns_vehicle_flag & x2202 >= vehicle_number,
    c("year", "y1", "yy1", "implicate", "wgt", "age", "age_band",
      "stock_factor", item_columns),
    with = FALSE
  ]
  data.table::setnames(one_vehicle, item_columns, names(vehicle_items))
  one_vehicle[, vehicle_number := vehicle_number]
  vehicle_list[[vehicle_number]] <- one_vehicle
}
vehicles <- data.table::rbindlist(vehicle_list, use.names = TRUE)

# 20a) A vehicle with model year 0 has no recorded model year; every such
#      record has a $0 value (checked 2026-10-12: 20 records in the three
#      waves), so dropping it leaves every total unchanged
no_model_year_flag <- vehicles$model_year <= 0
if (any(vehicles$value[no_model_year_flag] > 0)) {
  stop("A vehicle with no model year has a positive value", call. = FALSE)
}
log_step(paste("Dropped", sum(no_model_year_flag),
               "vehicle records with no model year (all valued $0)"))
vehicles <- vehicles[!no_model_year_flag]

vehicles[, vehicle_age := pmax(0, year - model_year)]
#     (0-1 and 2-4 years are merged into 0-4: the 0-1 cells were thin; owner
#     decision of 2026-10-13, plan Amendments)
vehicles[, vehicle_age_class := data.table::fcase(
  vehicle_age <= 4, "0-4 years",
  vehicle_age <= 9, "5-9 years",
  vehicle_age <= 14, "10-14 years",
  default = "15 years or more"
)]
vehicles[, bought_new_flag := new_used == 1L]
vehicles[, financed_flag := loan_flag == 1L]

# 21) Recent purchase (U1 Amendment 2): bought used with the year got in the
#     survey year or the year before; bought new with a model year that
#     recent; or a loan taken out that recently
vehicles[, recent_purchase_flag :=
           (new_used == 2L & year_got >= year - 1L) |
           (new_used == 1L & model_year >= year - 1L) |
           (financed_flag & loan_year >= year - 1L)]
vehicles[is.na(recent_purchase_flag), recent_purchase_flag := FALSE]

# 22) Dollars and the loan rate (percent x 100; -1 = no interest)
vehicles[, value_nom := pmax(0, value)]
vehicles[, value_pce25 := value_nom * stock_factor]
vehicles[, owed_nom := data.table::fifelse(financed_flag, owed, NA_real_)]
vehicles[, owed_pce25 := owed_nom * stock_factor]
vehicles[, loan_year := data.table::fifelse(financed_flag,
                                            as.numeric(loan_year), NA_real_)]
vehicles <- merge(
  vehicles,
  pce_index[, list(loan_year = as.numeric(pce_year),
                   loan_year_factor = pce_factor)],
  by = "loan_year", all.x = TRUE
)
vehicles[, borrowed_nom := data.table::fifelse(financed_flag & borrowed > 0,
                                               borrowed, NA_real_)]
vehicles[, borrowed_pce25 := borrowed_nom * loan_year_factor]
vehicles[, apr_no_interest_flag := financed_flag & apr == -1]
vehicles[, apr_pct := data.table::fifelse(financed_flag, pmax(0, apr) / 100,
                                          NA_real_)]
vehicles <- vehicles[, list(
  year, y1, yy1, implicate, wgt, age, age_band, vehicle_number,
  model_year, vehicle_age, vehicle_age_class, bought_new_flag,
  recent_purchase_flag, financed_flag, loan_year, apr_pct,
  apr_no_interest_flag, value_nom, value_pce25, owed_nom, owed_pce25,
  borrowed_nom, borrowed_pce25
)]
data.table::setorder(vehicles, year, y1, vehicle_number)

# 23) The detailed vehicles sum to each household's detailed value
detailed_check <- vehicles[, list(total = sum(value_nom)),
                           by = c("year", "y1")]
detailed_check <- merge(households[, c("year", "y1", "detailed_value_nom")],
                        detailed_check, by = c("year", "y1"), all.x = TRUE)
detailed_check[is.na(total), total := 0]
if (max(abs(detailed_check$total - detailed_check$detailed_value_nom)) >
      identity_tol_dollars) {
  stop("Vehicle rows do not sum to the households' detailed vehicle value",
       call. = FALSE)
}
if (pce_first_year > min(households$home_purchase_year, na.rm = TRUE)) {
  log_step(paste0(
    "Home purchases before ", pce_first_year, " have no PCE value: ",
    sum(households$home_purchase_year < pce_first_year, na.rm = TRUE),
    " records keep a nominal cost only"
  ))
}

################################################################################
###                           9. Write the checkpoints                       ###
################################################################################

saveRDS(households, paste0(households_stem, ".rds"))
arrow::write_parquet(households, paste0(households_stem, ".parquet"))
saveRDS(vehicles, paste0(vehicles_stem, ".rds"))
arrow::write_parquet(vehicles, paste0(vehicles_stem, ".parquet"))
log_step(paste0(
  "Wrote ", nrow(households), " household records and ", nrow(vehicles),
  " vehicle records to ", path_processed
))
