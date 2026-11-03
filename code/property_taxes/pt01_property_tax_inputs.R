# pt01_property_tax_inputs -- property tax and income-tax inputs, 1989-2025
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U1 of records/plans/2026-10-19_property-tax-descriptives.md. Reads the
# full public SCF file of every wave (hashes checked against
# data/raw/scf/manifest.csv) and the panel written by 02, and writes, to
# data/processed/property_taxes/:
#   pt_households.rds/.parquet  one row per household-implicate, all waves:
#                               the annual property tax on the home, owner
#                               and mortgage-free flags, the panel's cuts,
#                               income, net worth, and home value; for 2019,
#                               2022, and 2025 also the income items TAXSIM
#                               needs (pt02)
# Variable definitions, codes, and codebook lines are in the variable doc,
# data/docs/scf/scf_property_tax_variables.md, under the module's name.
# Checks (each stops the script before anything is written):
# - every full-file record joins one-to-one to a panel record on Y1;
# - every X722 frequency code with a tax amount is one the Fed's MCONV
#   macro converts (bulletin.macro.txt lines 26-32);
# - owners (X721 not Inap.) agree with the extract's HOUSECL = 1.
# Run from the project root as
#   Rscript code/property_taxes/pt01_property_tax_inputs.R

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here", "data.table", "haven", "arrow", "digest",
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
library(data.table)

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at its path under the project root
here::i_am("code/property_taxes/pt01_property_tax_inputs.R")
path_scf_raw <- here::here("data", "raw", "scf")
path_out <- here::here("data", "processed", "property_taxes")
manifest_path <- file.path(path_scf_raw, "manifest.csv")
panel_path <- here::here("data", "processed", "scf_panel_1989_2025.parquet")
households_stem <- file.path(path_out, "pt_households")

# 3) The waves, and the three that get federal income tax (pt02)
wave_years <- c(1989L, 1992L, 1995L, 1998L, 2001L, 2004L, 2007L, 2010L,
                2013L, 2016L, 2019L, 2022L, 2025L)
taxsim_years <- c(2019L, 2022L, 2025L)

# 4) Items read from every wave: the property tax amount and frequency, the
#    farm or ranch home type, and the home value (for the tax-to-value rate)
ptax_items <- c("x721", "x722", "x508", "x716")

# 5) Items read for the TAXSIM waves (codebook lines in the variable doc):
#    income by source, Social Security benefits, filing, spouse age, and the
#    first mortgage's balance and rate (for the mortgage-interest deduction)
taxsim_items <- c(
  "x5702", "x5704", "x5708", "x5710", "x5712", "x5714", "x5716", "x5722",
  "x5306", "x5307", "x5311", "x5312", "x5744", "x5746", "x19",
  "x805", "x816"
)

# 6) The Fed's MCONV codes (payments to a monthly basis): code = monthly
#    multiplier (bulletin.macro.txt lines 29-31)
mconv_factor <- c(
  "2" = 52 / 12, "3" = 26 / 12, "4" = 1, "5" = 1 / 3, "6" = 1 / 12,
  "11" = 1 / 6, "12" = 1 / 2, "31" = 2, "23" = 13 / 12, "24" = 52 / 72
)

# 7) Owner-HOUSECL agreement required (share of records), and the most
#    records whose frequency code MCONV does not convert (all waves)
owner_agreement_min <- 0.995
unconverted_max <- 50L

################################################################################
###                              3. Define helpers                           ###
################################################################################

# 8) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

# 9) Annualize an amount with its MCONV frequency code; NA when the code is
#    not one MCONV converts
annualize_mconv <- function(amount, frequency) {
  monthly_factor <- unname(mconv_factor[as.character(frequency)])
  amount * monthly_factor * 12
}

################################################################################
###                   4. Check the inputs and read the panel                 ###
################################################################################

# 10) Every wave's full public file must match its manifest hash
manifest <- utils::read.csv(
  manifest_path,
  colClasses = "character", na.strings = "", check.names = FALSE
)
input_rows <- manifest[
  manifest$file_role == "full_public" &
    manifest$year %in% as.character(wave_years), ,
  drop = FALSE
]
if (nrow(input_rows) != length(wave_years)) {
  stop("Expected ", length(wave_years), " full public files in the ",
       "manifest, found ", nrow(input_rows), call. = FALSE)
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
log_step(paste(nrow(input_rows), "full public files match the manifest"))

# 11) Panel columns the module uses: ids, weight, cuts, income (survey-year
#     nominal, with CPILAG) and its prior-year components, and the balance
#     sheet items that define owning outright
panel_columns <- c(
  "year", "y1", "yy1", "implicate", "wgt", "age", "age_group_fed",
  "race4", "race_2025_break", "educ4", "sex_cut", "family_structure",
  "homeowner", "income_group", "inccat", "nwcat", "married",
  "kids",
  "income_nom", "income_pce25", "networth_nom", "networth_pce25",
  "houses_nom", "houses_pce25", "nh_mort_nom", "heloc_nom",
  "penacctwd_nom"
)
panel <- data.table::as.data.table(arrow::read_parquet(
  panel_path, col_select = tidyselect::all_of(panel_columns)
))
for (factor_col in names(panel)[vapply(panel, is.factor, logical(1L))]) {
  data.table::set(panel, j = factor_col,
                  value = as.character(panel[[factor_col]]))
}

################################################################################
###                        5. Read the full files                            ###
################################################################################

# 12) Unzip each full public file to a temp directory, read the needed
#     columns (labels dropped), and join to the panel on Y1 (X1 in 1989,
#     as 02 does)
temp_root <- tempfile("property_tax_")
dir.create(temp_root)
wave_tables <- list()
for (wave_year in wave_years) {
  zip_path <- here::here(input_rows$local_path[
    input_rows$year == as.character(wave_year)
  ])
  wave_temp_dir <- file.path(temp_root, wave_year)
  dir.create(wave_temp_dir)
  unzipped <- utils::unzip(zip_path, exdir = wave_temp_dir)
  if (length(unzipped) != 1L || !grepl("\\.dta$", unzipped)) {
    stop(wave_year, " full public zip does not hold exactly one .dta",
         call. = FALSE)
  }
  id_column <- if (wave_year == 1989L) "x1" else "y1"
  wanted <- c(id_column, ptax_items,
              if (wave_year %in% taxsim_years) taxsim_items)
  full <- haven::read_dta(unzipped, col_select = tidyselect::any_of(
    c(wanted, toupper(wanted))
  ))
  full <- data.table::as.data.table(haven::zap_formats(
    haven::zap_labels(haven::zap_label(full))
  ))
  data.table::setnames(full, tolower(names(full)))
  missing_columns <- setdiff(wanted, names(full))
  if (length(missing_columns) > 0L) {
    stop(wave_year, " full public file lacks: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  data.table::setnames(full, id_column, "y1")
  full[, year := wave_year]

  # 13) One-to-one join on year and Y1
  wave_panel <- panel[year == wave_year]
  if (anyDuplicated(full$y1) > 0L || nrow(full) != nrow(wave_panel)) {
    stop(wave_year, ": full file has ", nrow(full), " rows; the panel has ",
         nrow(wave_panel), call. = FALSE)
  }
  joined <- merge(wave_panel, full, by = c("year", "y1"), all = TRUE)
  if (nrow(joined) != nrow(wave_panel) || anyNA(joined$wgt) ||
        anyNA(joined$x721)) {
    stop(wave_year, ": the full file and the panel do not match one to one",
         call. = FALSE)
  }
  wave_tables[[as.character(wave_year)]] <- joined
  log_step(paste0(wave_year, ": joined ", nrow(joined), " records on Y1"))
}
unlink(temp_root, recursive = TRUE)
records <- data.table::rbindlist(wave_tables, use.names = TRUE, fill = TRUE)

################################################################################
###                       6. Build the property tax                          ###
################################################################################

# 14) Owner: X721 is asked of every owner of any part of the home; 0 is Inap.
records[, owner_flag := x721 != 0]
owner_agreement <- records[, mean(owner_flag == homeowner)]
if (owner_agreement < owner_agreement_min) {
  stop("Owners (X721 not Inap.) agree with HOUSECL = 1 on only ",
       round(100 * owner_agreement, 2), "% of records", call. = FALSE)
}
log_step(paste0("Owners agree with HOUSECL = 1 on ",
                round(100 * owner_agreement, 3), "% of records"))

# 15) A frequency MCONV does not convert: 25 (every two years) is read as
#     half the amount yearly, and any other (20 = five times a year, 22 =
#     varies, -7 = other) as a yearly amount, the modal code; stop if that
#     touches more than a handful of records
unconverted <- records[
  x721 > 0 & !as.character(x722) %in% names(mconv_factor),
  list(n_records = .N), by = list(year, x722)
]
if (sum(unconverted$n_records) > unconverted_max) {
  stop(sum(unconverted$n_records), " records have a tax amount with a ",
       "frequency code MCONV does not convert", call. = FALSE)
}
if (nrow(unconverted) > 0L) {
  log_step(paste0(
    "Frequency codes MCONV does not convert, read as yearly: ",
    paste0(unconverted$year, " code ", unconverted$x722, " (",
           unconverted$n_records, " records)", collapse = "; ")
  ))
}
records[x721 > 0 & x722 == 25, x721 := x721 / 2]
records[x721 > 0 & !as.character(x722) %in% names(mconv_factor), x722 := 6]

# 16) Annual property tax, survey-year nominal dollars: X721 = -1 (none) is
#     $0, a positive amount is annualized, non-owners are NA
records[, ptax_nom := data.table::fcase(
  x721 > 0, annualize_mconv(x721, x722),
  x721 == -1, 0,
  default = NA_real_
)]
records[, ptax_payer_flag := owner_flag & ptax_nom > 0]
records[, farm_ranch_flag := owner_flag & x508 %in% c(1L, 2L)]

# 17) PCE 2025 dollars with the survey-year factor the panel used for the
#     home value (houses_pce25 / houses_nom is that factor for every record
#     with a home; it is the same within a wave)
pce_factor <- records[houses_nom > 0,
                      list(stock_factor = stats::median(houses_pce25 /
                                                          houses_nom)),
                      by = year]
records <- merge(records, pce_factor, by = "year", all.x = TRUE)
records[, ptax_pce25 := ptax_nom * stock_factor]

# 18) Owning outright: an owner with no mortgage or land contract on the
#     home and no home equity line; the looser version drops the HELOC test
records[, owns_outright_flag := owner_flag & nh_mort_nom <= 0 &
          heloc_nom <= 0]
records[, no_first_lien_flag := owner_flag & nh_mort_nom <= 0]
records[, tenure := data.table::fcase(
  !owner_flag, "Renter or other",
  owns_outright_flag, "Owner, no mortgage",
  default = "Owner with mortgage or HELOC"
)]

# 19) Shares: the tax over income (survey-year nominal on both sides; NA
#     when income is not positive) and over the home's value (X716 > 0)
records[, ptax_income_ratio := data.table::fifelse(
  income_nom > 0 & !is.na(ptax_nom), ptax_nom / income_nom, NA_real_
)]
records[, ptax_value_ratio := data.table::fifelse(
  x716 > 0 & !is.na(ptax_nom), ptax_nom / x716, NA_real_
)]
records[, ptax_networth_ratio := data.table::fifelse(
  networth_nom > 0 & !is.na(ptax_nom), ptax_nom / networth_nom, NA_real_
)]

# 20) For sums over all households, the tax with renters at $0
records[, ptax_all_nom := data.table::fifelse(is.na(ptax_nom), 0, ptax_nom)]
records[, ptax_all_pce25 := ptax_all_nom * stock_factor]

log_step(paste0(
  "2025: ", records[year == 2025L & implicate == 1L & owner_flag, .N],
  " owner cases; ",
  records[year == 2025L & implicate == 1L & farm_ranch_flag, .N],
  " farm or ranch owner cases"
))

################################################################################
###                              7. Save                                     ###
################################################################################

# 21) Write the checkpoint (rds keeps types; parquet for other tools)
dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
data.table::setorder(records, year, y1)
saveRDS(records, paste0(households_stem, ".rds"))
arrow::write_parquet(records, paste0(households_stem, ".parquet"))
log_step(paste0("Wrote ", nrow(records), " household-implicate rows to ",
                households_stem, ".rds/.parquet"))
