# pt02_federal_income_tax -- federal income tax for 2019, 2022, 2025 (TAXSIM)
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U2 of records/plans/2026-10-19_property-tax-descriptives.md. Reads
# pt01's checkpoint and writes data/processed/property_taxes/pt_fedtax.rds:
# one row per household-implicate in the 2019, 2022, and 2025 waves with the
# federal income tax liability after credits (TAXSIM `fiitax`; refundable
# credits make it negative), in nominal survey-year dollars (the tax-year
# amount times the wave's CPILAG, the same uplift the SCF puts on income).
#
# The calculator is NBER TAXSIM 35, run locally by the `usincometaxes`
# package (WebAssembly; no data leave the machine). Federal only: the public
# SCF has no state identifier.
# Tax years and law:
# - 2019 wave: tax year 2018, that year's law.
# - 2022 wave: tax year 2021, that year's law, which carried the one-year
#   American Rescue Plan child tax credit (fully refundable, larger); its
#   rates are not typical and are flagged in pt03's tables.
# - 2025 wave: tax year 2024. The package's TAXSIM stops at 2023, so 2024 is
#   approximated by 2023 law applied to incomes deflated by the IRS
#   2023-to-2024 inflation adjustment, the joint standard deduction ratio
#   $29,200 / $27,700 (Rev. Proc. 2023-34 and Rev. Proc. 2022-38), with the
#   tax reinflated by the same ratio. Unindexed items (the $2,000 child
#   credit) are then slightly off; the approximation is noted in the tables.
# Simplifications (plan, Owner-judgment item 3):
# - A couple files jointly only when X5746 = 1; every other unit files one
#   single or head-of-household return with all of the unit's income.
# - Dependents = KIDS, all assumed under 17 (age 10).
# - Social Security = the reported benefits (X5306, X5311, annualized with
#   MCONV), capped at X5722; the rest of X5722, plus IRA and pension-account
#   withdrawals, is taxed as pensions.
# - Itemized deductions: the property tax (pt01) and the first mortgage's
#   interest (balance X805 x rate X816); state income tax is not known.
# Run from the project root as
#   Rscript code/property_taxes/pt02_federal_income_tax.R

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed
required_packages <- c("here", "data.table", "usincometaxes")
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
here::i_am("code/property_taxes/pt02_federal_income_tax.R")
path_processed <- here::here("data", "processed", "property_taxes")
households_path <- file.path(path_processed, "pt_households.rds")
fedtax_path <- file.path(path_processed, "pt_fedtax.rds")

# 3) Wave -> tax year run in TAXSIM, and the scale applied to every dollar
#    input (and undone on the tax) to stand in for the true tax year
wave_setup <- data.table(
  year = c(2019L, 2022L, 2025L),
  tax_year_true = c(2018L, 2021L, 2024L),
  tax_year_run = c(2018L, 2021L, 2023L),
  dollar_scale = c(1, 1, 27700 / 29200)
)

# 4) CPILAG of each wave (bulletin.macro.txt lines 132, 144, 150, as in 02):
#    the uplift from prior-year to survey-year dollars on INCOME
cpilag <- c("2019" = 3758 / 3691, "2022" = 4315 / 3992, "2025" = 4750 / 4625)

# 5) MCONV monthly factors for the Social Security frequency (pt01)
mconv_factor <- c(
  "2" = 52 / 12, "3" = 26 / 12, "4" = 1, "5" = 1 / 3, "6" = 1 / 12,
  "11" = 1 / 6, "12" = 1 / 2, "31" = 2, "23" = 13 / 12, "24" = 52 / 72
)

################################################################################
###                              3. Define helpers                           ###
################################################################################

# 6) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

# 7) An SCF amount where -1 means "nothing": negative codes become 0
nonneg <- function(value) {
  value[is.na(value) | value < 0] <- 0
  value
}

# 8) Annual Social Security from a benefit and its frequency (unknown
#    frequencies, such as -7 = other, read as monthly, the modal code)
annual_benefit <- function(amount, frequency) {
  monthly_factor <- unname(mconv_factor[as.character(frequency)])
  monthly_factor[is.na(monthly_factor)] <- 1
  nonneg(amount) * monthly_factor * 12
}

################################################################################
###                         4. Build the TAXSIM input                        ###
################################################################################

# 9) The three waves from pt01
households <- readRDS(households_path)
taxsim_base <- households[year %in% wave_setup$year]
taxsim_base <- merge(taxsim_base, wave_setup, by = "year")
taxsim_base[, cpilag := unname(cpilag[as.character(year)])]

# 10) Filing: joint when the couple filed jointly; spouse age only then
taxsim_base[, joint_flag := married == 1L & x5746 == 1L]
taxsim_base[, mstat := data.table::fifelse(joint_flag, "married, jointly",
                                           "single")]
taxsim_base[, sage := data.table::fifelse(joint_flag, pmax(x19, 18), 0)]
taxsim_base[, depx := pmin(kids, 3L)]

# 11) Social Security from the benefit items, capped at X5722; the rest of
#     X5722 and the account withdrawals (prior-year dollars) are pensions
taxsim_base[, ss_reported := annual_benefit(x5306, x5307) +
              annual_benefit(x5311, x5312)]
taxsim_base[, ss_pension_total := nonneg(x5722)]
taxsim_base[, gssi := pmin(ss_reported, ss_pension_total)]
taxsim_base[, pensions := ss_pension_total - gssi +
              nonneg(penacctwd_nom / cpilag)]

# 12) Deductions: property tax and first-mortgage interest, prior-year
taxsim_base[, proptax := data.table::fifelse(is.na(ptax_nom), 0,
                                             ptax_nom / cpilag)]
taxsim_base[, mortgage := nonneg(x805) * nonneg(x816) / 10000]

# 13) The TAXSIM frame; capital gains and self-employment keep their sign
#     (a -1 "nothing" code becomes 0), and every dollar input is scaled
taxsim_base[, taxsimid := seq_len(.N)]
dollar_inputs <- list(
  pwages = quote(nonneg(x5702)),
  psemp = quote(data.table::fifelse(x5704 == -1, 0, x5704)),
  intrec = quote(nonneg(x5708)),
  dividends = quote(nonneg(x5710)),
  ltcg = quote(data.table::fifelse(x5712 == -1, 0, x5712)),
  otherprop = quote(data.table::fifelse(x5714 == -1, 0, x5714)),
  pui = quote(nonneg(x5716)),
  gssi = quote(gssi),
  pensions = quote(pensions),
  proptax = quote(proptax),
  mortgage = quote(mortgage)
)
taxsim_input <- taxsim_base[, list(
  taxsimid, year = tax_year_run, mstat, state = "No State",
  page = pmax(age, 18), sage, depx,
  age1 = data.table::fifelse(depx >= 1L, 10, 0),
  age2 = data.table::fifelse(depx >= 2L, 10, 0),
  age3 = data.table::fifelse(depx >= 3L, 10, 0)
)]
for (input_name in names(dollar_inputs)) {
  data.table::set(
    taxsim_input, j = input_name,
    value = taxsim_base[, eval(dollar_inputs[[input_name]])] *
      taxsim_base$dollar_scale
  )
}
log_step(paste0("TAXSIM input: ", nrow(taxsim_input), " household-implicate ",
                "rows (", taxsim_base[, sum(joint_flag)], " joint)"))

################################################################################
###                              5. Run TAXSIM                               ###
################################################################################

# 14) Federal liability after credits, by wave (one call per tax year)
taxsim_output <- usincometaxes::taxsim_calculate_taxes(
  as.data.frame(taxsim_input),
  marginal_tax_rates = "Wages",
  return_all_information = FALSE
)
taxsim_output <- data.table::as.data.table(taxsim_output)
if (nrow(taxsim_output) != nrow(taxsim_input) ||
      anyNA(taxsim_output$fiitax)) {
  stop("TAXSIM returned ", nrow(taxsim_output), " rows for ",
       nrow(taxsim_input), " inputs, or a missing liability", call. = FALSE)
}

################################################################################
###                    6. Back to survey-year dollars and save               ###
################################################################################

# 15) Undo the scale, then put the tax in survey-year dollars with CPILAG
fedtax <- merge(
  taxsim_base[, list(taxsimid, year, y1, implicate, tax_year_true,
                     tax_year_run, dollar_scale, cpilag, joint_flag)],
  taxsim_output[, list(taxsimid, fiitax)],
  by = "taxsimid"
)
fedtax[, fedtax_nom := fiitax / dollar_scale * cpilag]
fedtax[, c("taxsimid", "fiitax") := NULL]

# 16) A diagnostic in the log: aggregate federal tax over aggregate income
diagnostic <- merge(
  fedtax, households[, list(year, y1, wgt, income_nom)], by = c("year", "y1")
)[, list(
  fedtax_total_bn = sum(wgt * fedtax_nom) / 1e9,
  income_total_bn = sum(wgt * income_nom) / 1e9
), by = year]
diagnostic[, average_rate := fedtax_total_bn / income_total_bn]
for (row_index in seq_len(nrow(diagnostic))) {
  log_step(paste0(
    diagnostic$year[row_index], ": federal income tax $",
    round(diagnostic$fedtax_total_bn[row_index]), " billion on income $",
    round(diagnostic$income_total_bn[row_index]), " billion (",
    round(100 * diagnostic$average_rate[row_index], 1), "%)"
  ))
}

saveRDS(fedtax, fedtax_path)
log_step(paste0("Wrote ", nrow(fedtax), " rows to ", fedtax_path))
