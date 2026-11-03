# pt03_property_tax_tables -- who pays property tax, how much, and vs income tax
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U3 of records/plans/2026-10-19_property-tax-descriptives.md. Reads
# pt01's and pt02's checkpoints and the replicate weights, and writes to
# output/property_taxes/tables/ (columns: output/property_taxes/README.md):
#   pt_who_pays.csv        2025, by cut: the share of households, of
#                          payers, of property tax dollars, of income, and
#                          of net worth in each group, and the payer rate
#   pt_levels.csv          2025, by cut: property tax among payers (mean,
#                          median, share of home value, share of net
#                          worth) and the total in billions
#   pt_income_share.csv    2025, by cut: property tax as a share of income
#                          (median among payers, ratio of totals among
#                          payers and among all households), and the shares
#                          of payers above 5 and 10 percent of income
#   pt_progressivity.csv   2019, 2022, 2025, by income group: effective
#                          rates and shares of property tax and federal
#                          income tax
#   pt_suits.csv           2019, 2022, 2025: Suits indexes of the two taxes
#   pt_owns_outright.csv   2025: owners with no mortgage against other
#                          owners and renters, and where the dollars of an
#                          exemption for them would go
#   pt_trend.csv           1989-2025: ownership, owning outright, the tax's
#                          level, and who pays it
# Every estimate carries an SE by the method of code/03_scf_estimators.R
# (ratios by the same method in pt00), except totals in billions and
# medians of tax-to-value ratios, which are point estimates. Income and net
# worth groups are the Fed's INCCAT and NWCAT, held fixed in the replicates,
# so their SEs are slightly understated.
# Caveats for every number built here:
# - The SCF records the property tax owners pay on their own home. Taxes on
#   other real estate, on businesses, and the part of landlords' taxes that
#   renters pay through rent are not measured; renters show $0.
# - Farm and ranch owners' taxes cover the farm or ranch.
# - Federal income tax is simulated (pt02); 2021 carried the one-year
#   expanded child tax credit, and 2024 is approximated with 2023 law.
# Run from the project root as
#   Rscript code/property_taxes/pt03_property_tax_tables.R

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

# 2) Stop unless this script sits at its path under the project root
here::i_am("code/property_taxes/pt03_property_tax_tables.R")
path_processed <- here::here("data", "processed", "property_taxes")
repwgts_path <- here::here("data", "processed",
                           "scf_repwgts_1989_2025.parquet")
path_tables <- here::here("output", "property_taxes", "tables")
source(here::here("code", "03_scf_estimators.R"))
source(here::here("code", "property_taxes", "pt00_functions.R"))

# 3) Lead year, the income-tax waves, and the 2021 flag
lead_year <- 2025L
fedtax_years <- c(2019L, 2022L, 2025L)
fedtax_notes <- c(
  "2019" = "tax year 2018 law",
  "2022" = "tax year 2021 law: one-year expanded child tax credit",
  "2025" = "tax year 2024 approximated by 2023 law, indexed"
)

# 4) Cuts for the 2025 tables: column -> label
cut_columns <- c(
  all_group = "All households",
  age_group_fed = "Age of reference person",
  income_group = "Income percentile",
  networth_group = "Net worth percentile",
  race4 = "Race and ethnicity",
  educ4 = "Education",
  family_structure = "Family structure",
  tenure = "Housing tenure"
)

# 5) NWCAT labels (the Fed's net worth percentile groups, 1-5)
networth_labels <- c("Less than 25", "25-49.9", "50-74.9", "75-89.9",
                     "90-100")

# 6) Burden thresholds: property tax as a share of income
burden_thresholds <- c(0.05, 0.10)

################################################################################
###                              3. Define helpers                           ###
################################################################################

# 7) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

# 8) Put an estimate table into the long layout every output shares
as_long <- function(estimates, measure, cut_column = NULL, note = "") {
  # data.table columns used unquoted, declared for lintr (as in 16)
  group <- estimate <- se <- n_unweighted <- NULL
  out <- data.table::copy(estimates)
  if (is.null(cut_column)) {
    out[, cut := "All households"]
    out[, group := "All households"]
  } else {
    out[, cut := cut_columns[[cut_column]]]
    data.table::setnames(out, cut_column, "group")
  }
  out[, measure := measure]
  out[, note := note]
  out[, list(year, cut, group, measure, estimate, se, n_unweighted, note)]
}

# 9) Mean, median, or share of `var` by one cut
estimate_by_cut <- function(data, var, stat, cut_column, measure,
                            subset = NULL, note = "") {
  estimates <- scf_estimate( # nolint: object_usage_linter.
    data, repwgts, var, stat = stat, by = cut_column, subset = subset
  )
  as_long(estimates, measure, cut_column, note)
}

# 10) Ratio of totals sum(num) / sum(den) by one cut
ratio_by_cut <- function(data, num, den, cut_column, measure,
                         subset = NULL, note = "") {
  estimates <- pt_ratio_estimate( # nolint: object_usage_linter.
    data, repwgts, num, den, by = cut_column, subset = subset
  )
  as_long(estimates, measure, cut_column, note)
}

# 11) Each group's share of the total of `var` (households in `subset`):
#     sum(var x 1[group]) / sum(var), one ratio per group
share_of_total <- function(data, var, cut_column, measure, subset = NULL,
                           note = "") {
  share_num <- NULL
  groups <- sort(unique(stats::na.omit(data[[cut_column]])))
  pieces <- lapply(groups, function(group_value) {
    data[, share_num := data.table::fifelse(
      !is.na(get(cut_column)) & get(cut_column) == group_value,
      get(var), 0
    )]
    estimates <- pt_ratio_estimate( # nolint: object_usage_linter.
      data, repwgts, "share_num", var, subset = subset
    )
    estimates[, (cut_column) := group_value]
    estimates
  })
  data[, share_num := NULL]
  as_long(data.table::rbindlist(pieces, use.names = TRUE), measure,
          cut_column, note)
}

# 12) Totals in billions of PCE 2025 dollars (WGT sums the five implicates
#     to one population), point estimates
total_by_cut <- function(data, var, cut_column, measure, subset = "TRUE") {
  wgt <- implicate <- NULL
  subset_mask <- data[, eval(parse(text = subset))]
  rows <- data[subset_mask]
  out <- rows[, list(
    estimate = sum(wgt * get(var)) / 1e9,
    se = NA_real_,
    n_unweighted = sum(implicate == 1L)
  ), by = c("year", cut_column)]
  as_long(out, measure, cut_column, "billions, PCE 2025 dollars; no SE")
}

# 13) Suits index with its SE: pooled point estimate; imputation variance
#     from the five implicates; sampling variance from the 999 replicate
#     weights on implicate 1 (households held in income order)
suits_with_se <- function(data, tax_var, subset_expr) {
  income_nom <- wgt <- implicate <- NULL
  subset_mask <- data[, eval(parse(text = subset_expr))]
  rows <- data[subset_mask & data$income_nom > 0]
  point <- pt_suits_index( # nolint: object_usage_linter.
    rows$income_nom, rows[[tax_var]], rows$wgt
  )
  by_implicate <- rows[, list(
    suits = pt_suits_index( # nolint: object_usage_linter.
      income_nom, get(tax_var), wgt
    )
  ), by = implicate]
  first_imp <- rows[implicate == 1L][order(income_nom)]
  match_index <- repwgts[first_imp, on = c("year", "y1"), which = TRUE,
                         mult = "first"]
  rw_cols <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
  weight_matrix <- as.matrix(repwgts[match_index, rw_cols, with = FALSE])
  weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0
  income_cum <- apply(weight_matrix * first_imp$income_nom, 2L, cumsum)
  tax_cum <- apply(weight_matrix * first_imp[[tax_var]], 2L, cumsum)
  income_share <- rbind(0, sweep(income_cum, 2L,
                                 income_cum[nrow(income_cum), ], "/"))
  tax_share <- rbind(0, sweep(tax_cum, 2L, tax_cum[nrow(tax_cum), ], "/"))
  steps <- nrow(income_share)
  area <- colSums(
    (income_share[-1L, , drop = FALSE] -
       income_share[-steps, , drop = FALSE]) *
      (tax_share[-1L, , drop = FALSE] + tax_share[-steps, , drop = FALSE]) /
      2
  )
  replicate_suits <- 1 - area / 0.5
  se_sampling <- stats::sd(replicate_suits)
  se_imputation <- stats::sd(by_implicate$suits)
  data.table::data.table(
    estimate = point,
    se = sqrt(se_sampling^2 + (6 / 5) * se_imputation^2),
    n_unweighted = nrow(first_imp)
  )
}

################################################################################
###                              4. Read the data                            ###
################################################################################

# 14) Households (pt01), the federal tax (pt02), and the replicate weights
households <- readRDS(file.path(path_processed, "pt_households.rds"))
fedtax <- readRDS(file.path(path_processed, "pt_fedtax.rds"))
households <- merge(
  households, fedtax[, list(year, y1, fedtax_nom)],
  by = c("year", "y1"), all.x = TRUE
)
repwgts <- data.table::as.data.table(arrow::read_parquet(repwgts_path))

# 15) Derived columns: labels, 0/1 flags for shares, income floored at 0
#     for income shares, and the home value in PCE 2025 dollars
households[, all_group := "All households"]
households[, networth_group := networth_labels[nwcat]]
households[, age_65_plus_flag := age >= 65]
households[, one := 1]
households[, owner01 := as.numeric(owner_flag)]
households[, payer01 := as.numeric(ptax_payer_flag)]
households[, outright01 := as.numeric(owns_outright_flag)]
households[, no_first_lien01 := as.numeric(no_first_lien_flag)]
households[, age_65_plus01 := as.numeric(age_65_plus_flag)]
households[, income_pos_nom := pmax(income_nom, 0)]
households[, home_value_pce25 := data.table::fifelse(
  x716 > 0, x716 * stock_factor, NA_real_
)]
for (threshold in burden_thresholds) {
  households[, (paste0("burden_over_", 100 * threshold)) :=
               as.numeric(ptax_income_ratio > threshold)]
}
lead <- households[year == lead_year]
log_step(paste0("Read ", nrow(households), " household-implicate rows; ",
                nrow(lead), " in ", lead_year))

################################################################################
###                           5. Who pays (2025)                             ###
################################################################################

# 16) For each cut: shares of households, payers, tax, income, net worth,
#     and the payer rate within the group
who_pays <- list()
for (cut_column in names(cut_columns)) {
  who_pays[[cut_column]] <- data.table::rbindlist(list(
    share_of_total(lead, "one", cut_column, "share_of_households"),
    share_of_total(lead, "payer01", cut_column, "share_of_payers"),
    share_of_total(lead, "ptax_all_nom", cut_column,
                   "share_of_property_tax"),
    share_of_total(lead, "income_pos_nom", cut_column, "share_of_income"),
    share_of_total(lead, "networth_nom", cut_column, "share_of_networth"),
    estimate_by_cut(lead, "payer01", "share", cut_column, "payer_rate"),
    estimate_by_cut(lead, "owner01", "share", cut_column, "owner_rate")
  ))
}
who_pays <- data.table::rbindlist(who_pays)
log_step(paste("Who pays:", nrow(who_pays), "rows"))

################################################################################
###                        6. Levels and shares (2025)                       ###
################################################################################

# 17) Among payers: mean and median tax, tax over home value and over net
#     worth, and the total tax by group
payer_subset <- "ptax_payer_flag"
levels_table <- list()
income_table <- list()
for (cut_column in names(cut_columns)) {
  levels_table[[cut_column]] <- data.table::rbindlist(list(
    estimate_by_cut(lead, "ptax_pce25", "mean", cut_column,
                    "mean_tax_payers", payer_subset,
                    "PCE 2025 dollars"),
    estimate_by_cut(lead, "ptax_pce25", "median", cut_column,
                    "median_tax_payers", payer_subset,
                    "PCE 2025 dollars"),
    estimate_by_cut(lead, "ptax_value_ratio", "median", cut_column,
                    "median_tax_to_home_value_payers",
                    "ptax_payer_flag & x716 > 0"),
    ratio_by_cut(lead, "ptax_nom", "x716", cut_column,
                 "tax_to_home_value_totals_payers",
                 "ptax_payer_flag & x716 > 0"),
    estimate_by_cut(lead, "ptax_networth_ratio", "median", cut_column,
                    "median_tax_to_networth_payers",
                    "ptax_payer_flag & networth_nom > 0"),
    total_by_cut(lead, "ptax_all_pce25", cut_column, "total_tax_billions")
  ))

  # 18) Tax over income: median among payers; ratio of totals among payers
  #     and among all households with positive income; burden shares
  income_table[[cut_column]] <- data.table::rbindlist(list(
    estimate_by_cut(lead, "ptax_income_ratio", "median", cut_column,
                    "median_tax_to_income_payers",
                    "ptax_payer_flag & income_nom > 0"),
    ratio_by_cut(lead, "ptax_nom", "income_nom", cut_column,
                 "tax_to_income_totals_payers",
                 "ptax_payer_flag & income_nom > 0"),
    ratio_by_cut(lead, "ptax_all_nom", "income_nom", cut_column,
                 "tax_to_income_totals_all", "income_nom > 0",
                 "renters count as $0"),
    estimate_by_cut(lead, "burden_over_5", "share", cut_column,
                    "share_payers_tax_over_5pct_income",
                    "ptax_payer_flag & income_nom > 0"),
    estimate_by_cut(lead, "burden_over_10", "share", cut_column,
                    "share_payers_tax_over_10pct_income",
                    "ptax_payer_flag & income_nom > 0")
  ))
}
levels_table <- data.table::rbindlist(levels_table)
income_table <- data.table::rbindlist(income_table)
log_step(paste("Levels:", nrow(levels_table), "rows; income shares:",
               nrow(income_table), "rows"))

################################################################################
###                  7. Progressivity against the income tax                 ###
################################################################################

# 19) By income group, in each income-tax wave: effective rates (ratios of
#     totals over households with positive income) and shares of each tax
fed_waves <- households[year %in% fedtax_years]
progressivity <- list()
for (wave_year in fedtax_years) {
  wave <- fed_waves[year == wave_year]
  wave_note <- fedtax_notes[[as.character(wave_year)]]
  progressivity[[as.character(wave_year)]] <- data.table::rbindlist(lapply(
    c("all_group", "income_group"), function(cut_column) {
      data.table::rbindlist(list(
        ratio_by_cut(wave, "ptax_all_nom", "income_nom", cut_column,
                     "property_tax_rate_all", "income_nom > 0",
                     "renters count as $0"),
        ratio_by_cut(wave, "ptax_nom", "income_nom", cut_column,
                     "property_tax_rate_owners",
                     "owner_flag & income_nom > 0"),
        ratio_by_cut(wave, "fedtax_nom", "income_nom", cut_column,
                     "federal_income_tax_rate", "income_nom > 0",
                     wave_note),
        share_of_total(wave, "income_pos_nom", cut_column,
                       "share_of_income", "income_nom > 0"),
        share_of_total(wave, "ptax_all_nom", cut_column,
                       "share_of_property_tax", "income_nom > 0"),
        share_of_total(wave, "fedtax_nom", cut_column,
                       "share_of_federal_income_tax", "income_nom > 0",
                       wave_note)
      ))
    }
  ))
}
progressivity <- data.table::rbindlist(progressivity)

# 20) Suits indexes: property tax over all households, over owners only
#     (ranked among owners), and the federal income tax
suits <- data.table::rbindlist(lapply(fedtax_years, function(wave_year) {
  wave <- fed_waves[year == wave_year]
  data.table::rbindlist(list(
    cbind(data.table::data.table(year = wave_year,
                                 tax = "Property tax, all households"),
          suits_with_se(wave, "ptax_all_nom", "TRUE")),
    cbind(data.table::data.table(year = wave_year,
                                 tax = "Property tax, owners only"),
          suits_with_se(wave, "ptax_nom", "owner_flag")),
    cbind(data.table::data.table(year = wave_year,
                                 tax = "Federal income tax"),
          suits_with_se(wave, "fedtax_nom", "TRUE"))
  ))
}))
suits[, note := fedtax_notes[as.character(year)]]
log_step(paste("Progressivity:", nrow(progressivity), "rows; Suits:",
               nrow(suits), "rows"))

################################################################################
###                  8. Owners with no mortgage (2025)                       ###
################################################################################

# 21) Profile by tenure: age, income, net worth, home value, and the tax
outright_profile <- data.table::rbindlist(list(
  estimate_by_cut(lead, "age", "median", "tenure", "median_age"),
  estimate_by_cut(lead, "age_65_plus01", "share", "tenure", "share_65_plus"),
  estimate_by_cut(lead, "income_pce25", "median", "tenure",
                  "median_income", note = "PCE 2025 dollars"),
  estimate_by_cut(lead, "networth_pce25", "median", "tenure",
                  "median_networth", note = "PCE 2025 dollars"),
  estimate_by_cut(lead, "home_value_pce25", "median", "tenure",
                  "median_home_value", "x716 > 0", "PCE 2025 dollars"),
  estimate_by_cut(lead, "ptax_pce25", "median", "tenure",
                  "median_tax_payers", payer_subset, "PCE 2025 dollars"),
  estimate_by_cut(lead, "ptax_income_ratio", "median", "tenure",
                  "median_tax_to_income_payers",
                  "ptax_payer_flag & income_nom > 0"),
  ratio_by_cut(lead, "ptax_nom", "income_nom", "tenure",
               "tax_to_income_totals_payers",
               "ptax_payer_flag & income_nom > 0"),
  share_of_total(lead, "ptax_all_nom", "tenure", "share_of_property_tax")
))

# 22) Where the dollars of an exemption for owners with no mortgage would go
#     (each group's share of the tax those owners pay), beside each group's
#     share of those owners and of all households
exemption_cuts <- c("networth_group", "income_group", "age_group_fed",
                    "race4")
exemption <- data.table::rbindlist(lapply(exemption_cuts, function(cut_col) {
  data.table::rbindlist(list(
    share_of_total(lead, "ptax_all_nom", cut_col,
                   "share_of_exemption_dollars", "owns_outright_flag"),
    share_of_total(lead, "one", cut_col,
                   "share_of_owners_no_mortgage", "owns_outright_flag"),
    estimate_by_cut(lead, "outright01", "share", cut_col,
                    "share_owning_outright_of_owners", "owner_flag")
  ))
}))
owns_outright <- data.table::rbindlist(list(outright_profile, exemption))
log_step(paste("Owns outright:", nrow(owns_outright), "rows"))

################################################################################
###                           9. Trend, 1989-2025                            ###
################################################################################

# 23) Every wave, all households
trend <- data.table::rbindlist(list(
  estimate_by_cut(households, "owner01", "share", "all_group",
                  "owner_rate"),
  estimate_by_cut(households, "outright01", "share", "all_group",
                  "share_owning_outright_of_owners", "owner_flag"),
  estimate_by_cut(households, "no_first_lien01", "share", "all_group",
                  "share_no_mortgage_any_heloc_of_owners", "owner_flag"),
  estimate_by_cut(households, "ptax_pce25", "median", "all_group",
                  "median_tax_payers", payer_subset, "PCE 2025 dollars"),
  estimate_by_cut(households, "ptax_pce25", "mean", "all_group",
                  "mean_tax_payers", payer_subset, "PCE 2025 dollars"),
  estimate_by_cut(households, "ptax_value_ratio", "median", "all_group",
                  "median_tax_to_home_value_payers",
                  "ptax_payer_flag & x716 > 0"),
  ratio_by_cut(households, "ptax_nom", "income_nom", "all_group",
               "tax_to_income_totals_owners",
               "owner_flag & income_nom > 0"),
  share_of_total(households, "ptax_all_nom", "tenure",
                 "share_of_property_tax"),
  share_of_total(households, "ptax_all_nom", "age_group_fed",
                 "share_of_property_tax"),
  total_by_cut(households, "ptax_all_pce25", "all_group",
               "total_tax_billions"),
  total_by_cut(households, "ptax_all_pce25", "all_group",
               "total_tax_billions_excl_farm_ranch", "!farm_ranch_flag")
))
log_step(paste("Trend:", nrow(trend), "rows"))

################################################################################
###                              10. Save                                    ###
################################################################################

# 24) Write each table, sorted
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
outputs <- list(
  pt_who_pays = who_pays,
  pt_levels = levels_table,
  pt_income_share = income_table,
  pt_progressivity = progressivity,
  pt_suits = suits,
  pt_owns_outright = owns_outright,
  pt_trend = trend
)
for (table_name in names(outputs)) {
  table_out <- outputs[[table_name]]
  sort_cols <- intersect(c("year", "measure", "cut", "group", "tax"),
                         names(table_out))
  data.table::setorderv(table_out, sort_cols)
  data.table::fwrite(table_out,
                     file.path(path_tables, paste0(table_name, ".csv")))
  log_step(paste0("Wrote ", table_name, ".csv (", nrow(table_out),
                  " rows)"))
}
