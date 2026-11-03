# 09_headship_by_cut -- CPS selection into headship by sex, race, education
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U3 of records/plans/2026-10-11_mr4-headship-selection.md (items H1
# and I2 of the U9 review triage). It reads the CPS person checkpoint
# written by 08 (data/processed/cps_persons_15_44.parquet), the ASEC person
# replicate weights for 2005-2025 (IPUMS extract, manifest roles
# ipums_repwt_data and ipums_repwt_ddi, written by 07), 08's
# headship_context.csv, and the SCF panel written by 02, and writes, to
# output/tables/:
#   headship_by_cut.csv          ASEC year x age band x cut (all; sex; race
#                                and ethnicity; education): the selection
#                                shares, each with a standard error
#   headship_scf_cps_by_cut.csv  SCF waves x band x cut: SCF reference
#                                persons against CPS SCF-rule reference
#                                persons in the same cut (a diagnostic)
#   headship_cps_age_split.csv   ASEC year: the share of SCF-rule reference
#                                persons aged 25-34 who are 25-29, with an
#                                SE (for the 25-29/30-34 question, Q)
# Every internal check stops the script before anything is written.
# Run from the project root as
#   Rscript code/09_headship_by_cut.R
#
# Measures (shares of the persons in the CPS band, ASECWT, halved in 2014):
#   scf_rule_rp_share    would be the SCF reference person of their unit
#                        (08, section 5): rp_couple_share + rp_single_share
#   rp_couple_share      reference person of a unit with a spouse (and,
#                        from 1995, an unmarried partner)
#   rp_single_share      reference person with no spouse or partner
#   couple_member_share  reference person or the reference person's spouse
#                        or partner in a couple unit (the SCF "couple" cell)
#   child_share          child of the householder, in a household
#   householder_spouse_partner_share  householder, or the householder's
#                        spouse or unmarried partner, in a household (NA
#                        before 1995, as in 08)
# As in 08, the reference-person and couple-member numerators take persons
# at the SCF band ages with no lower bound (so the under-25 and under-35
# bands also count the few aged under 18); every other numerator and every
# denominator is the CPS band (18 and over).
#
# Cuts, matched to the SCF cuts in 02 (MR4 plan O1-O3, built in 08):
# - sex: the PERSON's sex. The SCF cut is couple / single male / single
#   female for the reference person, so its single-male cell matches
#   rp_single_share among men, its single-female cell rp_single_share among
#   women, and its couple cell couple_member_share.
# - race4: the person's own race and Hispanic origin; the SCF's RACECL4
#   describes the survey respondent. race_cps_break_flag marks 2003
#   (multiple race allowed; new Hispanic question). In the SCF/CPS table,
#   scf_race_break_flag marks the SCF race coding breaks of 1998, 2004, and
#   2025 (scf_definition_breaks() in 00_functions.R, as in 05).
# - educ4: EDUC in the four EDCL groups. educ_cps_break_flag marks 1992
#   (degrees replace years of school); schooling_incomplete_flag marks the
#   18-24 band, many of whom are still in school.
#
# Standard errors (I2; MR4 plan O4 and O7):
# - 2005-2025: successive difference replication with the 160 ASEC person
#   replicate weights, se = sqrt(4/160 x sum_r (theta_r - theta)^2), theta
#   from the full-sample weight (IPUMS CPS, "Replicate Weights in the
#   Current Population Survey", cps.ipums.org/cps/repwt.shtml, read
#   2026-10-11; 4/160 = 0.025 in its Stata example). se_method "sdr_160".
# - 1989-2004 (no public replicate weights): se_srs x sqrt(design effect),
#   with the design effect the median of (se / se_srs)^2 over 2005-2009 for
#   the same band, cut level, and measure. se_method "deff_approx". These
#   are approximations, not replicate estimates.
# - se_srs is the simple-random-sampling SE, the square root of
#   p (1 - p) / n_unweighted, throughout; design_effect is the squared
#   ratio of se to se_srs.
# - Persons aged under 15 who are SCF-rule reference persons are outside
#   the replicate extract (ages 15-44); they keep the full-sample weight in
#   every replicate, so they add no sampling variance. Their count is
#   logged.
#
# Caveats: as in 08 (the CPS householder is not the SCF reference person;
# same-calendar-year alignment; weight-basis breaks in weight_basis and
# weight_break_flag). Shares for small cut levels in the 18-24 band carry
# large SEs; the SE columns show it.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c(
  "here", "ipumsr", "tidyselect", "data.table", "arrow", "Matrix"
)
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

# 2) Stop unless this script sits at code/09_headship_by_cut.R
here::i_am("code/09_headship_by_cut.R")
source(here::here("code", "00_functions.R"))

persons_path <- here::here("data", "processed", "cps_persons_15_44.parquet")
ipums_manifest_path <- here::here("data", "raw", "ipums_cps", "manifest.csv")
headship_path <- here::here("output", "tables", "headship_context.csv")
panel_path <- here::here(
  "data", "processed", "scf_panel_1989_2025.parquet"
)
path_tables <- here::here("output", "tables")
by_cut_output_path <- file.path(path_tables, "headship_by_cut.csv")
scf_cps_output_path <- file.path(path_tables, "headship_scf_cps_by_cut.csv")
split_output_path <- file.path(path_tables, "headship_cps_age_split.csv")

# 3) Years and age bands, as in 08 (section 2); step 9 checks the bands
#    against 08's table
asec_years <- 1989L:2025L
scf_wave_years <- seq(1989L, 2025L, by = 3L)
age_bands <- data.table::data.table(
  age_band = c("under 25", "25-29", "30-34", "35-44", "under 35"),
  cps_age_min = c(18L, 25L, 30L, 35L, 18L),
  cps_age_max = c(24L, 29L, 34L, 44L, 34L),
  scf_age_min = c(0L, 25L, 30L, 35L, 0L),
  scf_age_max = c(24L, 29L, 34L, 44L, 34L)
)
age_bands[, cps_age_range := paste0(cps_age_min, "-", cps_age_max)]
partner_first_year <- 1995L

# 4) Cut types in output order, and the break years named in the header
cut_types <- c("all", "sex", "race4", "educ4")
race_cps_break_year <- 2003L
educ_cps_break_year <- 1992L
# SCF race coding breaks (RACECL4: 1998, 2004, 2025), from the shared
# break table scf_definition_breaks() in code/00_functions.R, so 09 and 05
# flag the same SCF waves (methodology finding MR-SURVEY LOW of 2026-10-12)
scf_definition_break_table <- scf_definition_breaks()
scf_race_break_years <- scf_definition_break_table$year[
  scf_definition_break_table$target == "race4"
]
if (length(scf_race_break_years) == 0L) {
  stop("scf_definition_breaks() has no race4 rows", call. = FALSE)
}
schooling_incomplete_band <- "under 25"

# 5) Measures: the numerator flag (step 10) behind each share, and whether
#    the numerator takes the SCF band ages (no lower bound) or the CPS band
measure_table <- data.table::data.table(
  measure = c(
    "scf_rule_rp_share", "rp_couple_share", "rp_single_share",
    "couple_member_share", "child_share", "householder_spouse_partner_share"
  ),
  numerator = c(
    "rp_flag", "rp_couple_flag", "rp_single_flag", "couple_member_flag",
    "child_flag", "hsp_flag"
  ),
  scf_band_ages_flag = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
)

# 6) Replicate weights: 160, ASEC 2005-2025; successive difference factor
#    4/160 (header); the design-effect years for the pre-2005 SEs; and the
#    largest allowed gap between a replicate's total and the full-sample
#    total, by year (MR4 plan U3)
replicate_count <- 160L
replicate_years <- 2005L:2025L
sdr_factor <- 4 / 160
deff_years <- 2005L:2009L
replicate_total_tolerance <- 0.02
max_rp_under_15 <- 50L

# 7) Successive-difference-replication standard error, for a vector of
#    full-sample estimates and a matrix of replicate estimates (one row per
#    estimate, one column per replicate). Approved by the owner as O7 of
#    the MR4 plan (2026-10-11).
sdr_se <- function(full_estimate, replicate_estimates) {
  sqrt(sdr_factor * rowSums((replicate_estimates - full_estimate)^2))
}

run_start <- Sys.time()

################################################################################
###                    3. Read the person checkpoint and 08                  ###
################################################################################

# 8) The CPS person checkpoint from 08, one row per person
persons <- data.table::as.data.table(arrow::read_parquet(persons_path))
if (anyDuplicated(persons, by = c("YEAR", "SERIAL", "PERNUM")) > 0L) {
  stop("YEAR, SERIAL, PERNUM is not unique in ", persons_path,
       call. = FALSE)
}
if (!identical(sort(unique(persons$YEAR)), asec_years)) {
  stop("The person checkpoint does not hold every ASEC year 1989-2025",
       call. = FALSE)
}
message("Read ", format(nrow(persons), big.mark = ","), " persons from ",
        basename(persons_path))

# 9) 08's table: the "all" rows of step 15 must reproduce it, and it
#    supplies each year's weight basis and break flag
headship_context <- data.table::fread(headship_path)
band_check <- unique(headship_context[, list(age_band, cps_age_range)])
if (!setequal(paste(band_check$age_band, band_check$cps_age_range),
              paste(age_bands$age_band, age_bands$cps_age_range))) {
  stop("Age bands differ from those in ", headship_path, call. = FALSE)
}
weight_basis_by_year <- unique(
  headship_context[, list(year, weight_basis, weight_break_flag)]
)
if (nrow(weight_basis_by_year) != length(asec_years)) {
  stop("headship_context.csv does not give one weight basis per year",
       call. = FALSE)
}

################################################################################
###                  4. Indicators, cut levels, and band rows                ###
################################################################################

# 10) Numerator flags (definitions in the header)
persons[, household_flag := GQ == 1L]
persons[, rp_couple_flag := scf_rule_role == "rp_couple"]
persons[, rp_single_flag := scf_rule_role == "rp_single"]
persons[, rp_flag := rp_couple_flag | rp_single_flag]
persons[, couple_member_flag := scf_rule_role %in%
          c("rp_couple", "rp_partner")]
persons[, child_flag := household_flag & relate_group == "child"]
persons[, hsp_flag := household_flag &
          relate_group %in% c("householder", "spouse", "partner")]
if (!identical(persons$rp_flag, persons$scf_rule_rp_flag)) {
  stop("rp_flag does not match scf_rule_rp_flag from 08", call. = FALSE)
}

# 11) Cut levels as character, one column per cut type; education is NA
#     below 15 (08), so those few persons drop out of the educ4 rows only
persons[, cut_all := "all"]
persons[, cut_sex := data.table::fifelse(SEX == 1L, "male", "female")]
persons[, cut_race4 := as.character(race4_cps)]
persons[, cut_educ4 := as.character(educ4_cps)]
if (anyNA(persons$SEX) || !all(persons$SEX %in% c(1L, 2L)) ||
      anyNA(persons$cut_race4)) {
  stop("SEX or race4_cps is missing or out of range in the checkpoint",
       call. = FALSE)
}
if (anyNA(persons$cut_educ4[persons$AGE >= 15L])) {
  stop("educ4_cps is missing for a person aged 15 or over", call. = FALSE)
}
level_order <- c(
  "all", "male", "female", levels(persons$race4_cps),
  levels(persons$educ4_cps)
)

# 12) Long band rows: one row per person and band whose denominator (CPS
#     band ages) or SCF-band-age numerators (reference persons and couple
#     members) include the person. in_cps_flag marks the denominator;
#     in_scf_ages_flag the SCF band ages
band_rows_list <- vector("list", nrow(age_bands))
for (band_index in seq_len(nrow(age_bands))) {
  in_cps_mask <- persons$AGE >= age_bands$cps_age_min[band_index] &
    persons$AGE <= age_bands$cps_age_max[band_index]
  in_scf_mask <- persons$AGE >= age_bands$scf_age_min[band_index] &
    persons$AGE <= age_bands$scf_age_max[band_index]
  keep_mask <- in_cps_mask |
    (in_scf_mask & (persons$rp_flag | persons$couple_member_flag))
  band_rows_list[[band_index]] <- data.table::data.table(
    person_index = which(keep_mask),
    age_band = age_bands$age_band[band_index],
    in_cps_flag = in_cps_mask[keep_mask],
    in_scf_ages_flag = in_scf_mask[keep_mask]
  )
}
band_rows <- data.table::rbindlist(band_rows_list)
rm(band_rows_list)

# 13) Quantity weights per band row: den (the denominator) and one column
#     per measure numerator, as 0/1 multipliers of the person's weight
band_rows[, den := as.numeric(in_cps_flag)]
for (measure_index in seq_len(nrow(measure_table))) {
  flag_name <- measure_table$numerator[measure_index]
  person_flag <- persons[[flag_name]][band_rows$person_index]
  age_flag <- if (measure_table$scf_band_ages_flag[measure_index]) {
    band_rows$in_scf_ages_flag
  } else {
    band_rows$in_cps_flag
  }
  data.table::set(band_rows, j = flag_name,
                  value = as.numeric(person_flag & age_flag))
}
quantity_columns <- c("den", measure_table$numerator)

################################################################################
###               5. Point estimates, every year (full-sample weight)        ###
################################################################################

# 14) Weighted sums of each quantity by year, band, cut type, and level;
#     n_unweighted counts the denominator's persons
band_rows[, YEAR := persons$YEAR[person_index]]
band_rows[, person_weight := persons$person_weight[person_index]]
point_list <- vector("list", length(cut_types))
for (cut_index in seq_along(cut_types)) {
  cut_column <- paste0("cut_", cut_types[cut_index])
  band_rows[, cut_level := persons[[cut_column]][person_index]]
  cut_sums <- band_rows[!is.na(cut_level), c(
    list(n_unweighted = sum(den)),
    lapply(.SD, function(quantity) sum(quantity * person_weight))
  ), by = list(YEAR, age_band, cut_level), .SDcols = quantity_columns]
  cut_sums[, cut_type := cut_types[cut_index]]
  point_list[[cut_index]] <- cut_sums
}
point_sums <- data.table::rbindlist(point_list)
band_rows[, cut_level := NULL]
data.table::setnames(point_sums, "YEAR", "year")

# 15) Check: the "all" rows reproduce 08's population and shares exactly
all_rows <- point_sums[cut_type == "all"]
all_check <- merge(
  all_rows, headship_context,
  by = c("year", "age_band"), sort = FALSE
)
if (nrow(all_check) != nrow(headship_context)) {
  stop("The 'all' rows do not match 08's year-band rows one to one",
       call. = FALSE)
}
all_gap <- c(
  all_check$den / all_check$population_weighted - 1,
  all_check$rp_flag / all_check$den - all_check$scf_rule_rp_share,
  all_check$child_flag / all_check$den - all_check$child_share,
  (all_check$hsp_flag / all_check$den -
     all_check$householder_spouse_partner_share)[
    all_check$year >= partner_first_year
  ]
)
if (anyNA(all_gap) || any(abs(all_gap) > 1e-9)) {
  stop("The 'all' rows do not reproduce headship_context.csv (largest gap ",
       signif(max(abs(all_gap), na.rm = TRUE), 3), ")", call. = FALSE)
}
message("Check passed: 'all' rows reproduce 08 in ", nrow(all_check),
        " year-band rows")

# 16) Check: each cut's levels add up to its "all" row, except that
#     educ4 leaves out the few reference persons under 15 (step 11)
for (cut_name in setdiff(cut_types, "all")) {
  level_totals <- point_sums[
    cut_type == cut_name,
    lapply(.SD, sum),
    by = list(year, age_band), .SDcols = quantity_columns
  ]
  additivity <- merge(
    level_totals, all_rows,
    by = c("year", "age_band"), suffixes = c("_levels", "_all")
  )
  for (quantity in quantity_columns) {
    gap <- additivity[[paste0(quantity, "_all")]] -
      additivity[[paste0(quantity, "_levels")]]
    allowed_gap <- if (cut_name == "educ4") {
      sum(persons$person_weight[persons$AGE < 15L])
    } else {
      0
    }
    if (any(gap < -1e-6) || any(gap > allowed_gap + 1e-6)) {
      stop("Cut ", cut_name, " levels do not add up to the 'all' rows for ",
           quantity, call. = FALSE)
    }
  }
}
message("Check passed: cut levels add up to the 'all' rows")

################################################################################
###                  6. Replicate weights and SDR sums, 2005-2025            ###
################################################################################

# 17) Find and read the replicate-weight extract named in the manifest
ipums_manifest <- utils::read.csv(
  ipums_manifest_path,
  colClasses = "character", na.strings = "", check.names = FALSE
)
repwt_ddi_row <- ipums_manifest[ipums_manifest$file_role == "ipums_repwt_ddi", ]
repwt_data_row <- ipums_manifest[
  ipums_manifest$file_role == "ipums_repwt_data",
]
if (nrow(repwt_ddi_row) != 1L || nrow(repwt_data_row) != 1L) {
  stop("Expected one ipums_repwt_ddi and one ipums_repwt_data row in ",
       ipums_manifest_path, ". Run code/07_download_ipums_cps.R first.",
       call. = FALSE)
}
replicate_names <- paste0("REPWTP", seq_len(replicate_count))
repwt_ddi <- ipumsr::read_ipums_ddi(here::here(repwt_ddi_row$local_path))
message("Reading ", basename(repwt_data_row$local_path))
repwt_raw <- ipumsr::read_ipums_micro(
  repwt_ddi,
  vars = tidyselect::all_of(
    c("YEAR", "SERIAL", "PERNUM", "ASECWT", "AGE", replicate_names)
  ),
  data_file = here::here(repwt_data_row$local_path), verbose = FALSE
)
repwt <- data.table::as.data.table(ipumsr::zap_ipums_attributes(repwt_raw))
rm(repwt_raw)
key_columns <- c("YEAR", "SERIAL", "PERNUM", "AGE")
repwt[, (key_columns) := lapply(.SD, as.integer), .SDcols = key_columns]
message("Read ", format(nrow(repwt), big.mark = ","), " replicate-weight ",
        "records in ", round(as.numeric(difftime(
          Sys.time(), run_start, units = "secs"
        ))), " seconds")

# 18) Check the extract: years 2005-2025, ages 15-44, unique persons, no
#     missing weight
if (!identical(sort(unique(repwt$YEAR)), replicate_years)) {
  stop("Replicate-weight extract years are not exactly 2005-2025",
       call. = FALSE)
}
if (min(repwt$AGE) < 15L || max(repwt$AGE) > 44L) {
  stop("Replicate-weight extract holds ages outside 15-44 (the case ",
       "selection)", call. = FALSE)
}
if (anyDuplicated(repwt, by = c("YEAR", "SERIAL", "PERNUM")) > 0L) {
  stop("YEAR, SERIAL, PERNUM is not unique in the replicate extract",
       call. = FALSE)
}
if (anyNA(repwt[, replicate_names, with = FALSE])) {
  stop("Some replicate weights are missing", call. = FALSE)
}

# 19) Join to the checkpoint persons of 2005-2025 (one to one); the
#     checkpoint's ASECWT and AGE must match. Persons aged 15-44 must all
#     match; only reference persons under 15 may lack replicate weights
persons[, person_index := .I]
replicate_persons <- persons[
  YEAR %in% replicate_years,
  list(YEAR, SERIAL, PERNUM, AGE, ASECWT, person_weight, person_index)
]
replicate_persons <- merge(
  replicate_persons, repwt,
  by = c("YEAR", "SERIAL", "PERNUM"), all.x = TRUE, sort = FALSE,
  suffixes = c("", "_repwt")
)
unmatched_flag <- is.na(replicate_persons$ASECWT_repwt)
if (nrow(replicate_persons) != nrow(persons[YEAR %in% replicate_years]) ||
      any(unmatched_flag & replicate_persons$AGE >= 15L)) {
  stop("Checkpoint persons aged 15-44 lack replicate weights",
       call. = FALSE)
}
if (sum(unmatched_flag) > max_rp_under_15) {
  stop(sum(unmatched_flag), " persons under 15 lack replicate weights ",
       "(more than ", max_rp_under_15, ")", call. = FALSE)
}
matched <- replicate_persons[!unmatched_flag]
if (any(abs(matched$ASECWT - matched$ASECWT_repwt) > 1e-6) ||
      any(matched$AGE != matched$AGE_repwt)) {
  stop("ASECWT or AGE differs between the checkpoint and the replicate ",
       "extract", call. = FALSE)
}
if (nrow(repwt) != nrow(matched)) {
  stop("The replicate extract holds persons not in the checkpoint",
       call. = FALSE)
}
message("Joined replicate weights: ", format(nrow(matched), big.mark = ","),
        " persons; ", sum(unmatched_flag), " reference persons under 15 ",
        "keep the full-sample weight in every replicate")
rm(repwt, matched)

# 20) Replicate weights on the person_weight scale: halved in 2014, as
#     ASECWT is in 08 (both HFLAG files pooled); persons without replicate
#     weights take person_weight in every replicate (header)
replicate_matrix <- as.matrix(
  replicate_persons[, replicate_names, with = FALSE]
)
replicate_matrix[replicate_persons$YEAR == 2014L, ] <-
  replicate_matrix[replicate_persons$YEAR == 2014L, ] / 2
for (row_index in which(unmatched_flag)) {
  replicate_matrix[row_index, ] <- replicate_persons$person_weight[row_index]
}

# 21) Check, by year, that every replicate's total weight is within 2
#     percent of the full-sample total (MR4 plan U3)
replicate_totals <- rowsum(replicate_matrix, replicate_persons$YEAR)
full_totals <- rowsum(replicate_persons$person_weight, replicate_persons$YEAR)
total_ratio <- sweep(replicate_totals, 1L, full_totals[, 1L], "/")
if (any(abs(total_ratio - 1) > replicate_total_tolerance)) {
  worst_year <- rownames(total_ratio)[
    which.max(apply(abs(total_ratio - 1), 1L, max))
  ]
  stop("A replicate's total weight differs from the full-sample total by ",
       "more than ", 100 * replicate_total_tolerance, " percent (worst ",
       "year ", worst_year, ", ratio ",
       signif(total_ratio[worst_year, ][which.max(abs(
         total_ratio[worst_year, ] - 1
       ))], 4), ")", call. = FALSE)
}
message("Check passed: replicate totals within ",
        100 * replicate_total_tolerance, " percent of full-sample totals ",
        "(largest gap ", signif(100 * max(abs(total_ratio - 1)), 3),
        " percent)")

# 22) Weighted sums with the full-sample weight (column 1) and each
#     replicate, by group (year, band, cut type, level, quantity), as a
#     sparse group-by-person matrix times the weight matrix
weight_matrix <- cbind(replicate_persons$person_weight, replicate_matrix)
rm(replicate_matrix)
row_of_person <- match(persons$person_index, replicate_persons$person_index)
replicate_band_rows <- band_rows[YEAR %in% replicate_years]
replicate_band_rows[, weight_row := row_of_person[person_index]]
sums_list <- list()
for (cut_name in cut_types) {
  cut_column <- paste0("cut_", cut_name)
  cut_level <- persons[[cut_column]][replicate_band_rows$person_index]
  for (quantity in quantity_columns) {
    member_mask <- replicate_band_rows[[quantity]] > 0 & !is.na(cut_level)
    group_key <- paste(
      replicate_band_rows$YEAR[member_mask],
      replicate_band_rows$age_band[member_mask],
      cut_name, cut_level[member_mask], quantity,
      sep = "|"
    )
    group_factor <- factor(group_key)
    membership <- Matrix::sparseMatrix(
      i = as.integer(group_factor),
      j = replicate_band_rows$weight_row[member_mask],
      x = 1,
      dims = c(nlevels(group_factor), nrow(weight_matrix))
    )
    group_sums <- as.matrix(membership %*% weight_matrix)
    rownames(group_sums) <- levels(group_factor)
    sums_list[[paste(cut_name, quantity)]] <- group_sums
  }
}
replicate_sums <- do.call(rbind, sums_list)
rm(sums_list, weight_matrix)
message("Replicate sums: ", nrow(replicate_sums), " groups x ",
        ncol(replicate_sums), " weights")

################################################################################
###                       7. Shares and standard errors                      ###
################################################################################

# 23) Shares from the point sums, long by measure
point_long <- data.table::melt(
  point_sums,
  id.vars = c("year", "age_band", "cut_type", "cut_level", "n_unweighted",
              "den"),
  measure.vars = measure_table$numerator,
  variable.name = "numerator", value.name = "numerator_weighted",
  variable.factor = FALSE
)
point_long <- merge(
  point_long, measure_table[, list(measure, numerator)],
  by = "numerator", sort = FALSE
)
point_long[, estimate := numerator_weighted / den]
point_long[measure == "householder_spouse_partner_share" &
             year < partner_first_year, estimate := NA_real_]
data.table::setnames(point_long, "den", "population_weighted")

# 24) Replicate shares for 2005-2025: numerator sums over denominator sums
#     for the same group; the full-sample column must equal the point
#     estimate
group_prefix <- function(year, age_band, cut_type, cut_level) {
  paste(year, age_band, cut_type, cut_level, sep = "|")
}
sdr_rows <- point_long[year %in% replicate_years]
numerator_keys <- paste(group_prefix(
  sdr_rows$year, sdr_rows$age_band, sdr_rows$cut_type, sdr_rows$cut_level
), sdr_rows$numerator, sep = "|")
denominator_keys <- paste(group_prefix(
  sdr_rows$year, sdr_rows$age_band, sdr_rows$cut_type, sdr_rows$cut_level
), "den", sep = "|")
if (anyNA(match(denominator_keys, rownames(replicate_sums)))) {
  stop("A denominator group has no replicate sums", call. = FALSE)
}
numerator_sums <- replicate_sums[
  match(numerator_keys, rownames(replicate_sums)), ,
  drop = FALSE
]
numerator_sums[is.na(numerator_sums)] <- 0
denominator_sums <- replicate_sums[
  match(denominator_keys, rownames(replicate_sums)), ,
  drop = FALSE
]
share_matrix <- numerator_sums / denominator_sums
full_gap <- share_matrix[, 1L] - sdr_rows$numerator_weighted /
  sdr_rows$population_weighted
if (any(abs(full_gap) > 1e-9)) {
  stop("Full-sample shares from the replicate step differ from the point ",
       "estimates", call. = FALSE)
}
sdr_rows[, se := sdr_se(share_matrix[, 1L], share_matrix[, -1L])]
sdr_rows[, se_method := "sdr_160"]
sdr_rows[is.na(estimate), se := NA_real_]

# 25) SRS SEs and design effects; pre-2005 SEs from the median 2005-2009
#     design effect of the same band, cut level, and measure (O4)
point_long[, se_srs := sqrt(estimate * (1 - estimate) / n_unweighted)]
point_long <- merge(
  point_long, sdr_rows[, list(year, age_band, cut_type, cut_level, measure,
                              se, se_method)],
  by = c("year", "age_band", "cut_type", "cut_level", "measure"),
  all.x = TRUE, sort = FALSE
)
point_long[, design_effect := (se / se_srs)^2]
deff_table <- point_long[
  year %in% deff_years & is.finite(design_effect),
  list(deff_median = stats::median(design_effect)),
  by = list(age_band, cut_type, cut_level, measure)
]
point_long <- merge(
  point_long, deff_table,
  by = c("age_band", "cut_type", "cut_level", "measure"),
  all.x = TRUE, sort = FALSE
)
pre_replicate_mask <- point_long$year < min(replicate_years)
point_long[pre_replicate_mask, se := se_srs * sqrt(deff_median)]
point_long[pre_replicate_mask, design_effect := deff_median]
point_long[pre_replicate_mask, se_method := "deff_approx"]
point_long[is.na(estimate), `:=`(se = NA_real_, design_effect = NA_real_)]

# 26) Check: every non-missing share has a finite, non-negative SE
se_missing_flag <- !is.na(point_long$estimate) &
  (is.na(point_long$se) | !is.finite(point_long$se) | point_long$se < 0)
if (any(se_missing_flag)) {
  missing_rows <- point_long[se_missing_flag]
  stop(sum(se_missing_flag), " shares have no valid SE (first: ",
       missing_rows$year[1L], " ", missing_rows$age_band[1L], " ",
       missing_rows$cut_type[1L], " ", missing_rows$cut_level[1L], " ",
       missing_rows$measure[1L], ")", call. = FALSE)
}

################################################################################
###            8. CPS split of reference persons aged 25-34 (for Q)          ###
################################################################################

# 27) The share of SCF-rule reference persons aged 25-34 who are 25-29, by
#     year, for the 25-29/30-34 question (MR4 plan, Q): the 25-29 RP sum
#     over the 25-29 plus 30-34 RP sums (cut "all"), with an SDR SE from
#     2005 and, before 2005, the SRS SE times the square root of the median
#     2005-2009 design effect, as for the shares (O4). n_rp_unweighted
#     counts the reference persons aged 25-34
rp_unweighted <- persons[
  rp_flag & AGE >= 25L & AGE <= 34L,
  list(n_rp_unweighted = .N),
  by = list(year = YEAR)
]
split_point <- merge(
  point_sums[cut_type == "all" & age_band == "25-29",
             list(year, rp_25_29 = rp_flag)],
  point_sums[cut_type == "all" & age_band == "30-34",
             list(year, rp_30_34 = rp_flag)],
  by = "year"
)
split_point <- merge(split_point, rp_unweighted, by = "year")
split_point[, cps_share_25_29 := rp_25_29 / (rp_25_29 + rp_30_34)]
split_keys_25_29 <- paste(replicate_years, "25-29|all|all|rp_flag", sep = "|")
split_keys_30_34 <- paste(replicate_years, "30-34|all|all|rp_flag", sep = "|")
if (anyNA(match(c(split_keys_25_29, split_keys_30_34),
                rownames(replicate_sums)))) {
  stop("Replicate sums lack the 25-29 or 30-34 reference-person groups",
       call. = FALSE)
}
split_rp_25_29 <- replicate_sums[split_keys_25_29, , drop = FALSE]
split_rp_30_34 <- replicate_sums[split_keys_30_34, , drop = FALSE]
split_matrix <- split_rp_25_29 / (split_rp_25_29 + split_rp_30_34)
split_sdr <- data.table::data.table(
  year = replicate_years,
  split_full = split_matrix[, 1L],
  se = sdr_se(split_matrix[, 1L], split_matrix[, -1L])
)
split_point <- merge(split_point, split_sdr, by = "year", all.x = TRUE)
if (any(abs(split_point$split_full - split_point$cps_share_25_29) > 1e-9,
        na.rm = TRUE)) {
  stop("The replicate step's full-sample 25-29 split differs from the ",
       "point estimate", call. = FALSE)
}
split_point[, se_srs := sqrt(
  cps_share_25_29 * (1 - cps_share_25_29) / n_rp_unweighted
)]
split_point[, design_effect := (se / se_srs)^2]
split_deff <- stats::median(split_point[year %in% deff_years, design_effect])
split_point[year < min(replicate_years), `:=`(
  se = se_srs * sqrt(split_deff), design_effect = split_deff
)]
split_point[, se_method := data.table::fifelse(
  year >= min(replicate_years), "sdr_160", "deff_approx"
)]
if (anyNA(split_point$se) || any(!is.finite(split_point$se))) {
  stop("The CPS 25-29 split has a missing SE", call. = FALSE)
}

################################################################################
###                9. SCF and CPS reference persons by cut (waves)           ###
################################################################################

# 28) CPS SCF-rule reference persons at the SCF band ages, by cut: the unit
#     type (couple, single male, single female) for the SCF's sex cut,
#     race4, and educ4
persons[, cut_unit_type := data.table::fifelse(
  rp_couple_flag, "couple",
  data.table::fifelse(
    rp_single_flag & SEX == 1L, "single male",
    data.table::fifelse(rp_single_flag & SEX == 2L, "single female",
                        NA_character_)
  )
)]
rp_cut_types <- c("unit_type", "race4", "educ4")
cps_rp_list <- list()
for (band_index in seq_len(nrow(age_bands))) {
  rp_mask <- persons$rp_flag & persons$YEAR %in% scf_wave_years &
    persons$AGE >= age_bands$scf_age_min[band_index] &
    persons$AGE <= age_bands$scf_age_max[band_index]
  for (cut_name in rp_cut_types) {
    cut_rows <- data.table::data.table(
      year = persons$YEAR[rp_mask],
      cut_level = persons[[paste0("cut_", cut_name)]][rp_mask],
      person_weight = persons$person_weight[rp_mask]
    )
    cut_sums <- cut_rows[
      !is.na(cut_level),
      list(cps_rp_weighted = sum(person_weight)),
      by = list(year, cut_level)
    ]
    cut_sums[, age_band := age_bands$age_band[band_index]]
    cut_sums[, cut_type := cut_name]
    cps_rp_list[[paste(band_index, cut_name)]] <- cut_sums
  }
}
cps_rp <- data.table::rbindlist(cps_rp_list)

# 29) SCF weighted reference persons (WGT summed over the five pooled
#     implicates, as in 08 step 35), by the same bands and cuts
scf_panel <- data.table::as.data.table(arrow::read_parquet(
  panel_path,
  col_select = c("year", "wgt", "age", "race4", "educ4", "sex_cut")
))
if (anyNA(scf_panel)) {
  stop("The SCF panel has missing year, wgt, age, or cut values",
       call. = FALSE)
}
scf_panel[, cut_unit_type := as.character(sex_cut)]
scf_panel[, cut_race4 := as.character(race4)]
scf_panel[, cut_educ4 := as.character(educ4)]
scf_rp_list <- list()
for (band_index in seq_len(nrow(age_bands))) {
  scf_mask <- scf_panel$age >= age_bands$scf_age_min[band_index] &
    scf_panel$age <= age_bands$scf_age_max[band_index]
  for (cut_name in rp_cut_types) {
    cut_rows <- data.table::data.table(
      year = scf_panel$year[scf_mask],
      cut_level = scf_panel[[paste0("cut_", cut_name)]][scf_mask],
      wgt = scf_panel$wgt[scf_mask]
    )
    cut_sums <- cut_rows[
      ,
      list(scf_rp_weighted = sum(wgt)),
      by = list(year, cut_level)
    ]
    cut_sums[, age_band := age_bands$age_band[band_index]]
    cut_sums[, cut_type := cut_name]
    scf_rp_list[[paste(band_index, cut_name)]] <- cut_sums
  }
}
scf_rp <- data.table::rbindlist(scf_rp_list)

# 30) Join (every cell present in both surveys, or the cell is reported
#     with NA on the empty side) and compute the ratio and each side's
#     share of the band's reference persons
scf_cps <- merge(
  cps_rp, scf_rp,
  by = c("year", "age_band", "cut_type", "cut_level"),
  all = TRUE, sort = FALSE
)
scf_cps[, scf_cps_rp_ratio := scf_rp_weighted / cps_rp_weighted]
scf_cps[, cps_cut_share := cps_rp_weighted /
          sum(cps_rp_weighted, na.rm = TRUE),
        by = list(year, age_band, cut_type)]
scf_cps[, scf_cut_share := scf_rp_weighted /
          sum(scf_rp_weighted, na.rm = TRUE),
        by = list(year, age_band, cut_type)]
scf_cps[, scf_rule_partners_identified_flag := year >= partner_first_year]
scf_cps[, scf_race_2025_break_flag := cut_type == "race4" & year == 2025L]
scf_cps[, race_cps_break_flag := cut_type == "race4" &
          year == min(scf_wave_years[scf_wave_years >= race_cps_break_year])]
# Every SCF race coding break (1998, 2004, 2025); scf_race_2025_break_flag
# is kept for compatibility and is the 2025 part of this flag
scf_cps[, scf_race_break_flag := cut_type == "race4" &
          year %in% scf_race_break_years]

################################################################################
###                      10. Write the tables and log                        ###
################################################################################

# 31) headship_by_cut.csv: one row per year x band x cut level x measure,
#     with the weight basis and break flags
by_cut <- merge(
  point_long, weight_basis_by_year,
  by = "year", sort = FALSE
)
by_cut[, scf_wave := year %in% scf_wave_years]
by_cut[, scf_rule_partners_identified_flag := year >= partner_first_year]
by_cut[, race_cps_break_flag := cut_type == "race4" &
         year == race_cps_break_year]
by_cut[, educ_cps_break_flag := cut_type == "educ4" &
         year == educ_cps_break_year]
by_cut[, schooling_incomplete_flag := cut_type == "educ4" &
         age_band == schooling_incomplete_band]
by_cut <- merge(
  by_cut, age_bands[, list(age_band, cps_age_range)],
  by = "age_band", sort = FALSE
)
by_cut[, age_band := factor(age_band, levels = age_bands$age_band)]
by_cut[, cut_type := factor(cut_type, levels = cut_types)]
by_cut[, cut_level := factor(cut_level, levels = level_order)]
by_cut[, measure := factor(measure, levels = measure_table$measure)]
data.table::setorder(by_cut, year, age_band, cut_type, cut_level, measure)
by_cut_table <- by_cut[, list(
  year, scf_wave, age_band, cps_age_range, cut_type, cut_level, measure,
  estimate, se, se_method, se_srs, design_effect, n_unweighted,
  population_weighted, weight_basis, weight_break_flag,
  scf_rule_partners_identified_flag, race_cps_break_flag,
  educ_cps_break_flag, schooling_incomplete_flag
)]
data.table::fwrite(by_cut_table, by_cut_output_path)
message("Wrote ", nrow(by_cut_table), " rows to ", by_cut_output_path)

# 32) headship_scf_cps_by_cut.csv: SCF waves x band x cut level
scf_cps[, age_band := factor(age_band, levels = age_bands$age_band)]
scf_cps[, cut_type := factor(cut_type, levels = rp_cut_types)]
data.table::setorder(scf_cps, year, age_band, cut_type, cut_level)
scf_cps_table <- scf_cps[, list(
  year, age_band, cut_type, cut_level, scf_rp_weighted, cps_rp_weighted,
  scf_cps_rp_ratio, scf_cut_share, cps_cut_share,
  scf_rule_partners_identified_flag, race_cps_break_flag,
  scf_race_2025_break_flag, scf_race_break_flag
)]
data.table::fwrite(scf_cps_table, scf_cps_output_path)
message("Wrote ", nrow(scf_cps_table), " rows to ", scf_cps_output_path)

# 33) headship_cps_age_split.csv: one row per ASEC year
split_table <- split_point[, list(
  year, scf_wave = year %in% scf_wave_years, cps_share_25_29, se,
  se_method, se_srs, design_effect, n_rp_unweighted,
  rp_25_29_weighted = rp_25_29, rp_30_34_weighted = rp_30_34
)]
data.table::setorder(split_table, year)
data.table::fwrite(split_table, split_output_path)
message("Wrote ", nrow(split_table), " rows to ", split_output_path)

# 34) Diagnostic summary: under-35 SCF-rule share by sex in four waves,
#     and the SE range by method
summary_rows <- by_cut_table[
  age_band == "under 35" & cut_type == "sex" &
    measure == "scf_rule_rp_share" & year %in% c(1989L, 2007L, 2019L, 2025L)
]
for (row_index in seq_len(nrow(summary_rows))) {
  message(sprintf(
    "  %d under 35, %-6s scf_rule_rp_share %.3f (se %.4f, %s)",
    summary_rows$year[row_index], summary_rows$cut_level[row_index],
    summary_rows$estimate[row_index], summary_rows$se[row_index],
    summary_rows$se_method[row_index]
  ))
}
se_summary <- by_cut_table[!is.na(se), list(
  median_se = stats::median(se), max_se = max(se),
  median_deff = stats::median(design_effect, na.rm = TRUE)
), by = se_method]
for (row_index in seq_len(nrow(se_summary))) {
  message(sprintf(
    "  SE %-11s median %.4f, max %.4f; median design effect %.2f",
    se_summary$se_method[row_index], se_summary$median_se[row_index],
    se_summary$max_se[row_index], se_summary$median_deff[row_index]
  ))
}
message("Finished in ", round(as.numeric(difftime(
  Sys.time(), run_start, units = "mins"
)), 2), " minutes")
