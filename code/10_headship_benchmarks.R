# 10_headship_benchmarks -- validate CPS householder and partner counts
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U4 of records/plans/2026-10-11_mr4-headship-selection.md (item I1 of
# the U9 review triage), as amended 2026-10-11 after the Census tables were
# opened. It reads the IPUMS CPS ASEC extracts named in
# data/raw/ipums_cps/manifest.csv (main, 1989-2025; cohab, 2007-2025, with
# PECOHAB and LINENO) and four Census tables in data/raw/census/, and writes
# output/tables/headship_benchmarks.csv, one row per comparison:
#   hh3       weighted householders by age (under 25, 25-29, 30-34, 35-44),
#             1989-2025, against Table HH-3 (households by age of
#             householder), within 1.0 percent
#   uc1       weighted opposite-sex unmarried couples against Table UC-1,
#             within 2.0 percent: 1996-2006 couples in which one partner is
#             the householder (RELATE 1114), UC-1's definition then; 2007-2023
#             every opposite-sex couple identified by PECOHAB, UC-1's
#             definition since 2007 (its footnote 1)
#   uc3       the PECOHAB couples of 2018 and 2019 by age of the male and of
#             the female partner (15-24, 25-29, 30-34, 35-39, 40-44),
#             against Table UC3 of each year's Families and Living
#             Arrangements tables, within 2.0 percent
# and a check of the RELATE partner codes against PECOHAB in every year
# 2007-2025, across the 2019 code change: the householder's unmarried
# partner (RELATE 1114, then 1116 and 1117) names the householder in
# PECOHAB, and the householder names the partner. Any miss stops the script
# after the table is written (as 04 and 08 do); each comparison's tolerance
# was fixed in the plan (O6) before the tables were opened.
# Owner decisions after the first run (Ben Glasner, 2026-10-11; plan
# Amendments):
# - The 2007-2023 UC-1 and the UC-3 comparisons are reported as a
#   diagnostic (test_role "diagnostic") and cannot stop the script. Our
#   PECOHAB couples run below Census in every year and cell (UC-1 1.5-4.2
#   percent, UC3 1.9-6.0 percent),
#   no couple weight closes the gap, and the RELATE partners match PECOHAB
#   exactly; the gap is read as Census edits to the cohabitation item that
#   the public file lacks. The partner codes are tested by UC-1 1996-2006
#   and the RELATE-PECOHAB check.
# - HH-3 2014 is a named exemption: Census's full-sample (98,000-address)
#   weights are not public, and halving ASECWT over both HFLAG files, as
#   IPUMS advises, puts under-25 householders 1.3 percent above HH-3. The
#   2014 cells are compared and reported with exempt_flag TRUE.
# Run from the project root as
#   Rscript code/10_headship_benchmarks.R
#
# Conventions:
# - Weights: ASECWT, halved in 2014 (both HFLAG files pooled), as in 08.
#   HH-3 "uses the householder's person weight" (its note), as we do. For a
#   couple, Census does not say whose weight it uses; we use the partner
#   with the lower PERNUM (the householder in a householder couple) and
#   report the mean of the two partners' weights as an alternative column.
# - Persons in households only (GQ == 1).
# - Weight vintage: IPUMS keeps the latest weights, so 2010 is on 2010-based
#   controls and 2020 on 2020-based controls, while the Census tables print
#   2010 and 2020 on the earlier controls. For those two years the test
#   uses the relative difference less the published revision shift of the
#   next year (2011r over 2011, 2021r over 2021), as 08 does for AD-1. For
#   2011 and 2021 the revised rows are used directly.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "ipumsr", "tidyselect", "data.table", "readxl")
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

# 2) Stop unless this script sits at code/10_headship_benchmarks.R
here::i_am("code/10_headship_benchmarks.R")

ipums_manifest_path <- here::here("data", "raw", "ipums_cps", "manifest.csv")
path_census <- here::here("data", "raw", "census")
hh3_path <- file.path(path_census, "hh3.xls")
uc1_path <- file.path(path_census, "uc1.xls")
uc3_paths <- c(
  "2018" = file.path(path_census, "tabuc3_2018.xls"),
  "2019" = file.path(path_census, "tabuc3_2019.xls")
)
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
benchmarks_output_path <- file.path(path_tables, "headship_benchmarks.csv")

# 3) Years, tolerances (MR4 plan O6, as amended), the vintage-adjusted
#    years with the revised year whose shift they use, and the years whose
#    IPUMS ASECWT carries weights revised to a later census (IPUMS replaced
#    the 2010-2012 and 2020-2021 weights; data/docs/cps/
#    cps_asec_1989_2025.md, ASECWT). Where a table prints a year twice, the
#    revised row is used for those years and the original row otherwise
#    (HH-3 prints 1993 and 1993r; IPUMS keeps 1993 on its original
#    controls, and our 1993 counts equal the original row)
asec_years <- 1989L:2025L
hh3_tolerance <- 0.010
uc_tolerance <- 0.020
uc1_householder_years <- 1996L:2006L
uc1_pecohab_years <- 2007L:2023L
uc3_years <- c(2018L, 2019L)
cohab_years <- 2007L:2025L
ipums_revised_years <- c(2010L:2012L, 2020L:2021L)
diagnostic_benchmark_years <- data.table::data.table(
  benchmark = c("uc1", "uc3"),
  first_year = c(2007L, 2007L)
)
hh3_exempt_year <- 2014L
hh3_exempt_reason <- paste(
  "2014 split sample: Census's full-sample weights are not public;",
  "ASECWT halved over both HFLAG files (owner decision, 2026-10-11)"
)
vintage_adjusted <- data.table::data.table(
  year = c(2010L, 2020L),
  shift_year = c(2011L, 2021L)
)

# 4) HH-3 age groups, and the UC3 partner age groups, as CPS age ranges
hh3_groups <- data.table::data.table(
  hh3_label = c("Under 25", "25 to 29 years", "30 to 34 years",
                "35 to 44 years"),
  age_group = c("under 25", "25-29", "30-34", "35-44"),
  age_min = c(0L, 25L, 30L, 35L),
  age_max = c(24L, 29L, 34L, 44L)
)
uc3_groups <- data.table::data.table(
  uc3_label = c(".15-24 years", ".25-29 years", ".30-34 years",
                ".35-39 years", ".40-44 years"),
  age_group = c("15-24", "25-29", "30-34", "35-39", "40-44"),
  age_min = c(15L, 25L, 30L, 35L, 40L),
  age_max = c(24L, 29L, 34L, 39L, 44L)
)

# 5) RELATE codes (data/docs/cps/cps_asec_relate_crosswalk_1989_2025.md):
#    householder 0101; unmarried partner of the householder 1114 (1995-2018),
#    1116 opposite sex and 1117 same sex (2019 on)
relate_householder <- 101L
relate_partner_codes <- c(1114L, 1116L, 1117L)

run_start <- Sys.time()

################################################################################
###                         3. Read the IPUMS extracts                       ###
################################################################################

# 6) Find each extract's data file and DDI in the manifest written by 07
ipums_manifest <- utils::read.csv(
  ipums_manifest_path,
  colClasses = "character", na.strings = "", check.names = FALSE
)
manifest_path_for <- function(file_role) {
  role_row <- ipums_manifest[ipums_manifest$file_role == file_role, ]
  if (nrow(role_row) != 1L) {
    stop("Expected one ", file_role, " row in ", ipums_manifest_path,
         ". Run code/07_download_ipums_cps.R first.", call. = FALSE)
  }
  here::here(role_row$local_path)
}

# 7) Main extract, every person 1989-2025 (all ages; HH-3 and UC-1 count
#    every age)
main_variables <- c(
  "YEAR", "SERIAL", "PERNUM", "ASECWT", "AGE", "SEX", "RELATE", "GQ"
)
main_ddi <- ipumsr::read_ipums_ddi(manifest_path_for("ipums_ddi"))
main_raw <- ipumsr::read_ipums_micro(
  main_ddi,
  vars = tidyselect::all_of(main_variables),
  data_file = manifest_path_for("ipums_data"), verbose = FALSE
)
cps <- data.table::as.data.table(ipumsr::zap_ipums_attributes(main_raw))
rm(main_raw)
integer_columns <- c("YEAR", "SERIAL", "PERNUM", "AGE", "SEX", "RELATE", "GQ")
cps[, (integer_columns) := lapply(.SD, as.integer),
    .SDcols = integer_columns]
cps[, person_weight := ASECWT]
cps[YEAR == 2014L, person_weight := ASECWT / 2]
message("Read ", format(nrow(cps), big.mark = ","), " persons (main)")

# 8) Cohab extract, every person 2007-2025, joined one to one on YEAR,
#    SERIAL, PERNUM; its ASECWT, AGE, SEX, and RELATE must match the main
#    extract's
cohab_variables <- c(
  "YEAR", "SERIAL", "PERNUM", "LINENO", "ASECWT", "AGE", "SEX", "RELATE",
  "PECOHAB"
)
cohab_ddi <- ipumsr::read_ipums_ddi(manifest_path_for("ipums_cohab_ddi"))
cohab_raw <- ipumsr::read_ipums_micro(
  cohab_ddi,
  vars = tidyselect::all_of(cohab_variables),
  data_file = manifest_path_for("ipums_cohab_data"), verbose = FALSE
)
cohab <- data.table::as.data.table(ipumsr::zap_ipums_attributes(cohab_raw))
rm(cohab_raw)
cohab_integer_columns <- c(
  "YEAR", "SERIAL", "PERNUM", "LINENO", "AGE", "SEX", "RELATE", "PECOHAB"
)
cohab[, (cohab_integer_columns) := lapply(.SD, as.integer),
      .SDcols = cohab_integer_columns]
if (!identical(sort(unique(cohab$YEAR)), cohab_years)) {
  stop("Cohab extract years are not exactly 2007-2025", call. = FALSE)
}
if (anyNA(cohab$LINENO) || anyNA(cohab$PECOHAB)) {
  stop("LINENO or PECOHAB is missing in the cohab extract", call. = FALSE)
}
cohab_main <- cps[YEAR %in% cohab_years]
cohab <- merge(
  cohab_main, cohab,
  by = c("YEAR", "SERIAL", "PERNUM"), suffixes = c("", "_cohab"),
  sort = FALSE
)
if (nrow(cohab) != nrow(cohab_main)) {
  stop("The cohab extract does not match the main extract one to one ",
       "for 2007-2025 (", nrow(cohab), " of ", nrow(cohab_main), ")",
       call. = FALSE)
}
if (any(abs(cohab$ASECWT - cohab$ASECWT_cohab) > 1e-6) ||
      any(cohab$AGE != cohab$AGE_cohab) || any(cohab$SEX != cohab$SEX_cohab) ||
      any(cohab$RELATE != cohab$RELATE_cohab)) {
  stop("ASECWT, AGE, SEX, or RELATE differs between the main and cohab ",
       "extracts", call. = FALSE)
}
rm(cohab_main)
if (anyDuplicated(cohab, by = c("YEAR", "SERIAL", "LINENO")) > 0L) {
  stop("LINENO does not identify persons within households",
       call. = FALSE)
}
message("Joined the cohab extract: ", format(nrow(cohab), big.mark = ","),
        " persons, 2007-2025")

################################################################################
###                    4. Our householders and couples                       ###
################################################################################

# 9) Householders (GQ == 1, RELATE 0101) by HH-3 age group and year
householders <- cps[GQ == 1L & RELATE == relate_householder]
ours_hh3_list <- vector("list", nrow(hh3_groups))
for (group_index in seq_len(nrow(hh3_groups))) {
  group_mask <- householders$AGE >= hh3_groups$age_min[group_index] &
    householders$AGE <= hh3_groups$age_max[group_index]
  group_sums <- householders[group_mask, list(
    ours = sum(person_weight), ours_alternative = NA_real_
  ), by = list(year = YEAR)]
  group_sums[, cell := hh3_groups$age_group[group_index]]
  ours_hh3_list[[group_index]] <- group_sums
}
ours_hh3 <- data.table::rbindlist(ours_hh3_list)
ours_hh3[, benchmark := "hh3"]

# 10) 1996-2006: households whose householder has an opposite-sex unmarried
#     partner (RELATE 1114). One per household; weighted by the
#     householder, with the mean of the two weights as the alternative
partners_hh <- cps[
  GQ == 1L & RELATE %in% relate_partner_codes &
    YEAR %in% uc1_householder_years,
  list(YEAR, SERIAL, partner_pernum = PERNUM, partner_sex = SEX,
       partner_weight = person_weight)
]
data.table::setorder(partners_hh, YEAR, SERIAL, partner_pernum)
partners_hh <- unique(partners_hh, by = c("YEAR", "SERIAL"))
householder_couples <- merge(
  householders[YEAR %in% uc1_householder_years,
               list(YEAR, SERIAL, householder_sex = SEX,
                    householder_weight = person_weight)],
  partners_hh,
  by = c("YEAR", "SERIAL"), sort = FALSE
)
if (nrow(householder_couples) != nrow(partners_hh)) {
  stop("Some 1996-2006 partners have no householder", call. = FALSE)
}
ours_uc1_householder <- householder_couples[
  householder_sex != partner_sex,
  list(
    ours = sum(householder_weight),
    ours_alternative = sum((householder_weight + partner_weight) / 2)
  ),
  by = list(year = YEAR)
]
ours_uc1_householder[, cell := "all couples"]
ours_uc1_householder[, benchmark := "uc1"]
ours_uc1_householder[, definition := "householder couples (RELATE)"]

# 11) 2007-2025: PECOHAB couples. A person's PECOHAB is the LINENO of a
#     cohabiting partner in the same household (0 if none). Each couple is
#     counted once, as the unordered pair; pairs named by only one partner
#     are counted too, and their number is logged. Weighted by the partner
#     with the lower PERNUM, with the mean of the two as the alternative
pointers <- cohab[GQ == 1L & PECOHAB > 0L, list(
  YEAR, SERIAL, from_pernum = PERNUM, partner_lineno = PECOHAB
)]
line_lookup <- cohab[GQ == 1L, list(
  YEAR, SERIAL, partner_lineno = LINENO, to_pernum = PERNUM
)]
pointers <- merge(
  pointers, line_lookup,
  by = c("YEAR", "SERIAL", "partner_lineno"), all.x = TRUE, sort = FALSE
)
if (anyNA(pointers$to_pernum)) {
  stop(sum(is.na(pointers$to_pernum)), " PECOHAB values name no person in ",
       "the household", call. = FALSE)
}
if (any(pointers$to_pernum == pointers$from_pernum)) {
  stop("Some persons name themselves in PECOHAB", call. = FALSE)
}
pointers[, pernum_low := pmin(from_pernum, to_pernum)]
pointers[, pernum_high := pmax(from_pernum, to_pernum)]
couples <- pointers[, list(named_by_int = .N),
                    by = list(YEAR, SERIAL, pernum_low, pernum_high)]
person_lookup <- cohab[, list(YEAR, SERIAL, PERNUM, AGE, SEX, person_weight)]
couples <- merge(
  couples, person_lookup,
  by.x = c("YEAR", "SERIAL", "pernum_low"),
  by.y = c("YEAR", "SERIAL", "PERNUM"), sort = FALSE
)
data.table::setnames(couples, c("AGE", "SEX", "person_weight"),
                     c("age_low", "sex_low", "weight_low"))
couples <- merge(
  couples, person_lookup,
  by.x = c("YEAR", "SERIAL", "pernum_high"),
  by.y = c("YEAR", "SERIAL", "PERNUM"), sort = FALSE
)
data.table::setnames(couples, c("AGE", "SEX", "person_weight"),
                     c("age_high", "sex_high", "weight_high"))
persons_in_couples <- c(
  paste(couples$YEAR, couples$SERIAL, couples$pernum_low),
  paste(couples$YEAR, couples$SERIAL, couples$pernum_high)
)
if (anyDuplicated(persons_in_couples) > 0L) {
  stop("Some persons belong to more than one PECOHAB couple",
       call. = FALSE)
}
message("PECOHAB couples 2007-2025: ", nrow(couples), "; named by one ",
        "partner only: ", sum(couples$named_by_int == 1L))
opposite_couples <- couples[sex_low != sex_high]
opposite_couples[, age_male := data.table::fifelse(
  sex_low == 1L, age_low, age_high
)]
opposite_couples[, age_female := data.table::fifelse(
  sex_low == 2L, age_low, age_high
)]
opposite_couples[, weight_mean := (weight_low + weight_high) / 2]
ours_uc1_pecohab <- opposite_couples[
  YEAR %in% uc1_pecohab_years,
  list(ours = sum(weight_low), ours_alternative = sum(weight_mean)),
  by = list(year = YEAR)
]
ours_uc1_pecohab[, cell := "all couples"]
ours_uc1_pecohab[, benchmark := "uc1"]
ours_uc1_pecohab[, definition := "all couples (PECOHAB)"]

# 12) 2018 and 2019: the same couples by age of the male and of the female
#     partner
ours_uc3_list <- list()
for (partner_sex in c("male", "female")) {
  partner_age <- opposite_couples[[paste0("age_", partner_sex)]]
  for (group_index in seq_len(nrow(uc3_groups))) {
    group_mask <- opposite_couples$YEAR %in% uc3_years &
      partner_age >= uc3_groups$age_min[group_index] &
      partner_age <= uc3_groups$age_max[group_index]
    group_sums <- opposite_couples[group_mask, list(
      ours = sum(weight_low), ours_alternative = sum(weight_mean)
    ), by = list(year = YEAR)]
    group_sums[, cell := paste(
      partner_sex, "partner", uc3_groups$age_group[group_index]
    )]
    ours_uc3_list[[paste(partner_sex, group_index)]] <- group_sums
  }
}
ours_uc3 <- data.table::rbindlist(ours_uc3_list)
ours_uc3[, benchmark := "uc3"]
ours_uc3[, definition := "all couples (PECOHAB)"]

# 13) RELATE against PECOHAB, 2007-2025: each RELATE partner of the
#     householder names the householder, and the householder names that
#     partner
householder_lines <- cohab[
  GQ == 1L & RELATE == relate_householder,
  list(YEAR, SERIAL, householder_lineno = LINENO,
       householder_pecohab = PECOHAB)
]
relate_partners <- cohab[
  GQ == 1L & RELATE %in% relate_partner_codes,
  list(YEAR, SERIAL, RELATE, LINENO, PECOHAB)
]
relate_partners <- merge(
  relate_partners, householder_lines,
  by = c("YEAR", "SERIAL"), sort = FALSE
)
relate_partners[, partner_names_householder_flag :=
                  PECOHAB == householder_lineno]
relate_partners[, householder_names_partner_flag :=
                  householder_pecohab == LINENO]
relate_check <- relate_partners[, list(
  relate_partners_n = .N,
  partner_misses_n = sum(!partner_names_householder_flag),
  householder_misses_n = sum(!householder_names_partner_flag)
), by = list(year = YEAR)]
data.table::setorder(relate_check, year)
if (!identical(relate_check$year, cohab_years)) {
  stop("Some 2007-2025 years have no RELATE partner of a householder",
       call. = FALSE)
}

################################################################################
###                          5. Read the Census tables                       ###
################################################################################

# 14) Read a Census sheet as a text matrix and confirm it is in thousands
read_census_cells <- function(path) {
  cells <- as.matrix(suppressMessages(readxl::read_excel(
    path,
    sheet = 1L, col_names = FALSE, col_types = "text"
  )))
  cells <- trimws(cells)
  if (!any(grepl("Numbers in thousands", cells, fixed = TRUE))) {
    stop(basename(path), " does not state that numbers are in thousands",
         call. = FALSE)
  }
  cells
}

# 15) HH-3: the header row holding the four age labels; year rows "YYYY"
#     with an optional footnote letter ("r" revised, "s" 2014 full sample,
#     "t" Vintage 2025)
hh3_cells <- read_census_cells(hh3_path)
hh3_header_row <- which(apply(hh3_cells, 1L, function(row_cells) {
  all(hh3_groups$hh3_label %in% row_cells)
}))
if (length(hh3_header_row) != 1L) {
  stop("Cannot find one HH-3 header row with the four age labels",
       call. = FALSE)
}
hh3_column <- match(hh3_groups$hh3_label, hh3_cells[hh3_header_row, ])
hh3_year_rows <- which(grepl("^[0-9]{4}[a-z]?$", hh3_cells[, 1L]))
hh3_published_list <- vector("list", nrow(hh3_groups))
for (group_index in seq_len(nrow(hh3_groups))) {
  hh3_published_list[[group_index]] <- data.table::data.table(
    row_label = hh3_cells[hh3_year_rows, 1L],
    cell = hh3_groups$age_group[group_index],
    published_thousands = suppressWarnings(as.numeric(
      hh3_cells[hh3_year_rows, hh3_column[group_index]]
    ))
  )
}
hh3_published <- data.table::rbindlist(hh3_published_list)
hh3_published[, benchmark := "hh3"]

# 16) UC-1: the "Total" column under "Unmarried couples"; year rows as in
#     HH-3; "N" (not available) before 1996
uc1_cells <- read_census_cells(uc1_path)
uc1_group_row <- which(apply(uc1_cells, 1L, function(row_cells) {
  any(row_cells %in% "Unmarried couples")
}))
if (length(uc1_group_row) != 1L ||
      uc1_cells[uc1_group_row + 1L,
                match("Unmarried couples", uc1_cells[uc1_group_row, ])] !=
        "Total") {
  stop("Cannot find UC-1's 'Unmarried couples' Total column",
       call. = FALSE)
}
uc1_column <- match("Unmarried couples", uc1_cells[uc1_group_row, ])
uc1_year_rows <- which(grepl("^[0-9]{4}[a-z]?$", uc1_cells[, 1L]))
uc1_published <- data.table::data.table(
  row_label = uc1_cells[uc1_year_rows, 1L],
  cell = "all couples",
  published_thousands = suppressWarnings(as.numeric(
    uc1_cells[uc1_year_rows, uc1_column]
  )),
  benchmark = "uc1"
)

# 17) UC3 (one file per year): the "Total" count in the rows below "AGE OF
#     MALE PARTNER" and "AGE OF FEMALE PARTNER"
uc3_published_list <- list()
for (uc3_year in names(uc3_paths)) {
  uc3_cells <- read_census_cells(uc3_paths[[uc3_year]])
  if (!grepl(paste0(": ", uc3_year, "$"), uc3_cells[2L, 1L])) {
    stop(basename(uc3_paths[[uc3_year]]), " is not the ", uc3_year,
         " UC3 table", call. = FALSE)
  }
  for (partner_sex in c("male", "female")) {
    section_row <- which(uc3_cells[, 1L] ==
                           paste("AGE OF", toupper(partner_sex), "PARTNER"))
    if (length(section_row) != 1L) {
      stop("UC3 ", uc3_year, ": cannot find the ", partner_sex,
           " partner age section", call. = FALSE)
    }
    section_rows <- section_row + seq_len(nrow(uc3_groups))
    if (!identical(uc3_cells[section_rows, 1L], uc3_groups$uc3_label)) {
      stop("UC3 ", uc3_year, ": unexpected ", partner_sex,
           " partner age rows", call. = FALSE)
    }
    uc3_published_list[[paste(uc3_year, partner_sex)]] <-
      data.table::data.table(
        row_label = uc3_year,
        cell = paste(partner_sex, "partner", uc3_groups$age_group),
        published_thousands = as.numeric(uc3_cells[section_rows, 2L]),
        benchmark = "uc3"
      )
  }
}
uc3_published <- data.table::rbindlist(uc3_published_list)

# 18) One published value per benchmark, cell, and year: for a year
#     printed twice, the revised row in the IPUMS-revised years and the
#     original row otherwise (step 3); the 2011 and 2021 pairs also give
#     the vintage shift
published_all <- data.table::rbindlist(
  list(hh3_published, uc1_published, uc3_published),
  use.names = TRUE
)
published_all[, year := as.integer(substr(row_label, 1L, 4L))]
published_all[, revised_flag := grepl("r$", row_label)]
published_all <- published_all[year %in% asec_years]
published_all[, has_revised := any(revised_flag),
              by = list(benchmark, cell, year)]
published_all[, use_revised_flag := year %in% ipums_revised_years]
published <- published_all[
  !has_revised | (revised_flag == use_revised_flag)
]
if (anyDuplicated(published, by = c("benchmark", "cell", "year")) > 0L) {
  stop("A Census table gives more than one row for a year and cell",
       call. = FALSE)
}
revision_shift <- merge(
  published_all[
    revised_flag == TRUE,
    list(benchmark, cell, year, revised = published_thousands)
  ],
  published_all[has_revised & !revised_flag,
                list(benchmark, cell, year,
                     unrevised = published_thousands)],
  by = c("benchmark", "cell", "year")
)
revision_shift[, revision_shift := revised / unrevised - 1]
revision_shift <- revision_shift[, list(
  benchmark, cell, shift_year = year, revision_shift
)]

################################################################################
###                         6. Compare and validate                          ###
################################################################################

# 19) Join ours to the published values for the years each comparison
#     covers; every expected cell must be present on both sides
ours_all <- data.table::rbindlist(list(
  ours_hh3[, definition := "householders (RELATE 0101)"],
  ours_uc1_householder, ours_uc1_pecohab, ours_uc3
), use.names = TRUE)
expected_cells <- data.table::rbindlist(list(
  data.table::CJ(benchmark = "hh3", cell = hh3_groups$age_group,
                 year = asec_years),
  data.table::CJ(benchmark = "uc1", cell = "all couples",
                 year = c(uc1_householder_years, uc1_pecohab_years)),
  data.table::CJ(benchmark = "uc3",
                 cell = unique(ours_uc3$cell), year = uc3_years)
))
comparison <- merge(
  expected_cells, ours_all,
  by = c("benchmark", "cell", "year"), all.x = TRUE, sort = FALSE
)
comparison <- merge(
  comparison,
  published[, list(benchmark, cell, year, row_label, published_thousands)],
  by = c("benchmark", "cell", "year"), all.x = TRUE, sort = FALSE
)
if (anyNA(comparison$ours) || anyNA(comparison$published_thousands)) {
  missing_cells <- comparison[is.na(ours) | is.na(published_thousands)]
  stop(nrow(missing_cells), " benchmark cells lack our value or a ",
       "published value (first: ", missing_cells$benchmark[1L], " ",
       missing_cells$cell[1L], " ", missing_cells$year[1L], ")",
       call. = FALSE)
}

# 20) Relative differences, the vintage adjustment for 2010 and 2020, and
#     the tolerance by benchmark
comparison[, relative_difference := ours / (1000 * published_thousands) - 1]
comparison[, relative_difference_alternative :=
             ours_alternative / (1000 * published_thousands) - 1]
comparison <- merge(
  comparison, vintage_adjusted,
  by = "year", all.x = TRUE, sort = FALSE
)
comparison <- merge(
  comparison, revision_shift,
  by = c("benchmark", "cell", "shift_year"), all.x = TRUE, sort = FALSE
)
adjusted_mask <- !is.na(comparison$shift_year)
if (anyNA(comparison$revision_shift[adjusted_mask])) {
  stop("A vintage-adjusted year has no published revision shift",
       call. = FALSE)
}
comparison[, comparison_basis := data.table::fifelse(
  adjusted_mask,
  paste0("vintage-adjusted by ", shift_year, "r - ", shift_year),
  "direct"
)]
comparison[, tested_difference := data.table::fifelse(
  adjusted_mask, relative_difference - revision_shift, relative_difference
)]
comparison[, tolerance := data.table::fifelse(
  benchmark == "hh3", hh3_tolerance, uc_tolerance
)]
comparison[, within_tolerance := abs(tested_difference) <= tolerance]
comparison <- merge(
  comparison, diagnostic_benchmark_years,
  by = "benchmark", all.x = TRUE, sort = FALSE
)
comparison[, test_role := data.table::fifelse(
  !is.na(first_year) & year >= first_year, "diagnostic", "test"
)]
comparison[, exempt_flag := benchmark == "hh3" & year == hh3_exempt_year]
comparison[, exemption_reason := data.table::fifelse(
  exempt_flag, hh3_exempt_reason, NA_character_
)]
comparison[, failed_flag := test_role == "test" & !exempt_flag &
             !within_tolerance]

################################################################################
###                         7. Write the table and log                       ###
################################################################################

# 21) headship_benchmarks.csv: one row per comparison
data.table::setorder(comparison, benchmark, cell, year)
benchmarks_table <- comparison[, list(
  benchmark, definition, cell, year, row_label, published_thousands,
  ours_thousands = ours / 1000,
  ours_alternative_thousands = ours_alternative / 1000,
  relative_difference, relative_difference_alternative, comparison_basis,
  revision_shift, tested_difference, tolerance, within_tolerance,
  test_role, exempt_flag, exemption_reason, failed_flag
)]
data.table::fwrite(benchmarks_table, benchmarks_output_path)
message("Wrote ", nrow(benchmarks_table), " comparisons to ",
        benchmarks_output_path)

# 22) Summary by benchmark and the RELATE-PECOHAB check
benchmark_summary <- benchmarks_table[, list(
  cells = .N, within = sum(within_tolerance), exempt = sum(exempt_flag),
  failed = sum(failed_flag), worst = max(abs(tested_difference))
), by = list(benchmark, test_role)]
for (row_index in seq_len(nrow(benchmark_summary))) {
  message(sprintf(
    paste0("%s (%s): %d of %d cells within tolerance, %d exempt, %d ",
           "failed; worst tested difference %.2f%%"),
    benchmark_summary$benchmark[row_index],
    benchmark_summary$test_role[row_index],
    benchmark_summary$within[row_index], benchmark_summary$cells[row_index],
    benchmark_summary$exempt[row_index], benchmark_summary$failed[row_index],
    100 * benchmark_summary$worst[row_index]
  ))
}
message("RELATE-PECOHAB check, 2007-2025: ",
        sum(relate_check$relate_partners_n), " RELATE partners; ",
        sum(relate_check$partner_misses_n), " do not name the householder; ",
        sum(relate_check$householder_misses_n), " are not named by the ",
        "householder")
message("Finished in ", round(as.numeric(difftime(
  Sys.time(), run_start, units = "mins"
)), 2), " minutes")

################################################################################
###                           8. Fail on any miss                            ###
################################################################################

# 23) Stop with a non-zero exit when any tested, non-exempt comparison or
#     the RELATE-PECOHAB check fails
failed_cells <- benchmarks_table[failed_flag == TRUE]
relate_failed <- relate_check[partner_misses_n > 0L | householder_misses_n > 0L]
if (nrow(failed_cells) > 0L || nrow(relate_failed) > 0L) {
  stop(
    nrow(failed_cells), " benchmark cells outside tolerance (",
    paste(utils::head(paste(failed_cells$benchmark, failed_cells$cell,
                            failed_cells$year), 10L), collapse = "; "),
    "); RELATE-PECOHAB mismatches in ", nrow(relate_failed), " years (",
    paste(relate_failed$year, collapse = ", "), ")", call. = FALSE
  )
}
message("All benchmark comparisons and the RELATE-PECOHAB check passed.")
