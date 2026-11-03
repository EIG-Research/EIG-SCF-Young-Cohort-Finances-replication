# 02_build_scf_panel -- stack the 1989-2025 SCF extracts and replicate weights
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U4 of records/plans/2026-10-11_stage1-scf-pipeline.md (with O1, O3,
# O7, O8, O9 and Amendment (b)-(c)). It reads the 13 summary extracts
# (scfp<year>), the replicate-weight files, and, for two checks only, the
# full public files, all as listed in data/raw/scf/manifest.csv, and writes
#   data/processed/scf_panel_1989_2025.parquet   (one row per case x implicate)
#   data/processed/scf_repwgts_1989_2025.parquet (one row per case)
# Downloaded files are data: they are unzipped into a temp directory with
# utils::unzip and read with haven, never executed. Every check stops with an
# informative error. Run from the project root as
#   Rscript code/02_build_scf_panel.R
# Checkpoint format exception to the R style rule (section 7): plan O9 and the
# U4 brief fix Parquet as the checkpoint format, so no .rds copy is written.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; all calls below
#    are qualified as pkg::fn()
required_packages <- c("here", "haven", "data.table", "arrow", "digest",
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

# 2) Stop unless this script sits at code/02_build_scf_panel.R in the project
here::i_am("code/02_build_scf_panel.R")

path_scf_raw <- here::here("data", "raw", "scf")
manifest_path <- file.path(path_scf_raw, "manifest.csv")
path_processed <- here::here("data", "processed")
panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
dir.create(path_processed, recursive = TRUE, showWarnings = FALSE)

run_start <- Sys.time()

# 3) Waves and the public-file family counts from each wave page
#    (data/docs/scf/scf_2025_overview.md,
#    "Case counts per wave"); every extract must have 5 x this many rows
wave_years <- c(
  1989L, 1992L, 1995L, 1998L, 2001L, 2004L, 2007L,
  2010L, 2013L, 2016L, 2019L, 2022L, 2025L
)
expected_cases <- c(
  3143L, 3906L, 4299L, 4305L, 4442L, 4519L, 4417L,
  6482L, 6015L, 6248L, 5777L, 4595L, 4360L
)
names(expected_cases) <- wave_years

# 4) CPI-U-RS constants from data/raw/scf/common/bulletin.macro.txt (2025
#    release, SHA-256 in the manifest). Section 5 re-reads the macro and
#    stops if any value below differs from it.
#    - cpibase: CPIBASE of the nominal-run %BULLIT call for the wave
#      (REAL=NO, ADJINC=YES), macro lines 4123-4135. It equals the September
#      CPI-U-RS x 100 of the survey year, the CPIADJ denominator at lines
#      77-149.
#    - cpilag: the macro sets CPILAG per year inside the year branches at
#      lines 76-151 (the %LET CPILAG lines 78, 84, ..., 150), as the annual
#      average CPI-U-RS of the survey year over that of the prior year. With
#      ADJINC=YES it multiplies INCOME and its components (the PYRINC array,
#      lines 905-909) and NORMINC (from 1995, line 911).
#    - The posted extracts come from the real run (REAL=YES, CPIBASE=4789,
#      lines 4138-4150), which multiplies every dollar variable by
#      CPIADJ = 4789 / cpibase (lines 3918-3942).
macro_constants <- data.frame(
  year = wave_years,
  cpibase = c(
    1898, 2112, 2261, 2400, 2614, 2785, 3058,
    3204, 3438, 3548, 3775, 4376, 4789
  ),
  cpibase_line = c(
    4135L, 4134L, 4133L, 4132L, 4131L, 4130L, 4129L,
    4128L, 4127L, 4126L, 4125L, 4124L, 4123L
  ),
  cpilag_numerator = c(
    1883, 2099, 2250, 2392, 2597, 2770, 3041,
    3198, 3420, 3528, 3758, 4315, 4750
  ),
  cpilag_denominator = c(
    1805, 2048, 2197, 2360, 2525, 2698, 2957,
    3147, 3369, 3483, 3691, 3992, 4625
  ),
  cpilag_line = c(
    78L, 84L, 90L, 96L, 102L, 108L, 114L,
    120L, 126L, 132L, 138L, 144L, 150L
  )
)
macro_constants$cpilag <-
  macro_constants$cpilag_numerator / macro_constants$cpilag_denominator
cpibase_real_run <- 4789

# 5) Dollar variables that get _nom and _pce25 versions. The extract value
#    stays under its own name as the _fed25 basis (2025 CPI-U-RS dollars).
#    Stocks (assets and debts, valued at the interview): the totals and every
#    component the balance-sheet doc lists inside them (FIN, LIQ, RETQLIQ,
#    BUS, NFIN, MRTHEL, INSTALL, DEBT).
stock_vars <- c(
  "networth", "asset", "fin", "nfin", "debt",
  "liq", "checking", "saving", "mma", "call", "prepaid",
  "cds", "nmmf", "stocks", "bond", "savbnd", "cashli", "othma", "othfin",
  "retqliq", "irakh", "thrift", "futpen", "currpen",
  "houses", "oresre", "nnresre", "bus", "actbus", "nonactbus",
  "vehic", "othnfin",
  "mrthel", "nh_mort", "heloc", "resdbt", "othloc", "ccbal",
  "install", "veh_inst", "edn_inst", "oth_inst", "bnpl", "odebt"
)
#    Income (prior calendar year, carrying the CPILAG uplift): the macro's
#    PYRINC array (lines 905-906; RENTINC is not in the extract) and NORMINC
income_vars <- c(
  "income", "norminc", "wageinc", "bussefarminc", "intdivinc", "kginc",
  "ssretinc", "transfothinc", "penacctwd"
)
dollar_vars <- c(stock_vars, income_vars)

# 6) Labels for the harmonized categories, in code order (codes from the
#    macro: AGECL line 353, EDCL lines 359-367, RACECL4 lines 480-482,
#    FAMSTRUCT lines 430-440, MARRIED lines 397-400, HHSEX line 348,
#    HOUSECL lines 2363-2373, INCCAT = %PCTL on INCOME at 0 20 40 60 80 90,
#    line 4011)
age_band_labels <- c(
  "under 25", "25-29", "30-34", "35-44", "45-54", "55-64", "65-74",
  "75 and over"
)
age_band_breaks <- c(-Inf, 24, 29, 34, 44, 54, 64, 74, Inf)
age_group_fed_labels <- c(
  "less than 35", "35-44", "45-54", "55-64", "65-74", "75 and over"
)
race4_labels <- c(
  "White non-Hispanic", "Black or African American non-Hispanic",
  "Hispanic or Latino", "Other or multiple race"
)
educ4_labels <- c(
  "No high school diploma or GED", "High school diploma or GED",
  "Some college or associate degree", "Bachelor's degree or higher"
)
sex_cut_labels <- c("couple", "single male", "single female")
family_structure_labels <- c(
  "Not married or LWP, with children",
  "Not married or LWP, no children, under 55",
  "Not married or LWP, no children, 55 or older",
  "Married or LWP, with children",
  "Married or LWP, no children"
)
income_group_labels <- c(
  "Less than 20", "20-39.9", "40-59.9", "60-79.9", "80-89.9", "90-100"
)
#    Five-year birth bands on years ending in 0 and 5 (O3), wide enough for
#    any reference person in 1989-2025; unused levels are dropped at the end
birth_band_starts <- seq(1880L, 2010L, by = 5L)
birth_band_labels <- sprintf(
  "%d-%02d", birth_band_starts, (birth_band_starts + 4L) %% 100L
)

# 7) Tolerances: implicate weight sums may differ by rounding of WGT; the
#    replicate-weight totals are bootstrap totals, so they are only checked
#    to be near the household total
weight_sum_rel_tol <- 1e-5
replicate_total_rel_tol <- 0.01

################################################################################
###                              3. Define helpers                           ###
################################################################################

# 8) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

################################################################################
###                     4. Check the inputs against the manifest             ###
################################################################################

# 9) Read the manifest with every column as character
if (!file.exists(manifest_path)) {
  stop("Manifest not found: ", manifest_path,
       "; run code/01_download_scf.R first", call. = FALSE)
}
manifest <- utils::read.csv(
  manifest_path,
  colClasses = "character", na.strings = "", check.names = FALSE
)

# 10) List the inputs this script reads: three files per wave, the macro,
#     and the PCE index
input_files <- rbind(
  data.frame(
    year = rep(as.character(wave_years), each = 3L),
    file_role = rep(c("extract", "replicate_weights", "full_public"),
                    times = length(wave_years)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    year = c("2025", NA_character_),
    file_role = c("bulletin_macro", "pce_index"),
    stringsAsFactors = FALSE
  )
)
input_files$local_path <- NA_character_
for (input_index in seq_len(nrow(input_files))) {
  role_flag <- manifest$file_role == input_files$file_role[input_index]
  year_flag <- if (is.na(input_files$year[input_index])) {
    is.na(manifest$year)
  } else {
    !is.na(manifest$year) & manifest$year == input_files$year[input_index]
  }
  manifest_rows <- manifest[role_flag & year_flag, , drop = FALSE]
  if (nrow(manifest_rows) != 1L) {
    stop(
      "Expected one manifest row for ", input_files$file_role[input_index],
      " ", input_files$year[input_index], ", found ", nrow(manifest_rows),
      call. = FALSE
    )
  }

  # 11) Stop if the file is missing or its SHA-256 differs from the manifest
  full_path <- here::here(manifest_rows$local_path)
  if (!file.exists(full_path)) {
    stop("Input file missing: ", full_path, call. = FALSE)
  }
  file_hash <- digest::digest(file = full_path, algo = "sha256")
  if (!identical(file_hash, manifest_rows$sha256)) {
    stop("SHA-256 of ", full_path, " does not match the manifest",
         call. = FALSE)
  }
  input_files$local_path[input_index] <- manifest_rows$local_path
}
log_step(paste(nrow(input_files), "input files match the manifest hashes"))

################################################################################
###                    5. Check the CPI-U-RS constants in the macro          ###
################################################################################

# 12) Read the macro as text (it is never run)
macro_path <- here::here(
  input_files$local_path[input_files$file_role == "bulletin_macro"]
)
macro_lines <- readLines(macro_path, warn = FALSE)

# 13) CPIBASE of each nominal-run %BULLIT call, with its line number
nominal_pattern <- paste0(
  "^%BULLIT\\(YEAR=([0-9]{4}),REAL=NO,ADJINC=YES,",
  "CPIBASE=([0-9]+),PUBLIC=YES\\);"
)
nominal_line <- grep(nominal_pattern, macro_lines)
parsed_cpibase <- data.frame(
  year = as.integer(sub(nominal_pattern, "\\1", macro_lines[nominal_line])),
  cpibase = as.numeric(sub(nominal_pattern, "\\2",
                           macro_lines[nominal_line])),
  cpibase_line = nominal_line
)

# 14) CPILAG and the CPIADJ denominator inside each year branch
branch_pattern <- "\\(&YEAR=([0-9]{4})\\) %THEN %DO;"
cpilag_pattern <- "^ *%LET CPILAG=([0-9]+)/([0-9]+);"
cpiadj_pattern <- "^ *%LET CPIADJ=&CPIBASE/([0-9]+);"
branch_line <- grep(branch_pattern, macro_lines)
parsed_cpilag <- data.frame(
  year = as.integer(sub(paste0(".*", branch_pattern, ".*"), "\\1",
                        macro_lines[branch_line])),
  cpiadj_denominator = NA_real_,
  cpilag_numerator = NA_real_,
  cpilag_denominator = NA_real_,
  cpilag_line = NA_integer_
)
for (branch_index in seq_along(branch_line)) {
  branch_body <- branch_line[branch_index] + 1L:2L
  cpiadj_hit <- branch_body[grepl(cpiadj_pattern, macro_lines[branch_body])]
  cpilag_hit <- branch_body[grepl(cpilag_pattern, macro_lines[branch_body])]
  if (length(cpiadj_hit) != 1L || length(cpilag_hit) != 1L) {
    stop("Macro year branch at line ", branch_line[branch_index],
         " lacks one CPIADJ and one CPILAG line", call. = FALSE)
  }
  parsed_cpilag$cpiadj_denominator[branch_index] <-
    as.numeric(sub(cpiadj_pattern, "\\1", macro_lines[cpiadj_hit]))
  parsed_cpilag$cpilag_numerator[branch_index] <-
    as.numeric(sub(cpilag_pattern, "\\1", macro_lines[cpilag_hit]))
  parsed_cpilag$cpilag_denominator[branch_index] <-
    as.numeric(sub(cpilag_pattern, "\\2", macro_lines[cpilag_hit]))
  parsed_cpilag$cpilag_line[branch_index] <- cpilag_hit
}

# 15) Stop unless the parsed values equal the constants in step 4
parsed_constants <- merge(parsed_cpibase, parsed_cpilag, by = "year")
parsed_constants <- parsed_constants[
  match(macro_constants$year, parsed_constants$year), , drop = FALSE
]
constant_columns <- c("cpibase", "cpibase_line", "cpilag_numerator",
                      "cpilag_denominator", "cpilag_line")
constants_match_flag <- nrow(parsed_cpibase) == length(wave_years) &&
  nrow(parsed_cpilag) == length(wave_years) &&
  !anyNA(parsed_constants$year) &&
  isTRUE(all.equal(
    as.list(macro_constants[, constant_columns]),
    as.list(parsed_constants[, constant_columns]),
    check.attributes = FALSE
  )) &&
  all(parsed_constants$cpiadj_denominator == macro_constants$cpibase)
if (!constants_match_flag) {
  stop("CPI-U-RS constants in ", macro_path, " differ from step 4; ",
       "update macro_constants and the line numbers", call. = FALSE)
}
real_run_count <- sum(grepl(
  "^%BULLIT\\(YEAR=[0-9]{4},REAL=YES,ADJINC=YES,CPIBASE=4789,PUBLIC=YES\\);",
  macro_lines
))
if (real_run_count != length(wave_years)) {
  stop("Expected ", length(wave_years), " real-run %BULLIT calls with ",
       "CPIBASE=4789 in ", macro_path, ", found ", real_run_count,
       call. = FALSE)
}
log_step("Macro CPIBASE and CPILAG constants match step 4")

################################################################################
###                       6. Build the PCE deflator, 2025 = 100              ###
################################################################################

# 16) Read the annual headline PCE price index (FRED DPCERG3A086NBEA,
#     2017 = 100) with explicit column types
pce_path <- here::here(
  input_files$local_path[input_files$file_role == "pce_index"]
)
pce_raw <- utils::read.csv(
  pce_path,
  colClasses = c("character", "numeric"), check.names = FALSE
)
if (!identical(names(pce_raw), c("observation_date", "DPCERG3A086NBEA"))) {
  stop("Unexpected columns in ", pce_path, call. = FALSE)
}
pce_index <- data.frame(
  year = as.integer(substr(pce_raw$observation_date, 1L, 4L)),
  pce_2017 = pce_raw$DPCERG3A086NBEA
)
needed_pce_years <- sort(unique(c(wave_years, wave_years - 1L)))
if (!all(needed_pce_years %in% pce_index$year) ||
      anyNA(pce_index$pce_2017) || anyDuplicated(pce_index$year) > 0L) {
  stop("PCE index lacks a value for a needed year or has duplicates: ",
       pce_path, call. = FALSE)
}

# 17) Rebase so the 2025 annual average = 100
pce_index$pce_2025 <-
  100 * pce_index$pce_2017 / pce_index$pce_2017[pce_index$year == 2025L]

# 18) Per-wave factors (O1): assets and debts use the survey-year index;
#     income, which refers to the prior calendar year, uses the prior
#     year's index after CPILAG is divided out
wave_factors <- macro_constants[, c("year", "cpibase", "cpilag")]
wave_factors$nominal_factor <- wave_factors$cpibase / cpibase_real_run
wave_factors$pce_stock_factor <- 100 / pce_index$pce_2025[
  match(wave_factors$year, pce_index$year)
]
wave_factors$pce_income_factor <- 100 / pce_index$pce_2025[
  match(wave_factors$year - 1L, pce_index$year)
]
for (factor_index in seq_len(nrow(wave_factors))) {
  log_step(sprintf(
    "%d: nominal = x %.6f; CPILAG %.6f; PCE stock x %.6f; income x %.6f",
    wave_factors$year[factor_index], wave_factors$nominal_factor[factor_index],
    wave_factors$cpilag[factor_index],
    wave_factors$pce_stock_factor[factor_index],
    wave_factors$pce_income_factor[factor_index]
  ))
}

################################################################################
###                           7. Build each wave                             ###
################################################################################

# 19) One temp directory for every unzipped file; removed in section 9
build_temp_dir <- tempfile("scf_build_")
dir.create(build_temp_dir)

panel_list <- list()
repwgts_list <- list()
extract_columns <- character(0L)
wt_names <- paste0("wt1b", 1L:999L)
mm_names <- paste0("mm", 1L:999L)
rw_names <- paste0("rw_", 1L:999L)

for (wave_year in wave_years) {
  wave_label <- as.character(wave_year)
  wave_temp_dir <- file.path(build_temp_dir, wave_label)
  dir.create(wave_temp_dir)
  case_count <- expected_cases[[wave_label]]
  factor_row <- wave_factors[wave_factors$year == wave_year, ]

  # 20) Unzip the wave's three Stata files; each zip must hold one .dta
  wave_dta <- character(0L)
  for (file_role in c("extract", "replicate_weights", "full_public")) {
    zip_path <- here::here(input_files$local_path[
      input_files$file_role == file_role &
        !is.na(input_files$year) & input_files$year == wave_label
    ])
    unzipped <- utils::unzip(zip_path, exdir = wave_temp_dir)
    if (length(unzipped) != 1L || !grepl("\\.dta$", unzipped)) {
      stop("Expected one .dta file in ", zip_path, ", found: ",
           paste(basename(unzipped), collapse = ", "), call. = FALSE)
    }
    wave_dta[[file_role]] <- unzipped
  }

  # 21) Read the extract; drop Stata labels and formats (values unchanged),
  #     lowercase the names, and require every column to be numeric
  extract <- haven::read_dta(wave_dta[["extract"]])
  extract <- as.data.frame(haven::zap_formats(haven::zap_label(extract)))
  names(extract) <- tolower(names(extract))
  if (anyDuplicated(names(extract)) > 0L) {
    stop(wave_year, " extract has duplicate lowercased names", call. = FALSE)
  }
  numeric_flag <- vapply(extract, is.numeric, logical(1L))
  if (!all(numeric_flag)) {
    stop(wave_year, " extract has non-numeric columns: ",
         paste(names(extract)[!numeric_flag], collapse = ", "), call. = FALSE)
  }

  # 22) IDs: the 1989 extract carries X1 and XX1 instead of Y1 and YY1
  #     (macro lines 159-192; scf_2025_ids_weights.md); rename them so every
  #     wave has yy1 (case) and y1 (record = 10 x case + implicate)
  if (wave_year == 1989L) {
    if (!all(c("x1", "xx1") %in% names(extract)) ||
          any(c("y1", "yy1") %in% names(extract))) {
      stop("1989 extract does not have X1 and XX1 as its IDs", call. = FALSE)
    }
    names(extract)[names(extract) == "x1"] <- "y1"
    names(extract)[names(extract) == "xx1"] <- "yy1"
  }
  missing_vars <- setdiff(
    c("y1", "yy1", "wgt", "age", "agecl", "racecl4", "racecl5", "racecl_ex",
      "edcl", "married", "hhsex", "famstruct", "housecl", "inccat",
      dollar_vars),
    names(extract)
  )
  if (length(missing_vars) > 0L) {
    stop(wave_year, " extract lacks: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }
  extract_columns <- union(extract_columns, names(extract))

  # 23) C1: rows = 5 x the documented public-file family count
  if (nrow(extract) != 5L * case_count) {
    stop(wave_year, " extract has ", nrow(extract), " rows; expected 5 x ",
         case_count, " = ", 5L * case_count, call. = FALSE)
  }

  # 24) C5: implicate = Y1 - 10 x YY1 (codebook 2025 line 2309), a whole
  #     number 1-5, with one row per case in each implicate
  implicate_num <- extract$y1 - 10 * extract$yy1
  if (anyNA(implicate_num) || !all(implicate_num %in% 1:5)) {
    stop(wave_year, ": y1 - 10 * yy1 is not always 1-5", call. = FALSE)
  }
  extract$year <- wave_year
  extract$implicate <- as.integer(implicate_num)
  implicate_counts <- tabulate(extract$implicate, nbins = 5L)
  if (any(implicate_counts != case_count) ||
        length(unique(extract$yy1)) != case_count ||
        anyDuplicated(extract$y1) > 0L) {
    stop(wave_year, ": implicates are not one row per case (counts ",
         paste(implicate_counts, collapse = "/"), ")", call. = FALSE)
  }

  # 25) Weight: WGT = X42001 / 5 (macro line 321), used as is; no missing or
  #     non-positive value, and equal sums across the five implicates
  if (anyNA(extract$wgt) || any(extract$wgt <= 0)) {
    stop(wave_year, ": wgt is missing or non-positive", call. = FALSE)
  }
  implicate_wgt_sum <- tapply(extract$wgt, extract$implicate, sum)
  wgt_sum_spread <- (max(implicate_wgt_sum) - min(implicate_wgt_sum)) /
    mean(implicate_wgt_sum)
  if (wgt_sum_spread > weight_sum_rel_tol) {
    stop(wave_year, ": implicate weight sums differ by a relative ",
         signif(wgt_sum_spread, 3), call. = FALSE)
  }

  # 26) Dollar bases. _nom = extract x CPIBASE_t / 4789 undoes the real
  #     run, giving the nominal historical tables' basis (income keeps the
  #     CPILAG uplift). The real run leaves values of -1 and -2 unscaled
  #     (macro line 3941), so they are left unscaled here too, in both _nom
  #     and _pce25. _pce25 applies O1: stocks x PCE_2025 / PCE_t; income /
  #     CPILAG_t x PCE_2025 / PCE_(t-1). Missing NORMINC (1989, 1992) stays
  #     missing.
  for (dollar_var in dollar_vars) {
    fed25_value <- extract[[dollar_var]]
    unscaled_flag <- !is.na(fed25_value) & fed25_value %in% c(-1, -2)
    nominal_value <- ifelse(
      unscaled_flag, fed25_value, fed25_value * factor_row$nominal_factor
    )
    if (dollar_var %in% income_vars) {
      pce25_value <- nominal_value / factor_row$cpilag *
        factor_row$pce_income_factor
    } else {
      pce25_value <- nominal_value * factor_row$pce_stock_factor
    }
    pce25_value <- ifelse(unscaled_flag, fed25_value, pce25_value)
    extract[[paste0(dollar_var, "_nom")]] <- nominal_value
    extract[[paste0(dollar_var, "_pce25")]] <- pce25_value
  }

  # 27) Age and the age groups. AGE is the reference person's age, top-coded
  #     at 95; AGECL must match the macro's own cut points (line 353)
  if (anyNA(extract$age) || any(extract$age != round(extract$age))) {
    stop(wave_year, ": age is missing or not whole", call. = FALSE)
  }
  extract$age_band <- cut(
    extract$age,
    breaks = age_band_breaks, labels = age_band_labels, right = TRUE
  )
  extract$young <- extract$age < 35
  agecl_from_age <- 1L + (extract$age >= 35) + (extract$age >= 45) +
    (extract$age >= 55) + (extract$age >= 65) + (extract$age >= 75)
  if (!isTRUE(all(extract$agecl == agecl_from_age))) {
    stop(wave_year, ": AGECL does not match AGE", call. = FALSE)
  }
  extract$age_group_fed <- factor(
    extract$agecl,
    levels = 1:6, labels = age_group_fed_labels
  )

  # 28) Birth year = survey year - age, which can be off by one (O3), and
  #     five-year birth bands on years ending in 0 and 5
  extract$birth_year <- as.integer(wave_year - extract$age)
  extract$birth_band <- factor(
    5L * (extract$birth_year %/% 5L),
    levels = birth_band_starts, labels = birth_band_labels
  )

  # 29) Race and ethnicity from RACECL4 (O8). Race describes the survey
  #     RESPONDENT, not the reference person (2025 bulletin, footnote 11;
  #     macro lines 451-454). 2025 changed the question (Hispanic origin no
  #     longer asked separately; MENA folded into white non-Hispanic, macro
  #     line 490), so race_2025_break flags a coding break.
  extract$race4 <- factor(
    extract$racecl4,
    levels = 1:4, labels = race4_labels
  )
  extract$race_2025_break <- wave_year == 2025L

  # 30) Education of the reference person: the bulletin's four EDCL groups
  #     (question changed in 2016; EDCL is the consistent cut)
  extract$educ4 <- factor(extract$edcl, levels = 1:4, labels = educ4_labels)

  # 31) Sex cut (O7): MARRIED 1 = married or living with a partner; the
  #     reference person of a mixed-sex couple is the man, so HHSEX (1 male,
  #     2 female) separates only single (not married or LWP) people
  sex_cut_code <- ifelse(
    extract$married == 1, 1L,
    ifelse(extract$married == 2 & extract$hhsex == 1, 2L,
           ifelse(extract$married == 2 & extract$hhsex == 2, 3L, NA_integer_))
  )
  extract$sex_cut <- factor(sex_cut_code, levels = 1:3,
                            labels = sex_cut_labels)

  # 32) Family structure (FAMSTRUCT), homeownership (HOUSECL 1 = owns its
  #     home, 2 = otherwise), and income group (INCCAT: percentile of INCOME
  #     over all families in the wave, the bulletin's income groups; the
  #     usual-income NINCCAT is undefined in 1989 and 1992)
  extract$family_structure <- factor(
    extract$famstruct,
    levels = 1:5, labels = family_structure_labels
  )
  extract$homeowner <- ifelse(
    extract$housecl %in% c(1, 2), extract$housecl == 1, NA
  )
  extract$income_group <- factor(
    extract$inccat,
    levels = 1:6, labels = income_group_labels
  )

  # 33) No harmonized category may be unmapped
  harmonized_columns <- c(
    "age_band", "young", "age_group_fed", "birth_year", "birth_band",
    "race4", "race_2025_break", "educ4", "sex_cut", "family_structure",
    "homeowner", "income_group"
  )
  unmapped_flag <- vapply(
    harmonized_columns, function(column) anyNA(extract[[column]]),
    logical(1L)
  )
  if (any(unmapped_flag)) {
    stop(wave_year, ": unmapped codes in ",
         paste(harmonized_columns[unmapped_flag], collapse = ", "),
         call. = FALSE)
  }

  # 34) Data checks from scf_2025_overview.md that apply to the build
  #     C8: Asian (RACECL5 = 4) and RACECL_EX other than 99 only from 2022
  if (wave_year < 2022L &&
        (any(extract$racecl5 == 4) || any(extract$racecl_ex != 99))) {
    stop(wave_year, ": Asian category present before 2022 (C8)",
         call. = FALSE)
  }
  #     C9: NORMINC missing in 1989 and 1992 only
  norminc_missing_flag <- is.na(extract$norminc)
  if ((wave_year <= 1992L && !all(norminc_missing_flag)) ||
        (wave_year > 1992L && any(norminc_missing_flag))) {
    stop(wave_year, ": NORMINC missingness differs from the docs (C9)",
         call. = FALSE)
  }
  #     C10: PREPAID is 0 before 2016 and BNPL is 0 before 2022
  if ((wave_year < 2016L && any(extract$prepaid != 0)) ||
        (wave_year < 2022L && any(extract$bnpl != 0))) {
    stop(wave_year, ": PREPAID or BNPL nonzero before entry (C10)",
         call. = FALSE)
  }
  #     C11: a female reference person among couples is rare (same-sex
  #     couples only); stop if the weighted share reaches 5 percent
  couple_flag <- extract$married == 1
  female_couple_share <- sum(extract$wgt[couple_flag & extract$hhsex == 2]) /
    sum(extract$wgt[couple_flag])
  if (female_couple_share >= 0.05) {
    stop(wave_year, ": weighted share of couples with a female reference ",
         "person is ", round(female_couple_share, 4), " (C11)", call. = FALSE)
  }
  #     C12: AGE top-coded at 95 and INCOME floored at 0
  if (max(extract$age) > 95 || min(extract$income) < 0) {
    stop(wave_year, ": age above 95 or negative income (C12)", call. = FALSE)
  }

  # 35) C4 and C2 against the full public file: WGT = X42001 / 5, and the
  #     de-inflated extract income, net of IRA and pension-account
  #     withdrawals (PENACCTWD, 0 before 2004), equals MAX(0, X5729) (macro
  #     lines 849-913). This confirms CPIBASE and CPILAG for every wave.
  full_id <- if (wave_year == 1989L) "x1" else "y1"
  full_file <- haven::read_dta(
    wave_dta[["full_public"]],
    col_select = tidyselect::any_of(
      c(full_id, toupper(full_id), "x42001", "X42001", "x5729", "X5729")
    )
  )
  full_file <- as.data.frame(haven::zap_formats(haven::zap_label(full_file)))
  names(full_file) <- tolower(names(full_file))
  full_match <- match(extract$y1, full_file[[full_id]])
  if (anyNA(full_match) || anyDuplicated(full_file[[full_id]]) > 0L ||
        nrow(full_file) != nrow(extract)) {
    stop(wave_year, ": extract and full public file records do not match ",
         "one to one on ", full_id, call. = FALSE)
  }
  x42001_matched <- full_file$x42001[full_match]
  if (any(abs(extract$wgt - x42001_matched / 5) > 1e-6 * x42001_matched)) {
    stop(wave_year, ": WGT differs from X42001 / 5 (C4)", call. = FALSE)
  }
  raw_income <- (extract$income_nom - extract$penacctwd_nom) /
    factor_row$cpilag
  reported_income <- pmax(0, full_file$x5729[full_match])
  income_gap <- abs(raw_income - reported_income)
  if (any(income_gap > 0.01 + 1e-9 * reported_income)) {
    stop(wave_year, ": de-inflated INCOME differs from MAX(0, X5729) by ",
         "up to ", signif(max(income_gap), 4), " (C2)", call. = FALSE)
  }

  # 36) Replicate weights (C6): one row per case, implicate 1 only, keyed by
  #     X1 in 1989 and by Y1 (with YY1) from 1992 (1989 and 1992 keys
  #     confirmed against the extract here; the docs leave them open)
  repwgts_raw <- haven::read_dta(wave_dta[["replicate_weights"]])
  repwgts_raw <- as.data.frame(
    haven::zap_formats(haven::zap_label(repwgts_raw))
  )
  names(repwgts_raw) <- tolower(names(repwgts_raw))
  rw_key_name <- if (wave_year == 1989L) "x1" else "y1"
  if (!all(c(rw_key_name, wt_names, mm_names) %in% names(repwgts_raw))) {
    stop(wave_year, ": replicate file lacks ", rw_key_name,
         " or WT1B1-WT1B999 / MM1-MM999", call. = FALSE)
  }
  rw_key <- repwgts_raw[[rw_key_name]]
  if (wave_year > 1989L &&
        !isTRUE(all(rw_key == 10 * repwgts_raw$yy1 + 1))) {
    stop(wave_year, ": replicate-file Y1 is not 10 x YY1 + 1", call. = FALSE)
  }
  implicate_one_y1 <- extract$y1[extract$implicate == 1L]
  if (anyNA(rw_key) || anyDuplicated(rw_key) > 0L ||
        length(rw_key) != case_count ||
        !setequal(rw_key, implicate_one_y1)) {
    stop(wave_year, ": replicate weights do not join one to one with the ",
         "implicate-1 records on ", rw_key_name, call. = FALSE)
  }

  # 37) rw_k = WT1Bk x MMk; a case not drawn into replicate k has missing
  #     WT1Bk and MMk, so both are floored at 0 as in the codebook's example,
  #     MAX(0,MULT) x MAX(0,RWGT) (codebook 2025 lines 2297-2305). The scale
  #     is that of X42001: each rw_k summed over the implicate-1 cases is
  #     about the household total, that is 5 x the implicate-1 sum of WGT.
  wt_matrix <- as.matrix(repwgts_raw[, wt_names])
  mm_matrix <- as.matrix(repwgts_raw[, mm_names])
  wt_matrix[is.na(wt_matrix) | wt_matrix < 0] <- 0
  mm_matrix[is.na(mm_matrix) | mm_matrix < 0] <- 0
  rw_matrix <- wt_matrix * mm_matrix
  colnames(rw_matrix) <- rw_names
  if (any(rowSums(rw_matrix) <= 0)) {
    stop(wave_year, ": ", sum(rowSums(rw_matrix) <= 0),
         " case(s) have zero weight in every replicate", call. = FALSE)
  }
  household_total <- sum(extract$wgt)
  replicate_ratio <- colSums(rw_matrix) / household_total
  if (any(abs(replicate_ratio - 1) > replicate_total_rel_tol)) {
    stop(wave_year, ": a replicate total is more than ",
         100 * replicate_total_rel_tol, "% from the household total",
         call. = FALSE)
  }
  rw_order <- order(rw_key)
  repwgts_list[[wave_label]] <- data.frame(
    year = rep(wave_year, case_count),
    y1 = rw_key[rw_order],
    rw_matrix[rw_order, , drop = FALSE],
    check.names = FALSE
  )

  # 38) Per-wave summary: rows, weighted household total, and the weighted
  #     median of networth (2025 dollars) in implicate 1
  implicate_one_flag <- extract$implicate == 1L
  networth_one <- extract$networth[implicate_one_flag]
  wgt_one <- extract$wgt[implicate_one_flag]
  networth_order <- order(networth_one)
  cumulative_share <- cumsum(wgt_one[networth_order]) / sum(wgt_one)
  median_networth <- networth_one[networth_order][
    which(cumulative_share >= 0.5)[1L]
  ]
  log_step(sprintf(
    paste0("%d: %s rows; weighted households %s; median networth ",
           "(implicate 1, 2025 CPI-U-RS $) %s; replicate totals / ",
           "households %.4f-%.4f; couples with female RP %.4f"),
    wave_year, format(nrow(extract), big.mark = ","),
    format(round(household_total), big.mark = ","),
    format(round(median_networth), big.mark = ","),
    min(replicate_ratio), max(replicate_ratio), female_couple_share
  ))

  panel_list[[wave_label]] <- extract
  unlink(wave_temp_dir, recursive = TRUE)
}

################################################################################
###                       8. Stack the waves and check                       ###
################################################################################

# 39) Stack; a variable absent from a wave's extract is missing there
panel <- data.table::rbindlist(panel_list, use.names = TRUE, fill = TRUE)
repwgts <- data.table::rbindlist(repwgts_list, use.names = TRUE)
rm(panel_list, repwgts_list)

# 40) Year and implicate as integers; drop unused birth bands; set the
#     column order: IDs, extract variables, dollar bases, harmonized columns
data.table::set(panel, j = "year", value = as.integer(panel$year))
data.table::set(panel, j = "birth_band", value = droplevels(panel$birth_band))
data.table::set(repwgts, j = "year", value = as.integer(repwgts$year))
id_columns <- c("year", "yy1", "y1", "implicate", "wgt")
basis_columns <- as.vector(rbind(
  paste0(dollar_vars, "_nom"), paste0(dollar_vars, "_pce25")
))
panel_columns <- c(
  id_columns, setdiff(extract_columns, id_columns), basis_columns,
  harmonized_columns
)
if (!setequal(panel_columns, names(panel))) {
  stop("Stacked panel columns differ from the expected set", call. = FALSE)
}
data.table::setcolorder(panel, panel_columns)

# 41) Re-check the stacked panel: rows per wave, no missing wgt or
#     implicate, no unmapped category
wave_rows <- table(factor(panel$year, levels = wave_years))
if (any(as.vector(wave_rows) != 5L * expected_cases)) {
  stop("Stacked panel rows per wave differ from 5 x the case counts",
       call. = FALSE)
}
if (anyNA(panel$wgt) || anyNA(panel$implicate)) {
  stop("Stacked panel has missing wgt or implicate", call. = FALSE)
}
for (column in harmonized_columns) {
  if (anyNA(panel[[column]])) {
    stop("Stacked panel has unmapped values in ", column, call. = FALSE)
  }
}

# 42) 2025: the nominal basis equals the extract and the PCE stock factor
#     is exactly 1
panel_2025_flag <- panel$year == 2025L
pce_stock_2025 <- wave_factors$pce_stock_factor[wave_factors$year == 2025L]
if (!identical(panel$networth_nom[panel_2025_flag],
               panel$networth[panel_2025_flag]) ||
      pce_stock_2025 != 1 ||
      !identical(panel$networth_pce25[panel_2025_flag],
                 panel$networth[panel_2025_flag])) {
  stop("2025 networth_nom or networth_pce25 differs from networth, or the ",
       "2025 PCE factor is not 1", call. = FALSE)
}

# 43) Replicate weights: one row per implicate-1 record of every wave
implicate_one_keys <- paste(
  panel$year[panel$implicate == 1L], panel$y1[panel$implicate == 1L]
)
repwgts_keys <- paste(repwgts$year, repwgts$y1)
if (anyDuplicated(repwgts_keys) > 0L ||
      length(repwgts_keys) != length(implicate_one_keys) ||
      !setequal(repwgts_keys, implicate_one_keys)) {
  stop("Replicate weights do not join one to one with the panel's ",
       "implicate-1 records on year and y1", call. = FALSE)
}
log_step(paste0(
  "Checks passed: ", format(nrow(panel), big.mark = ","), " panel rows, ",
  ncol(panel), " columns; ", format(nrow(repwgts), big.mark = ","),
  " replicate-weight rows"
))

################################################################################
###                         9. Write the checkpoints                         ###
################################################################################

# 44) Write both Parquet files (overwriting any earlier run) and read the
#     row counts back
arrow::write_parquet(panel, panel_path)
arrow::write_parquet(repwgts, repwgts_path)
panel_rows_written <- nrow(arrow::read_parquet(panel_path,
                                               col_select = "year"))
repwgts_rows_written <- nrow(arrow::read_parquet(repwgts_path,
                                                 col_select = "year"))
if (panel_rows_written != nrow(panel) ||
      repwgts_rows_written != nrow(repwgts)) {
  stop("Row counts read back from the Parquet files differ", call. = FALSE)
}

# 45) Remove the temp directory and report sizes and run time
unlink(build_temp_dir, recursive = TRUE)
elapsed_seconds_num <- round(
  as.numeric(difftime(Sys.time(), run_start, units = "secs")), 1L
)
log_step(paste0(
  "Wrote ", file.path("data", "processed", basename(panel_path)), " (",
  format(file.size(panel_path), big.mark = ","), " bytes) and ",
  file.path("data", "processed", basename(repwgts_path)), " (",
  format(file.size(repwgts_path), big.mark = ","), " bytes) in ",
  elapsed_seconds_num, " seconds"
))
