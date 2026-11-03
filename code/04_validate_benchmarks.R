# 04_validate_benchmarks -- reproduce the Fed's SCF tables by age, 1989-2025
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U5 of records/plans/2026-10-11_stage1-scf-pipeline.md (O4 and the
# 2026-10-11 amendment). For each of the 13 waves and the six age groups of
# the reference person, it compares the mean and median of family income, net
# worth, and debt (for families holding debt) with the Fed's public historical
# tables, twice:
#   (i)  extract values (2025 CPI-U-RS dollars) against the real tables;
#   (ii) the panel's `_nom` values against the nominal tables.
# Tolerance (O4): pass if |ours - published| <= max(half the published
# rounding unit, 0.5 percent of published). Any miss stops the script with an
# error after writing output/tables/validation_benchmarks.csv.
# It then compares 2025 standard errors with the bulletin's
# 2025_tables1_2_stderrs.xlsx and reports differences as warnings only.
# Before any of this, it runs code/tests/test_scf_estimators.R and stops if
# a unit test fails.
# Run from the project root as
#   Rscript code/04_validate_benchmarks.R

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "data.table", "arrow", "readxl", "dplyr")
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

# 2) Stop unless this script sits at code/04_validate_benchmarks.R; then
#    run the estimator unit tests (code/tests/test_scf_estimators.R) in a
#    fresh Rscript process and stop if any check fails, so no benchmark is
#    computed with a broken estimator. The test script exits non-zero at its
#    first failed check; system2() would only warn about that status, so the
#    warning is replaced by the stop() below, which names the failure.
here::i_am("code/04_validate_benchmarks.R")
test_script_path <- here::here("code", "tests", "test_scf_estimators.R")
rscript_path <- file.path(R.home("bin"), "Rscript")
test_output <- suppressWarnings(system2(
  rscript_path, shQuote(test_script_path),
  stdout = TRUE, stderr = TRUE
))
test_status <- attr(test_output, "status")
if (!is.null(test_status) && test_status != 0L) {
  stop("Estimator unit tests failed (code/tests/test_scf_estimators.R): ",
       paste(utils::tail(test_output, 3L), collapse = " | "),
       call. = FALSE)
}
message("Estimator unit tests passed: ",
        sum(grepl("^PASS: ", test_output)), " checks ",
        "(code/tests/test_scf_estimators.R)")
source(here::here("code", "03_scf_estimators.R"))

path_common <- here::here("data", "raw", "scf", "common")
path_processed <- here::here("data", "processed")
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

workbook_paths <- c(
  real = file.path(path_common, "scf2025_tables_public_real_historical.xlsx"),
  nominal = file.path(
    path_common, "scf2025_tables_public_nominal_historical.xlsx"
  )
)
stderr_path <- file.path(path_common, "2025_tables1_2_stderrs.xlsx")
panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
output_path <- file.path(path_tables, "validation_benchmarks.csv")
se_output_path <- file.path(path_tables, "validation_stderrs_2025.csv")

# 3) Tolerances: O4 for point estimates; a looser, warning-only bound for
#    standard errors (ours from public data, the bulletin's from internal
#    data, so they are not expected to agree closely)
tolerance_share <- 0.005
se_tolerance_share <- 0.10

# 4) Table age labels (en dashes normalized to hyphens) and the panel's
#    age_group_fed levels, which follow the macro's AGECL
#    (bulletin.macro.txt line 353)
age_label_map <- c(
  "less than 35" = "less than 35",
  "35-44" = "35-44",
  "45-54" = "45-54",
  "55-64" = "55-64",
  "65-74" = "65-74",
  "75 or more" = "75 and over"
)

# 5) Panel variable for each table variable and basis. Debt is conditional on
#    holding debt: the tables report "value of holdings for families holding
#    debt" (Table 13, "Any debt"), and HDEBT = (DEBT > 0) in
#    bulletin.macro.txt line 2786
panel_variables <- data.table::data.table(
  basis = rep(c("real", "nominal"), each = 3L),
  variable = rep(c("income", "networth", "debt"), times = 2L),
  panel_var = c(
    "income", "networth", "debt", "income_nom", "networth_nom", "debt_nom"
  )
)

run_start <- Sys.time()

################################################################################
###                    3. Define workbook-reading helpers                    ###
################################################################################

# 6) Read one sheet as a character matrix with no header row
read_sheet_text <- function(path, sheet) {
  sheet_data <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet, col_names = FALSE, col_types = "text"
  ))
  sheet_matrix <- as.matrix(sheet_data)
  sheet_matrix[is.na(sheet_matrix)] <- ""
  sheet_matrix
}

# 7) Normalize a label: en dashes to hyphens, squeezed spaces, lower case
normalize_label <- function(text) {
  text <- gsub("–|—", "-", text)
  tolower(trimws(gsub("[[:space:]]+", " ", text)))
}

# 8) Decimal places of a published number, ignoring float noise
count_decimals <- function(value) {
  value_text <- sub("0+$", "", formatC(abs(value), format = "f", digits = 6L))
  nchar(sub("^[0-9]*\\.?", "", value_text))
}

# 9) Find the six age rows below the "Age of reference person" label that
#    follows row `start_row`; return their row numbers named by panel level
find_age_rows <- function(sheet_matrix, start_row, sheet) {
  first_col <- normalize_label(sheet_matrix[, 1L])
  label_rows <- which(grepl("^age of reference person", first_col))
  label_row <- label_rows[label_rows > start_row][1L]
  if (is.na(label_row)) {
    stop("No 'Age of reference person' block in sheet ", sheet, call. = FALSE)
  }
  block_rows <- label_row + seq_len(6L)
  block_labels <- first_col[block_rows]
  if (!setequal(block_labels, names(age_label_map))) {
    stop(
      "Unexpected age labels in sheet ", sheet, ": ",
      paste(block_labels, collapse = "; "),
      call. = FALSE
    )
  }
  stats::setNames(block_rows, age_label_map[block_labels])
}

# 10) Turn published cells into a long table in dollars, with each cell's
#     rounding unit taken from the most decimals in its column of the sheet
#     (values are in thousands; "*" marks ten or fewer observations)
published_cells <- function(sheet_matrix, age_rows, col_index, year_value,
                            statistic, data_rows) {
  column_values <- suppressWarnings(
    as.numeric(sheet_matrix[data_rows, col_index])
  )
  column_decimals <- max(
    count_decimals(column_values[!is.na(column_values)])
  )
  data.table::data.table(
    year = as.integer(year_value),
    age_group_fed = names(age_rows),
    statistic = statistic,
    published = 1000 * suppressWarnings(
      as.numeric(sheet_matrix[age_rows, col_index])
    ),
    rounding_unit = 1000 * 10^(-column_decimals)
  )
}

# 11) Parse a wide table (Table 1 income or Table 4 net worth): years in the
#     "Family characteristic" header row, Median and Mean in a row below it
parse_wide_table <- function(path, sheet, variable) {
  sheet_matrix <- read_sheet_text(path, sheet)
  first_col <- normalize_label(sheet_matrix[, 1L])
  header_row <- which(first_col == "family characteristic")[1L]
  if (is.na(header_row)) {
    stop("No 'Family characteristic' header in sheet ", sheet, call. = FALSE)
  }
  units_row_text <- normalize_label(sheet_matrix[seq_len(header_row), ])
  if (!any(grepl("^thousands of", units_row_text))) {
    stop("Sheet ", sheet, " does not say its units are thousands",
         call. = FALSE)
  }
  year_cells <- trimws(sheet_matrix[header_row, ])
  year_filled <- suppressWarnings(as.integer(year_cells))
  for (col_index in seq_along(year_filled)[-1L]) {
    if (is.na(year_filled[col_index])) {
      year_filled[col_index] <- year_filled[col_index - 1L]
    }
  }
  stat_rows <- header_row + seq_len(3L)
  stat_row <- stat_rows[apply(
    sheet_matrix[stat_rows, , drop = FALSE], 1L,
    function(row_text) any(normalize_label(row_text) == "median")
  )][1L]
  stat_cells <- normalize_label(sheet_matrix[stat_row, ])
  age_rows <- find_age_rows(sheet_matrix, header_row, sheet)
  data_rows <- seq.int(stat_row + 1L, nrow(sheet_matrix))
  stat_cols <- which(stat_cells %in% c("median", "mean") &
                       !is.na(year_filled))
  parsed <- data.table::rbindlist(lapply(stat_cols, function(col_index) {
    published_cells(
      sheet_matrix, age_rows, col_index, year_filled[col_index],
      stat_cells[col_index], data_rows
    )
  }))
  data.table::set(parsed, j = "variable", value = variable)
  data.table::set(parsed, j = "sheet", value = sheet)
  parsed[]
}

# 12) Parse a Table 13 sheet (one wave): the "Any debt" column, from the age
#     block below the "Median value ..." or "Mean value ..." section label
parse_debt_sheet <- function(path, sheet) {
  sheet_matrix <- read_sheet_text(path, sheet)
  all_text <- normalize_label(sheet_matrix)
  dim(all_text) <- dim(sheet_matrix)
  year_hit <- regmatches(
    all_text[, 1L],
    regexpr("^[0-9]{4}(?= survey of consumer finances)", all_text[, 1L],
            perl = TRUE)
  )
  # The sheet name ("Table 13 YY ...") sets the wave. The in-sheet labels are
  # checked against it but cannot override it: in both workbooks the 2013 and
  # 2016 sheets carry a "2019 Survey of Consumer Finances" label in their
  # first block (a copy error; their values differ from the 2019 sheets'),
  # and the benchmark comparison below confirms the wave of every sheet
  sheet_year <- as.integer(sub("^Table 13 ([0-9]{2}) .*$", "\\1", sheet))
  sheet_year <- ifelse(sheet_year >= 89L, 1900L, 2000L) + sheet_year
  if (is.na(sheet_year) || length(year_hit) == 0L) {
    stop("No survey year in the name or labels of sheet ", sheet,
         call. = FALSE)
  }
  if (!all(as.integer(year_hit) == sheet_year)) {
    message(
      "  NOTE: sheet ", sheet, " is labeled ",
      paste(unique(year_hit), collapse = " and "), "; using ", sheet_year,
      " from its name"
    )
  }
  year_hit <- sheet_year
  header_row <- which(all_text[, 1L] == "family characteristic")[1L]
  debt_col <- which(grepl("^any debt$", all_text[header_row, ]))
  if (length(debt_col) != 1L) {
    stop("No single 'Any debt' column in sheet ", sheet, call. = FALSE)
  }
  section_pattern <- paste0(
    "^(median|mean) value of holdings for families holding debt ",
    "\\(thousands of"
  )
  section_hits <- which(
    matrix(grepl(section_pattern, all_text), nrow = nrow(all_text)),
    arr.ind = TRUE
  )
  if (nrow(section_hits) != 1L) {
    stop("Expected one median or mean section in sheet ", sheet,
         call. = FALSE)
  }
  section_row <- section_hits[1L, "row"]
  statistic <- sub(" .*", "", all_text[section_row, section_hits[1L, "col"]])
  age_rows <- find_age_rows(sheet_matrix, section_row, sheet)
  data_rows <- seq.int(section_row + 1L, nrow(sheet_matrix))
  parsed <- published_cells(
    sheet_matrix, age_rows, debt_col, year_hit, statistic, data_rows
  )
  data.table::set(parsed, j = "variable", value = "debt")
  data.table::set(parsed, j = "sheet", value = sheet)
  parsed[]
}

# 13) Read the first cell (the title) of each candidate sheet, so sheets are
#     chosen by their titles, not only their names
sheet_titles <- function(path, sheet_names) {
  vapply(sheet_names, function(sheet) {
    title_cell <- suppressMessages(readxl::read_excel(
      path,
      sheet = sheet, range = readxl::cell_limits(c(1L, 1L), c(1L, 1L)),
      col_names = FALSE, col_types = "text"
    ))
    if (nrow(title_cell) == 0L) "" else normalize_label(title_cell[[1L]][1L])
  }, character(1L))
}

################################################################################
###                       4. Parse the published tables                      ###
################################################################################

# 14) For each workbook: Table 1 (income; two sheets), Table 4 (net worth),
#     and the per-wave Table 13 sheets (debt), excluding the "Alternate 13"
#     sheets, which split debt types but report the same "Any debt" column
published_list <- list()
for (basis in names(workbook_paths)) {
  workbook_path <- workbook_paths[[basis]]
  message("Reading published tables (", basis, "): ", basename(workbook_path))
  all_sheets <- readxl::excel_sheets(workbook_path)
  candidate_sheets <- grep("^Table (1|4|13) ", paste0(all_sheets, " "),
                           value = TRUE)
  candidate_sheets <- trimws(candidate_sheets)
  titles <- sheet_titles(workbook_path, candidate_sheets)
  income_sheets <- candidate_sheets[grepl("^1\\. before-tax family income",
                                          titles)]
  networth_sheets <- candidate_sheets[grepl("^4\\. family net worth", titles)]
  debt_sheets <- candidate_sheets[grepl("^13\\. family holdings of debt",
                                        titles)]
  message(
    "  sheets found: income ", length(income_sheets), ", net worth ",
    length(networth_sheets), ", debt ", length(debt_sheets)
  )
  if (length(income_sheets) != 2L || length(networth_sheets) != 1L ||
        length(debt_sheets) != 26L) {
    stop(
      "Expected 2 income, 1 net worth, and 26 debt sheets in ",
      basename(workbook_path), call. = FALSE
    )
  }
  basis_tables <- c(
    lapply(income_sheets, function(sheet) {
      parse_wide_table(workbook_path, sheet, "income")
    }),
    list(parse_wide_table(workbook_path, networth_sheets, "networth")),
    lapply(debt_sheets, function(sheet) {
      parse_debt_sheet(workbook_path, sheet)
    })
  )
  basis_published <- data.table::rbindlist(basis_tables)
  basis_published[, basis := basis]
  published_list[[basis]] <- basis_published
}
published <- data.table::rbindlist(published_list, use.names = TRUE)

# 15) Expect every basis x year x age group x variable x statistic once
expected_rows <- 2L * 13L * 6L * 3L * 2L
published_count <- nrow(unique(
  published[, list(basis, year, age_group_fed, variable, statistic)]
))
if (published_count != expected_rows || nrow(published) != expected_rows) {
  stop(
    "Parsed ", nrow(published), " published cells (", published_count,
    " unique); expected ", expected_rows,
    call. = FALSE
  )
}
if (anyNA(published$published)) {
  stop("Some published age-group cells are missing or suppressed",
       call. = FALSE)
}
message("Parsed ", nrow(published), " published cells")

################################################################################
###                        5. Estimate from the panel                        ###
################################################################################

# 16) Read only the columns needed. Weight: WGT, the extract weight
#     (= X42001 / 5) for the five pooled implicates (registry entry `scf`)
panel_columns <- c(
  "year", "yy1", "y1", "implicate", "wgt", "age_group_fed",
  unique(panel_variables$panel_var)
)
panel <- data.table::as.data.table(dplyr::collect(dplyr::select(
  arrow::open_dataset(panel_path), dplyr::all_of(panel_columns)
)))
panel[, age_group_fed := as.character(age_group_fed)]
message("Read panel: ", nrow(panel), " rows, ", ncol(panel), " columns")

# 17) Point estimates by year and age group for each basis, variable, and
#     statistic (no replicate weights needed for the point comparison)
estimate_list <- list()
for (row_index in seq_len(nrow(panel_variables))) {
  spec <- panel_variables[row_index]
  holding_condition <- if (spec$variable == "debt") {
    paste0(spec$panel_var, " > 0")
  } else {
    NULL
  }
  for (statistic in c("median", "mean")) {
    estimate_table <- scf_estimate(
      panel, NULL, spec$panel_var,
      stat = statistic, by = "age_group_fed", cond = holding_condition
    )
    estimate_table[, `:=`(
      basis = spec$basis, variable = spec$variable, statistic = statistic
    )]
    estimate_list[[length(estimate_list) + 1L]] <- estimate_table
  }
}
estimates <- data.table::rbindlist(estimate_list, use.names = TRUE)
message("Estimated ", nrow(estimates), " cells")

################################################################################
###                    6. Compare and write the validation table             ###
################################################################################

# 18) Join published and estimated cells one to one; a missing match is an
#     error, not a dropped row
comparison <- merge(
  published,
  estimates[, list(basis, year, age_group_fed, variable, statistic,
                   ours = estimate, n_unweighted)],
  by = c("basis", "year", "age_group_fed", "variable", "statistic"),
  all = TRUE
)
if (nrow(comparison) != expected_rows || anyNA(comparison$ours) ||
      anyNA(comparison$published)) {
  stop("Published and estimated cells do not match one to one",
       call. = FALSE)
}

# 19) Apply the O4 tolerance
comparison[, diff := ours - published]
comparison[, pct_diff := 100 * diff / published]
comparison[, tolerance := pmax(0.5 * rounding_unit,
                               tolerance_share * abs(published))]
comparison[, pass := abs(diff) <= tolerance]
data.table::setnames(comparison, "age_group_fed", "age_group")
data.table::setorder(comparison, basis, variable, statistic, year, age_group)
validation_table <- comparison[, list(
  basis, year, age_group, variable, statistic, published, ours, diff,
  pct_diff, tolerance, pass
)]
data.table::fwrite(validation_table, output_path)
message("Wrote ", nrow(validation_table), " comparisons to ", output_path)

# 20) Report the 2025 under-35 median net worth (the bulletin's lead number)
lead_cell <- validation_table[
  basis == "real" & year == 2025L & age_group == "less than 35" &
    variable == "networth" & statistic == "median"
]
message(
  "2025 under-35 median net worth (2025 dollars): published ",
  lead_cell$published, ", ours ", round(lead_cell$ours, 2L)
)

pass_summary <- validation_table[, list(
  comparisons = .N, passed = sum(pass),
  worst_pct_diff = max(abs(pct_diff))
), by = basis]
for (row_index in seq_len(nrow(pass_summary))) {
  message(
    "Basis ", pass_summary$basis[row_index], ": ",
    pass_summary$passed[row_index], " of ",
    pass_summary$comparisons[row_index], " within tolerance; largest miss ",
    round(pass_summary$worst_pct_diff[row_index], 3L), " percent"
  )
}

################################################################################
###                  7. Compare 2025 standard errors (warnings)              ###
################################################################################

# 21) Parse the bulletin's Tables 1 and 2 (income and net worth, 2022 and
#     2025, thousands of 2025 dollars): the 2025 Median, Mean, and Standard
#     Error columns, located by their header labels
parse_stderr_sheet <- function(sheet, variable) {
  sheet_matrix <- read_sheet_text(stderr_path, sheet)
  first_col <- normalize_label(sheet_matrix[, 1L])
  header_row <- which(first_col == "family characteristic")[1L]
  stat_labels <- normalize_label(sheet_matrix[header_row, ])
  year_labels <- normalize_label(sheet_matrix[header_row + 1L, ])
  for (col_index in seq_along(stat_labels)[-1L]) {
    if (stat_labels[col_index] == "") {
      stat_labels[col_index] <- stat_labels[col_index - 1L]
    }
  }
  age_rows <- find_age_rows(sheet_matrix, header_row, sheet)
  rbindlist_input <- lapply(c("median", "mean"), function(statistic) {
    stat_cols <- which(grepl(paste0("^", statistic, " "), stat_labels))
    value_col <- stat_cols[year_labels[stat_cols] == "2025"]
    se_col <- value_col + 1L
    if (length(value_col) != 1L || year_labels[se_col] != "standard error") {
      stop("Cannot find the 2025 ", statistic, " and its standard error in ",
           sheet, call. = FALSE)
    }
    data.table::data.table(
      age_group_fed = names(age_rows),
      variable = variable,
      statistic = statistic,
      bulletin_estimate = 1000 * as.numeric(sheet_matrix[age_rows, value_col]),
      bulletin_se = 1000 * as.numeric(sheet_matrix[age_rows, se_col])
    )
  })
  data.table::rbindlist(rbindlist_input)
}
stderr_sheets <- readxl::excel_sheets(stderr_path)
stderr_titles <- sheet_titles(stderr_path, stderr_sheets)
income_se_sheet <- stderr_sheets[grepl("median and mean family income",
                                       stderr_titles)]
networth_se_sheet <- stderr_sheets[grepl("median and mean net worth",
                                         stderr_titles)]
if (length(income_se_sheet) != 1L || length(networth_se_sheet) != 1L) {
  stop("Cannot find the income and net worth sheets in ",
       basename(stderr_path), call. = FALSE)
}
bulletin_se <- data.table::rbindlist(list(
  parse_stderr_sheet(income_se_sheet, "income"),
  parse_stderr_sheet(networth_se_sheet, "networth")
))

# 22) Our 2025 estimates with replicate-weight SEs (WT1B x MM from 02), on
#     the real (extract) basis, which is the bulletin's basis
panel_2025 <- panel[year == 2025L]
repwgts_2025 <- data.table::as.data.table(dplyr::collect(dplyr::filter(
  arrow::open_dataset(repwgts_path), year == 2025L
)))
message("Read 2025 replicate weights: ", nrow(repwgts_2025), " cases")
se_list <- list()
for (variable in c("income", "networth")) {
  for (statistic in c("median", "mean")) {
    se_table <- scf_estimate(
      panel_2025, repwgts_2025, variable,
      stat = statistic, by = "age_group_fed"
    )
    se_table[, `:=`(variable = variable, statistic = statistic)]
    se_list[[length(se_list) + 1L]] <- se_table
  }
}
our_se <- data.table::rbindlist(se_list, use.names = TRUE)
se_comparison <- merge(
  bulletin_se,
  our_se[, list(age_group_fed, variable, statistic, ours_estimate = estimate,
                ours_se = se, ours_se_sampling = se_sampling,
                ours_se_imputation = se_imputation)],
  by = c("age_group_fed", "variable", "statistic")
)
se_comparison[, se_ratio := ours_se / bulletin_se]
se_comparison[, within_se_tolerance :=
                abs(se_ratio - 1) <= se_tolerance_share]
data.table::setnames(se_comparison, "age_group_fed", "age_group")
data.table::fwrite(se_comparison, se_output_path)
message("Wrote ", nrow(se_comparison), " SE comparisons to ", se_output_path)
se_misses <- se_comparison[within_se_tolerance == FALSE]
if (nrow(se_misses) > 0L) {
  warning(
    nrow(se_misses), " of ", nrow(se_comparison), " 2025 standard errors ",
    "differ from the bulletin's by more than ", 100 * se_tolerance_share,
    " percent (ratio ours/bulletin): ",
    paste0(
      se_misses$variable, " ", se_misses$statistic, " ", se_misses$age_group,
      " ", round(se_misses$se_ratio, 2L),
      collapse = "; "
    ),
    call. = FALSE
  )
}

################################################################################
###                      8. Fail on any benchmark miss                       ###
################################################################################

# 23) Stop with a non-zero exit when any comparison is outside tolerance
message(
  "Finished in ",
  round(as.numeric(difftime(Sys.time(), run_start, units = "mins")), 2L),
  " minutes"
)
failures <- validation_table[pass == FALSE]
if (nrow(failures) > 0L) {
  stop(
    nrow(failures), " of ", nrow(validation_table), " benchmark ",
    "comparisons are outside tolerance:\n",
    paste0(
      "  ", failures$basis, " ", failures$year, " ", failures$age_group, " ",
      failures$variable, " ", failures$statistic, ": published ",
      failures$published, ", ours ", round(failures$ours, 2L), " (",
      round(failures$pct_diff, 3L), "%)",
      collapse = "\n"
    ),
    call. = FALSE
  )
}
message("All ", nrow(validation_table), " benchmark comparisons passed.")
