# 16_bottom_earners -- bottom earners, wealth groups, and young households
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Workstream W3 of records/plans/2026-10-13_young-bottom-earners-closeout.md
# (decision D4: "bottom earners" are the Fed's income percentile groups,
# below 20 and 20-39.9, with the net worth percentile groups as the wealth
# counterpart). These are diagnostic tables, not a draft. It reads
#   data/processed/scf_panel_1989_2025.parquet       (written by 02)
#   data/processed/scf_repwgts_1989_2025.parquet     (written by 02)
#   the Fed's nominal historical tables, in data/raw/scf/common/
#     (scf2025_tables_public_nominal_historical.xlsx)
# sources the estimator library code/03_scf_estimators.R, and writes, to
# output/tables/:
#   bottom_earners_validation.csv  our nominal medians and means of income
#                                  and net worth by income and net worth
#                                  percentile group (Fed Tables 1 and 4),
#                                  our shares of debtors with payments over
#                                  40 percent of income or 60 days late
#                                  (Table 18), and the bulletin's late and
#                                  payment-burden shares, 2022 and 2025
#                                  (scf26.pdf, Table 5), against the
#                                  published values
#   bottom_earners_by_group.csv    year x grouping x group x outcome x
#                                  statistic, with SEs
#   bottom_earners_changes.csv     the changes 1989-2025, 2019-2022, and
#                                  2022-2025 of every cell, with SEs
#   wealth_shares.csv              shares of total net worth held by the
#                                  bottom 50 percent and top 10 percent of
#                                  the net worth distribution and by
#                                  under-35 households, and the under-35
#                                  share of households, with SEs
#   bottom_earners_age_mix.csv     the age mix of each income and net worth
#                                  group by wave, and age-standardized
#                                  median net worth and income
# Run from the project root as
#   Rscript code/16_bottom_earners.R
# The validation runs first, on point estimates only, and the script stops
# there, after writing the validation table, if any cell misses.
#
# Groupings (column `grouping`):
#   all                   all families
#   income_group          INCCAT, the percentile of INCOME over all families
#                         in the wave (macro %PCTL, bulletin.macro.txt lines
#                         3961-4024; cut points 20, 40, 60, 80, 90)
#   networth_group        NWCAT, the percentile of NETWORTH over all
#                         families (same macro; cut points 25, 50, 75, 90)
#   age_group             AGECL of the reference person (the Fed's groups)
#   under35_income_group  under-35 households (`young`, AGE < 35) by INCCAT,
#                         that is, by the all-family income cutoffs
#
# Outcomes (variables from the summary extract; bulletin.macro.txt lines):
#   networth  NETWORTH = ASSET - DEBT (line 2790), median and mean
#   income    INCOME, prior calendar year (lines 847-913), median and mean
#   hhouses   HHOUSES = (HOUSES ^= 0), owns the primary residence (2361)
#   hretqliq  HRETQLIQ = (RETQLIQ > 0), any retirement account (1789-1815)
#   hstocks   HSTOCKS = (STOCKS > 0), directly held stock (line 1541)
#   hdebt     HDEBT = (DEBT > 0), any debt (line 2786)
#   pir40     PIR40 = (PIRTOTAL > .4), debt payments over 40 percent of
#             income (line 3304); 0 for families without debt
#   late      LATE = (X3004 = 5), any loan payment late or missed in the
#             past year (line 1017); 0 for families without debt
#   late60    LATE60 = (X3005 = 1), behind 60 days or more (line 1019)
# `statistic`: median, mean, share (over all families in the group), or
# share_debtors (pir40 and late60 among families with debt, hdebt = 1, the
# basis of the Fed's Table 18). The panel codes every flag 0/1 (checked in
# step 26).
#
# Caveats that apply to every number written here:
# - Young households EXCLUDE young adults living inside someone else's
#   primary economic unit (PROJECT.md, Additional Context).
# - Dollars are on the project basis `_pce25` (headline PCE price index,
#   2025 dollars; income deflated with the prior-year index), not the
#   Fed's CPI-U-RS real figures; the validation uses nominal dollars.
# - The SCF is a repeated cross-section: a change compares the families in
#   a group in one wave with those in the same group in another wave, not
#   the same families. Changes treat the two waves as independent samples
#   (se of a difference = sqrt(se1^2 + se2^2)), with no covariance term:
#   an assumption, as each wave is a separately drawn cross-section.
#   Income and net worth groups are re-ranked in every wave, so the
#   families in a group change in other ways too, for example in age;
#   bottom_earners_age_mix.csv reports the age mix and age-standardized
#   medians (step 57).
# - `definition_break` names a documented change in an outcome's
#   construction in that wave, from the shared table scf_definition_breaks()
#   in 00_functions.R through an explicit outcome map, plus 16's own pir40
#   rows (step 15; data/docs/scf/scf_bottom_earners_variables.md);
#   `measurement_note` names a documented change around the question that
#   is not a change in its wording or construction (step 16). The 2022-2025
#   rise in late and late60 is an upper bound on the real change.
#
# Method: point estimates come from scf_estimate() in 03, with its default
# pooling = "pooled" (WGT-weighted statistic over the five stacked
# implicates), using the Fed's INCCAT and NWCAT groups, so they reproduce
# the published tables. Standard errors: se^2 = replicate sampling variance
# on implicate 1 + (6/5) x between-implicate variance (03). For the all and
# age_group rows they come from scf_estimate(). For the income_group,
# networth_group, and under35_income_group rows, households are re-ranked
# into the groups by the %PCTL rule inside each of the 999 replicates and
# inside each implicate (steps 37-39), so the SEs include the uncertainty
# of the group cutoffs; `se_fixed_membership` keeps the SE with the groups
# held fixed. small_cell = TRUE when n_unweighted < 100 and tiny_cell = TRUE
# when n_unweighted <= 10 (the conventions of 05). The wealth shares (steps
# 41-44) re-rank households by net worth in the same way.
#
# Changes (steps 50-53): level change with a normal interval and z; percent
# change of dollar rows with a log-ratio interval,
# 100 x (exp(log(m2 / m1) +/- 1.96 x sqrt((se1 / m1)^2 + (se2 / m2)^2)) - 1),
# left empty, with `pct_suppressed_reason`, when the start value is not
# positive, its coefficient of variation exceeds 0.2, or the end value is
# not positive. `q_value` is the Benjamini-Hochberg adjustment of the
# level test's two-sided p-value within each family, a family being one
# period x one grouping (all its groups, outcomes, and statistics).
#
# Exceptions to the EIG R style guide:
# - data.table, not the tidyverse, to match 03's data.table interface.
# - Short helpers (section 4) read the workbook and orchestrate calls to
#   scf_estimate(); weights and variance stay in 03 and in the visible
#   loop of section 8, which writes the re-ranked variance inline.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c("here", "data.table", "arrow", "readxl", "tidyselect")
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
`:=` <- year <- implicate <- wgt <- inccat <- nwcat <- young <- grouping <-
  group <- group_rank <- outcome <- statistic <- estimate <- se <-
  n_unweighted <- small_cell <- tiny_cell <- ci_low <- ci_high <-
  definition_break <- measurement_note <- published <- ours <- diff <-
  pct_diff <- tolerance <- pass <- rounding_unit <- source_table <-
  variable <- income_group <- age_group_fed <- networth_pce25 <- y1 <-
  group_label <- grouping_rank <- outcome_rank <- period <- year_start <-
  year_end <- estimate_start <- estimate_end <- se_start <- se_end <-
  change <- se_change <- pct_change <- se_pct_change <- pct_ci_low <-
  pct_ci_high <- z_change <- significant_flag <- published_text <-
  n_unweighted_start <-
  n_unweighted_end <- measure <- se_sampling <- se_imputation <- units <-
  statistic_rank <- se_note <- target <- note <- spec_statistic <- NULL

################################################################################
###                        2. Set paths and constants                        ###
################################################################################

# 2) Stop unless this script sits at code/16_bottom_earners.R
here::i_am("code/16_bottom_earners.R")
source(here::here("code", "03_scf_estimators.R"))
source(here::here("code", "00_functions.R"))

path_processed <- here::here("data", "processed")
path_common <- here::here("data", "raw", "scf", "common")
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

panel_path <- file.path(path_processed, "scf_panel_1989_2025.parquet")
repwgts_path <- file.path(path_processed, "scf_repwgts_1989_2025.parquet")
workbook_path <- file.path(path_common,
                           "scf2025_tables_public_nominal_historical.xlsx")
validation_path <- file.path(path_tables, "bottom_earners_validation.csv")
by_group_path <- file.path(path_tables, "bottom_earners_by_group.csv")
changes_path <- file.path(path_tables, "bottom_earners_changes.csv")
wealth_shares_path <- file.path(path_tables, "wealth_shares.csv")
age_mix_path <- file.path(path_tables, "bottom_earners_age_mix.csv")

# 3) Validation tolerance, as in 04 (O4 of the Stage 1 plan): pass if
#    |ours - published| <= max(half the published rounding unit, 0.5
#    percent of published)
tolerance_share <- 0.005

# 4) Cell-size flags, as in 05: small_cell below 100 implicate-1 cases,
#    tiny_cell at 10 or fewer (the Fed's "*" rule)
small_cell_threshold <- 100L
tiny_cell_threshold <- 10L

# 5) Normal critical value for the 95% intervals (as in 12)
z_95 <- 1.96

# 6) Runtime guard (performance-cost-governance.md): stop if the replicate
#    loop is projected past 60 minutes after its first wave
max_projected_minutes <- 60

# 7) Periods for the change table: the long run and the last two
#    between-wave changes
change_periods <- data.table::data.table(
  period = c("1989-2025", "2019-2022", "2022-2025"),
  year_start = c(1989L, 2019L, 2022L),
  year_end = c(2025L, 2022L, 2025L)
)

# 8) Wealth-share cut points on the net worth ranking, from the macro's
#    NWCAT rule: a family is in the bottom half when the cumulative WGT
#    share through it (households sorted by NETWORTH) is below 0.5, and in
#    the top tenth when it is at least 0.9 (bulletin.macro.txt lines
#    3990-4003)
bottom_share_cut <- 0.5
top_share_cut <- 0.9

# 9) Cut points of the %PCTL rule for the group SEs (bulletin.macro.txt
#    lines 3961-4024): a family's group is 1 plus the number of cut points
#    at or below the cumulative weight share through it, households sorted
#    by the ranking variable and then Y1 (the macro's BY &VAR &PID). The
#    rule is checked against INCCAT and NWCAT in step 36
rank_spec <- data.table::data.table(
  grouping = c("income_group", "networth_group", "under35_income_group"),
  rank_var = c("income_pce25", "networth_pce25", "income_pce25"),
  young_only = c(FALSE, FALSE, TRUE)
)
rank_cuts <- list(
  income_pce25 = c(0.2, 0.4, 0.6, 0.8, 0.9),
  networth_pce25 = c(0.25, 0.5, 0.75, 0.9)
)

# 10) Percent changes are left empty when the start value's coefficient of
#     variation (se / estimate) exceeds this (methodology review MR-SPEC)
max_start_cv <- 0.2

# 11) Reference waves for the age-standardized medians (step 57): the
#     first wave, and the start waves of the 2019-2022 and 2022-2025
#     changes
age_reference_years <- c(1989L, 2019L, 2022L)

run_start <- Sys.time()

# 12) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%H:%M:%S"), "] ", text)
}

################################################################################
###                      3. Define outcomes and groups                       ###
################################################################################

# 13) Outcomes (definitions in the header). `cond` restricts the rows
#     scf_estimate() uses; share_debtors rows keep families with debt only
outcome_spec <- data.table::data.table(
  outcome = c("networth", "networth", "income", "income", "hhouses",
              "hretqliq", "hstocks", "hdebt", "pir40", "late", "late60",
              "pir40", "late60"),
  statistic = c("median", "mean", "median", "mean", rep("share", 7L),
                "share_debtors", "share_debtors"),
  var = c("networth_pce25", "networth_pce25", "income_pce25",
          "income_pce25", "hhouses", "hretqliq", "hstocks", "hdebt",
          "pir40", "late", "late60", "pir40", "late60"),
  stat = c("median", "mean", "median", "mean", rep("share", 9L)),
  cond = c(rep(NA_character_, 11L), "hdebt == 1", "hdebt == 1"),
  basis = c(rep("pce25", 4L), rep("share", 9L))
)
outcome_spec[, outcome_rank := seq_len(nrow(outcome_spec))]

# 14) Group labels. Income groups use the panel factor `income_group`
#     (INCCAT labels, built in 02); net worth groups follow NWCAT 1-5 in
#     the order of the Fed's tables; age groups use the panel factor
#     `age_group_fed` (AGECL)
networth_labels <- c("Less than 25", "25-49.9", "50-74.9", "75-89.9",
                     "90-100")
grouping_order <- c("all", "income_group", "networth_group", "age_group",
                    "under35_income_group")
all_label <- "All families"

# 15) Definition breaks: the shared table scf_definition_breaks() (00),
#     read at run time, mapped to 16's outcomes by `break_map`, plus two
#     pir40 rows that the shared table has no target for. `statistic` NA in
#     the map means every statistic of the outcome; `years` NA means every
#     year of the target. The map:
#     - income -> income (INCOME; for example 2004, PENACCTWD collected
#       separately)
#     - hretqliq -> retqliq (HRETQLIQ flags RETQLIQ above zero)
#     - hhouses -> homeowner (HOUSECL 1995): HHOUSES equals HOUSECL = 1 in
#       every row of the panel (variable note), so a change in who counts
#       as an owner changes HHOUSES
#     - hdebt, and the share_debtors rows (whose base is the debtors) ->
#       install 2022 only: buy-now-pay-later balances enter DEBT in 2022,
#       which adds a new type of debt and so can change who is a debtor.
#       Other changes inside DEBT's components are not mapped, as totals
#       carry no flag (the convention of 05 and of the shared table)
#     16's own rows (macro lines; no shared target):
#     - pir40 2010: the list of payments in TPAY drops PAYLC3, PAYORE3,
#       PAYPEN3, and PAYPEN6 with the 2010 grid reductions (lines 3240-3258)
#     - pir40 2022: BNPLPAY = 0.33 x BNPL enters TPAY; BNPL is 0 before
#       2022 (lines 2666-2675, 2881, 3248, 3258)
#     Not flagged: networth (a total), hstocks, late, late60 (one formula in
#     every wave; X3004 and X3005 wording identical in the 2019, 2022, and
#     2025 codebooks). The script stops if a mapped target is missing from
#     the shared table
break_map <- data.table::data.table(
  outcome = c("income", "hretqliq", "hhouses", "hdebt", "pir40", "late60"),
  statistic = c(NA_character_, NA_character_, NA_character_, NA_character_,
                "share_debtors", "share_debtors"),
  target = c("income", "retqliq", "homeowner", "install", "install",
             "install"),
  years = list(NA_integer_, NA_integer_, NA_integer_, 2022L, 2022L, 2022L)
)
shared_breaks <- data.table::as.data.table(scf_definition_breaks())
missing_targets <- setdiff(break_map$target, shared_breaks$target)
if (length(missing_targets) > 0L) {
  stop("scf_definition_breaks() has no rows for mapped target(s): ",
       paste(missing_targets, collapse = ", "), call. = FALSE)
}
mapped_parts <- list()
for (map_index in seq_len(nrow(break_map))) {
  target_rows <- shared_breaks[target == break_map$target[map_index]]
  map_years <- break_map$years[[map_index]]
  if (!anyNA(map_years)) {
    target_rows <- target_rows[year %in% map_years]
    if (nrow(target_rows) != length(map_years)) {
      stop("scf_definition_breaks() lacks ", break_map$target[map_index],
           " in ", paste(map_years, collapse = ", "), call. = FALSE)
    }
  }
  mapped_parts[[map_index]] <- data.table::data.table(
    outcome = break_map$outcome[map_index],
    statistic = break_map$statistic[map_index],
    year = target_rows$year,
    note = target_rows$note
  )
}
own_breaks <- data.table::data.table(
  outcome = "pir40", statistic = NA_character_, year = c(2010L, 2022L),
  note = c(
    paste0("pir40 2010: payment items PAYLC3, PAYORE3, PAYPEN3, PAYPEN6 ",
           "dropped (macro lines 3240-3258)"),
    "pir40 2022: BNPL payments (0.33 x BNPL) enter total payments (macro)"
  )
)
# one row per (outcome, statistic, year): statistic NA rows apply to both
# statistics of the outcome
break_rows <- data.table::rbindlist(c(mapped_parts, list(own_breaks)))
break_rows <- merge(
  break_rows,
  outcome_spec[, list(outcome, spec_statistic = statistic)],
  by = "outcome", allow.cartesian = TRUE
)
break_rows <- break_rows[is.na(statistic) | statistic == spec_statistic]
definition_breaks <- break_rows[
  , list(definition_break = paste(unique(note), collapse = "; ")),
  by = list(outcome, statistic = spec_statistic, year)
]

# 16) Documented changes around a question that leave its wording and
#     construction unchanged: in 2025 a new opening "Economic Experiences"
#     module asks whether the family missed a regular payment on rent, a
#     mortgage, an auto, student, or other loan, or a credit card in the
#     past year (X25008-X25014), before the debt section that holds X3004
#     and X3005 (2025_scf_changes.pdf, pages 1-2). The Fed does not flag a
#     comparability break. Asking about missed payments first may raise
#     the reporting of late payments later in the interview, so the
#     2022-2025 rise in late and late60 is read as an upper bound on the
#     real change; no external series was used to bound it (no new data)
measurement_notes <- data.table::data.table(
  outcome = c("late", "late60"),
  year = c(2025L, 2025L),
  measurement_note = paste0(
    "2025: new opening questions on missed payments (X25008-X25014, ",
    "2025_scf_changes.pdf pp. 1-2) may raise reporting; X3004/X3005 ",
    "unchanged; read the 2022-2025 rise as an upper bound (no external ",
    "benchmark used)"
  )
)

# 17) Panel columns this script reads
panel_columns <- c(
  "year", "y1", "yy1", "implicate", "wgt", "inccat", "nwcat",
  "income_group", "age_group_fed", "young", "networth_pce25",
  "income_pce25", "networth_nom", "income_nom", "hhouses", "hretqliq",
  "hstocks", "hdebt", "pir40", "late", "late60"
)
flag_columns <- c("hhouses", "hretqliq", "hstocks", "hdebt", "pir40",
                  "late", "late60")

################################################################################
###                            4. Define helpers                             ###
################################################################################

# 18) Read one workbook sheet as a character matrix with no header row
read_sheet_text <- function(sheet) {
  sheet_data <- suppressMessages(readxl::read_excel(
    workbook_path,
    sheet = sheet, col_names = FALSE, col_types = "text"
  ))
  sheet_matrix <- as.matrix(sheet_data)
  sheet_matrix[is.na(sheet_matrix)] <- ""
  sheet_matrix
}

# 19) Normalize a label: en dashes to hyphens, squeezed spaces, lower case
normalize_label <- function(text) {
  text <- gsub("–|—", "-", text)
  tolower(trimws(gsub("[[:space:]]+", " ", text)))
}

# 20) Decimal places of a published number, ignoring float noise (as 04)
count_decimals <- function(value) {
  value_text <- sub("0+$", "", formatC(abs(value), format = "f", digits = 6L))
  nchar(sub("^[0-9]*\\.?", "", value_text))
}

# 21) Rows of the published groups in a sheet: the "All families" row and
#     the rows below each block label, checked against the expected labels;
#     returns grouping, group (our label), and row number
published_group_rows <- function(sheet_matrix, sheet, blocks) {
  first_col <- normalize_label(sheet_matrix[, 1L])
  all_row <- which(first_col == "all families")[1L]
  if (is.na(all_row)) {
    stop("No 'All families' row in sheet ", sheet, call. = FALSE)
  }
  row_parts <- list(data.table::data.table(
    grouping = "all", group = all_label, row = all_row
  ))
  for (block_index in seq_len(nrow(blocks))) {
    block_row <- which(first_col == blocks$block_label[block_index])[1L]
    labels_expected <- blocks$labels[[block_index]]
    block_rows <- block_row + seq_along(labels_expected)
    if (is.na(block_row) ||
          !identical(first_col[block_rows],
                     normalize_label(blocks$table_labels[[block_index]]))) {
      stop("Unexpected '", blocks$block_label[block_index], "' block in ",
           "sheet ", sheet, ": ", paste(first_col[block_rows], collapse = "; "),
           call. = FALSE)
    }
    row_parts[[length(row_parts) + 1L]] <- data.table::data.table(
      grouping = blocks$grouping[block_index], group = labels_expected,
      row = block_rows
    )
  }
  data.table::rbindlist(row_parts)
}

# 22) Published cells of one column for the given rows, in the units of
#     the sheet times `scale`; the rounding unit comes from the most
#     decimals in the column, as in 04 ("*" marks ten or fewer cases)
published_column <- function(sheet_matrix, group_rows, col_index,
                             first_data_row, scale) {
  column_values <- suppressWarnings(as.numeric(
    sheet_matrix[seq.int(first_data_row, nrow(sheet_matrix)), col_index]
  ))
  column_decimals <- max(count_decimals(column_values[!is.na(column_values)]))
  data.table::data.table(
    grouping = group_rows$grouping,
    group = group_rows$group,
    published = scale * suppressWarnings(
      as.numeric(sheet_matrix[group_rows$row, col_index])
    ),
    published_text = sheet_matrix[group_rows$row, col_index],
    rounding_unit = scale * 10^(-column_decimals)
  )
}

# 23) Run scf_estimate() (03) once per outcome row, label the rows, and
#     collect the estimator's warnings so they are counted, not printed
estimate_outcomes <- function(design, wave_repwgts, spec, by_cols) {
  warning_text <- character(0L)
  estimate_list <- vector("list", nrow(spec))
  for (spec_index in seq_len(nrow(spec))) {
    cond_text <- spec$cond[spec_index]
    estimate_one <- withCallingHandlers(
      scf_estimate( # nolint: object_usage_linter.
        design, wave_repwgts,
        var = spec$var[spec_index], stat = spec$stat[spec_index],
        by = by_cols, cond = if (is.na(cond_text)) NULL else cond_text
      ),
      warning = function(condition) {
        warning_text <<- c(warning_text, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    for (label_col in c("outcome", "statistic", "basis")) {
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

# 24) Stack one copy of the rows per grouping (a copy of the input), so
#     each outcome is one scf_estimate() call grouped by (grouping, group);
#     a household appears once in each grouping it belongs to. The groups
#     are the header's: all families, INCCAT, NWCAT, AGECL, and under-35
#     households by INCCAT
stack_groups <- function(rows) {
  # data.table column names used unquoted (see 03)
  `:=` <- income_group <- nwcat <- age_group_fed <- young <- NULL
  data.table::rbindlist(list(
    data.table::copy(rows)[, `:=`(grouping = "all", group = all_label)][],
    data.table::copy(rows)[, `:=`(grouping = "income_group",
                                  group = as.character(income_group))][],
    data.table::copy(rows)[, `:=`(grouping = "networth_group",
                                  group = networth_labels[nwcat])][],
    data.table::copy(rows)[, `:=`(grouping = "age_group",
                                  group = as.character(age_group_fed))][],
    data.table::copy(rows[young == TRUE])[
      , `:=`(grouping = "under35_income_group",
             group = as.character(income_group))
    ][]
  ), use.names = TRUE)
}

################################################################################
###                    5. Read the panel and check its coding                ###
################################################################################

# 25) Read the needed panel columns. Weight: WGT, the extract weight
#     (= X42001 / 5) for the five pooled implicates (registry entry `scf`)
if (!file.exists(panel_path) || !file.exists(repwgts_path)) {
  stop("Panel or replicate-weight parquet missing in ", path_processed,
       "; run code/02_build_scf_panel.R first", call. = FALSE)
}
panel <- data.table::as.data.table(
  arrow::read_parquet(
    panel_path, col_select = tidyselect::all_of(panel_columns)
  )
)
log_step(paste0("Read ", format(nrow(panel), big.mark = ","),
                " panel rows"))

# 26) Check the coding this script relies on: every flag is 0/1 with no
#     missing value; INCCAT is 1-6 and matches the income_group factor;
#     NWCAT is 1-5; `young` equals the Fed's "less than 35" age group
flag_values <- unique(unlist(panel[, flag_columns, with = FALSE]))
if (anyNA(flag_values) || !all(flag_values %in% c(0, 1))) {
  stop("A holding or payment flag is missing or not 0/1: ",
       paste(sort(flag_values), collapse = ", "), call. = FALSE)
}
if (anyNA(panel$inccat) || !all(panel$inccat %in% 1:6) ||
      !identical(as.integer(panel$income_group), as.integer(panel$inccat))) {
  stop("INCCAT is missing, outside 1-6, or disagrees with income_group",
       call. = FALSE)
}
if (anyNA(panel$nwcat) || !all(panel$nwcat %in% 1:5)) {
  stop("NWCAT is missing or outside 1-5", call. = FALSE)
}
if (!identical(panel$young,
               as.character(panel$age_group_fed) == "less than 35")) {
  stop("`young` disagrees with age_group_fed == 'less than 35'",
       call. = FALSE)
}
income_labels <- levels(panel$income_group)
age_labels <- levels(panel$age_group_fed)

################################################################################
###            6. Validate against the Fed's published tables                ###
################################################################################

# 27) Parse Tables 1 (income; two sheets) and 4 (net worth) of the nominal
#     workbook: years in the "Family characteristic" header row, Median
#     and Mean in a row below it, values in thousands of dollars
blocks_wide <- data.table::data.table(
  grouping = c("income_group", "networth_group"),
  block_label = c("percentile of income", "percentile of net worth"),
  labels = list(income_labels, networth_labels),
  table_labels = list(
    c("Less than 20", "20-39.9", "40-59.9", "60-79.9", "80-89.9", "90-100"),
    networth_labels
  )
)
all_sheets <- readxl::excel_sheets(workbook_path)
wide_sheets <- data.table::data.table(
  sheet = c("Table 1 89-98", "Table 1 01-25", "Table 4"),
  variable = c("income", "income", "networth")
)
if (!all(c(wide_sheets$sheet, "Table 18") %in% all_sheets)) {
  stop("Expected sheets Table 1 89-98, Table 1 01-25, Table 4, and Table 18 ",
       "in ", basename(workbook_path), call. = FALSE)
}
published_parts <- list()
for (sheet_index in seq_len(nrow(wide_sheets))) {
  sheet <- wide_sheets$sheet[sheet_index]
  sheet_matrix <- read_sheet_text(sheet)
  first_col <- normalize_label(sheet_matrix[, 1L])
  header_row <- which(first_col == "family characteristic")[1L]
  units_text <- normalize_label(sheet_matrix[seq_len(header_row), ])
  if (is.na(header_row) || !any(grepl("^thousands of", units_text))) {
    stop("No header row, or units not thousands, in sheet ", sheet,
         call. = FALSE)
  }
  header_cells <- trimws(sheet_matrix[header_row, ])
  year_filled <- suppressWarnings(as.integer(header_cells))
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
  group_rows <- published_group_rows(sheet_matrix, sheet, blocks_wide)
  for (col_index in which(stat_cells %in% c("median", "mean") &
                            !is.na(year_filled))) {
    cells <- published_column(sheet_matrix, group_rows, col_index,
                              stat_row + 1L, 1000)
    cells[, `:=`(
      source_table = paste0("Fed historical tables (nominal), ", sheet),
      year = year_filled[col_index],
      variable = wide_sheets$variable[sheet_index],
      statistic = stat_cells[col_index],
      units = "dollars"
    )]
    published_parts[[length(published_parts) + 1L]] <- cells
  }
}

# 28) Parse Table 18: four blocks of 13 waves (block label in the header
#     row, waves in the row below); keep "Debtors with ratio greater than
#     40 percent" (pir40 among debtors) and "Debtors with any payment past
#     due 60 days or more" (late60 among debtors), in percent, by income
#     group, age, and net worth group
sheet_matrix <- read_sheet_text("Table 18")
first_col <- normalize_label(sheet_matrix[, 1L])
header_row <- which(first_col == "family characteristic")[1L]
block_cells <- normalize_label(sheet_matrix[header_row, ])
for (col_index in seq_along(block_cells)[-1L]) {
  if (block_cells[col_index] == "") {
    block_cells[col_index] <- block_cells[col_index - 1L]
  }
}
year_cells <- suppressWarnings(as.integer(sheet_matrix[header_row + 1L, ]))
age_block_label <- first_col[grepl("^age of reference person", first_col)][1L]
blocks_18 <- data.table::data.table(
  grouping = c("income_group", "age_group", "networth_group"),
  block_label = c("percentile of income", age_block_label,
                  "percentile of net worth"),
  labels = list(income_labels, age_labels, networth_labels),
  table_labels = list(
    c("Less than 20", "20-39.9", "40-59.9", "60-79.9", "80-89.9", "90-100"),
    c("Less than 35", "35-44", "45-54", "55-64", "65-74", "75 or more"),
    networth_labels
  )
)
group_rows_18 <- published_group_rows(sheet_matrix, "Table 18", blocks_18)
table_18_blocks <- c(
  pir40 = "debtors with ratio greater than 40 percent",
  late60 = "debtors with any payment past due 60 days or more"
)
for (outcome_name in names(table_18_blocks)) {
  block_cols <- which(block_cells == table_18_blocks[[outcome_name]] &
                        !is.na(year_cells))
  if (length(block_cols) != 13L) {
    stop("Table 18: expected 13 waves in block '",
         table_18_blocks[[outcome_name]], "', found ", length(block_cols),
         call. = FALSE)
  }
  for (col_index in block_cols) {
    cells <- published_column(sheet_matrix, group_rows_18, col_index,
                              header_row + 2L, 1)
    cells[, `:=`(
      source_table = "Fed historical tables (nominal), Table 18",
      year = year_cells[col_index],
      variable = outcome_name,
      statistic = "share_debtors",
      units = "percent"
    )]
    published_parts[[length(published_parts) + 1L]] <- cells
  }
}

# 29) The bulletin's all-family shares for 2022 and 2025 (scf26.pdf,
#     "Changes in U.S. Family Finances from 2022 to 2025", Table 5, page
#     30: "Late on payments" 12.2 and 19.6; "Late on payments by 2 months
#     or more" 4.9 and 8.2; "Fraction with payment to income ratio greater
#     than 40 percent" 6.5 and 8.6), printed to one decimal
published_parts[[length(published_parts) + 1L]] <- data.table::data.table(
  grouping = "all", group = all_label,
  published = c(12.2, 19.6, 4.9, 8.2, 6.5, 8.6),
  published_text = c("12.2", "19.6", "4.9", "8.2", "6.5", "8.6"),
  rounding_unit = 0.1,
  source_table = "Fed bulletin scf26.pdf, Table 5",
  year = rep(c(2022L, 2025L), times = 3L),
  variable = rep(c("late", "late60", "pir40"), each = 2L),
  statistic = "share",
  units = "percent"
)
published_cells <- data.table::rbindlist(published_parts, use.names = TRUE)
expected_cells <- 2L * 13L * 12L * 2L + 2L * 13L * 18L + 6L
if (nrow(published_cells) != expected_cells) {
  stop("Parsed ", nrow(published_cells), " published cells; expected ",
       expected_cells, call. = FALSE)
}

# 30) Cells printed as a marker, not a number ("*" for ten or fewer cases;
#     Table 4 prints a dagger, which the workbook does not define, for the
#     2010 and 2013 median net worth of the bottom net worth quarter) are
#     not compared; each is named in the log
marker_cells <- published_cells[is.na(published)]
for (row_index in seq_len(nrow(marker_cells))) {
  log_step(paste0(
    "Not compared (published '", marker_cells$published_text[row_index],
    "'): ", marker_cells$source_table[row_index], " ",
    marker_cells$year[row_index], " ", marker_cells$group[row_index], " ",
    marker_cells$variable[row_index], " ", marker_cells$statistic[row_index]
  ))
}
published_cells <- published_cells[!is.na(published)]
published_cells[, published_text := NULL]
log_step(paste0("Parsed ", nrow(published_cells) + nrow(marker_cells),
                " published cells; ", nrow(published_cells), " numeric"))

# 31) Our point estimates (no replicate weights needed): nominal medians
#     and means by group for Tables 1 and 4, and the debtor and all-family
#     shares for Table 18 and the bulletin, in percent
point_design <- stack_groups(panel)
point_design <- point_design[grouping != "under35_income_group"]
point_spec <- data.table::data.table(
  outcome = c("income", "income", "networth", "networth", "pir40",
              "late60", "late", "late60", "pir40"),
  statistic = c("median", "mean", "median", "mean", "share_debtors",
                "share_debtors", "share", "share", "share"),
  var = c("income_nom", "income_nom", "networth_nom", "networth_nom",
          "pir40", "late60", "late", "late60", "pir40"),
  stat = c("median", "mean", "median", "mean", rep("share", 5L)),
  cond = c(rep(NA_character_, 4L), "hdebt == 1", "hdebt == 1",
           rep(NA_character_, 3L)),
  basis = c(rep("nominal", 4L), rep("share", 5L))
)
point_estimates <- estimate_outcomes(point_design, NULL, point_spec,
                                     c("grouping", "group"))
point_estimates[basis == "share", estimate := 100 * estimate]
rm(point_design)

# 32) Join one to one (a published cell without an estimate is an error)
#     and apply the tolerance of 04
validation <- merge(
  published_cells,
  point_estimates[, list(year, grouping, group, variable = outcome,
                         statistic, ours = estimate, n_unweighted)],
  by = c("year", "grouping", "group", "variable", "statistic"),
  all.x = TRUE, sort = FALSE
)
if (nrow(validation) != nrow(published_cells) || anyNA(validation$ours)) {
  stop("Some published cells have no matching estimate", call. = FALSE)
}
validation[, diff := ours - published]
validation[, pct_diff := 100 * diff / published]
validation[, tolerance := pmax(0.5 * rounding_unit,
                               tolerance_share * abs(published))]
validation[, pass := abs(diff) <= tolerance]
validation[, grouping_rank := match(grouping, grouping_order)]
validation[, group_rank := data.table::fcase(
  grouping == "income_group", match(group, income_labels),
  grouping == "networth_group", match(group, networth_labels),
  grouping == "age_group", match(group, age_labels),
  default = 0L
)]
data.table::setorderv(validation, c("source_table", "variable", "statistic",
                                    "year", "grouping_rank", "group_rank"))
validation <- validation[, list(
  source_table, year, grouping, group, variable, statistic, units,
  published, ours, diff, pct_diff, tolerance, pass, n_unweighted
)]
data.table::fwrite(validation, validation_path)
log_step(paste0("Wrote ", nrow(validation), " comparisons to ",
                validation_path))

# 33) Stop on any miss, naming each one
failures <- validation[pass == FALSE]
if (nrow(failures) > 0L) {
  stop(
    nrow(failures), " of ", nrow(validation), " published comparisons are ",
    "outside tolerance:\n",
    paste0("  ", failures$source_table, " ", failures$year, " ",
           failures$grouping, " ", failures$group, " ", failures$variable,
           " ", failures$statistic, ": published ", failures$published,
           ", ours ", signif(failures$ours, 6L), collapse = "\n"),
    call. = FALSE
  )
}
log_step(paste0("All ", nrow(validation), " published comparisons pass; ",
                "largest |pct_diff| ",
                signif(max(abs(validation$pct_diff)), 3L), " percent"))

################################################################################
###                7. Estimate every group and wave, with SEs                ###
################################################################################

repwgts <- data.table::as.data.table(arrow::read_parquet(repwgts_path))
log_step(paste0("Read ", format(nrow(repwgts), big.mark = ","),
                " replicate-weight rows"))
rw_cols <- grep("^rw_[0-9]+$", names(repwgts), value = TRUE)
rw_cols <- rw_cols[order(as.integer(sub("^rw_", "", rw_cols)))]
wave_years <- sort(unique(panel$year))
group_parts <- list()
share_parts <- list()
rerank_parts <- list()
age_share_parts <- list()
loop_start <- Sys.time()

# Outcome variables have no missing value (the re-ranked SEs below do not
# drop rows the way scf_estimate() does)
if (anyNA(panel[, unique(outcome_spec$var), with = FALSE])) {
  stop("An outcome variable has missing values", call. = FALSE)
}

# Age-group indicators for the age mix of each group (step 35)
age_spec <- data.table::data.table(
  outcome = age_labels, statistic = "share",
  var = paste0("age_flag_", seq_along(age_labels)), stat = "share",
  cond = NA_character_, basis = "share"
)
for (age_index in seq_along(age_labels)) {
  data.table::set(panel, j = age_spec$var[age_index],
                  value = as.numeric(as.character(panel$age_group_fed) ==
                                       age_labels[age_index]))
}

# Mean and share outcomes (columns of a value matrix) and medians, for the
# re-ranked SEs; a share_debtors value is the flag times hdebt, over the
# debtors' weight
mean_spec <- outcome_spec[stat != "median"]
median_spec <- outcome_spec[stat == "median"]
debtor_flag <- !is.na(mean_spec$cond)

for (wave_year in wave_years) {
  wave_start <- Sys.time()
  wave_panel <- panel[year == wave_year]
  wave_repwgts <- repwgts[year == wave_year]

  # 34) Group statistics: point estimates on the Fed's groups, with SEs
  #     that hold group membership fixed (scf_estimate(), one call per
  #     outcome over the stacked groupings of this wave)
  wave_label <- as.character(wave_year)
  wave_design <- stack_groups(wave_panel)
  group_parts[[wave_label]] <- estimate_outcomes(
    wave_design, wave_repwgts, outcome_spec, c("grouping", "group")
  )

  # 35) Age mix of the income and net worth groups: the share of each AGECL
  #     group among the group's households (SEs hold membership fixed)
  age_share_parts[[wave_label]] <- estimate_outcomes(
    wave_design[grouping %in% c("income_group", "networth_group")],
    wave_repwgts, age_spec, c("grouping", "group")
  )
  rm(wave_design)

  # 36) Check the %PCTL rule (step 9): re-ranking the pooled implicates
  #     must reproduce INCCAT and NWCAT for (almost) every row; ties at a
  #     cutoff are the only expected difference
  for (rank_var in names(rank_cuts)) {
    rank_order <- order(wave_panel[[rank_var]], wave_panel$y1)
    cum_share <- numeric(nrow(wave_panel))
    cum_share[rank_order] <- cumsum(wave_panel$wgt[rank_order]) /
      sum(wave_panel$wgt)
    rule_group <- 1L + rowSums(outer(cum_share, rank_cuts[[rank_var]], ">="))
    fed_group <- if (rank_var == "income_pce25") {
      wave_panel$inccat
    } else {
      wave_panel$nwcat
    }
    mismatch_count <- sum(rule_group != fed_group)
    if (mismatch_count > 0.001 * nrow(wave_panel)) {
      stop(wave_year, ": the %PCTL rule misplaces ", mismatch_count,
           " rows against the Fed's group of ", rank_var, call. = FALSE)
    }
    if (mismatch_count > 0L) {
      log_step(paste0(wave_year, ": %PCTL rule differs from the Fed's group ",
                      "for ", mismatch_count, " row(s) of ", rank_var))
    }
  }

  # 37) Re-ranked imputation variance: within each implicate, rank
  #     households by the %PCTL rule with WGT, then compute every statistic
  #     of every group; the variance is taken over the five implicates
  imputation_parts <- list()
  for (implicate_number in 1:5) {
    imp_rows <- wave_panel[implicate == implicate_number]
    for (rank_index in seq_len(nrow(rank_spec))) {
      rank_var <- rank_spec$rank_var[rank_index]
      group_labels <- if (rank_var == "income_pce25") {
        income_labels
      } else {
        networth_labels
      }
      rank_order <- order(imp_rows[[rank_var]], imp_rows$y1)
      cum_share <- numeric(nrow(imp_rows))
      cum_share[rank_order] <- cumsum(imp_rows$wgt[rank_order]) /
        sum(imp_rows$wgt)
      rule_group <- 1L + rowSums(outer(cum_share, rank_cuts[[rank_var]],
                                       ">="))
      member_base <- if (rank_spec$young_only[rank_index]) {
        imp_rows$young
      } else {
        rep(TRUE, nrow(imp_rows))
      }
      for (group_index in seq_along(group_labels)) {
        member <- member_base & rule_group == group_index
        estimate_imp <- rep(NA_real_, nrow(outcome_spec))
        for (spec_index in seq_len(nrow(outcome_spec))) {
          keep <- member
          if (!is.na(outcome_spec$cond[spec_index])) {
            keep <- keep & imp_rows$hdebt == 1
          }
          value <- imp_rows[[outcome_spec$var[spec_index]]][keep]
          weight <- imp_rows$wgt[keep]
          if (length(value) == 0L) {
            next
          }
          if (outcome_spec$stat[spec_index] == "median") {
            value_order <- order(value)
            estimate_imp[spec_index] <- scf_sorted_median(
              value[value_order], weight[value_order]
            )
          } else {
            estimate_imp[spec_index] <- sum(value * weight) / sum(weight)
          }
        }
        imputation_parts[[length(imputation_parts) + 1L]] <-
          data.table::data.table(
            grouping = rank_spec$grouping[rank_index],
            group = group_labels[group_index],
            outcome = outcome_spec$outcome,
            statistic = outcome_spec$statistic,
            implicate = implicate_number,
            estimate_imp = estimate_imp
          )
      }
    }
  }
  imputation_rerank <- data.table::rbindlist(imputation_parts)[
    , list(imputation_var = stats::var(estimate_imp, na.rm = TRUE)),
    by = list(grouping, group, outcome, statistic)
  ]

  # 38) Re-ranked sampling variance: on implicate 1, re-rank households by
  #     the %PCTL rule under each of the 999 replicate weights (missing or
  #     negative weights set to 0, as in 03), so each replicate has its own
  #     group membership, then compute every statistic with that
  #     replicate's weights
  imp1_rows <- wave_panel[implicate == 1L]
  match_index <- wave_repwgts[imp1_rows, on = c("year", "y1"), which = TRUE,
                              mult = "first"]
  if (anyNA(match_index)) {
    stop(wave_year, ": implicate-1 rows without replicate weights",
         call. = FALSE)
  }
  weight_matrix <- as.matrix(wave_repwgts[match_index, rw_cols,
                                          with = FALSE])
  weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0
  case_count <- nrow(weight_matrix)
  replicate_total <- colSums(weight_matrix)
  value_matrix <- as.matrix(imp1_rows[, mean_spec$var, with = FALSE])
  value_matrix[, debtor_flag] <- value_matrix[, debtor_flag] *
    imp1_rows$hdebt
  sampling_parts <- list()
  for (rank_index in seq_len(nrow(rank_spec))) {
    rank_var <- rank_spec$rank_var[rank_index]
    group_labels <- if (rank_var == "income_pce25") {
      income_labels
    } else {
      networth_labels
    }
    rank_order <- order(imp1_rows[[rank_var]], imp1_rows$y1)
    cum_sorted <- apply(weight_matrix[rank_order, , drop = FALSE], 2L,
                        cumsum) / rep(replicate_total, each = case_count)
    group_sorted <- matrix(1L, nrow = case_count, ncol = ncol(cum_sorted))
    for (cut_point in rank_cuts[[rank_var]]) {
      group_sorted <- group_sorted + (cum_sorted >= cut_point)
    }
    group_matrix <- group_sorted
    group_matrix[rank_order, ] <- group_sorted
    rm(cum_sorted, group_sorted)
    if (rank_spec$young_only[rank_index]) {
      group_matrix[!imp1_rows$young, ] <- 0L
    }
    for (group_index in seq_along(group_labels)) {
      member_weight <- weight_matrix * (group_matrix == group_index)
      base_total <- colSums(member_weight)
      debtor_total <- drop(crossprod(imp1_rows$hdebt, member_weight))
      replicate_mean <- crossprod(value_matrix, member_weight) /
        rbind(base_total, debtor_total)[1L + debtor_flag, , drop = FALSE]
      replicate_mean[!is.finite(replicate_mean)] <- NA_real_
      replicate_median <- matrix(NA_real_, nrow = nrow(median_spec),
                                 ncol = ncol(member_weight))
      for (median_index in seq_len(nrow(median_spec))) {
        value <- imp1_rows[[median_spec$var[median_index]]]
        value_order <- order(value)
        replicate_median[median_index, ] <- scf_replicate_medians(
          value[value_order], member_weight[value_order, , drop = FALSE]
        )
      }
      sampling_parts[[length(sampling_parts) + 1L]] <- data.table::data.table(
        grouping = rank_spec$grouping[rank_index],
        group = group_labels[group_index],
        outcome = c(mean_spec$outcome, median_spec$outcome),
        statistic = c(mean_spec$statistic, median_spec$statistic),
        sampling_var = c(
          apply(replicate_mean, 1L, stats::var, na.rm = TRUE),
          apply(replicate_median, 1L, stats::var, na.rm = TRUE)
        ),
        n_replicates_empty = c(
          ifelse(debtor_flag, sum(debtor_total <= 0), sum(base_total <= 0)),
          rep(sum(base_total <= 0), nrow(median_spec))
        )
      )
      rm(member_weight)
    }
    rm(group_matrix)
  }
  rm(weight_matrix, value_matrix)

  # 39) Collect the re-ranked variance parts; step 47 combines them as in
  #     03, se = sqrt(sampling + (6/5) x imputation)
  rerank_wave <- merge(
    data.table::rbindlist(sampling_parts), imputation_rerank,
    by = c("grouping", "group", "outcome", "statistic")
  )
  rerank_wave[, year := wave_year]
  rerank_parts[[wave_label]] <- rerank_wave

  # 40) Under-35 share of households (scf_estimate(), share of `young`)
  household_share <- estimate_outcomes(
    wave_panel, wave_repwgts,
    data.table::data.table(outcome = "household_share_under35",
                           statistic = "share", var = "young",
                           stat = "share", cond = NA_character_,
                           basis = "share"),
    NULL
  )

  # 41) Wealth shares, point estimates: the share of the wave's total net
  #     worth (WGT-weighted, five pooled implicates) held by families in
  #     the bottom half (NWCAT 1-2) and top tenth (NWCAT 5) of the net
  #     worth distribution and by under-35 households. The ratio does not
  #     depend on the deflator, so the pce25 values give the nominal share
  weighted_networth <- wave_panel$networth_pce25 * wave_panel$wgt
  total_networth <- sum(weighted_networth)
  point_share <- c(
    networth_share_bottom50 =
      sum(weighted_networth[wave_panel$nwcat <= 2]) / total_networth,
    networth_share_top10 =
      sum(weighted_networth[wave_panel$nwcat == 5]) / total_networth,
    networth_share_under35 =
      sum(weighted_networth[wave_panel$young]) / total_networth
  )

  # 42) Check that re-ranking the pooled implicates by the NWCAT rule
  #     (step 8) reproduces the Fed's NWCAT shares, so the re-ranked
  #     variance draws below estimate the same statistic
  pooled_sorted <- data.table::copy(wave_panel)
  data.table::setorderv(pooled_sorted, c("networth_pce25", "y1"))
  pooled_cum <- cumsum(pooled_sorted$wgt) / sum(pooled_sorted$wgt)
  pooled_weighted <- pooled_sorted$networth_pce25 * pooled_sorted$wgt
  rerank_gap <- abs(c(
    sum(pooled_weighted[pooled_cum < bottom_share_cut]) / total_networth -
      point_share[["networth_share_bottom50"]],
    sum(pooled_weighted[pooled_cum >= top_share_cut]) / total_networth -
      point_share[["networth_share_top10"]]
  ))
  if (max(rerank_gap) > 0.0005) {
    stop(wave_year, ": re-ranked net worth shares differ from the NWCAT ",
         "shares by up to ", signif(max(rerank_gap), 3L), call. = FALSE)
  }

  # 43) Imputation variance: re-rank within each implicate (WGT), compute
  #     the three shares, and take their variance across the five
  implicate_share <- matrix(NA_real_, nrow = 5L, ncol = 3L)
  for (implicate_number in 1:5) {
    imp_rows <- wave_panel[implicate == implicate_number]
    data.table::setorderv(imp_rows, c("networth_pce25", "y1"))
    imp_cum <- cumsum(imp_rows$wgt) / sum(imp_rows$wgt)
    imp_weighted <- imp_rows$networth_pce25 * imp_rows$wgt
    implicate_share[implicate_number, ] <- c(
      sum(imp_weighted[imp_cum < bottom_share_cut]),
      sum(imp_weighted[imp_cum >= top_share_cut]),
      sum(imp_weighted[imp_rows$young])
    ) / sum(imp_weighted)
  }
  imputation_var <- apply(implicate_share, 2L, stats::var)

  # 44) Sampling variance: implicate 1 with each of the 999 replicate
  #     weights (missing or negative weights set to 0, as in 03); re-rank
  #     in every replicate, then the variance of the 999 shares
  imp1_rows <- wave_panel[implicate == 1L]
  data.table::setorderv(imp1_rows, c("networth_pce25", "y1"))
  match_index <- wave_repwgts[imp1_rows, on = c("year", "y1"), which = TRUE,
                              mult = "first"]
  if (anyNA(match_index)) {
    stop(wave_year, ": implicate-1 rows without replicate weights",
         call. = FALSE)
  }
  weight_matrix <- as.matrix(wave_repwgts[match_index, rw_cols,
                                          with = FALSE])
  weight_matrix[is.na(weight_matrix) | weight_matrix < 0] <- 0
  case_count <- nrow(weight_matrix)
  cum_share_matrix <- apply(weight_matrix, 2L, cumsum) /
    rep(colSums(weight_matrix), each = case_count)
  networth_matrix <- imp1_rows$networth_pce25 * weight_matrix
  replicate_total <- colSums(networth_matrix)
  replicate_share <- cbind(
    colSums(networth_matrix * (cum_share_matrix < bottom_share_cut)),
    colSums(networth_matrix * (cum_share_matrix >= top_share_cut)),
    colSums(networth_matrix[imp1_rows$young, , drop = FALSE])
  ) / replicate_total
  sampling_var <- apply(replicate_share, 2L, stats::var)
  rm(weight_matrix, cum_share_matrix, networth_matrix)

  share_parts[[as.character(wave_year)]] <- data.table::rbindlist(list(
    data.table::data.table(
      year = wave_year, measure = names(point_share),
      estimate = unname(point_share),
      se = sqrt(sampling_var + (6 / 5) * imputation_var),
      se_sampling = sqrt(sampling_var),
      se_imputation = sqrt(imputation_var),
      n_unweighted = c(sum(imp1_rows$nwcat <= 2), sum(imp1_rows$nwcat == 5),
                       sum(imp1_rows$young))
    ),
    household_share[, list(year, measure = outcome, estimate, se,
                           se_sampling, se_imputation, n_unweighted)]
  ), use.names = TRUE)

  # 45) Log the wave and, after the first, the projected loop time
  wave_minutes <- as.numeric(difftime(Sys.time(), wave_start,
                                      units = "mins"))
  log_step(paste0(wave_year, ": ",
                  nrow(group_parts[[as.character(wave_year)]]),
                  " group cells and 4 wealth-share cells in ",
                  round(wave_minutes, 2L), " min"))
  if (wave_year == wave_years[1L] &&
        wave_minutes * length(wave_years) > max_projected_minutes) {
    stop("Projected replicate loop of ",
         round(wave_minutes * length(wave_years)), " minutes exceeds ",
         max_projected_minutes, call. = FALSE)
  }
}
rm(repwgts)
log_step(paste0("Replicate loop finished in ",
                round(as.numeric(difftime(Sys.time(), loop_start,
                                          units = "mins")), 2L),
                " min"))

################################################################################
###                   8. Assemble the by-group table                         ###
################################################################################

# 46) Stack the waves; every cell must have a standard error
by_group <- data.table::rbindlist(group_parts, use.names = TRUE)
if (anyNA(by_group$se)) {
  stop(sum(is.na(by_group$se)), " group cell(s) have no standard error",
       call. = FALSE)
}

# 47) Standard errors: keep scf_estimate()'s fixed-membership SE as
#     se_fixed_membership on every row, and replace the SE of the
#     income_group, networth_group, and under35_income_group rows with the
#     re-ranked SE (steps 37-39); a re-ranked SE must exist for each of them
rerank_se <- data.table::rbindlist(rerank_parts, use.names = TRUE)
rerank_se[, se_sampling_rerank := sqrt(sampling_var)]
rerank_se[, se_imputation_rerank := sqrt(imputation_var)]
rerank_se[, se_rerank := sqrt(sampling_var + (6 / 5) * imputation_var)]
by_group[, se_fixed_membership := se]
rows_before <- nrow(by_group)
by_group <- merge(
  by_group,
  rerank_se[, list(year, grouping, group, outcome, statistic, se_rerank,
                   se_sampling_rerank, se_imputation_rerank,
                   n_replicates_empty_rerank = n_replicates_empty)],
  by = c("year", "grouping", "group", "outcome", "statistic"),
  all.x = TRUE, sort = FALSE
)
reranked_flag <- by_group$grouping %in% rank_spec$grouping
if (nrow(by_group) != rows_before ||
      anyNA(by_group$se_rerank[reranked_flag])) {
  stop("Re-ranked SEs do not match the group cells one to one",
       call. = FALSE)
}
by_group[reranked_flag, `:=`(
  se = se_rerank, se_sampling = se_sampling_rerank,
  se_imputation = se_imputation_rerank,
  n_replicates_empty = n_replicates_empty_rerank
)]
by_group[, se_method := data.table::fifelse(
  reranked_flag, "groups re-ranked in every replicate and implicate",
  "scf_estimate() (03); groups fixed by definition"
)]
se_ratio <- by_group[reranked_flag & year %in% c(2022L, 2025L) &
                       se_fixed_membership > 0,
                     list(ratio = se / se_fixed_membership),
                     by = list(grouping, statistic)][
  , list(median_ratio = stats::median(ratio), min_ratio = min(ratio),
         max_ratio = max(ratio)), by = list(grouping, statistic)
]
for (row_index in seq_len(nrow(se_ratio))) {
  log_step(paste0(
    "SE re-ranked / fixed, 2022 and 2025, ", se_ratio$grouping[row_index],
    " ", se_ratio$statistic[row_index], ": median ",
    round(se_ratio$median_ratio[row_index], 2L), " (",
    round(se_ratio$min_ratio[row_index], 2L), " to ",
    round(se_ratio$max_ratio[row_index], 2L), ")"
  ))
}

# 48) Intervals, cell flags, definition breaks, and measurement notes
by_group[, ci_low := estimate - z_95 * se]
by_group[, ci_high := estimate + z_95 * se]
by_group[, small_cell := n_unweighted < small_cell_threshold]
by_group[, tiny_cell := n_unweighted <= tiny_cell_threshold]
by_group <- merge(by_group, definition_breaks,
                  by = c("outcome", "statistic", "year"),
                  all.x = TRUE, sort = FALSE)
by_group <- merge(by_group, measurement_notes, by = c("outcome", "year"),
                  all.x = TRUE, sort = FALSE)
if (nrow(by_group) != rows_before) {
  stop("Joining breaks or notes changed the number of group cells",
       call. = FALSE)
}

# 49) Row order: grouping, group (table order), outcome spec, year
by_group[, grouping_rank := match(grouping, grouping_order)]
by_group[, group_rank := data.table::fcase(
  grouping %in% c("income_group", "under35_income_group"),
  match(group, income_labels),
  grouping == "networth_group", match(group, networth_labels),
  grouping == "age_group", match(group, age_labels),
  default = 1L
)]
by_group <- merge(by_group,
                  outcome_spec[, list(outcome, statistic, outcome_rank)],
                  by = c("outcome", "statistic"), sort = FALSE)
if (nrow(by_group) != rows_before || anyNA(by_group$group_rank) ||
      anyNA(by_group$grouping_rank)) {
  stop("A group label is not in the expected lists", call. = FALSE)
}
data.table::setorderv(by_group, c("grouping_rank", "group_rank",
                                  "outcome_rank", "year"))
by_group_columns <- c(
  "year", "grouping", "group", "group_rank", "outcome", "statistic",
  "basis", "estimate", "se", "se_sampling", "se_imputation", "ci_low",
  "ci_high", "n_unweighted", "n_weighted", "n_replicates_empty",
  "small_cell", "tiny_cell", "definition_break", "measurement_note",
  "se_fixed_membership", "se_method"
)
by_group_out <- by_group[, by_group_columns, with = FALSE]
data.table::fwrite(by_group_out, by_group_path)
log_step(paste0("Wrote ", nrow(by_group_out), " rows to ", by_group_path))

################################################################################
###                        9. Changes between waves                          ###
################################################################################

# 50) For each period, join the start and end waves cell by cell; the
#     breaks and notes are those of any wave after the start, up to the end
cell_keys <- c("grouping", "group", "group_rank", "outcome", "statistic",
               "basis", "outcome_rank", "grouping_rank")
change_parts <- list()
for (period_index in seq_len(nrow(change_periods))) {
  first_year <- change_periods$year_start[period_index]
  last_year <- change_periods$year_end[period_index]
  start_cells <- by_group[year == first_year, c(
    cell_keys, "estimate", "se", "n_unweighted"
  ), with = FALSE]
  end_cells <- by_group[year == last_year, c(
    cell_keys, "estimate", "se", "n_unweighted"
  ), with = FALSE]
  data.table::setnames(start_cells, c("estimate", "se", "n_unweighted"),
                       c("estimate_start", "se_start", "n_unweighted_start"))
  data.table::setnames(end_cells, c("estimate", "se", "n_unweighted"),
                       c("estimate_end", "se_end", "n_unweighted_end"))
  period_cells <- merge(start_cells, end_cells, by = cell_keys)
  if (nrow(period_cells) != nrow(start_cells) ||
        nrow(period_cells) != nrow(end_cells)) {
    stop("Cells of ", first_year, " and ", last_year, " do not match one ",
         "to one", call. = FALSE)
  }
  period_breaks <- definition_breaks[
    year > first_year & year <= last_year,
    list(definition_break = paste(definition_break, collapse = "; ")),
    by = list(outcome, statistic)
  ]
  period_notes <- measurement_notes[
    year > first_year & year <= last_year,
    list(measurement_note = paste(measurement_note, collapse = "; ")),
    by = outcome
  ]
  period_cells <- merge(period_cells, period_breaks,
                        by = c("outcome", "statistic"), all.x = TRUE,
                        sort = FALSE)
  period_cells <- merge(period_cells, period_notes, by = "outcome",
                        all.x = TRUE, sort = FALSE)
  period_cells[, `:=`(period = change_periods$period[period_index],
                      year_start = first_year, year_end = last_year)]
  change_parts[[period_index]] <- period_cells
}
changes <- data.table::rbindlist(change_parts, use.names = TRUE, fill = TRUE)

# 51) Level change: se = sqrt(se1^2 + se2^2) (independent samples), a
#     normal 95% interval, z, and significant_flag (the level test)
changes[, change := estimate_end - estimate_start]
changes[, se_change := sqrt(se_start^2 + se_end^2)]
changes[, ci_low := change - z_95 * se_change]
changes[, ci_high := change + z_95 * se_change]
changes[, z_change := data.table::fifelse(se_change > 0,
                                          change / se_change, NA_real_)]
changes[, significant_flag := ci_low > 0 | ci_high < 0]

# 52) Percent change of dollar rows on the log-ratio scale (methodology
#     review MR-INFERENCE): log(m2 / m1) has se = sqrt((se1 / m1)^2 +
#     (se2 / m2)^2), and the interval is 100 x (exp(log r +/- 1.96 se) - 1).
#     Left empty, with the reason, when the start value is not positive,
#     its coefficient of variation exceeds max_start_cv (MR-SPEC), or the
#     end value is not positive; share rows have no percent change
changes[, pct_suppressed_reason := data.table::fcase(
  basis != "pce25", NA_character_,
  estimate_start <= 0, "start estimate not positive",
  se_start / estimate_start > max_start_cv,
  paste0("start CV above ", max_start_cv),
  estimate_end <= 0, "end estimate not positive",
  default = NA_character_
)]
pct_flag <- changes$basis == "pce25" & is.na(changes$pct_suppressed_reason)
changes[, se_log_ratio := data.table::fifelse(
  pct_flag,
  sqrt((se_start / estimate_start)^2 + (se_end / estimate_end)^2),
  NA_real_
)]
changes[, pct_change := data.table::fifelse(
  pct_flag, 100 * (estimate_end / estimate_start - 1), NA_real_
)]
changes[, pct_ci_low := 100 * (exp(log(estimate_end / estimate_start) -
                                     z_95 * se_log_ratio) - 1)]
changes[, pct_ci_high := 100 * (exp(log(estimate_end / estimate_start) +
                                      z_95 * se_log_ratio) - 1)]
changes[, pct_significant_flag := pct_ci_low > 0 | pct_ci_high < 0]
disagree_count <- changes[pct_flag & significant_flag != pct_significant_flag,
                          .N]
log_step(paste0(
  sum(pct_flag), " percent changes computed, ",
  sum(changes$basis == "pce25" & !pct_flag), " suppressed; the level and ",
  "log-ratio tests disagree on ", disagree_count
))

# 53) Benjamini-Hochberg q-values of the level test, within each family:
#     one period x one grouping (all its groups, outcomes, and statistics),
#     over the cells with a z
changes[, p_value := 2 * stats::pnorm(-abs(z_change))]
changes[, q_value := stats::p.adjust(p_value, method = "BH"),
        by = list(period, grouping)]

changes[, small_cell := n_unweighted_start < small_cell_threshold |
          n_unweighted_end < small_cell_threshold]
changes[, tiny_cell := n_unweighted_start <= tiny_cell_threshold |
          n_unweighted_end <= tiny_cell_threshold]
data.table::setorderv(changes, c("period", "grouping_rank", "group_rank",
                                 "outcome_rank"))
change_columns <- c(
  "period", "year_start", "year_end", "grouping", "group", "group_rank",
  "outcome", "statistic", "basis", "estimate_start", "estimate_end",
  "se_start", "se_end", "n_unweighted_start", "n_unweighted_end", "change",
  "se_change", "ci_low", "ci_high", "z_change", "significant_flag",
  "q_value", "pct_change", "se_log_ratio", "pct_ci_low", "pct_ci_high",
  "pct_significant_flag", "pct_suppressed_reason", "small_cell",
  "tiny_cell", "definition_break", "measurement_note"
)
changes_out <- changes[, change_columns, with = FALSE]
data.table::fwrite(changes_out, changes_path)
log_step(paste0("Wrote ", nrow(changes_out), " rows to ", changes_path))

################################################################################
###                          10. Wealth shares                               ###
################################################################################

# 54) One row per wave and measure, with 95% intervals
wealth_shares <- data.table::rbindlist(share_parts, use.names = TRUE)
measure_order <- c("networth_share_bottom50", "networth_share_top10",
                   "networth_share_under35", "household_share_under35")
if (anyNA(wealth_shares$se) ||
      nrow(wealth_shares) != length(wave_years) * length(measure_order)) {
  stop("Wealth shares are incomplete or lack a standard error",
       call. = FALSE)
}
wealth_shares[, ci_low := estimate - z_95 * se]
wealth_shares[, ci_high := estimate + z_95 * se]

# 55) Say how each SE was made. The under-35 share of households barely
#     varies across the replicate weights, which, like WGT, are calibrated
#     to CPS control totals (05, header, MR3), so its SE is near zero and
#     is not the precision of an independent SCF measurement
wealth_shares[, se_note := data.table::fifelse(
  measure == "household_share_under35",
  paste0("scf_estimate() (03); near zero because the weights are ",
         "calibrated to CPS totals, so understated"),
  "households re-ranked by net worth in every replicate and implicate"
)]
wealth_shares[, statistic_rank := match(measure, measure_order)]
data.table::setorderv(wealth_shares, c("statistic_rank", "year"))
wealth_shares <- wealth_shares[, list(
  year, measure, estimate, se, se_sampling, se_imputation, ci_low, ci_high,
  n_unweighted, se_note
)]
data.table::fwrite(wealth_shares, wealth_shares_path)
log_step(paste0("Wrote ", nrow(wealth_shares), " rows to ",
                wealth_shares_path))

################################################################################
###                   11. Age mix and age-standardized medians               ###
################################################################################

# 56) The age mix of each income and net worth group (step 35): the share
#     of the group's households in each AGECL group, with SEs that hold
#     group membership fixed
age_shares <- data.table::rbindlist(age_share_parts, use.names = TRUE)
age_shares <- age_shares[, list(
  year, grouping, group, measure = "age_share", age_group = outcome,
  age_reference = NA_character_, estimate, se, n_unweighted,
  se_note = "scf_estimate() (03) with group membership held fixed"
)]

# 57) Age-standardized medians (methodology review MR-DESIGN): in each wave
#     and Fed group, reweight households so that the group's AGECL mix
#     equals its mix in a reference wave (WGT x reference share / own
#     share within the age group), then take the weighted median over the
#     five pooled implicates; reference "none" is the observed median.
#     Point estimates only
mix_design <- stack_groups(panel)[
  grouping %in% c("income_group", "networth_group"),
  list(year, grouping, group, age_group = as.character(age_group_fed), wgt,
       implicate, networth_pce25, income_pce25)
]
own_mix <- mix_design[, list(age_weight = sum(wgt)),
                      by = list(year, grouping, group, age_group)]
own_mix[, own_share := age_weight / sum(age_weight),
        by = list(year, grouping, group)]
if (nrow(own_mix) != length(wave_years) * 11L * length(age_labels)) {
  stop("Some wave x group cell lacks an age group; the standardization ",
       "needs every age group in every cell", call. = FALSE)
}
group_case_count <- mix_design[implicate == 1L, list(n_unweighted = .N),
                               by = list(year, grouping, group)]
standardized_parts <- list()
for (age_reference in c("none", as.character(age_reference_years))) {
  reference_design <- merge(
    mix_design, own_mix[, list(year, grouping, group, age_group, own_share)],
    by = c("year", "grouping", "group", "age_group")
  )
  if (age_reference == "none") {
    reference_design[, standard_wgt := wgt]
  } else {
    reference_mix <- own_mix[year == as.integer(age_reference),
                             list(grouping, group, age_group,
                                  reference_share = own_share)]
    reference_design <- merge(reference_design, reference_mix,
                              by = c("grouping", "group", "age_group"))
    reference_design[, standard_wgt := wgt * reference_share / own_share]
  }
  for (median_var in c("networth_pce25", "income_pce25")) {
    data.table::setorderv(reference_design,
                          c("year", "grouping", "group", median_var))
    standardized <- reference_design[
      , list(estimate = scf_sorted_median(get(median_var), standard_wgt)),
      by = list(year, grouping, group)
    ]
    standardized[, `:=`(
      measure = paste0(sub("_pce25$", "", median_var), "_median"),
      age_reference = age_reference
    )]
    standardized_parts[[length(standardized_parts) + 1L]] <- standardized
  }
}
standardized_medians <- data.table::rbindlist(standardized_parts)

# 58) Check: the observed medians (reference "none") equal the medians of
#     bottom_earners_by_group.csv
observed_check <- merge(
  standardized_medians[age_reference == "none",
                       list(year, grouping, group, measure, estimate)],
  by_group[statistic == "median",
           list(year, grouping, group,
                measure = paste0(outcome, "_median"), by_group = estimate)],
  by = c("year", "grouping", "group", "measure")
)
if (nrow(observed_check) != nrow(standardized_medians) / 4L ||
      max(abs(observed_check$estimate - observed_check$by_group)) > 1e-6) {
  stop("Observed medians of the age-mix table differ from the by-group ",
       "table", call. = FALSE)
}
standardized_medians <- merge(standardized_medians, group_case_count,
                              by = c("year", "grouping", "group"))
standardized_medians[, `:=`(
  age_group = NA_character_, se = NA_real_,
  se_note = "point estimate only (no SE computed)"
)]
age_mix <- data.table::rbindlist(list(age_shares, standardized_medians),
                                 use.names = TRUE)
age_mix[, grouping_rank := match(grouping, grouping_order)]
age_mix[, group_rank := data.table::fifelse(
  grouping == "income_group", match(group, income_labels),
  match(group, networth_labels)
)]
age_mix[, statistic_rank := match(measure, c("age_share", "networth_median",
                                             "income_median"))]
age_mix[, age_rank := match(age_group, age_labels)]
data.table::setorderv(age_mix, c("grouping_rank", "group_rank",
                                 "statistic_rank", "age_reference",
                                 "age_rank", "year"))
age_mix <- age_mix[, list(
  year, grouping, group, group_rank, measure, age_group, age_reference,
  estimate, se, n_unweighted, se_note
)]
data.table::fwrite(age_mix, age_mix_path)
log_step(paste0("Wrote ", nrow(age_mix), " rows to ", age_mix_path))

# 59) Log the headline cells for the session record; a suppressed percent
#     change is logged as its dollar change
headline <- changes_out[
  period == "2022-2025" & statistic == "median" &
    grouping %in% c("income_group", "networth_group") &
    group %in% c("Less than 20", "20-39.9", "Less than 25", "25-49.9",
                 "90-100")
]
for (row_index in seq_len(nrow(headline))) {
  headline_text <- if (is.na(headline$pct_change[row_index])) {
    paste0("$", round(headline$change[row_index]), " (",
           round(headline$ci_low[row_index]), " to ",
           round(headline$ci_high[row_index]), "); percent suppressed: ",
           headline$pct_suppressed_reason[row_index])
  } else {
    paste0(round(headline$pct_change[row_index], 1L), "% (",
           round(headline$pct_ci_low[row_index], 1L), " to ",
           round(headline$pct_ci_high[row_index], 1L), ")")
  }
  log_step(paste0(
    "2022-2025 ", headline$grouping[row_index], " ",
    headline$group[row_index], " median ", headline$outcome[row_index],
    ": ", headline_text
  ))
}
log_step(paste0(
  "Finished in ",
  round(as.numeric(difftime(Sys.time(), run_start, units = "mins")), 2L),
  " minutes"
))
