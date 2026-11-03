# test_committed_outputs -- committed figure inputs match the latest run
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Most outputs are ignored by git, but the figures and the four tables the
# figure scripts read (06, 13, 17) are committed, so a clone that cannot run
# the full pipeline can rebuild and view the figures (.gitignore, "Exceptions").
# This check keeps those committed files honest. Run it from the project root
# after a pipeline run (make check-project does):
#   Rscript code/tests/test_committed_outputs.R
# It fails (exit 1) when:
#   - the list below differs from the files git tracks under output/, so the
#     .gitignore exceptions and this list have drifted apart;
#   - a listed file is missing from the working tree; or
#   - a committed table differs from the working copy, meaning the latest run
#     produced numbers that are not committed. Re-commit the table (and its
#     figure) or find out why the run changed.
# A committed figure that differs is reported but does not fail: PNG bytes can
# change with the fonts and renderer of the machine that drew them, while the
# numbers behind them are covered by the table check.

################################################################################
###                        1. Set paths and the list                         ###
################################################################################

# 1) Stop unless this script sits at code/tests/, and work from the root
here::i_am("code/tests/test_committed_outputs.R")
project_root <- here::here()

# 2) The committed outputs; keep in step with the .gitignore exceptions
committed_tables <- c(
  "output/tables/fig_inputs_young.csv",
  "output/tables/u35_balance_sheet.csv",
  "output/tables/networth_change_by_age.csv",
  "output/tables/bottom_earners_changes.csv",
  "output/tables/u35_networth_percentiles.csv"
)
# The draft's number ledger is internal: it is checked only where its script
# is present (it is left out of the public replication package)
if (file.exists(file.path(project_root, "code", "20_draft_numbers.R"))) {
  committed_tables <- c(committed_tables, "output/tables/draft_numbers.csv")
}
committed_figures <- c(
  "output/figures/young_median_networth_by_year.png",
  "output/figures/cohort_median_networth_by_age.png",
  "output/figures/u35_balance_sheet_bars_2022_2025.png",
  "output/figures/u35_balance_sheet_trend.png",
  "output/figures/networth_change_by_age_2022_2025.png",
  "output/figures/bottom_earners_change_2022_2025.png",
  "output/figures/u35_networth_percentile_change_2022_2025.png"
)
committed_outputs <- c(committed_tables, committed_figures)

# 3) Run git from the project root and return its exit status and output
run_git <- function(git_args) {
  git_output <- suppressWarnings(system2(
    "git", c("-C", shQuote(project_root), git_args),
    stdout = TRUE, stderr = TRUE
  ))
  git_status <- attr(git_output, "status")
  list(status = if (is.null(git_status)) 0L else git_status,
       output = git_output)
}

################################################################################
###                        2. Check the list against git                     ###
################################################################################

failures <- character(0)

# 1) The files git tracks under output/ (placeholders excluded) must equal the
#    list, so a new exception or a dropped one cannot go unnoticed
tracked <- run_git(c("ls-files", "--", "output/tables", "output/figures"))
if (tracked$status != 0L) {
  stop("git ls-files failed; run this from a git checkout:\n",
       paste(tracked$output, collapse = "\n"), call. = FALSE)
}
tracked_outputs <- tracked$output[!grepl("[.]gitkeep$", tracked$output)]
not_tracked <- setdiff(committed_outputs, tracked_outputs)
not_listed <- setdiff(tracked_outputs, committed_outputs)
if (length(not_tracked) > 0L) {
  failures <- c(failures, paste0("listed but not tracked by git: ",
                                 not_tracked))
}
if (length(not_listed) > 0L) {
  failures <- c(failures, paste0("tracked by git but not listed here: ",
                                 not_listed))
}

# 2) Every listed file must exist in the working tree
missing_files <- committed_outputs[
  !file.exists(file.path(project_root, committed_outputs))
]
if (length(missing_files) > 0L) {
  failures <- c(failures, paste0("missing from the working tree: ",
                                 missing_files))
}

################################################################################
###                        3. Compare the working copy with HEAD             ###
################################################################################

# 1) A table that differs from HEAD fails; git compares the normalized (LF)
#    content, so line-ending conversion on checkout is not a difference
for (table_path in setdiff(committed_tables, c(not_tracked, missing_files))) {
  table_diff <- run_git(c("diff", "--quiet", "HEAD", "--", table_path))
  if (table_diff$status == 0L) {
    message("PASS: ", table_path, " matches the committed copy")
  } else {
    failures <- c(failures, paste0(
      "differs from the committed copy (re-commit it with its figure, or ",
      "find out why the run changed): ", table_path
    ))
  }
}

# 2) A figure that differs is reported only
for (figure_path in setdiff(committed_figures,
                            c(not_tracked, missing_files))) {
  figure_diff <- run_git(c("diff", "--quiet", "HEAD", "--", figure_path))
  if (figure_diff$status == 0L) {
    message("PASS: ", figure_path, " matches the committed copy")
  } else {
    message("NOTE: ", figure_path, " differs from the committed copy ",
            "(not a failure; re-commit it if its table changed)")
  }
}

################################################################################
###                        4. Report                                         ###
################################################################################

if (length(failures) > 0L) {
  message(paste0("FAIL: ", failures, collapse = "\n"))
  quit(status = 1L)
}
message("All ", length(committed_outputs),
        " committed outputs are tracked, present, and current")
