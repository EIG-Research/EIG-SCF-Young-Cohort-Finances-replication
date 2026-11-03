# run_all.R -- run the numbered scripts in fresh R processes and log the run
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

################################################################################
###                           1. Select scripts                              ###
################################################################################

# 1) Toggle numbered scripts with TRUE/FALSE; they run in the order listed
run_script_flag <- c(
  "01_download_scf.R" = TRUE,
  "02_build_scf_panel.R" = TRUE,
  "04_validate_benchmarks.R" = TRUE,
  "05_young_cohort_descriptives.R" = TRUE,
  "06_young_cohort_figures.R" = TRUE,
  "07_download_ipums_cps.R" = TRUE,
  "08_headship_context.R" = TRUE,
  "09_headship_by_cut.R" = TRUE,
  "10_headship_benchmarks.R" = TRUE,
  "11_selection_sensitivity.R" = TRUE,
  "12_u35_balance_sheet.R" = TRUE,
  "13_u35_balance_sheet_figures.R" = TRUE,
  "14_rate_price_inputs.R" = TRUE,
  "15_rate_price_test.R" = TRUE,
  "16_bottom_earners.R" = TRUE,
  "17_bottom_earners_figures.R" = TRUE,
  "18_u35_networth_percentiles.R" = TRUE,
  "19_u35_percentile_figure.R" = TRUE,
  "20_draft_numbers.R" = TRUE
)
# 03_scf_estimators.R and 00_functions.R define functions only and have no
# flag; 04, 05, 11, 12, 15, 16, 18, and 20 source 03, and 05, 09, 11, 12,
# 15, and 16 source 00

# 2) Internal scripts, left out of the public replication package
#    (records/plans/2026-11-03_replication-safety-fixes.md, U1). Exception to
#    the EIG R style guide, section 9: a flagged internal script whose file
#    is absent is skipped with a message; any other missing script still
#    stops the run.
internal_script <- c("20_draft_numbers.R")

scripts_to_run <- names(run_script_flag)[run_script_flag]

################################################################################
###                    2. Find the project and open the log                  ###
################################################################################

# 3) Stop unless this file sits at code/run_all.R under the working directory
if (!requireNamespace("here", quietly = TRUE)) {
  stop(
    "Package 'here' is required; install it with install.packages(\"here\")",
    call. = FALSE
  )
}
here::i_am("code/run_all.R")

# 4) Run from the root, so each child loads the project .Rprofile (renv);
#    section 3 restores the prior directory, even after a failure
prior_wd <- setwd(here::here())

# 5) Name one log per run; every path below is relative to the project root
dir.create(file.path("output", "logs"), recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(
  "output", "logs",
  paste0("run_all_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

# 6) Write one timestamped line to the console and to the run log
log_step <- function(text) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
  message(line)
  write(line, file = log_file, append = TRUE)
}

# 7) Record what produced this run: R version, git commit, renv.lock hash
git_sha <- suppressWarnings(tryCatch(
  system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE,
          stderr = FALSE),
  error = function(e) character(0L)
))
if (length(git_sha) != 1L) git_sha <- "unavailable (not a git checkout)"

renv_lock_md5 <- "none (no renv.lock)"
if (file.exists("renv.lock")) {
  renv_lock_md5 <- unname(tools::md5sum("renv.lock"))
}

log_step("Starting code/run_all.R")
log_step(paste("R version:", R.version.string))
log_step(paste("Git commit:", git_sha))
log_step(paste("renv.lock md5:", renv_lock_md5))
log_step(paste("Log file:", log_file))

################################################################################
###                            3. Run scripts                                ###
################################################################################

# 8) Run the scripts, then return to the prior directory even after a failure,
#    so an interactive session is not left in the project root
tryCatch(
  {
    # 9) Skip absent internal scripts (step 2), then check every other
    #    selected script exists before running any of them
    internal_absent_flag <- scripts_to_run %in% internal_script &
      !file.exists(file.path("code", scripts_to_run))
    if (any(internal_absent_flag)) {
      log_step(paste(
        "Skipping internal script not in this checkout:",
        scripts_to_run[internal_absent_flag]
      ))
    }
    scripts_to_run <- scripts_to_run[!internal_absent_flag]
    script_paths <- file.path("code", scripts_to_run)
    missing_scripts <- script_paths[!file.exists(script_paths)]
    if (length(missing_scripts) > 0L) {
      log_step(paste("ERROR: selected script not found:", missing_scripts))
      stop(
        "Selected script not found: ",
        paste(missing_scripts, collapse = ", "),
        call. = FALSE
      )
    }

    if (length(scripts_to_run) == 0L) {
      log_step("No scripts selected; every flag is FALSE.")
    }

    # 10) Source each script in its own Rscript process, then record its
    #    session information
    rscript_path <- file.path(R.home("bin"), "Rscript")

    for (script_path in script_paths) {
      log_step(paste("Running:", script_path))
      child_expression <- paste0(
        "source(\"", script_path, "\"); ",
        "writeLines(c(\"--- sessionInfo() ---\", ",
        "utils::capture.output(utils::sessionInfo())))"
      )
      child_output <- tempfile(fileext = ".log")
      script_start <- Sys.time()

      # 11) Merge the child's stdout and stderr into one file, then append it
      exit_status <- system2(
        rscript_path,
        c("-e", shQuote(child_expression)),
        stdout = child_output,
        stderr = child_output
      )
      file.append(log_file, child_output)
      output_lines <- readLines(child_output, warn = FALSE)
      unlink(child_output)

      elapsed_seconds_num <- round(
        as.numeric(difftime(Sys.time(), script_start, units = "secs")), 2L
      )

      # 12) Stop at the first failure and show the end of the child's output
      if (exit_status != 0L) {
        message(paste(utils::tail(output_lines, 20L), collapse = "\n"))
        log_step(paste0(
          "ERROR: ", script_path, " failed with exit status ", exit_status,
          " after ", elapsed_seconds_num, " seconds"
        ))
        stop(
          script_path, " failed with exit status ", exit_status,
          "; see ", log_file,
          call. = FALSE
        )
      }

      log_step(paste0(
        "Completed: ", script_path, " (", elapsed_seconds_num, " seconds)"
      ))
    }

    log_step("Finished code/run_all.R")
  },
  finally = setwd(prior_wd)
)
