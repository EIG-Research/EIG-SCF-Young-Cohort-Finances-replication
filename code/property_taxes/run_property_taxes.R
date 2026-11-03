# run_property_taxes -- run the property-tax module's scripts in fresh processes
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# The property-tax module (plan:
# records/plans/2026-10-19_property-tax-descriptives.md) is separate from the
# young-cohort pipeline: code/run_all.R does not run it.
# It needs the panel and replicate weights written by code/02 and the raw full
# public files downloaded by code/01. Run from the project root as
#   Rscript code/property_taxes/run_property_taxes.R
# pt00_functions.R defines functions only and is sourced by pt03.

here::i_am("code/property_taxes/run_property_taxes.R")

# 1) The scripts, in order
module_scripts <- c(
  "pt01_property_tax_inputs.R",
  "pt02_federal_income_tax.R",
  "pt03_property_tax_tables.R",
  "pt04_property_tax_figures.R"
)

# 2) Run each in a fresh R process from the project root; stop at a failure
prior_wd <- setwd(here::here())
on.exit(setwd(prior_wd), add = TRUE)
rscript_path <- file.path(R.home("bin"), "Rscript")
for (script_name in module_scripts) {
  script_path <- file.path("code", "property_taxes", script_name)
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] Running ",
          script_path)
  exit_status <- system2(rscript_path, script_path)
  if (!identical(exit_status, 0L)) {
    stop(script_path, " failed (exit status ", exit_status, ")",
         call. = FALSE)
  }
}
message("Property-tax module complete")
