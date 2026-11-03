# Code: how to run this project's pipeline

This folder holds the R pipeline for the Survey of Consumer Finances (SCF), 1989-2025. You do
not need an AI agent to run or read it; this page is for any collaborator.

## What is here

| Script | What it does | Reads | Writes |
|---|---|---|---|
| `run_all.R` | Runs the numbered scripts in order and logs the run | the scripts below | `output/logs/run_all_<time>.log` |
| `01_download_scf.R` | Finds each wave's files on the Federal Reserve's SCF pages and downloads the Stata summary extract, full public file, and replicate weights for all 13 waves, plus the 2025 historical tables, the bulletin macro, the annual PCE price index, and the weekly Freddie Mac 30-year mortgage rate (FRED `MORTGAGE30US`, 2019-2025); skips files whose SHA-256 matches the manifest | federalreserve.gov, FRED | `data/raw/scf/<year>/`, `data/raw/scf/common/`, `data/raw/scf/manifest.csv` |
| `02_build_scf_panel.R` | Stacks the 13 summary extracts (all five implicates), adds nominal and PCE-real 2025-dollar values, age and birth bands, and the harmonized demographic cuts; asserts row counts, weights, and joins | `data/raw/scf/` | `data/processed/scf_panel_1989_2025.parquet`, `data/processed/scf_repwgts_1989_2025.parquet` |
| `03_scf_estimators.R` | Functions only: `scf_estimate()` for weighted means, medians, and shares, pooling the five implicates, with standard errors from the 999 replicate weights and the (6/5) imputation term | — | — |
| `04_validate_benchmarks.R` | Reproduces the Fed's published mean and median net worth, income, and debt by age for every year, on the real and nominal bases, and fails if any cell misses | the checkpoints, `data/raw/scf/common/*.xlsx` | `output/tables/validation_benchmarks.csv`, `output/tables/validation_stderrs_2025.csv` |
| `05_young_cohort_descriptives.R` | Diagnostic tables for young households by year, age band, demographic cut, and birth cohort, with standard errors, small- and tiny-cell flags, and a `definition_break` note from the shared break table in `00_functions.R`; plus the figure input for 06, with `tiny_cell` | the checkpoints | `output/tables/young_*.csv`, `output/tables/cohort_by_age.csv`, `output/tables/fig_inputs_young.csv` |
| `06_young_cohort_figures.R` | Two diagnostic EIG-styled figures: median net worth by age group and year, and by birth cohort and age (leaving out birth-band cells at a mean age under 25 and cells of 10 or fewer cases) | `output/tables/fig_inputs_young.csv` | `output/figures/young_median_networth_by_year.png`, `output/figures/cohort_median_networth_by_age.png` |
| `00_functions.R` | Functions only: `lee_trim_stat()` (a weighted mean or median after trimming a share of the weight from the top or bottom) and `lee_trim_estimate()` (that statistic with SEs by the method of 03), for the Lee bounds in 11; `shift_share_decompose()`, which splits a change in a per-household total into count, mix, and value parts that add up exactly, for 15; and `scf_definition_breaks()`, the one table of documented SCF cross-wave definition breaks (target, first wave, note), read by 05, 09, and 12 | — | — |
| `07_download_ipums_cps.R` | Submits and downloads three IPUMS CPS ASEC extracts (definitions in `code/extracts/`): the 1989-2025 person file, the 2005-2025 person replicate weights for ages 15-44 (about 0.8 GB), and the 2007-2025 cohabiting-partner pointer PECOHAB; and five Census tables (AD-1, HH-3, UC-1, and UC3 for 2018 and 2019); skips files whose SHA-256 matches the manifest, so a re-run needs no API key. It saves the extract definition with LF line endings, matching git, so its hash survives a checkout. A first run needs `IPUMS_API_KEY` in the environment or on an `IPUMS_API_KEY=` line of the user `.Renviron` (only that line is read) | IPUMS API, census.gov | `data/raw/ipums_cps/`, `data/raw/census/` |
| `08_headship_context.R` | By SCF age band and year: the share of young adults who would be the SCF reference person of their unit (the selection rate into SCF young households), living arrangements (householder, spouse, partner, child of householder, other, group quarters), the weight basis with break flags at 2010, 2020, and 2025, and SCF reference persons per CPS person as a calibration check; a validation against Census Table AD-1 (2010 adjusted by AD-1's own 2011r - 2011 reweight shift, no exemptions) that fails on any miss; and the 2007-2010 child-of-householder change, raw and bridged across the 2010 reweight | the IPUMS extract, `ad1.xls`, the SCF panel | `output/tables/headship_context.csv`, `output/tables/headship_ad1_validation.csv`, `output/tables/headship_bridged_changes.csv` |
| `09_headship_by_cut.R` | The CPS selection shares (SCF-rule reference person, split into couple and single units; couple member; child of householder; householder, spouse, or partner) by SCF age band and by sex, race and ethnicity, and education, each with a standard error (successive difference replication with the 160 ASEC replicate weights from 2005; a design-effect approximation before); SCF against CPS reference persons by cut, with the SCF race coding breaks (1998, 2004, 2025) flagged from `00_functions.R`; and the CPS split of reference persons aged 25-34 | 08's person checkpoint and table, the replicate-weight extract, the SCF panel | `output/tables/headship_by_cut.csv`, `output/tables/headship_scf_cps_by_cut.csv`, `output/tables/headship_cps_age_split.csv` |
| `10_headship_benchmarks.R` | Validates CPS householders by age against Census HH-3 (1 percent) and householder couples against UC-1 for 1996-2006 (2 percent), checks the RELATE partner codes against PECOHAB in 2007-2025, and reports all PECOHAB couples against UC-1 and UC3 as a diagnostic; fails on any tested miss (HH-3 2014 is a named exemption) | the IPUMS extracts, `data/raw/census/` | `output/tables/headship_benchmarks.csv` |
| `11_selection_sensitivity.R` | Sensitivity of the SCF young-household estimates to selection into headship: estimates by age band, under 35 reweighted to the CPS age mix, and Lee (2009) bounds on the change from 1989 and 2007 under monotone selection and comparable populations across waves; and the SCF-versus-CPS 25-29/30-34 split | the SCF checkpoints, `young_by_age_band.csv`, 08's and 09's tables, 08's person checkpoint | `output/tables/selection_sensitivity.csv`, `output/tables/scf_cps_age_split.csv`, `output/tables/scf_cps_age_single_year.csv` |
| `12_u35_balance_sheet.R` | Balance sheet of under-35 households near the median net worth: for each wave, the mean of 12 asset and debt components among households between the 40th and 60th percentiles of net worth (main band; 45th-55th as sensitivity), which sum to the band's mean net worth, with holding rates, the band's median income, and standard errors that re-estimate the band cutoffs in every replicate and implicate; also median net worth by the Fed's age groups in 2022 and 2025 with the percent change and its 95% interval. Both band tables end with `definition_break`, the documented SCF definition changes of each component (from `00_functions.R`). Checks the adding-up identities and the Fed's published medians | the checkpoints, `output/tables/validation_benchmarks.csv` | `output/tables/u35_balance_sheet.csv`, `output/tables/u35_balance_sheet_sensitivity.csv`, `output/tables/networth_change_by_age.csv` |
| `13_u35_balance_sheet_figures.R` | Three diagnostic EIG-styled figures: stacked bars of every balance-sheet item in 2022 and 2025 (assets above zero, debts below, bands joining each item across years); small multiples of each item, 1989-2025, with 95% intervals and open circles at definition breaks; and the percent change in median net worth by age, 2022-2025, with 95% error bars | `output/tables/u35_balance_sheet.csv`, `output/tables/networth_change_by_age.csv` | `output/figures/u35_balance_sheet_bars_2022_2025.png`, `output/figures/u35_balance_sheet_trend.png`, `output/figures/networth_change_by_age_2022_2025.png` |
| `14_rate_price_inputs.R` | Vehicle, home, and first-mortgage detail for 2019, 2022, and 2025 from the full public files (the rate-versus-price test, `records/plans/2026-10-12_rate-vs-price-test.md`): one row per household and one per detailed vehicle, with NADA vehicle values, purchase years, loan rates, and amounts borrowed, nominal and in PCE 2025 dollars (purchase prices with the purchase-year index). Checks the join on `Y1` and that the items reproduce the panel's VEHIC, VEH_INST, and HOUSES. Variables: `data/docs/scf/scf_rate_price_variables.md` | `data/raw/scf/` full public files, the PCE index, the SCF panel | `data/processed/rate_price_households.rds/.parquet`, `data/processed/rate_price_vehicles.rds/.parquet` |
| `15_rate_price_test.R` | For under-35 households, 35-44, and the under-35 40th-60th net worth band: vehicle holding, value per vehicle by vehicle age, and loan terms of recent purchases; home ownership, recent purchases, purchase prices, and mortgage rates; each with SEs by the method of 03, the 2019-2022 and 2022-2025 changes with z, and a shift-share of vehicle value and home equity per household. Band SEs hold the cutoffs fixed and are understated. Descriptive only | 14's checkpoints, the SCF panel and replicate weights, `young_by_age_band.csv` (a wiring check) | `output/tables/rate_price_vehicles.csv`, `rate_price_housing.csv`, `rate_price_changes.csv`, `rate_price_shift_share.csv` |
| `16_bottom_earners.R` | Bottom earners, wealth groups, and young households (plan `records/plans/2026-10-13_young-bottom-earners-closeout.md`, W3): for all families, the Fed's income percentile groups (INCCAT), net worth percentile groups (NWCAT), and age groups, and for under-35 households by INCCAT, in every wave: median and mean net worth and income (PCE 2025 dollars) and the shares owning a home, a retirement account, or directly held stock, with any debt, with debt payments over 40 percent of income (PIR40), late on a payment (LATE), or 60 days late (LATE60), the last two also among debtors. Point estimates use the Fed's groups; SEs by the method of 03, re-ranking households into the income and net worth groups inside every replicate and implicate. Changes 1989-2025, 2019-2022, and 2022-2025 (waves treated as independent samples): level change with z and a Benjamini-Hochberg q-value per period x grouping, and percent change with a log-ratio interval, suppressed when the base is not positive or its CV exceeds 0.2. Also the shares of total net worth held by the bottom 50 percent, the top 10 percent, and under-35 households (households re-ranked in every replicate and implicate), and the age mix of each income and net worth group with age-standardized medians. Breaks come from `scf_definition_breaks()` (00) through an outcome map. First checks its nominal medians and means of income and net worth by INCCAT and NWCAT against the Fed's Tables 1 and 4, its debtor shares with PIR40 and LATE60 against Table 18, and the bulletin's 2022 and 2025 LATE, LATE60, and PIR40 shares (Table 5), and fails on any miss. Variables: `data/docs/scf/scf_bottom_earners_variables.md` | the checkpoints, `data/raw/scf/common/scf2025_tables_public_nominal_historical.xlsx` | `output/tables/bottom_earners_validation.csv`, `bottom_earners_by_group.csv`, `bottom_earners_changes.csv`, `wealth_shares.csv`, `bottom_earners_age_mix.csv` |
| `17_bottom_earners_figures.R` | One diagnostic EIG-styled figure: the percent change in median net worth and median income from 2022 to 2025 by income percentile group, with 95% log-ratio error bars from 16 (styled as Figure C of 13); stops if a plotted cell is a small cell, crosses a definition break, or has a suppressed percent change | `output/tables/bottom_earners_changes.csv` | `output/figures/bottom_earners_change_2022_2025.png` |
| `18_u35_networth_percentiles.R` | Under-35 net worth at the 10th-90th percentiles in 2022 and 2025 (03's median rule at other p, pooled implicates; SEs from the 999 replicates and the implicates), with the level change and the percent change with 12's log-ratio interval, suppressed where a base is not positive or its CV exceeds 0.2; and homeownership around 12's 40th-60th band: all under-35 households, in, below, and above the band, where owners and renters sit, and 2025 in the 2022 dollar band (plan `records/plans/2026-10-16_draft-v2-replies.md`). Checks its 50th percentile and SE against 12's median and its in-band homeownership against 12 | the checkpoints, `networth_change_by_age.csv`, `u35_balance_sheet.csv` | `output/tables/u35_networth_percentiles.csv`, `output/tables/u35_band_composition.csv` |
| `19_u35_percentile_figure.R` | One diagnostic EIG-styled figure: the percent change in under-35 net worth by percentile, 2022-2025, with 95% error bars (styled as Figure C of 13); the 10th-30th percentiles, near or below zero, show their two levels instead | `output/tables/u35_networth_percentiles.csv` | `output/figures/u35_networth_percentile_change_2022_2025.png` |
| `20_draft_numbers.R` | Internal to EIG and not in the public replication package: checks the numbers an EIG draft states against the tables above. Documented in the project records | the tables above | an internal table |
| `21_datawrapper_figures.R` | Internal to EIG and not in the public replication package: publishes Datawrapper versions of the figures above. Not run by `run_all.R`; documented in the project records | the five figure input tables | internal Datawrapper exports |
| `tests/test_scf_estimators.R` | Unit tests for `scf_estimate()` on synthetic data | — | — |
| `tests/test_selection_bounds.R` | Hand-checked unit tests for `lee_trim_stat()`; 11 runs them first | — | — |
| `tests/test_rate_price_decomposition.R` | Hand-checked unit tests for `shift_share_decompose()`; 15 runs them first | — | — |
| `tests/test_output_table_dictionary.R` | Checks that `data/docs/output_tables.md` lists every column of each table it documents, in header order; exits 1 on any mismatch. Not part of the pipeline: run it after a script changes a table's columns | `data/docs/output_tables.md`, `output/tables/*.csv` | — |
| `tests/test_committed_outputs.R` | Checks the committed figures and figure-input tables (see "What git keeps"): fails if they drift from the `.gitignore` exceptions, are missing, or if a committed table differs from the latest run; a differing figure is reported only. `make check-project` runs it after the pipeline | the committed files under `output/`, git | — |

The columns of the output tables are defined in `data/docs/output_tables.md`, which also
lists the conditions that travel with a shared table: the young-household (PEU) selection
caveat, the PCE 2025-dollar basis, and the IPUMS CPS citation required for every table built
from the CPS. Variable definitions, the dollar basis of the extracts, and cross-wave changes are documented
in `data/docs/scf/` and the project overlay `data/docs/dataset-registry.yaml`.

## Run it

You need R (4.3 or later) and these packages: `here`, `data.table`, `haven`, `arrow`,
`readxl`, `httr2`, `digest`, `ggplot2`, `ipumsr`, `tidyselect`, `Matrix`, `ragg`,
`readr`, `dplyr`, `tidyr`, `tibble`, and `scales`. From the repository root, run:

```sh
Rscript code/run_all.R
```

The first run downloads about 0.5 GB. Later runs re-hash the files and download nothing.
`run_all.R` runs each selected script in a fresh R process, stops at the first script that
fails (naming it), and writes one log per run to `output/logs/`.

## Turn scripts on or off

At the top of `run_all.R`, `run_script_flag` lists each numbered script with `TRUE` or
`FALSE`. Scripts run in the order listed. Add a line when you add a script.

## Check before sharing

`make check-project` confirms every script parses, lints `code/` against the project's
`.lintr`, and then runs the pipeline. `04_validate_benchmarks.R`,
`11_selection_sensitivity.R`, and `15_rate_price_test.R` run their unit tests first and stop if any fails; run them alone
with `Rscript code/tests/test_scf_estimators.R`, `Rscript code/tests/test_selection_bounds.R`, and `Rscript code/tests/test_rate_price_decomposition.R`. The R code follows the EIG R style guide.

## What git keeps

Code and the data documentation in `data/docs/` are committed. Raw data (`data/raw/`),
processed checkpoints (`data/processed/`), logs, and most tables are ignored by git: rerun the
pipeline to recreate them.

The exception is the figures. The seven PNGs in `output/figures/`,
the five tables the figure scripts read (`fig_inputs_young.csv`, `u35_balance_sheet.csv`,
`networth_change_by_age.csv`, `bottom_earners_changes.csv`, `u35_networth_percentiles.csv`),
and one internal table are committed; the list is the
"Exceptions" block of `.gitignore`. On a machine that cannot run the full pipeline, rebuild the figures
from a clone with only the figure scripts, which need no raw data:

```sh
Rscript code/06_young_cohort_figures.R
Rscript code/13_u35_balance_sheet_figures.R
Rscript code/17_bottom_earners_figures.R
Rscript code/19_u35_percentile_figure.R
```

After a pipeline run that changes one of those tables, commit the table and its figure
together; `code/tests/test_committed_outputs.R` (part of `make check-project`) fails until
you do.
