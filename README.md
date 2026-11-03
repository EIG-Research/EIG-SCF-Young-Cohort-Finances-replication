# Did Young Americans Really Just Get Poorer? Why Context (and Confidence Bands) Matter: An SCF Investigation — Replication Package

This repository contains the public replication materials for **Did Young Americans Really Just Get Poorer? Why Context (and Confidence Bands) Matter: An SCF Investigation**, an
EIG analysis of the Survey of Consumer Finances, 1989-2025:
the analysis code, the data documentation needed to obtain and prepare the inputs, and
the figures and tables the analysis produces.

## Research question

- How do the finances of young U.S. households (net worth, assets, debt, and income) compare
  with those of earlier young cohorts at the same ages, how do they vary across demographic
  groups, and how have they changed over time, from 1989 through the 2025 SCF?
- Two lenses:
  1. **Age-at-a-point-in-time (lead lens):** young households, meaning a reference person under 35,
     in bands under 25, 25-29, and 30-34, with 35-44 as a comparison band, in each survey
     year, compared across years.
  2. **Birth-cohort (second lens):** synthetic cohorts in five-year birth bands of the
     reference person (birth year = survey year minus age), followed across successive waves.
     Small cells are flagged, not dropped.
- Demographic cuts: race and ethnicity, education, and sex of the reference person lead;
  family structure (including marital status), homeownership, and income group are also
  reported. Region only where the public file allows it.

## What is here

- `code/` — analysis scripts. The pipeline entry point is `code/run_all.R`, which runs the
  numbered scripts in order, each in a fresh R process, and writes a log to `output/logs/`.
  `code/README.md` describes each script.
- `code/extracts/` — the IPUMS CPS extract definitions that `code/07_download_ipums_cps.R`
  submits.
- `output/figures/` and `output/tables/` — the figures and tables the code produces.
- `DATA.md` — the datasets used, their sources and vintages, and how to obtain them. Raw
  input data are **not** included.
- `CITATION.cff` — how to cite this work.

## How to replicate

1. Install R and the packages the scripts use: arrow, data.table, digest, dplyr, ggplot2,
   haven, here, httr2, ipumsr, Matrix, ragg, readr, readxl, scales, tibble, tidyr,
   tidyselect, and usincometaxes (the last only for `code/property_taxes/`). Most scripts
   stop at startup and name any package that is missing.
2. Register for an IPUMS account and an API key (see `DATA.md`), and set the environment
   variable `IPUMS_API_KEY`. The SCF, FRED, and Census inputs need no account.
3. From the repository root, run `Rscript code/run_all.R`. The download scripts (01 and
   07) fetch every input into `data/raw/`; the later scripts build the analysis files and
   write the outputs. Toggle the script flags at the top of `run_all.R` to control which
   steps run.
4. Outputs are written to `output/`.

The figures here were drawn with EIG's house theme and fonts, which are not part of this
package. Re-running the figure scripts uses a plain fallback theme with the same colors and
type sizes; the plotted values are identical, but the typeface and some styling differ.

## Notes

Raw input data are not redistributed. See `DATA.md` for access instructions. Please open an
issue if a step does not reproduce.
