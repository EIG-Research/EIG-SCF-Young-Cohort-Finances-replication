# Property taxes: a separate module

This folder answers a question outside the young-cohort project: **who pays property taxes
on their homes, how much, and how the tax compares with the federal income tax in
progressivity.** It covers all SCF households, every age, 1989-2025, with 2025 as the lead
year. Plan: `records/plans/2026-10-19_property-tax-descriptives.md`.

It is kept apart from the main pipeline on purpose: `code/run_all.R` does not run it, it
writes only to `output/property_taxes/` and `data/processed/property_taxes/`, and it changes
no numbered script or existing table. It reads the panel and replicate weights that
`code/02_build_scf_panel.R` writes and the full public files `code/01_download_scf.R`
downloads, and it sources the estimator library `code/03_scf_estimators.R`.

## Scripts

| Script | What it does | Writes |
|---|---|---|
| `run_property_taxes.R` | Runs pt01-pt04 in fresh R processes and stops at the first failure | — |
| `pt00_functions.R` | Functions only: `pt_ratio_estimate()` (a ratio of weighted totals, with SEs by the method of 03) and `pt_suits_index()` (the Suits index of a tax against income) | — |
| `pt01_property_tax_inputs.R` | Reads `X721` (real estate taxes on the home), `X722` (its frequency), `X508`, and `X716` from all 13 full public files, plus the income items TAXSIM needs for 2019, 2022, and 2025; joins on Y1; annualizes the tax with the Fed's `MCONV`; flags owners, payers, owners with no mortgage, and farm or ranch owners. Checks the join, the frequency codes, and owners against HOUSECL | `data/processed/property_taxes/pt_households.rds/.parquet` |
| `pt02_federal_income_tax.R` | Federal income tax after credits for the 2019, 2022, and 2025 waves with NBER TAXSIM 35 (the `usincometaxes` package, run locally). Tax year 2024 is approximated by 2023 law on indexed incomes | `data/processed/property_taxes/pt_fedtax.rds` |
| `pt03_property_tax_tables.R` | The seven tables listed in `output/property_taxes/README.md`, with SEs from the 999 replicate weights and five implicates | `output/property_taxes/tables/*.csv` |
| `pt04_property_tax_figures.R` | Two diagnostic EIG-styled figures | `output/property_taxes/figures/*.png` |

## Run it

From the project root (about 90 seconds once the panel exists):

```sh
Rscript code/property_taxes/run_property_taxes.R
```

Extra package beyond the main pipeline: `usincometaxes` (TAXSIM 35 in WebAssembly; no data
leave the machine).

## Read before quoting a number

- **What the SCF measures.** `X721` is the real estate tax owners report on their own home
  (and, for farm and ranch owners, the farm or ranch). Taxes on second homes, rental
  property, and businesses are not asked, and renters pay property tax only indirectly,
  through rent, which the SCF does not record. Totals here (about $428 billion in 2025) are
  owner-occupied homes only, not all property tax revenue.
- **"Owns outright"** means no mortgage, land contract, or home equity line on the home.
- **Income tax is simulated**, federal only, with simplifications listed in the header of
  `pt02_federal_income_tax.R`. Its level runs above IRS totals because SCF income runs above
  AGI; use it for the shape across income groups, not the dollar level.
- **Income and net worth groups** are the Fed's INCCAT and NWCAT, held fixed in the
  replicates, so their SEs are slightly understated.
- Variable definitions and codebook lines: `data/docs/scf/scf_property_tax_variables.md`.
