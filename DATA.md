# Data Documentation — Did Young Americans Really Just Get Poorer? Why Context (and Confidence Bands) Matter: An SCF Investigation

This file documents the datasets used in the analysis: their sources, vintages, and how
each was used. The raw input data are not redistributed here; the download scripts fetch
them from the original publishers.

## Datasets

| Dataset | Source / agency | Vintage | How it was used | Access notes |
|---|---|---|---|---|
| Survey of Consumer Finances (SCF) | Federal Reserve Board | 2025 release of 2026-10-09: codebk2025.txt (October 2026 version) and bulletin.macro.txt; all variables below are pinned to it; Summary extracts scfp1989-scfp2025 as re-released with the 2025 release (2025 dollars); Cross-wave definitions 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2022 from each wave's codebook and the macro's year branches; 2025 release (codebk2025.txt); 2025 release (bulletin.macro.txt); 2025 release (codebk2025.txt); same structure 1989-2022 codebooks; 2025 release (bulletin.macro.txt; scfp2025); 2025 release (bulletin.macro.txt; codebk2025.txt X443); 2025 release (bulletin.macro.txt; codebk2025.txt X5729); 2025 release (bulletin.macro.txt; codebk2025.txt X7362, X7650); 2025 release (bulletin.macro.txt; codebk2025.txt X14); 2025 release (bulletin.macro.txt; codebk2025.txt X8021); 2025 release (bulletin.macro.txt; codebk2025.txt X6809, X6810); 2025 release (bulletin.macro.txt; codebk2025.txt X5931); 2025 release (bulletin.macro.txt; codebk2025.txt X8023); 2025 release (bulletin.macro.txt; codebk2025.txt X30022); Full public files 2019 (Sep 2020), 2022 (Oct 2023), 2025 (Oct 2026); codebk2019/2022/2025.txt | Variables: YY1, Y1, X42001, WGT, WT1B*, MM*, NETWORTH, ASSET, FIN, NFIN, DEBT, HOUSES, ORESRE, NNRESRE, BUS, VEHIC, RETQLIQ, LIQ, MRTHEL, RESDBT, CCBAL, INSTALL, EDN_INST, VEH_INST, OTH_INST, BNPL, OTHLOC, ODEBT, INCOME, NORMINC, AGE, AGECL, HHSEX, RACECL4, RACECL5, RACECL, EDCL, EDUC, MARRIED, KIDS, FAMSTRUCT, HOUSECL, INCCAT, REGION, X2201, X2202, X2205, X8166, X7543, X7540, X2206, X2208, X2209, X2218, X2219, X716, X720, X718, X717, X723, X802, X7137, X804, X816, X820, X7053, X7054 | Public download, no account needed |
| Current Population Survey (CPS) | U.S. Census Bureau and Bureau of Labor Statistics, via IPUMS CPS | IPUMS CPS ASEC samples cps1989_03s through cps2025_03s (one ASEC sample per year; 2014 is one sample holding both split-panel files); IPUMS documentation read 2026-10-11; Census Table AD-1 (ad1.xls), internet release December 2025, CPS ASEC 1981-2025: validation target; IPUMS CPS ASEC 2005-2025 (ages 15-44, REPWTP) and 2007-2025 (all persons, PECOHAB and LINENO); DDI files of 2026-10-11 (IPUMS CPS DOI 10.18128/D030.V13.0); Census Tables HH-3 (release December 2025, 1960-2025), UC-1 (release November 2024, 1960-2023), and UC3 for 2018 and 2019 (releases November 2018, November 2019): benchmarks | Variables: YEAR, SERIAL, PERNUM, ASECWT, ASECFLAG, HFLAG, GQ, AGE, SEX, RELATE, MARST, RACE, HISPAN, EDUC, REPWTP, PECOHAB, LINENO, ASECWTH | Microdata need a free IPUMS account and API key; Census tables are public |

## Obtaining the data

Two scripts download every input. Run them from the repository root, directly or through
`code/run_all.R`. Each writes a `manifest.csv` with the SHA-256 hash and retrieval date of
every file it downloads, and a re-run skips files that are already present and unchanged.

**SCF, FRED series: `code/01_download_scf.R`** (no account needed). It writes to
`data/raw/scf/`:

- for each wave from 1989 to 2025, the Stata versions of the summary extract
  (`scfpYYYYs.zip`), the full public data set (`scfYYYYs.zip`), and the replicate weights
  (`scfYYYYrw1s.zip`), from each wave's page at
  https://www.federalreserve.gov/econres/scfindex.htm;
- from the 2025 release, the nominal and real historical tables, the 2025 standard-error
  tables, and `bulletin.macro.txt`, in `data/raw/scf/common/`;
- the annual PCE chain-type price index, FRED series `DPCERG3A086NBEA` (BEA NIPA Table
  1.1.4, line 2), 1988-2025, and the weekly 30-year fixed mortgage rate, FRED series
  `MORTGAGE30US`, 2019-2025, also in `data/raw/scf/common/`. BEA revises the PCE series, so
  a later download can differ slightly from the one used here.

**CPS, Census tables: `code/07_download_ipums_cps.R`**. It writes to
`data/raw/ipums_cps/` and `data/raw/census/`:

- three IPUMS CPS ASEC microdata extracts, defined in `code/extracts/*.json`, which the
  script submits through the IPUMS API, waits for, and downloads. This needs a free IPUMS
  CPS account (https://cps.ipums.org/cps/) and an API key
  (https://account.ipums.org/api_keys), set in the environment variable `IPUMS_API_KEY`.
  IPUMS assigns each extract a new number, and the data version can differ from the one
  used here (IPUMS CPS DOI 10.18128/D030.V13.0);
- Census Tables AD-1, HH-3, and UC-1, and Table UC3 for 2018 and 2019, from
  https://www2.census.gov/programs-surveys/demo/tables/families/.

The IPUMS terms of use require any publication that uses these data to cite IPUMS CPS with
its data version; take the citation from https://cps.ipums.org/cps/citation.shtml.

## Other data sources

| Data Source | Type | Owner/Location | Time Coverage | Access Notes |
|---|---|---|---|---|
| Survey of Consumer Finances, summary extract (`scfp*`) | Household survey, public microdata | Federal Reserve Board, https://www.federalreserve.gov/econres/scfindex.htm | 1989-2025, triennial (13 waves) | Public download; five implicates per case; use `WGT` |
| Survey of Consumer Finances, full public data set (`scf*`) | Household survey, public microdata | Federal Reserve Board | 1989-2025 | Public download; needed for variables not in the extract (for example detailed debt types) |
| SCF replicate weights | Survey weights | Federal Reserve Board | 1989-2025 | Public download; 999 bootstrap replicates for standard errors |
| SCF historical tables (public data) | Published estimates | Federal Reserve Board | 1989-2025 | Public Excel; benchmark to validate the pipeline's estimates |
| SCF 2025 bulletin, "Changes in U.S. Family Finances from 2022 to 2025" | Published report | Federal Reserve Board, released 2026-10-09 | 2022-2025 | Public; validation target for 2025 estimates |
| PCE chain-type price index (headline), annual | Price index | Bureau of Economic Analysis, NIPA Table 1.1.4, line 2; FRED series `DPCERG3A086NBEA` | 1988-2025 (1988 deflates 1989-wave income) | Public; rebased to 2025 = 100 as the project deflator |
| 30-year fixed mortgage rate, weekly | Interest rate | Freddie Mac Primary Mortgage Market Survey; FRED series `MORTGAGE30US` | 2019-2025 | Public; context series |
