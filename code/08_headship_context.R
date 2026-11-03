# 08_headship_context -- CPS ASEC headship context for SCF young households
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U9c of records/plans/2026-10-11_stage1-scf-pipeline.md (U9 Amendment).
# It reads the IPUMS CPS ASEC extract 1989-2025 (person level) named in
# data/raw/ipums_cps/manifest.csv, the SCF panel written by 02, and Census
# Table AD-1 (data/raw/census/ad1.xls), and writes, to output/tables/:
#   headship_context.csv         ASEC year x age band: the share of persons
#                                who would be the SCF reference person of
#                                their unit (scf_rule_rp_share, the headline
#                                selection rate), weighted shares of persons
#                                by relationship to the householder, the
#                                weight basis, and, in the SCF waves, SCF
#                                reference persons per CPS person (a
#                                calibration check)
#   headship_ad1_validation.csv  our share of 18-24 and 25-34 year-olds who
#                                are a child of the householder, against
#                                AD-1, by sex and for both sexes
#   headship_bridged_changes.csv the 2007-2010 change in that AD-1 share,
#                                raw and bridged across the 2010 reweight
# and, to data/processed/ (U1 of records/plans/
# 2026-10-11_mr4-headship-selection.md), for 09-11:
#   cps_persons_15_44.rds/.parquet  one row per ASEC person aged 15-44 (and
#                                any SCF-rule reference person under 15):
#                                weights, relationship, the SCF-rule role
#                                (section 6), and the race and education
#                                cuts matched to the SCF's (O1, O2)
# Any AD-1 miss outside the tolerance stops the script with an error after
# every table is written (as in 04); no year is exempt.
# Run from the project root as
#   Rscript code/08_headship_context.R
#
# Documentation: data/docs/cps/cps_asec_1989_2025.md (variables, 2014,
# weights), data/docs/cps/cps_asec_relate_crosswalk_1989_2025.md (RELATE
# codes by year), data/docs/cps/cps_asec_ad1_replication.md (AD-1).
#
# Caveats for every number written here:
# - The CPS householder is not the SCF reference person. The SCF unit is the
#   primary economic unit (PEU) and its reference person is chosen by SCF
#   rules; the CPS householder is the person in whose name the housing unit
#   is owned or rented. scf_rule_rp_share applies the SCF rule to CPS
#   households (section 5); householder_share does not.
# - The ASEC year and the SCF survey year are aligned as the same calendar
#   year. This is a choice (U9 Amendment), not a match of field periods: the
#   ASEC is collected in February-April, while the SCF is fielded over most
#   of the survey year and into the next (the 2025 codebook reports data
#   shipments from May 9, 2025 to May 20, 2026). The March ASEC of year t+1
#   is about as close to the median SCF interview as that of year t.
# - The CPS adult universe here starts at age 18, so the "under 25" band is
#   18-24 in the CPS (and "under 35" is 18-34). The SCF bands count every
#   reference person under 25 (1995 and 1998 include a few aged 17).
# - Citation: publications using these data must cite IPUMS CPS with the
#   data version (https://cps.ipums.org/cps/citation.shtml; also in the DDI).

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; calls below are
#    qualified as pkg::fn(), except data.table syntax inside `[`
required_packages <- c(
  "here", "ipumsr", "tidyselect", "data.table", "arrow", "readxl"
)
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

# 2) Stop unless this script sits at code/08_headship_context.R
here::i_am("code/08_headship_context.R")

path_ipums_raw <- here::here("data", "raw", "ipums_cps")
ipums_manifest_path <- file.path(path_ipums_raw, "manifest.csv")
ad1_path <- here::here("data", "raw", "census", "ad1.xls")
panel_path <- here::here(
  "data", "processed", "scf_panel_1989_2025.parquet"
)
path_tables <- here::here("output", "tables")
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
headship_output_path <- file.path(path_tables, "headship_context.csv")
validation_output_path <- file.path(
  path_tables, "headship_ad1_validation.csv"
)
bridged_output_path <- file.path(path_tables, "headship_bridged_changes.csv")
path_processed <- here::here("data", "processed")
dir.create(path_processed, recursive = TRUE, showWarnings = FALSE)
persons_checkpoint_stem <- file.path(path_processed, "cps_persons_15_44")

# 3) Years: every ASEC 1989-2025, and the 13 SCF waves (same calendar year)
asec_years <- 1989L:2025L
scf_wave_years <- seq(1989L, 2025L, by = 3L)
summary_years <- c(1989L, 2007L, 2019L, 2025L)

# 4) Age bands. CPS ages are inclusive; the CPS adult universe starts at 18.
#    SCF bands use the reference person's age with no lower bound, matching
#    the panel's age_band (02) and the pooled under-35 group (05)
age_bands <- data.table::data.table(
  age_band = c("under 25", "25-29", "30-34", "35-44", "under 35"),
  cps_age_min = c(18L, 25L, 30L, 35L, 18L),
  cps_age_max = c(24L, 29L, 34L, 44L, 34L),
  scf_age_min = c(0L, 25L, 30L, 35L, 0L),
  scf_age_max = c(24L, 29L, 34L, 44L, 34L)
)
age_bands[, cps_age_range := paste0(cps_age_min, "-", cps_age_max)]

# 5) RELATE codes by year, from data/docs/cps/
#    cps_asec_relate_crosswalk_1989_2025.md ("Codes by period"). A code
#    found outside its years, or not listed, stops the script (section 4).
#    - spouse: 0201 through 2018; 0202 and 0203 (opposite, same sex) from
#      2019
#    - child: 0301, plus 0303 (stepchild, coded apart in 1989-1993 only)
#    - partner: 1114 for 1995-2018; 1116 and 1117 from 2019
#    - partner_roommate: 1113 "Partner/roommate" in 1989-1994, which mixes
#      unmarried partners with roommates, so partner_share is NA then
#    - other: parent, sibling, grandchild, other relative, housemate,
#      roomer, foster child, other nonrelative
relate_map <- data.table::data.table(
  RELATE = c(
    101L, 201L, 202L, 203L, 301L, 303L, 1113L, 1114L, 1116L, 1117L,
    501L, 701L, 901L, 1001L, 1115L, 1241L, 1242L, 1260L
  ),
  first_year = c(
    1989L, 1989L, 2019L, 2019L, 1989L, 1989L, 1989L, 1995L, 2019L, 2019L,
    1989L, 1989L, 1989L, 1989L, 1995L, 1995L, 1989L, 1989L
  ),
  last_year = c(
    2025L, 2018L, 2025L, 2025L, 2025L, 1993L, 1994L, 2018L, 2025L, 2025L,
    2025L, 2025L, 2025L, 2025L, 2025L, 2025L, 2025L, 2025L
  ),
  relate_group = c(
    "householder", "spouse", "spouse", "spouse", "child", "child",
    "partner_roommate", "partner", "partner", "partner",
    rep("other", 8L)
  )
)
partner_first_year <- 1995L

# 6) AD-1 validation: |ours - published| <= 0.5 percentage points. AD-1
#    rounds percentages to 0.1, and our estimate uses the latest IPUMS
#    weights, so the bound allows rounding plus small weight-vintage noise.
ad1_tolerance_pp <- 0.5

# 7) AD-1 years compared on an adjusted weight basis (owner decision, Ben
#    Glasner, 2026-10-11; replaces the earlier blanket 2010 exemption).
#    IPUMS replaced the 2010-2012 ASEC weights based on 2000 controls with
#    weights based on 2010 controls (data/docs/cps/cps_asec_1989_2025.md,
#    ASECWT "Population controls"; cps_asec_ad1_replication.md, recipe
#    step 6). AD-1 has no revised 2010 row, so its 2010 cells keep the old
#    controls. AD-1 prints 2011 on both bases ("2011" and "2011r"), so its
#    own 2011r - 2011 difference, cell by cell (same age panel and sex),
#    measures the reweight effect. The 2010 published percent is moved by
#    that shift before the comparison, at the same 0.5-point tolerance:
#      adjusted published 2010 = published 2010 + (published 2011r -
#                                                  published 2011)
#    The unadjusted difference is written beside it. 2011 and 2021 compare
#    with their revised rows directly (step 42), and 2020 passes as is.
ad1_vintage_adjustments <- data.table::data.table(
  year = 2010L,
  bridge_year = 2011L,
  comparison_basis = "2010 adjusted by AD-1 2011r - 2011 shift"
)
comparison_basis_default <- "published"

# 8) Weight basis of IPUMS ASECWT by ASEC year (cps_asec_1989_2025.md,
#    ASECWT "Population controls"; cps_asec_ad1_replication.md, footnote
#    t). IPUMS keeps only the latest weights: 2010-2012 were reweighted to
#    2010-census controls, and 2020 and 2021 carry the 2020-census-based
#    (Blended Base) weights that Census recommends for comparisons with
#    2022 and later. 2025 uses the Vintage 2025 population estimates (AD-1
#    footnote t: 2024-2025 changes "reflect both demographic change and
#    methodological updates"). The docs do not name the controls behind
#    1989-2009, so those years are labeled as released, and any earlier
#    control change is not flagged. weight_break_flag is TRUE in the first
#    year of each new basis (2010, 2020, 2025), where the change from the
#    year before mixes behavior with reweighting. 2020 is also the
#    pandemic ASEC; ASECWTCVD is not in the extract.
weight_basis_table <- data.table::data.table(
  first_year = c(1989L, 2010L, 2013L, 2020L, 2025L),
  last_year = c(2009L, 2012L, 2019L, 2024L, 2025L),
  weight_basis = c(
    "pre-2010 controls (as released)",
    "2010-based (IPUMS reweight 2010-2012)",
    "2010-based",
    "2020-based",
    "2020-based, Vintage 2025 estimates"
  )
)
weight_break_years <- c(2010L, 2020L, 2025L)

# 9) The 2007-2010 change in the AD-1 child-of-householder share, raw and
#    bridged across the 2010 reweight (SCF waves on either side of it)
bridge_start_year <- 2007L
bridge_end_year <- 2010L

# 10) AD-1 years validated against an alternative definition (owner
#    decision, 2026-10-11). For 2003 the comparison uses child of the
#    householder EXCLUDING children who are married with spouse present
#    (MARST == 1). This definition is INFERRED from the data (the standard
#    definition misses in 2003 while the totals agree; this one matches
#    the published 2003 cells); Census does not document it. The same
#    0.5-point tolerance applies, and a 2003 miss still stops the script.
definition_default <- "child of householder"
definition_excl_married <- "inferred: excluding married spouse present"
ad1_alternative_definitions <- data.table::data.table(
  year = 2003L,
  definition_used = definition_excl_married
)

# 11) Race and ethnicity matched to the SCF's RACECL4 groups (O1 of the
#     MR4 plan; labels as race4 in 02). The CPS records each person's own
#     race; the SCF's describes its respondent. Hispanic (HISPAN 100-612) of
#     any race; otherwise White alone (RACE 100), Black alone (RACE 200), and
#     every other code, including each multiple-race code from 2003, as
#     other or multiple. HISPAN 901 and 902 (do not know, no response,
#     before 2003) count as not Hispanic, as IPUMS folds them into 000 from
#     2003 (cps_asec_1989_2025.md, HISPAN). Any other RACE or HISPAN code
#     stops the script.
race4_cps_labels <- c(
  "White non-Hispanic", "Black or African American non-Hispanic",
  "Hispanic or Latino", "Other or multiple race"
)
race_codes_valid <- c(100L, 200L, 300L, 650L, 651L, 652L, 700L, 801L:830L)
hispan_min_hispanic <- 100L
hispan_max_hispanic <- 612L
hispan_codes_not_hispanic <- c(0L, 901L, 902L)

# 12) Education matched to the SCF's EDCL groups (O2 of the MR4 plan;
#     labels as educ4 in 02), from IPUMS EDUC (cps_asec_1989_2025.md, EDUC).
#     Before 1992 EDUC counts years of school, so 1-3 years of college are
#     "some college" and 4 or more are "bachelor's or higher". Code 070
#     (grade 12, completion not stated) and 001 (NIU) are deliberately not
#     mapped: EDUC's universe is persons 15 and over, so either code among
#     them stops the script, as does any other unmapped code.
educ4_cps_labels <- c(
  "No high school diploma or GED", "High school diploma or GED",
  "Some college or associate degree", "Bachelor's degree or higher"
)
educ_map <- data.table::data.table(
  EDUC = c(
    2L, 10L, 11L, 12L, 13L, 14L, 20L, 21L, 22L, 30L, 31L, 32L, 40L, 50L,
    60L, 71L,
    72L, 73L,
    80L, 81L, 90L, 91L, 92L, 100L,
    110L, 111L, 120L, 121L, 122L, 123L, 124L, 125L
  ),
  educ4_code = c(
    rep(1L, 16L),
    rep(2L, 2L),
    rep(3L, 6L),
    rep(4L, 8L)
  )
)
educ_universe_min_age <- 15L

# 13) Ages kept in the person checkpoint: 15-44, so 10 can compare
#     householders with HH-3's "15 to 24" cell
checkpoint_min_age <- 15L
checkpoint_max_age <- 44L

run_start <- Sys.time()

################################################################################
###                       3. Read the IPUMS CPS extract                      ###
################################################################################

# 14) Find the data file and DDI codebook in the manifest written by 07
ipums_manifest <- utils::read.csv(
  ipums_manifest_path,
  colClasses = "character", na.strings = "", check.names = FALSE
)
ddi_rows <- ipums_manifest[ipums_manifest$file_role == "ipums_ddi", ]
data_rows <- ipums_manifest[ipums_manifest$file_role == "ipums_data", ]
if (nrow(ddi_rows) != 1L || nrow(data_rows) != 1L) {
  stop("Expected one ipums_ddi and one ipums_data row in ",
       ipums_manifest_path, call. = FALSE)
}
ddi_path <- here::here(ddi_rows$local_path)
data_path <- here::here(data_rows$local_path)
if (!file.exists(ddi_path) || !file.exists(data_path)) {
  stop("IPUMS extract files are missing: ", ddi_path, ", ", data_path,
       ". Run code/07_download_ipums_cps.R first.", call. = FALSE)
}

# 15) Read the variables used here; drop IPUMS labels so codes are integers
cps_variables <- c(
  "YEAR", "SERIAL", "PERNUM", "ASECWT", "AGE", "SEX", "RELATE", "MARST",
  "RACE", "HISPAN", "EDUC", "ASECFLAG", "HFLAG", "GQ"
)
message("Reading ", basename(data_path), " (", basename(ddi_path), ")")
cps_ddi <- ipumsr::read_ipums_ddi(ddi_path)
cps_raw <- ipumsr::read_ipums_micro(
  cps_ddi,
  vars = tidyselect::all_of(cps_variables),
  data_file = data_path, verbose = FALSE
)
cps <- data.table::as.data.table(ipumsr::zap_ipums_attributes(cps_raw))
rm(cps_raw)
message("Read ", format(nrow(cps), big.mark = ","), " person records in ",
        round(as.numeric(difftime(Sys.time(), run_start, units = "secs"))),
        " seconds")

# 16) Integer codes for the categorical variables
integer_columns <- c(
  "YEAR", "SERIAL", "PERNUM", "AGE", "SEX", "RELATE", "MARST", "RACE",
  "HISPAN", "EDUC", "ASECFLAG", "HFLAG", "GQ"
)
cps[, (integer_columns) := lapply(.SD, as.integer),
    .SDcols = integer_columns]

################################################################################
###                    4. Check the extract (DD-8 claims)                    ###
################################################################################

# 17) One ASEC sample per year 1989-2025, ASEC records only (ASECFLAG == 1;
#     a March Basic record would be 2)
extract_years <- sort(unique(cps$YEAR))
if (!identical(extract_years, asec_years)) {
  stop("Extract years are not exactly 1989-2025: ",
       paste(extract_years, collapse = ", "), call. = FALSE)
}
if (anyNA(cps$ASECFLAG) || any(cps$ASECFLAG != 1L)) {
  stop("ASECFLAG is not 1 for every record; the extract holds non-ASEC ",
       "(March Basic) records", call. = FALSE)
}

# 18) ASECWT: present and non-negative (ipumsr has already divided it by
#     10,000; a few records carry a zero weight in some years)
if (anyNA(cps$ASECWT) || any(cps$ASECWT < 0)) {
  stop("ASECWT is missing or negative", call. = FALSE)
}

# 19) HFLAG exists only in 2014, where both split files are present
hflag_years <- sort(unique(cps$YEAR[!is.na(cps$HFLAG)]))
hflag_codes_2014 <- sort(unique(cps$HFLAG[cps$YEAR == 2014L]))
if (!identical(hflag_years, 2014L) ||
      !identical(hflag_codes_2014, c(0L, 1L))) {
  stop("HFLAG is not 0/1 in 2014 and missing elsewhere", call. = FALSE)
}

# 20) 2014 weighting. Each HFLAG file (0 = 5/8 file, 1 = 3/8 file) is
#     weighted to the full population, so the pooled file sums to twice it
#     (cps_asec_1989_2025.md, ASECWT and HFLAG). Following the docs, we use
#     the full sample, as AD-1 does (its footnote s: the 98,000-address
#     sample), and halve ASECWT in 2014. Shares are unchanged by halving;
#     counts become population counts. Check: each file sums to within 3
#     percent of the mean of the 2013 and 2015 totals.
weight_by_year <- cps[, list(weight_sum = sum(ASECWT)), by = YEAR]
neighbor_total <- mean(weight_by_year[YEAR %in% c(2013L, 2015L), weight_sum])
weight_by_file_2014 <- cps[YEAR == 2014L,
                           list(weight_sum = sum(ASECWT)), by = HFLAG]
if (any(abs(weight_by_file_2014$weight_sum / neighbor_total - 1) > 0.03)) {
  stop("A 2014 HFLAG file does not sum to about the US population",
       call. = FALSE)
}
cps[, person_weight := ASECWT]
cps[YEAR == 2014L, person_weight := ASECWT / 2]

# 21) Exactly one householder (RELATE 0101) in every household (GQ == 1)
householders_per_unit <- cps[
  GQ == 1L,
  list(householder_count = sum(RELATE == 101L)),
  by = list(YEAR, SERIAL)
]
if (any(householders_per_unit$householder_count != 1L)) {
  stop("Some households (GQ == 1) do not have exactly one RELATE 0101 ",
       "person", call. = FALSE)
}

# 22) Map RELATE by year; stop on any code not mapped for its year
year_codes <- unique(cps[, list(YEAR, RELATE)])
year_codes_mapped <- merge(
  year_codes, relate_map,
  by = "RELATE", all.x = TRUE, sort = FALSE
)
unmapped_flag <- is.na(year_codes_mapped$relate_group) |
  year_codes_mapped$YEAR < year_codes_mapped$first_year |
  year_codes_mapped$YEAR > year_codes_mapped$last_year
if (any(unmapped_flag)) {
  unmapped_codes <- year_codes_mapped[unmapped_flag]
  stop("RELATE codes not mapped for their year (YEAR:RELATE): ",
       paste0(unmapped_codes$YEAR, ":", unmapped_codes$RELATE,
              collapse = ", "), call. = FALSE)
}
rows_before_join <- nrow(cps)
cps <- merge(
  cps, year_codes_mapped[, list(YEAR, RELATE, relate_group)],
  by = c("YEAR", "RELATE"), all.x = TRUE, sort = FALSE
)
if (nrow(cps) != rows_before_join || anyNA(cps$relate_group)) {
  stop("Joining relate_group changed the row count or left NAs",
       call. = FALSE)
}
message("RELATE mapped for every year: ", nrow(year_codes),
        " year-code pairs")

################################################################################
###                   5. SCF-rule reference person in the CPS                ###
################################################################################

# 23) The SCF reference person of a unit is the man in a mixed-sex couple,
#     the older partner in a same-sex couple, and otherwise the single
#     economically dominant person (methodology review MR-WEIGHTS,
#     review-reports/u9/; reconstruction in review-reports/u9/_scratch/
#     cps_scf_style_rp.R, matched to the SCF here in the SCF waves). In a
#     CPS household (GQ == 1) the unit is the householder's, and its couple
#     is the householder and the householder's own spouse or unmarried
#     partner (RELATE groups "spouse" and "partner"); another couple in the
#     household (a married child, say) is not the householder's couple.
#     Group-quarters residents (GQ == 2) are outside the SCF frame and are
#     never reference persons. Unmarried partners are identifiable from
#     1995 (code 1114); code 1113 in 1989-1994 mixes partners with
#     roommates, so in those years only spouses form couples, and
#     scf_rule_partners_identified_flag is FALSE (a woman householder with
#     a male partner then counts as the reference person). If more than one
#     spouse or partner of the householder is coded, the spouse comes
#     first, then the lowest PERNUM. A same-sex couple of equal age keeps
#     the householder.
if (anyDuplicated(cps, by = c("YEAR", "SERIAL", "PERNUM")) > 0L) {
  stop("YEAR, SERIAL, PERNUM does not identify persons uniquely",
       call. = FALSE)
}
householders <- cps[
  GQ == 1L & relate_group == "householder",
  list(YEAR, SERIAL, householder_pernum = PERNUM,
       householder_age = AGE, householder_sex = SEX)
]
couple_candidates <- cps[
  GQ == 1L & relate_group %in% c("spouse", "partner"),
  list(YEAR, SERIAL, PERNUM, relate_group, AGE, SEX)
]
couple_candidates[, partner_rank_int := data.table::fifelse(
  relate_group == "spouse", 1L, 2L
)]
data.table::setorder(couple_candidates, YEAR, SERIAL, partner_rank_int,
                     PERNUM)
couple_partners <- unique(couple_candidates, by = c("YEAR", "SERIAL"))
message("SCF rule: ", nrow(couple_partners), " householder couples; ",
        nrow(couple_candidates) - nrow(couple_partners),
        " extra spouse or partner records not used")
scf_rule_units <- merge(
  householders,
  couple_partners[, list(YEAR, SERIAL, partner_pernum = PERNUM,
                         partner_age = AGE, partner_sex = SEX)],
  by = c("YEAR", "SERIAL"), all.x = TRUE, sort = FALSE
)
if (nrow(scf_rule_units) != nrow(householders) ||
      sum(!is.na(scf_rule_units$partner_pernum)) != nrow(couple_partners)) {
  stop("Householder couples did not join one to one to the householders",
       call. = FALSE)
}
scf_rule_units[, partner_is_rp_flag := !is.na(partner_sex) & (
  (partner_sex != householder_sex & partner_sex == 1L) |
    (partner_sex == householder_sex & partner_age > householder_age)
)]
scf_rule_units[, rp_pernum := data.table::fifelse(
  partner_is_rp_flag, partner_pernum, householder_pernum
)]

# 24) Mark each unit's reference person (one per household), and keep them
#     at every age for the band numerators
cps[, scf_rule_rp_flag := FALSE]
cps[scf_rule_units, on = c("YEAR", "SERIAL", PERNUM = "rp_pernum"),
    scf_rule_rp_flag := TRUE]
if (sum(cps$scf_rule_rp_flag) != nrow(scf_rule_units)) {
  stop("SCF-rule reference persons do not number one per household",
       call. = FALSE)
}
message("SCF rule: the partner, not the householder, is the reference ",
        "person in ", sum(scf_rule_units$partner_is_rp_flag), " of ",
        nrow(scf_rule_units), " households")
scf_rule_rps <- cps[scf_rule_rp_flag == TRUE,
                    list(YEAR, AGE, person_weight)]
message("SCF rule: ", sum(scf_rule_rps$AGE < 18L), " reference-person ",
        "records are under 18; they count in the under-25 and under-35 ",
        "bands, as in the SCF bands")

################################################################################
###            6. SCF-rule roles, cuts, and the person checkpoint            ###
################################################################################

# 25) Each person's role in the SCF-rule unit of their household, for the
#     sex cut (O3 of the MR4 plan): rp_couple (reference person of a unit
#     with a couple), rp_single (reference person with no spouse or partner
#     identified), rp_partner (the other member of that couple), child
#     (child of the householder, in a household), group_quarters (GQ == 2),
#     and other (everyone else in a household, including a spouse or
#     partner not used in step 23). Before 1995 couples are spouses only
#     (step 23), so a cohabiting reference person is rp_single then.
couple_units <- scf_rule_units[!is.na(partner_pernum)]
couple_units[, rp_partner_pernum := data.table::fifelse(
  partner_is_rp_flag, householder_pernum, partner_pernum
)]
single_units <- scf_rule_units[is.na(partner_pernum)]
cps[, scf_rule_role := "other"]
cps[GQ == 2L, scf_rule_role := "group_quarters"]
cps[GQ == 1L & relate_group == "child", scf_rule_role := "child"]
cps[single_units, on = c("YEAR", "SERIAL", PERNUM = "rp_pernum"),
    scf_rule_role := "rp_single"]
cps[couple_units, on = c("YEAR", "SERIAL", PERNUM = "rp_pernum"),
    scf_rule_role := "rp_couple"]
cps[couple_units, on = c("YEAR", "SERIAL", PERNUM = "rp_partner_pernum"),
    scf_rule_role := "rp_partner"]
role_counts <- table(factor(
  cps$scf_rule_role,
  levels = c("rp_couple", "rp_single", "rp_partner")
))
role_rp_flag <- cps$scf_rule_role %in% c("rp_couple", "rp_single")
if (role_counts[["rp_couple"]] != nrow(couple_units) ||
      role_counts[["rp_partner"]] != nrow(couple_units) ||
      role_counts[["rp_single"]] != nrow(single_units) ||
      !identical(role_rp_flag, cps$scf_rule_rp_flag)) {
  stop("SCF-rule roles do not match the units of step 23 (one rp_couple ",
       "and one rp_partner per couple unit, one rp_single per other unit)",
       call. = FALSE)
}
message("SCF-rule roles: ", role_counts[["rp_couple"]], " couple units, ",
        role_counts[["rp_single"]], " single units")

# 26) Race and ethnicity (O1), for every person; stop on a code outside
#     the documented lists
race_invalid_flag <- is.na(cps$RACE) | !(cps$RACE %in% race_codes_valid)
hispanic_flag <- !is.na(cps$HISPAN) &
  cps$HISPAN >= hispan_min_hispanic & cps$HISPAN <= hispan_max_hispanic
hispan_invalid_flag <- !hispanic_flag &
  (is.na(cps$HISPAN) | !(cps$HISPAN %in% hispan_codes_not_hispanic))
if (any(race_invalid_flag) || any(hispan_invalid_flag)) {
  invalid_race <- unique(cps[race_invalid_flag, list(YEAR, RACE)])
  invalid_hispan <- unique(cps[hispan_invalid_flag, list(YEAR, HISPAN)])
  stop("Unmapped RACE codes (YEAR:RACE): ",
       paste0(invalid_race$YEAR, ":", invalid_race$RACE, collapse = ", "),
       "; unmapped HISPAN codes (YEAR:HISPAN): ",
       paste0(invalid_hispan$YEAR, ":", invalid_hispan$HISPAN,
              collapse = ", "), call. = FALSE)
}
race4_cps_code <- data.table::fifelse(
  hispanic_flag, 3L,
  data.table::fifelse(
    cps$RACE == 100L, 1L,
    data.table::fifelse(cps$RACE == 200L, 2L, 4L)
  )
)
cps[, race4_cps := factor(race4_cps_code, levels = 1:4,
                          labels = race4_cps_labels)]

# 27) Education (O2), for persons 15 and over (EDUC's universe); NA below
#     15. Stop on any code not in educ_map at those ages
cps[, educ4_code := NA_integer_]
cps[educ_map, on = "EDUC", educ4_code := i.educ4_code]
educ_universe_flag <- cps$AGE >= educ_universe_min_age
educ_unmapped_flag <- educ_universe_flag & is.na(cps$educ4_code)
if (any(educ_unmapped_flag)) {
  unmapped_educ <- unique(cps[educ_unmapped_flag, list(YEAR, EDUC)])
  data.table::setorder(unmapped_educ, YEAR, EDUC)
  stop("Unmapped EDUC codes among persons 15 and over (YEAR:EDUC): ",
       paste0(unmapped_educ$YEAR, ":", unmapped_educ$EDUC, collapse = ", "),
       call. = FALSE)
}
cps[!educ_universe_flag, educ4_code := NA_integer_]
cps[, educ4_cps := factor(educ4_code, levels = 1:4,
                          labels = educ4_cps_labels)]
cps[, educ4_code := NULL]

# 28) Person checkpoint for 09-11: ages 15-44, plus any SCF-rule reference
#     person under 15 (so 09 can rebuild the band numerators of step 31
#     exactly). Saved as .rds and .parquet with the same rows and order
#     (R coding rules, section 7)
checkpoint_mask <- cps$AGE <= checkpoint_max_age &
  (cps$AGE >= checkpoint_min_age | cps$scf_rule_rp_flag)
cps_persons <- cps[checkpoint_mask, list(
  YEAR, SERIAL, PERNUM, HFLAG, GQ, AGE, SEX, MARST, RELATE, relate_group,
  RACE, HISPAN, EDUC, race4_cps, educ4_cps, ASECWT, person_weight,
  scf_rule_rp_flag, scf_rule_role
)]
data.table::setorder(cps_persons, YEAR, SERIAL, PERNUM)
saveRDS(cps_persons, paste0(persons_checkpoint_stem, ".rds"))
arrow::write_parquet(cps_persons, paste0(persons_checkpoint_stem, ".parquet"))
message("Wrote ", format(nrow(cps_persons), big.mark = ","),
        " persons to ", persons_checkpoint_stem, ".rds and .parquet")
rm(cps_persons)

################################################################################
###            7. Persons aged 18-44 and their relationship flags            ###
################################################################################

# 29) Keep ages 18-44 (every band and both AD-1 panels); SEX must be 1 or 2
#     and GQ 1 or 2 (vacant units, GQ == 0, carry no persons)
cps_adults <- cps[AGE >= 18L & AGE <= 44L]
rm(cps)
if (anyNA(cps_adults$SEX) || !all(cps_adults$SEX %in% c(1L, 2L))) {
  stop("SEX is not 1 or 2 for every person aged 18-44", call. = FALSE)
}
if (anyNA(cps_adults$GQ) || !all(cps_adults$GQ %in% c(1L, 2L))) {
  stop("GQ is not 1 or 2 for every person aged 18-44", call. = FALSE)
}

# 30) Indicators for each relationship group and for group quarters. A
#     group-quarters resident (GQ == 2) is not in a household, so has no
#     householder: from 1994 IPUMS codes most of them RELATE 0101, but the
#     householder, spouse, partner, child, and other flags count only
#     persons in households (GQ == 1), and group-quarters residents form
#     their own category (gq_share). The categories partition every band.
#     child_ad1_flag is the AD-1 definition (RELATE child in any housing
#     type) and is the one the AD-1 validation uses: AD-1 counts every
#     child of a householder, including unmarried college students in
#     dormitories, whom the CPS lists in their parents' home.
cps_adults[, household_flag := GQ == 1L]
cps_adults[, group_quarters_flag := GQ == 2L]
cps_adults[, householder_flag := relate_group == "householder" &
             household_flag]
cps_adults[, spouse_flag := relate_group == "spouse" & household_flag]
cps_adults[, partner_flag := relate_group == "partner" & household_flag]
cps_adults[, partner_roommate_flag := relate_group == "partner_roommate" &
             household_flag]
cps_adults[, child_flag := relate_group == "child" & household_flag]
cps_adults[, child_ad1_flag := relate_group == "child"]
cps_adults[, other_flag := relate_group == "other" & household_flag]

################################################################################
###                    8. Headship shares by year and age band               ###
################################################################################

# 31) Weighted shares (ASECWT, halved in 2014) of all persons in the band,
#     including non-institutional group-quarters residents (GQ = 2), as AD-1
#     does. scf_rule_rp_share is the share of persons who would be the SCF
#     reference person of their unit (step 23): the selection rate into
#     the SCF's young-household universe, and the headline selection
#     column. Its numerator takes reference persons at the SCF band ages
#     (no lower bound, as the SCF bands and the calibration check do), so
#     the under-25 and under-35 bands also count the few reference persons
#     aged under 18 (step 24); its denominator is the CPS band.
#     householder_share_hh_only repeats the householder share among
#     persons in households (GQ == 1) only. child_share_ad1_definition is
#     the AD-1 child-of-householder share (any housing type).
headship_list <- vector("list", nrow(age_bands))
for (band_index in seq_len(nrow(age_bands))) {
  band_mask <- cps_adults$AGE >= age_bands$cps_age_min[band_index] &
    cps_adults$AGE <= age_bands$cps_age_max[band_index]
  band_rows <- cps_adults[band_mask]
  rp_band_mask <- scf_rule_rps$AGE >= age_bands$scf_age_min[band_index] &
    scf_rule_rps$AGE <= age_bands$scf_age_max[band_index]
  rp_band_table <- scf_rule_rps[rp_band_mask, list(
    scf_rule_rp_weighted = sum(person_weight)
  ), by = YEAR]
  band_table <- band_rows[, list(
    population_weighted = sum(person_weight),
    n_unweighted = .N,
    householder_share = sum(person_weight * householder_flag) /
      sum(person_weight),
    spouse_share = sum(person_weight * spouse_flag) / sum(person_weight),
    partner_share = sum(person_weight * partner_flag) / sum(person_weight),
    partner_roommate_share = sum(person_weight * partner_roommate_flag) /
      sum(person_weight),
    child_share = sum(person_weight * child_flag) / sum(person_weight),
    other_share = sum(person_weight * other_flag) / sum(person_weight),
    gq_share = sum(person_weight * group_quarters_flag) / sum(person_weight),
    child_share_ad1_definition = sum(person_weight * child_ad1_flag) /
      sum(person_weight),
    householder_share_hh_only =
      sum(person_weight * householder_flag * household_flag) /
      sum(person_weight * household_flag)
  ), by = YEAR]
  band_table <- merge(band_table, rp_band_table, by = "YEAR", sort = FALSE)
  if (nrow(band_table) != length(asec_years)) {
    stop("SCF-rule reference persons are missing for some years in band ",
         age_bands$age_band[band_index], call. = FALSE)
  }
  band_table[, scf_rule_rp_share :=
               scf_rule_rp_weighted / population_weighted]
  band_table[, age_band := age_bands$age_band[band_index]]
  band_table[, cps_age_range := age_bands$cps_age_range[band_index]]
  headship_list[[band_index]] <- band_table
}
headship <- data.table::rbindlist(headship_list)
data.table::setnames(headship, "YEAR", "year")

# 32) Partners cannot be told from roommates before 1995 (code 1113), so
#     partner_share is NA then; 1113 is reported as partner_roommate_share,
#     which is NA from 1995. The six household group shares and gq_share
#     sum to one in every row, and with no group-quarters householder,
#     householder_share equals householder_share_hh_only x (1 - gq_share).
headship[year < partner_first_year, partner_share := NA_real_]
headship[year >= partner_first_year, partner_roommate_share := NA_real_]
headship[, scf_rule_partners_identified_flag := year >= partner_first_year]
share_total <- headship$householder_share + headship$spouse_share +
  data.table::fcoalesce(headship$partner_share, 0) +
  data.table::fcoalesce(headship$partner_roommate_share, 0) +
  headship$child_share + headship$other_share + headship$gq_share
if (any(abs(share_total - 1) > 1e-9)) {
  stop("Relationship and group-quarters shares do not sum to one in some ",
       "year-band rows", call. = FALSE)
}
householder_gap <- headship$householder_share -
  headship$householder_share_hh_only * (1 - headship$gq_share)
if (any(abs(householder_gap) > 1e-9)) {
  stop("householder_share counts persons outside households in some ",
       "year-band rows", call. = FALSE)
}

# 33) Householder or spouse (all years), and householder, spouse, or
#     partner (from 1995): living in one's own household as its head or the
#     head's spouse or partner
headship[, householder_spouse_share := householder_share + spouse_share]
headship[, householder_spouse_partner_share :=
           householder_share + spouse_share + partner_share]
headship[, scf_wave := year %in% scf_wave_years]

# 34) Weight basis and break flags (step 8); every year gets one basis
headship[, weight_basis := NA_character_]
for (basis_index in seq_len(nrow(weight_basis_table))) {
  basis_mask <- headship$year >= weight_basis_table$first_year[basis_index] &
    headship$year <= weight_basis_table$last_year[basis_index]
  headship[basis_mask,
           weight_basis := weight_basis_table$weight_basis[basis_index]]
}
if (anyNA(headship$weight_basis)) {
  stop("Some ASEC years have no weight basis", call. = FALSE)
}
headship[, weight_break_flag := year %in% weight_break_years]

################################################################################
###                    9. SCF calibration check (SCF waves)                  ###
################################################################################

# 35) SCF weighted reference persons (PEUs) by wave and band. WGT is X42001
#     divided by 5 (dataset-registry.yaml, scf WGT), so the sum of wgt over
#     the five pooled implicates is the household count; no further division
#     by 5. Check: the 2025 pooled sum is the 134,963,691 households the
#     registry records. scf_n_implicate_rows counts case x implicate rows
#     (five per case); a case whose imputed age differs across implicates
#     can contribute rows to two bands.
scf_panel <- data.table::as.data.table(arrow::read_parquet(
  panel_path,
  col_select = c("year", "implicate", "wgt", "age")
))
scf_total_2025 <- sum(scf_panel[year == 2025L, wgt])
if (abs(scf_total_2025 / 134963691 - 1) > 1e-4) {
  stop("2025 pooled SCF wgt sums to ", round(scf_total_2025),
       ", not the registry's 134,963,691 households", call. = FALSE)
}
if (!identical(sort(unique(scf_panel$year)), scf_wave_years)) {
  stop("SCF panel waves are not the 13 waves 1989-2025", call. = FALSE)
}

scf_list <- vector("list", nrow(age_bands))
for (band_index in seq_len(nrow(age_bands))) {
  scf_band_mask <- scf_panel$age >= age_bands$scf_age_min[band_index] &
    scf_panel$age <= age_bands$scf_age_max[band_index]
  scf_band_table <- scf_panel[scf_band_mask, list(
    scf_rp_weighted = sum(wgt),
    scf_n_implicate_rows = .N
  ), by = year]
  scf_band_table[, age_band := age_bands$age_band[band_index]]
  scf_list[[band_index]] <- scf_band_table
}
scf_counts <- data.table::rbindlist(scf_list)
if (nrow(scf_counts) != length(scf_wave_years) * nrow(age_bands)) {
  stop("SCF counts are missing for some wave x band cells", call. = FALSE)
}

# 36) Join to the CPS rows (one SCF row per SCF-wave CPS row) and divide.
#     This ratio is a calibration check, not a headship measure: the SCF
#     weight is raked to control totals by fine age categories computed
#     from the March CPS of the survey year using SCF unit definitions
#     (Kennickell and Woodburn, 1997 revision, section IIIa,
#     https://www.federalreserve.gov/Pubs/OSS/oss2/papers/wgt95.pdf; the
#     2025 codebook still describes X42001 as a Kennickell-Woodburn
#     consistent weight). So SCF reference persons per CPS person should
#     reproduce scf_rule_rp_share, and a gap flags a difference in unit
#     rules, age measurement, or raking cells, not in SCF coverage.
rows_before_join <- nrow(headship)
headship <- merge(
  headship, scf_counts,
  by = c("year", "age_band"), all.x = TRUE, sort = FALSE
)
if (nrow(headship) != rows_before_join ||
      sum(!is.na(headship$scf_rp_weighted)) != nrow(scf_counts)) {
  stop("SCF counts did not join one to one to the SCF-wave CPS rows",
       call. = FALSE)
}
headship[, scf_rp_per_person_calibration_check :=
           scf_rp_weighted / population_weighted]

################################################################################
###                      10. Census Table AD-1 validation                    ###
################################################################################

# 37) Read the sheet as text and confirm its units
ad1_sheets <- readxl::excel_sheets(ad1_path)
ad1_sheet <- ad1_sheets[grepl("AD-1", ad1_sheets, fixed = TRUE)]
if (length(ad1_sheet) != 1L) {
  stop("Expected one AD-1 sheet in ", ad1_path, call. = FALSE)
}
ad1_cells <- as.matrix(suppressMessages(readxl::read_excel(
  ad1_path,
  sheet = ad1_sheet, col_names = FALSE, col_types = "text"
)))
ad1_cells <- trimws(ad1_cells)
if (!any(grepl("Numbers in thousands", ad1_cells, fixed = TRUE))) {
  stop("AD-1 does not state that numbers are in thousands", call. = FALSE)
}

# 38) Locate columns by label: the row holding "Men" and "Women", the
#     header rows below it down to the first age panel, and in each sex
#     block the "Total", "Child of householder", and "Percent" columns
row_labels <- ad1_cells[, 1L]
sex_row <- which(apply(ad1_cells, 1L, function(row_cells) {
  any(row_cells %in% "Men") && any(row_cells %in% "Women")
}))
first_panel_row <- which(grepl("18 to 24 years", row_labels, fixed = TRUE))
if (length(sex_row) != 1L || length(first_panel_row) != 1L ||
      first_panel_row <= sex_row + 1L) {
  stop("Cannot find the AD-1 sex header and the 18-24 panel", call. = FALSE)
}
header_rows <- seq.int(sex_row + 1L, first_panel_row - 1L)
column_sex <- ad1_cells[sex_row, ]
for (column_index in seq.int(2L, ncol(ad1_cells))) {
  if (is.na(column_sex[column_index])) {
    column_sex[column_index] <- column_sex[column_index - 1L]
  }
}
column_header <- apply(
  ad1_cells[header_rows, , drop = FALSE], 2L,
  function(header_cells) {
    paste(header_cells[!is.na(header_cells)], collapse = " ")
  }
)
ad1_columns <- data.table::CJ(
  sex = c("Men", "Women"),
  measure = c("Total", "Child of householder", "Percent"),
  sorted = FALSE
)
ad1_columns[, column_index := NA_integer_]
for (column_row in seq_len(nrow(ad1_columns))) {
  match_index <- which(
    column_sex %in% ad1_columns$sex[column_row] &
      column_header %in% ad1_columns$measure[column_row]
  )
  if (length(match_index) != 1L) {
    stop("AD-1: expected one '", ad1_columns$measure[column_row],
         "' column for ", ad1_columns$sex[column_row], call. = FALSE)
  }
  ad1_columns$column_index[column_row] <- match_index
}

# 39) Year rows are labeled "..YYYY" with an optional footnote letter
#     ("r" revised to the latest decennial census, "s" the 2014 split
#     sample, "t" the Vintage 2025 controls). Each row takes the age panel
#     of the nearest panel label above it.
panel_label_flag <- grepl("^[.]?(18 to 24|25 to 34) years$", row_labels)
year_label_flag <- grepl("^[.]{2}[0-9]{4}[a-z]?$", row_labels)
row_panel <- rep(NA_character_, length(row_labels))
current_panel <- NA_character_
for (row_index in seq_along(row_labels)) {
  if (panel_label_flag[row_index]) {
    current_panel <- if (grepl("18 to 24", row_labels[row_index])) {
      "18-24"
    } else {
      "25-34"
    }
  }
  row_panel[row_index] <- current_panel
}
year_rows <- which(year_label_flag)
if (anyNA(row_panel[year_rows])) {
  stop("AD-1 has a year row above the first age panel", call. = FALSE)
}
ad1_column_index <- stats::setNames(
  ad1_columns$column_index,
  paste(ad1_columns$sex, ad1_columns$measure)
)
ad1_rows <- data.table::data.table(
  age_panel = row_panel[year_rows],
  ad1_row_label = sub("^[.]{2}", "", row_labels[year_rows]),
  men_total = ad1_cells[year_rows, ad1_column_index[["Men Total"]]],
  men_child = ad1_cells[
    year_rows, ad1_column_index[["Men Child of householder"]]
  ],
  men_pct = ad1_cells[year_rows, ad1_column_index[["Men Percent"]]],
  women_total = ad1_cells[year_rows, ad1_column_index[["Women Total"]]],
  women_child = ad1_cells[
    year_rows, ad1_column_index[["Women Child of householder"]]
  ],
  women_pct = ad1_cells[year_rows, ad1_column_index[["Women Percent"]]]
)
ad1_rows[, year := as.integer(substr(ad1_row_label, 1L, 4L))]
ad1_rows[, revised_flag := grepl("r$", ad1_row_label)]
ad1_rows <- ad1_rows[year %in% asec_years]

# 40) Numbers. Every year row, revised or not, must be numeric
number_columns <- c(
  "men_total", "men_child", "men_pct", "women_total", "women_child",
  "women_pct"
)
ad1_rows[, (number_columns) := lapply(.SD, as.numeric),
         .SDcols = number_columns]
if (anyNA(ad1_rows[, number_columns, with = FALSE])) {
  stop("AD-1 has a missing or non-numeric value in 1989-2025",
       call. = FALSE)
}

# 41) Published values, long by sex, for every year row (including both
#     2011 rows). AD-1 has no both-sexes column, so the both-sexes percent
#     is derived from the published thousands (rounding of the thousands
#     moves it by well under 0.1 point)
ad1_men <- ad1_rows[, list(
  year, age_panel, ad1_row_label, revised_flag, sex = "men",
  published_total_thousands = men_total,
  published_child_thousands = men_child,
  published_pct = men_pct, published_pct_source = "table"
)]
ad1_women <- ad1_rows[, list(
  year, age_panel, ad1_row_label, revised_flag, sex = "women",
  published_total_thousands = women_total,
  published_child_thousands = women_child,
  published_pct = women_pct, published_pct_source = "table"
)]
ad1_both <- ad1_rows[, list(
  year, age_panel, ad1_row_label, revised_flag, sex = "both",
  published_total_thousands = men_total + women_total,
  published_child_thousands = men_child + women_child,
  published_pct = 100 * (men_child + women_child) /
    (men_total + women_total),
  published_pct_source = "derived from thousands"
)]
ad1_published_all <- data.table::rbindlist(
  list(ad1_men, ad1_women, ad1_both)
)

# 42) One row per year, panel, and sex: where AD-1 gives a revised row
#     (2011r, 2021r), use it, since IPUMS ASECWT carries the weights revised
#     to the latest decennial census for those years
#     (cps_asec_ad1_replication.md, recipe step 6); otherwise the single row
ad1_published_all[, has_revised := any(revised_flag),
                  by = list(year, age_panel, sex)]
ad1_published <- ad1_published_all[revised_flag | !has_revised]
rows_per_cell <- ad1_published[, .N, by = list(year, age_panel, sex)]
if (any(rows_per_cell$N != 1L) ||
      nrow(rows_per_cell) != 2L * 3L * length(asec_years)) {
  stop("AD-1 does not give exactly one row per year 1989-2025 in each ",
       "age panel and sex", call. = FALSE)
}

# 43) Reweight shift by cell (step 7): published 2011r - published 2011,
#     in points, from the two 2011 rows of each panel and sex
bridge_rows <- ad1_published_all[
  year %in% ad1_vintage_adjustments$bridge_year
]
bridge_rows_per_cell <- bridge_rows[
  , list(n_revised = sum(revised_flag), n_unrevised = sum(!revised_flag)),
  by = list(year, age_panel, sex)
]
if (nrow(bridge_rows_per_cell) != 2L * 3L * nrow(ad1_vintage_adjustments) ||
      any(bridge_rows_per_cell$n_revised != 1L) ||
      any(bridge_rows_per_cell$n_unrevised != 1L)) {
  stop("AD-1 does not give one revised and one unrevised row per panel ",
       "and sex in the bridge year(s) ",
       paste(ad1_vintage_adjustments$bridge_year, collapse = ", "),
       call. = FALSE)
}
reweight_shifts <- bridge_rows[, list(
  reweight_shift_pp = published_pct[revised_flag] -
    published_pct[!revised_flag]
), by = list(bridge_year = year, age_panel, sex)]
reweight_shifts <- merge(
  reweight_shifts,
  ad1_vintage_adjustments[, list(year, bridge_year, comparison_basis)],
  by = "bridge_year", sort = FALSE
)

# 44) Our estimates (AD-1 recipe, cps_asec_ad1_replication.md): denominator
#     every person aged 18-24 or 25-34 including GQ = 2; numerator RELATE
#     0301, plus 0303 in 1989-1993 (child_ad1_flag, any housing type);
#     weight ASECWT, halved in 2014 (counts only). The share under the
#     inferred definition (children not married with spouse present, MARST
#     != 1) is computed for every cell but compared only in the years
#     listed in ad1_alternative_definitions. ours_pct_hh_only (persons in
#     households, GQ == 1) is a sensitivity column that never enters the
#     pass test.
ad1_persons <- cps_adults[AGE <= 34L]
ad1_persons[, age_panel := data.table::fifelse(AGE <= 24L, "18-24", "25-34")]
ad1_persons[, sex := data.table::fifelse(SEX == 1L, "men", "women")]
ad1_persons_both <- data.table::copy(ad1_persons)
ad1_persons_both[, sex := "both"]
ad1_persons <- data.table::rbindlist(list(ad1_persons, ad1_persons_both))
rm(ad1_persons_both)
if (anyNA(ad1_persons$MARST)) {
  stop("MARST is missing for some persons aged 18-34", call. = FALSE)
}
ad1_persons[, child_not_married_sp_flag := child_ad1_flag & MARST != 1L]
ad1_ours <- ad1_persons[, list(
  ours_total_thousands = sum(person_weight) / 1000,
  child_default_thousands = sum(person_weight * child_ad1_flag) / 1000,
  child_excl_married_thousands =
    sum(person_weight * child_not_married_sp_flag) / 1000,
  child_hh_only_thousands =
    sum(person_weight * child_ad1_flag * household_flag) / 1000,
  total_hh_only_thousands = sum(person_weight * household_flag) / 1000,
  n_unweighted = .N
), by = list(year = YEAR, age_panel, sex)]
ad1_ours[, ours_pct_hh_only :=
           100 * child_hh_only_thousands / total_hh_only_thousands]
ad1_ours[, ours_pct_child_of_householder :=
           100 * child_default_thousands / ours_total_thousands]
ad1_ours[, ours_pct_excl_married_sp_present :=
           100 * child_excl_married_thousands / ours_total_thousands]

# 45) Compare cell by cell under the definition and comparison basis of
#     each year; every cell must fall within the tolerance
validation <- merge(
  ad1_published[, !c("revised_flag", "has_revised")], ad1_ours,
  by = c("year", "age_panel", "sex"), all = TRUE
)
expected_cells <- 2L * 3L * length(asec_years)
if (nrow(validation) != expected_cells || anyNA(validation$published_pct) ||
      anyNA(validation$ours_pct_child_of_householder)) {
  stop("AD-1 and our estimates do not match one to one (expected ",
       expected_cells, " cells)", call. = FALSE)
}
if (!all(ad1_vintage_adjustments$year %in% validation$year) ||
      !all(ad1_alternative_definitions$year %in% validation$year)) {
  stop("A vintage-adjusted or alternative-definition year is not in AD-1",
       call. = FALSE)
}
validation <- merge(
  validation,
  reweight_shifts[, list(year, age_panel, sex, reweight_shift_pp,
                         comparison_basis)],
  by = c("year", "age_panel", "sex"), all.x = TRUE
)
validation <- merge(
  validation, ad1_alternative_definitions,
  by = "year", all.x = TRUE
)
if (nrow(validation) != expected_cells ||
      sum(!is.na(validation$reweight_shift_pp)) !=
        2L * 3L * nrow(ad1_vintage_adjustments)) {
  stop("Joining reweight shifts or definitions changed the row count",
       call. = FALSE)
}
validation[is.na(definition_used), definition_used := definition_default]
validation[is.na(comparison_basis),
           comparison_basis := comparison_basis_default]
validation[, alternative_definition_flag :=
             definition_used == definition_excl_married]
validation[, ours_child_thousands := data.table::fifelse(
  alternative_definition_flag,
  child_excl_married_thousands, child_default_thousands
)]
validation[, ours_pct := data.table::fifelse(
  alternative_definition_flag,
  ours_pct_excl_married_sp_present, ours_pct_child_of_householder
)]
validation[, published_pct_compared :=
             published_pct + data.table::fcoalesce(reweight_shift_pp, 0)]
validation[, diff_pp := ours_pct - published_pct_compared]
validation[, diff_pp_unadjusted := ours_pct - published_pct]
validation[, tolerance_pp := ad1_tolerance_pp]
validation[, within_tolerance := abs(diff_pp) <= tolerance_pp]
validation[, pass := within_tolerance]
validation[, sex := factor(sex, levels = c("men", "women", "both"))]
data.table::setorder(validation, age_panel, sex, year)
validation_table <- validation[, list(
  year, ad1_row_label, age_panel, sex, definition_used, comparison_basis,
  published_pct_source, published_total_thousands,
  published_child_thousands, published_pct, reweight_shift_pp,
  published_pct_compared, ours_total_thousands, ours_child_thousands,
  ours_pct, diff_pp, diff_pp_unadjusted, tolerance_pp, within_tolerance,
  pass, ours_pct_child_of_householder, ours_pct_excl_married_sp_present,
  ours_pct_hh_only, n_unweighted
)]

# 46) The 2007-2010 change in the AD-1 child-of-householder share (our
#     estimate, AD-1 definition), raw and bridged. 2007 is on the pre-2010
#     controls and 2010 on the 2010-based ones (step 8), so the raw change
#     mixes behavior with the reweight. The bridged change puts 2010 back
#     on the old basis with AD-1's own 2011r - 2011 shift for the same cell
#     (step 43): bridged = (ours 2010 - shift) - ours 2007. AD-1's
#     published change, on one basis throughout, is shown beside them.
bridge_start <- validation[year == bridge_start_year, list(
  age_panel, sex, ours_start_pct = ours_pct_child_of_householder,
  published_start_pct = published_pct
)]
bridge_end <- validation[year == bridge_end_year, list(
  age_panel, sex, ours_end_pct = ours_pct_child_of_householder,
  published_end_pct = published_pct, reweight_shift_pp
)]
bridged_changes <- merge(
  bridge_start, bridge_end,
  by = c("age_panel", "sex"), sort = FALSE
)
if (nrow(bridged_changes) != 2L * 3L ||
      anyNA(bridged_changes$reweight_shift_pp)) {
  stop("The 2007-2010 bridge is missing a cell or its reweight shift",
       call. = FALSE)
}
start_basis_flag <- bridge_start_year >= weight_basis_table$first_year &
  bridge_start_year <= weight_basis_table$last_year
end_basis_flag <- bridge_end_year >= weight_basis_table$first_year &
  bridge_end_year <= weight_basis_table$last_year
bridged_changes[, start_year := bridge_start_year]
bridged_changes[, end_year := bridge_end_year]
bridged_changes[, weight_basis_start :=
                  weight_basis_table$weight_basis[start_basis_flag]]
bridged_changes[, weight_basis_end :=
                  weight_basis_table$weight_basis[end_basis_flag]]
bridged_changes[, raw_change_pp := ours_end_pct - ours_start_pct]
bridged_changes[, bridged_change_pp :=
                  ours_end_pct - reweight_shift_pp - ours_start_pct]
bridged_changes[, published_change_pp :=
                  published_end_pct - published_start_pct]
bridged_changes[, bridge_source := "AD-1 2011r - 2011, same cell"]
data.table::setorder(bridged_changes, age_panel, sex)
bridged_table <- bridged_changes[, list(
  start_year, end_year, age_panel, sex, weight_basis_start,
  weight_basis_end, ours_start_pct, ours_end_pct, raw_change_pp,
  reweight_shift_pp, bridged_change_pp, published_start_pct,
  published_end_pct, published_change_pp, bridge_source
)]

################################################################################
###                       11. Write the tables and log                       ###
################################################################################

# 47) headship_context.csv: one row per ASEC year x age band. The headline
#     selection column, scf_rule_rp_share, comes first after the counts;
#     the relationship shares and gq_share partition the band
headship[, age_band := factor(age_band, levels = age_bands$age_band)]
data.table::setorder(headship, year, age_band)
headship_table <- headship[, list(
  year, scf_wave, age_band, cps_age_range, weight_basis, weight_break_flag,
  population_weighted, n_unweighted, scf_rule_rp_share,
  scf_rule_partners_identified_flag, householder_share, spouse_share,
  partner_share, partner_roommate_share, child_share, other_share,
  gq_share, householder_spouse_share, householder_spouse_partner_share,
  householder_share_hh_only, child_share_ad1_definition, scf_rp_weighted,
  scf_n_implicate_rows, scf_rp_per_person_calibration_check
)]
data.table::fwrite(headship_table, headship_output_path)
message("Wrote ", nrow(headship_table), " rows to ", headship_output_path)

# 48) headship_ad1_validation.csv and headship_bridged_changes.csv
data.table::fwrite(validation_table, validation_output_path)
message("Wrote ", nrow(validation_table), " comparisons to ",
        validation_output_path)
data.table::fwrite(bridged_table, bridged_output_path)
message("Wrote ", nrow(bridged_table), " bridged changes to ",
        bridged_output_path)

# 49) Diagnostic summary: under 35 (CPS 18-34) in four SCF waves, and the
#     largest gap between the calibration check and scf_rule_rp_share
summary_rows <- headship_table[
  age_band == "under 35" & year %in% summary_years
]
for (row_index in seq_len(nrow(summary_rows))) {
  message(
    "Summary ", summary_rows$year[row_index], " under 35 (CPS 18-34): ",
    "SCF-rule reference person ",
    sprintf("%.3f", summary_rows$scf_rule_rp_share[row_index]),
    ", householder ",
    sprintf("%.3f", summary_rows$householder_share[row_index]),
    ", child of householder (AD-1 definition) ",
    sprintf("%.3f", summary_rows$child_share_ad1_definition[row_index]),
    ", SCF calibration check ",
    sprintf("%.3f",
            summary_rows$scf_rp_per_person_calibration_check[row_index])
  )
}
calibration_rows <- headship_table[
  age_band == "under 35" & scf_wave == TRUE
]
calibration_gap <- calibration_rows$scf_rp_per_person_calibration_check -
  calibration_rows$scf_rule_rp_share
message(
  "Calibration check, under 35: SCF reference persons per CPS person ",
  "within 0.004 of scf_rule_rp_share in ", sum(abs(calibration_gap) <= 0.004),
  " of ", nrow(calibration_rows), " waves; largest gap ",
  sprintf("%+.4f", calibration_gap[which.max(abs(calibration_gap))]), " (",
  calibration_rows$year[which.max(abs(calibration_gap))], ")"
)
for (row_index in seq_len(nrow(bridged_table))) {
  message(
    "Bridged ", bridge_start_year, "-", bridge_end_year, " change, child ",
    "of householder ", bridged_table$age_panel[row_index], " ",
    bridged_table$sex[row_index], ": raw ",
    sprintf("%+.2f", bridged_table$raw_change_pp[row_index]),
    ", bridged ",
    sprintf("%+.2f", bridged_table$bridged_change_pp[row_index]),
    ", AD-1 published ",
    sprintf("%+.2f", bridged_table$published_change_pp[row_index]),
    " points"
  )
}

# 50) Validation summary, adjusted and alternative-definition cells, and
#     run time
worst_row <- validation_table[which.max(abs(diff_pp))]
message(
  "AD-1 validation: ", sum(validation_table$pass), " of ",
  nrow(validation_table), " cells within ", ad1_tolerance_pp,
  " points (no exemptions); worst ", worst_row$year, " ",
  worst_row$age_panel, " ", worst_row$sex, " (ours ",
  round(worst_row$ours_pct, 2L), ", compared ",
  round(worst_row$published_pct_compared, 2L), ")"
)
adjusted_rows <- validation_table[
  comparison_basis != comparison_basis_default
]
message(
  "AD-1 vintage-adjusted checks (",
  paste(unique(adjusted_rows$comparison_basis), collapse = "; "), "): ",
  nrow(adjusted_rows), " cells, ", sum(adjusted_rows$within_tolerance),
  " within tolerance; worst adjusted ",
  sprintf("%+.2f", adjusted_rows[which.max(abs(diff_pp)), diff_pp]),
  " points, worst unadjusted ",
  sprintf("%+.2f",
          adjusted_rows[which.max(abs(diff_pp_unadjusted)),
                        diff_pp_unadjusted]),
  " points"
)
alternative_rows <- validation_table[definition_used != definition_default]
message(
  "AD-1 alternative-definition checks (", definition_excl_married, "): ",
  nrow(alternative_rows), " cells (year ",
  paste(unique(alternative_rows$year), collapse = ", "), "), ",
  sum(alternative_rows$within_tolerance), " within tolerance; worst ",
  sprintf("%+.2f", alternative_rows[which.max(abs(diff_pp)), diff_pp]),
  " points"
)
message(
  "Finished in ",
  round(as.numeric(difftime(Sys.time(), run_start, units = "mins")), 2L),
  " minutes"
)

################################################################################
###                        12. Fail on any AD-1 miss                         ###
################################################################################

# 51) Stop with a non-zero exit when any cell misses
failures <- validation_table[pass == FALSE]
if (nrow(failures) > 0L) {
  stop(
    nrow(failures), " of ", nrow(validation_table), " AD-1 comparisons ",
    "are outside the ", ad1_tolerance_pp, "-point tolerance:\n",
    paste0(
      "  ", failures$ad1_row_label, " ", failures$age_panel, " ",
      failures$sex, " (", failures$definition_used, "; ",
      failures$comparison_basis, "): compared ",
      round(failures$published_pct_compared, 2L),
      ", ours ", round(failures$ours_pct, 2L), " (",
      sprintf("%+.2f", failures$diff_pp), " points)",
      collapse = "\n"
    ),
    call. = FALSE
  )
}
message("All ", nrow(validation_table), " AD-1 comparisons passed.")
