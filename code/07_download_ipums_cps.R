# 07_download_ipums_cps -- download the IPUMS CPS ASEC extract and Census AD-1
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U9b of records/plans/2026-10-11_stage1-scf-pipeline.md (U9 Amendment),
# extended by U2 of records/plans/2026-10-11_mr4-headship-selection.md.
# This script only downloads: it defines, submits, waits for, and downloads
# three IPUMS CPS microdata extracts (ASEC 1989-2025, person level; the
# ASEC person replicate weights for 2005-2025, ages 15-44; and the
# cohabiting-partner pointer PECOHAB for 2007-2025), and it downloads
# five Census tables (AD-1, HH-3, UC-1, and UC-3 for 2018 and 2019). It never
# opens the data. Run it from the project root as
#   Rscript code/07_download_ipums_cps.R
# A re-run skips every file whose SHA-256 matches data/raw/ipums_cps/
# manifest.csv, and needs no API key when everything is already present. If
# an extract was submitted but not downloaded, a re-run resumes it by the
# extract number in the manifest instead of submitting a new one. Each
# extract is handled on its own: a complete one is skipped while the other
# is submitted or resumed.
#
# API key: ipumsr reads the environment variable IPUMS_API_KEY. If it is
# unset, this script reads the user .Renviron (Documents/.Renviron, where
# RStudio on Windows keeps it, then ~/.Renviron). The key is never printed,
# logged, or written anywhere by this script.
#
# Citation: the IPUMS terms of use require every publication that uses these
# data to cite IPUMS CPS, with the data version. Take the exact citation from
# https://cps.ipums.org/cps/citation.shtml (it is also in the extract's DDI
# codebook, data/raw/ipums_cps/cps_*.xml) when the data are used in a work
# product.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; all calls below
#    are qualified as pkg::fn()
required_packages <- c("here", "httr2", "digest", "ipumsr")
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

# 2) Stop unless this script sits at code/07_download_ipums_cps.R
here::i_am("code/07_download_ipums_cps.R")

path_ipums_raw <- here::here("data", "raw", "ipums_cps")
path_census_raw <- here::here("data", "raw", "census")
path_extracts <- here::here("code", "extracts")
manifest_path <- file.path(path_ipums_raw, "manifest.csv")
dir.create(path_ipums_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(path_census_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(path_extracts, recursive = TRUE, showWarnings = FALSE)

# 3) The IPUMS CPS extracts: one ASEC sample (cpsYYYY_03s) per year, person
#    level (rectangular on persons), fixed-width, the ipumsr default. IPUMS
#    adds its preselected technical variables to these. One list element per
#    extract; its manifest roles are <role_prefix>_data, _ddi, and
#    _extract_definition.
#    - main (U9b): 1989-2025, the variables 08 reads. Its roles keep the
#      U9b names (ipums_data, ...), which 08 reads.
#    - repwt (MR4 plan U2): 2005-2025, the 160 person replicate weights
#      REPWTP1-REPWTP160 ("only available for CPS from 2005 and on"; IPUMS
#      REPWTP page, read 2026-10-11), for persons aged 15-44 only (a case
#      selection on AGE), to be joined to the main extract on YEAR, SERIAL,
#      and PERNUM. The request names REPWTP; the DDI lists the 160 weights.
#    - cohab (MR4 plan U2, amended 2026-10-11): 2007-2025, every person,
#      with PECOHAB (the LINENO of a cohabiting partner in the household,
#      0 if none; "Since 2007", IPUMS PECOHAB page, read 2026-10-11) and
#      LINENO, for the unmarried-couple benchmarks in 10. AGE, SEX, RELATE,
#      and ASECWT let 10 check the join to the main extract.
ipums_collection <- "cps"
asec_years <- 1989L:2025L
asec_variables <- c(
  "YEAR", "SERIAL", "PERNUM", "ASECWT", "AGE", "SEX", "RELATE", "MARST",
  "RACE", "HISPAN", "EDUC", "ASECFLAG", "HFLAG", "GQ"
)
repwt_years <- 2005L:2025L
repwt_variables <- c("YEAR", "SERIAL", "PERNUM", "ASECWT", "AGE", "REPWTP")
repwt_replicate_count <- 160L
cohab_years <- 2007L:2025L
cohab_variables <- c(
  "YEAR", "SERIAL", "PERNUM", "LINENO", "ASECWT", "AGE", "SEX", "RELATE",
  "PECOHAB"
)
ipums_extracts <- list(
  main = list(
    role_prefix = "ipums",
    years = asec_years,
    variables = asec_variables,
    age_selection = integer(0L),
    ddi_variables = asec_variables,
    description = paste(
      "EIG SCF young cohorts (U9): CPS ASEC 1989-2025, person level,",
      "for headship context"
    ),
    json_path = file.path(
      "code", "extracts", "ipums_cps_asec_1989_2025.json"
    )
  ),
  repwt = list(
    role_prefix = "ipums_repwt",
    years = repwt_years,
    variables = repwt_variables,
    age_selection = 15L:44L,
    ddi_variables = c(
      setdiff(repwt_variables, "REPWTP"),
      paste0("REPWTP", seq_len(repwt_replicate_count))
    ),
    description = paste(
      "EIG SCF young cohorts (MR4 U2): CPS ASEC 2005-2025 person replicate",
      "weights, ages 15-44, for standard errors"
    ),
    json_path = file.path(
      "code", "extracts", "ipums_cps_asec_repwt_2005_2025.json"
    )
  ),
  cohab = list(
    role_prefix = "ipums_cohab",
    years = cohab_years,
    variables = cohab_variables,
    age_selection = integer(0L),
    ddi_variables = cohab_variables,
    description = paste(
      "EIG SCF young cohorts (MR4 U2): CPS ASEC 2007-2025 cohabiting",
      "partner pointer, for the unmarried-couple benchmarks"
    ),
    json_path = file.path(
      "code", "extracts", "ipums_cps_asec_cohab_2007_2025.json"
    )
  )
)
ipums_source_page <- "https://cps.ipums.org/cps/"
wait_timeout_seconds <- 3600L
wait_max_delay_seconds <- 60L
# Download cap for one IPUMS data file (MR4 plan, D5), checked against the
# size IPUMS reports before the download when it reports one
ipums_cap_bytes <- 2e9

# 4) Census tables, one row each: AD-1 (living arrangements of adults 18
#    and over), the U9c validation target; and, for the MR4 plan (U4,
#    downloads D1-D4 approved 2026-10-11): HH-3 (households by age of
#    householder), UC-1 (opposite-sex unmarried couples), and UC-3 from the
#    2018 and 2019 Families and Living Arrangements tables (unmarried
#    couples, either side of the 2019 RELATE change)
census_base_url <- "https://www2.census.gov/programs-surveys/demo/tables/"
census_page_url <- "https://www.census.gov/data/tables/"
census_files <- data.frame(
  file_role = c(
    "census_ad1", "census_hh3", "census_uc1", "census_uc3_2018",
    "census_uc3_2019"
  ),
  url = paste0(census_base_url, c(
    "families/time-series/adults/ad1.xls",
    "families/time-series/households/hh3.xls",
    "families/time-series/adults/uc1.xls",
    "families/2018/cps-2018/tabuc3-all.xls",
    "families/2019/cps-2019/tabuc3-all.xls"
  )),
  source_page = paste0(census_page_url, c(
    "time-series/demo/families/adults.html",
    "time-series/demo/families/households.html",
    "time-series/demo/families/adults.html",
    "2018/demo/families/cps-2018.html",
    "2019/demo/families/cps-2019.html"
  )),
  local_path = file.path("data", "raw", "census", c(
    "ad1.xls", "hh3.xls", "uc1.xls", "tabuc3_2018.xls", "tabuc3_2019.xls"
  )),
  notes = c(
    paste(
      "Census Table AD-1, young adults living at home",
      "(historical living arrangements of adults); U9c validation target"
    ),
    "Census Table HH-3, households by age of householder; MR4 U4 target",
    "Census Table UC-1, opposite-sex unmarried couples; MR4 U4 target",
    paste(
      "Census Table UC3, Families and Living Arrangements 2018,",
      "unmarried couples; MR4 U4 target"
    ),
    paste(
      "Census Table UC3, Families and Living Arrangements 2019,",
      "unmarried couples; MR4 U4 target"
    )
  ),
  stringsAsFactors = FALSE
)

# 5) Download limits and the manifest columns (as in data/raw/scf)
max_retries_int <- 2L
transient_status_codes <- c(408L, 429L, 500L, 502L, 503L, 504L)
user_agent_text <- "EIG-SCF-Young-Cohort-Finances 07_download_ipums_cps.R"

manifest_columns <- c(
  "year", "file_role", "source_page", "url", "local_path",
  "download_date", "size_bytes", "sha256", "notes"
)

run_start <- Sys.time()

################################################################################
###                           3. Define helpers                              ###
################################################################################

# 6) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

# 7) Perform a request, retrying a transient failure at most max_retries_int
#    times; an HTTP error status is returned, not raised
perform_with_retry <- function(request, label, path = NULL) {
  request <- httr2::req_error(request, is_error = function(resp) FALSE)
  for (attempt_int in seq_len(max_retries_int + 1L)) {
    outcome <- tryCatch(
      httr2::req_perform(request, path = path),
      error = function(e) e
    )
    network_error_flag <- inherits(outcome, "error")
    transient_status_flag <- !network_error_flag &&
      httr2::resp_status(outcome) %in% transient_status_codes
    if (!network_error_flag && !transient_status_flag) {
      return(outcome)
    }
    problem_text <- if (network_error_flag) {
      conditionMessage(outcome)
    } else {
      paste("HTTP", httr2::resp_status(outcome))
    }
    if (attempt_int > max_retries_int) {
      stop(
        label, " failed after ", max_retries_int, " retries: ", problem_text,
        call. = FALSE
      )
    }
    log_step(paste0(
      "RETRY ", attempt_int, " of ", max_retries_int, " for ", label,
      " after: ", problem_text
    ))
    Sys.sleep(5L * attempt_int)
  }
}

# 8) Classify a file against the manifest: "pending" (absent), "record"
#    (present, no manifest row), "skip" (hash matches), or "mismatch"
classify_file <- function(local_path, manifest) {
  full_path <- here::here(local_path)
  if (!file.exists(full_path)) {
    return("pending")
  }
  manifest_index <- match(local_path, manifest$local_path)
  if (is.na(manifest_index)) {
    return("record")
  }
  file_hash <- digest::digest(file = full_path, algo = "sha256")
  if (identical(file_hash, manifest$sha256[manifest_index])) {
    "skip"
  } else {
    "mismatch"
  }
}

# 9) Replace the manifest row for one file (hashing it from disk), sort the
#    manifest by path, and write it, so a stopped run can resume
write_manifest_row <- function(manifest, year, file_role, source_page, url,
                               local_path, download_date, notes) {
  full_path <- here::here(local_path)
  new_row <- data.frame(
    year = year,
    file_role = file_role,
    source_page = source_page,
    url = url,
    local_path = local_path,
    download_date = download_date,
    size_bytes = format(file.size(full_path), scientific = FALSE),
    sha256 = digest::digest(file = full_path, algo = "sha256"),
    notes = notes,
    stringsAsFactors = FALSE
  )
  manifest <- manifest[manifest$local_path != local_path, , drop = FALSE]
  manifest <- rbind(manifest, new_row)
  manifest <- manifest[order(manifest$local_path), , drop = FALSE]
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  manifest
}

################################################################################
###                            4. Read the manifest                          ###
################################################################################

# 10) Read the manifest with every column as character, or start an empty one
if (file.exists(manifest_path)) {
  manifest <- utils::read.csv(
    manifest_path,
    colClasses = "character", na.strings = "", check.names = FALSE
  )
  if (!identical(names(manifest), manifest_columns)) {
    stop(
      "Manifest columns do not match the expected columns in ",
      manifest_path, call. = FALSE
    )
  }
  if (anyDuplicated(manifest$local_path) > 0L) {
    stop("Manifest has duplicate local_path rows: ", manifest_path,
         call. = FALSE)
  }
} else {
  manifest <- as.data.frame(
    stats::setNames(
      replicate(length(manifest_columns), character(0L), simplify = FALSE),
      manifest_columns
    ),
    stringsAsFactors = FALSE
  )
}
log_step(paste("Manifest rows at start:", nrow(manifest)))

################################################################################
###                        5. Download the Census tables                     ###
################################################################################

# 11) For each Census table: skip, record, or download it; stop on a hash
#     mismatch without touching the file
census_status <- stats::setNames(
  character(nrow(census_files)), census_files$file_role
)
for (census_index in seq_len(nrow(census_files))) {
  census_role <- census_files$file_role[census_index]
  census_url <- census_files$url[census_index]
  census_page <- census_files$source_page[census_index]
  census_path <- census_files$local_path[census_index]
  census_notes <- census_files$notes[census_index]
  census_status[[census_role]] <- classify_file(census_path, manifest)
  if (census_status[[census_role]] == "mismatch") {
    stop(
      "SHA-256 of ", census_path, " does not match the manifest; nothing ",
      "was overwritten. Inspect, then move or delete the file or its ",
      "manifest row by hand.", call. = FALSE
    )
  } else if (census_status[[census_role]] == "skip") {
    log_step(paste("SKIP (sha256 matches manifest):", census_path))
  } else if (census_status[[census_role]] == "record") {
    log_step(paste("RECORD (present, no manifest row):", census_path))
    manifest <- write_manifest_row(
      manifest, NA_character_, census_role, census_page, census_url,
      census_path, NA_character_,
      paste0(
        census_notes, "; present before a manifest row existed, so hashed ",
        "from disk (file modified ",
        format(file.mtime(here::here(census_path)), "%Y-%m-%d"), ")"
      )
    )
  } else {
    # 12) Download to a .part file; reject an error status, an HTML page,
    #     an empty file, or a size that differs from Content-Length
    census_full_path <- here::here(census_path)
    census_part_path <- paste0(census_full_path, ".part")
    unlink(census_part_path)
    log_step(paste("DOWNLOAD:", census_url, "->", census_path))
    census_request <- httr2::request(census_url)
    census_request <- httr2::req_user_agent(census_request, user_agent_text)
    census_request <- httr2::req_timeout(census_request, 300L)
    census_response <- perform_with_retry(
      census_request, paste("download", census_url), path = census_part_path
    )
    census_status_code <- httr2::resp_status(census_response)
    census_content_type <- httr2::resp_header(
      census_response, "content-type", ""
    )
    census_length_bytes <- suppressWarnings(as.numeric(
      httr2::resp_header(census_response, "content-length", NA_character_)
    ))
    census_part_bytes <- file.size(census_part_path)
    census_problem <- if (census_status_code != 200L) {
      paste("HTTP", census_status_code)
    } else if (grepl("text/html", census_content_type, ignore.case = TRUE)) {
      "server returned an HTML page"
    } else if (is.na(census_part_bytes) || census_part_bytes == 0) {
      "empty file"
    } else if (!is.na(census_length_bytes) && census_length_bytes > 0 &&
                 census_part_bytes != census_length_bytes) {
      paste0(
        "size ", census_part_bytes, " differs from Content-Length ",
        census_length_bytes
      )
    } else {
      NA_character_
    }
    if (!is.na(census_problem)) {
      unlink(census_part_path)
      stop("Download of ", census_url, " failed: ", census_problem,
           call. = FALSE)
    }
    if (!file.rename(census_part_path, census_full_path)) {
      stop("Could not rename ", census_part_path, " to ", census_full_path,
           call. = FALSE)
    }
    manifest <- write_manifest_row(
      manifest, NA_character_, census_role, census_page, census_url,
      census_path, format(Sys.Date(), "%Y-%m-%d"), census_notes
    )
    log_step(paste0(
      "Saved ", census_path, " (", format(census_part_bytes, big.mark = ","),
      " bytes)"
    ))
  }
}

################################################################################
###                  6. Check the IPUMS files against the manifest           ###
################################################################################

# 13) An extract is complete when the manifest has exactly one row for each
#     of its three roles and every file matches its hash; when both are
#     complete, nothing below needs the API key
extract_complete_flag <- stats::setNames(
  logical(length(ipums_extracts)), names(ipums_extracts)
)
for (extract_name in names(ipums_extracts)) {
  extract_roles <- paste0(
    ipums_extracts[[extract_name]]$role_prefix,
    c("_data", "_ddi", "_extract_definition")
  )
  extract_status <- vapply(extract_roles, function(file_role) {
    role_index <- which(manifest$file_role == file_role)
    if (length(role_index) != 1L) {
      return("pending")
    }
    classify_file(manifest$local_path[role_index], manifest)
  }, character(1L))
  mismatch_roles <- names(extract_status)[extract_status == "mismatch"]
  if (length(mismatch_roles) > 0L) {
    stop(
      "SHA-256 does not match the manifest for: ",
      paste(manifest$local_path[manifest$file_role %in% mismatch_roles],
            collapse = ", "),
      ". Nothing was overwritten. Inspect, then move or delete the file or ",
      "its manifest row by hand.", call. = FALSE
    )
  }
  extract_complete_flag[[extract_name]] <- all(extract_status == "skip")
  if (extract_complete_flag[[extract_name]]) {
    log_step(paste0(
      "SKIP (sha256 matches manifest): IPUMS ", extract_name,
      " extract data, DDI, and definition"
    ))
  }
}
all_extracts_complete_flag <- all(extract_complete_flag)
if (all_extracts_complete_flag) {
  log_step("Every IPUMS extract is complete; no API key needed")
}

################################################################################
###                          7. Load the IPUMS API key                       ###
################################################################################

# 14) Only when an extract is not complete: use IPUMS_API_KEY if set, or
#     else take it from the first user .Renviron that exists. Only the
#     line that starts with IPUMS_API_KEY= is used (the last one, if
#     several, as readRenviron() would); surrounding spaces and one pair
#     of matching quotes are removed, and it is set with Sys.setenv().
#     Nothing else in the file is parsed, so no warning can echo a line of
#     it, and the value is never shown; ipumsr reads it from the
#     environment.
if (!all_extracts_complete_flag && !nzchar(Sys.getenv("IPUMS_API_KEY"))) {
  renviron_candidates <- c(
    documents = file.path(Sys.getenv("USERPROFILE"), "Documents", ".Renviron"),
    home = path.expand("~/.Renviron")
  )
  renviron_exists_flag <- nzchar(renviron_candidates) &
    file.exists(renviron_candidates)
  if (any(renviron_exists_flag)) {
    renviron_label <- names(renviron_candidates)[renviron_exists_flag][1L]
    log_step(paste0(
      "IPUMS_API_KEY not set; looking for it in the user .Renviron (",
      renviron_label, ")"
    ))
    renviron_lines <- readLines(
      renviron_candidates[renviron_exists_flag][1L],
      warn = FALSE, encoding = "UTF-8"
    )
    key_line_flag <- grepl("^[[:space:]]*IPUMS_API_KEY[[:space:]]*=",
                           renviron_lines)
    if (any(key_line_flag)) {
      key_value <- sub(
        "^[[:space:]]*IPUMS_API_KEY[[:space:]]*=[[:space:]]*", "",
        renviron_lines[key_line_flag][sum(key_line_flag)]
      )
      key_value <- sub("[[:space:]]+$", "", key_value)
      key_value <- sub("^\"(.*)\"$", "\\1", key_value)
      key_value <- sub("^'(.*)'$", "\\1", key_value)
      Sys.setenv(IPUMS_API_KEY = key_value)
      rm(key_value)
    }
    rm(renviron_lines)
  }
}
if (!all_extracts_complete_flag && !nzchar(Sys.getenv("IPUMS_API_KEY"))) {
  stop(
    "IPUMS_API_KEY is not set, and no non-empty IPUMS_API_KEY= line was ",
    "found in the user .Renviron. Add a line IPUMS_API_KEY=<your key> to ",
    "your user .Renviron (Documents/.Renviron on Windows with RStudio, or ",
    "~/.Renviron), or set the environment variable, then re-run. Keys are ",
    "issued at https://account.ipums.org/api_keys.", call. = FALSE
  )
}


################################################################################
###              8. Resume or submit, wait for, and download each            ###
################################################################################

# 15) Work on each incomplete extract in turn; the steps below run once per
#     extract, with that extract's roles, years, and variables
extract_number_by_name <- stats::setNames(
  rep(NA_integer_, length(ipums_extracts)), names(ipums_extracts)
)
for (extract_name in names(ipums_extracts)) {
  extract_spec <- ipums_extracts[[extract_name]]
  role_data <- paste0(extract_spec$role_prefix, "_data")
  role_ddi <- paste0(extract_spec$role_prefix, "_ddi")
  role_definition <- paste0(extract_spec$role_prefix, "_extract_definition")
  extract_json_path <- extract_spec$json_path

  # 16) Read the extract number recorded at submission, if any
  extract_number_int <- NA_integer_
  definition_index <- match(extract_json_path, manifest$local_path)
  if (!is.na(definition_index)) {
    number_match <- regmatches(
      manifest$notes[definition_index],
      regexpr("ipums_extract_number=[0-9]+",
              manifest$notes[definition_index])
    )
    if (length(number_match) == 1L) {
      extract_number_int <- as.integer(sub(".*=", "", number_match))
    }
  }
  extract_number_by_name[[extract_name]] <- extract_number_int
  if (extract_complete_flag[[extract_name]]) {
    next
  }

  # 17) Resume a recorded extract; one that failed, was canceled, or
  #     expired (completed, files deleted) is resubmitted from the saved
  #     definition
  submit_flag <- is.na(extract_number_int)
  replaced_note <- ""
  if (!is.na(extract_number_int)) {
    log_step(paste0(
      "RESUME: IPUMS ", extract_name, " extract ", ipums_collection, ":",
      extract_number_int, " (number from the manifest)"
    ))
    extract_info <- ipumsr::get_extract_info(
      paste0(ipums_collection, ":", extract_number_int)
    )
    log_step(paste("Extract status:", extract_info$status))
    expired_flag <- extract_info$status == "completed" &&
      length(extract_info$download_links) == 0L
    failed_flag <- extract_info$status %in% c("failed", "canceled")
    if (expired_flag || failed_flag) {
      log_step(paste0(
        "Extract ", extract_number_int, " is ",
        if (expired_flag) "expired" else extract_info$status,
        "; resubmitting the saved definition"
      ))
      replaced_note <- paste0(
        "; replaces extract ", extract_number_int, " (",
        if (expired_flag) "expired" else extract_info$status, ")"
      )
      submit_flag <- TRUE
    }
  }

  # 18) For a new extract, confirm exactly one ASEC sample (cpsYYYY_03s)
  #     for each of its years, then define the extract (with the AGE case
  #     selection, if any) and save it as JSON
  if (submit_flag && !file.exists(here::here(extract_json_path))) {
    sample_info <- as.data.frame(ipumsr::get_sample_info(ipums_collection))
    asec_sample_flag <- grepl("^cps[0-9]{4}_03s$", sample_info$name) &
      grepl("ASEC", sample_info$description, fixed = TRUE)
    asec_samples <- sample_info[asec_sample_flag, , drop = FALSE]
    asec_samples$year_int <- as.integer(substr(asec_samples$name, 4L, 7L))
    asec_samples <- asec_samples[
      asec_samples$year_int %in% extract_spec$years, ,
      drop = FALSE
    ]
    samples_per_year <- table(
      factor(asec_samples$year_int, levels = extract_spec$years)
    )
    if (any(samples_per_year != 1L)) {
      stop(
        "Expected one ASEC sample per year for the ", extract_name,
        " extract; check these years: ",
        paste(names(samples_per_year)[samples_per_year != 1L],
              collapse = ", "),
        call. = FALSE
      )
    }
    asec_sample_ids <- asec_samples$name[order(asec_samples$year_int)]
    log_step(paste0(
      "ASEC samples for ", extract_name, ": ", length(asec_sample_ids),
      " (", asec_sample_ids[1L], " to ",
      asec_sample_ids[length(asec_sample_ids)], ")"
    ))
    extract_variables <- as.list(extract_spec$variables)
    if (length(extract_spec$age_selection) > 0L) {
      age_index <- match("AGE", extract_spec$variables)
      extract_variables[[age_index]] <- ipumsr::var_spec(
        "AGE",
        case_selections = sprintf("%02d", extract_spec$age_selection),
        case_selection_type = "general"
      )
    }
    extract_definition <- ipumsr::define_extract_micro(
      collection = ipums_collection,
      description = extract_spec$description,
      samples = asec_sample_ids,
      variables = extract_variables,
      data_format = "fixed_width",
      data_structure = "rectangular",
      rectangular_on = "P"
    )
    ipumsr::save_extract_as_json(
      extract_definition, here::here(extract_json_path), overwrite = TRUE
    )
    # Rewrite it with LF line endings. save_extract_as_json() writes CRLF
    # on Windows, but git stores and checks the file out with LF
    # (.gitattributes: * text=auto eol=lf), so a CRLF hash would stop the
    # next run after any checkout that rewrites the file.
    extract_json_lines <- readLines(here::here(extract_json_path))
    extract_json_connection <- file(
      here::here(extract_json_path), open = "wb"
    )
    writeLines(extract_json_lines, extract_json_connection, sep = "\n")
    close(extract_json_connection)
    log_step(paste("Saved the extract definition (LF):", extract_json_path))
  } else if (submit_flag) {
    # 19) Reuse the saved definition so a resubmission matches the original
    extract_definition <- ipumsr::define_extract_from_json(
      here::here(extract_json_path)
    )
    log_step(paste("Read the saved extract definition:", extract_json_path))
  }

  # 20) Submit, then record the extract number at once so a stopped run
  #     resumes this extract
  if (submit_flag) {
    submitted_extract <- ipumsr::submit_extract(extract_definition)
    extract_number_int <- as.integer(submitted_extract$number)
    extract_number_by_name[[extract_name]] <- extract_number_int
    log_step(paste0(
      "SUBMITTED: IPUMS ", extract_name, " extract ", ipums_collection, ":",
      extract_number_int
    ))
    selection_note <- if (length(extract_spec$age_selection) > 0L) {
      paste0(
        "; case selection AGE ", min(extract_spec$age_selection), "-",
        max(extract_spec$age_selection)
      )
    } else {
      ""
    }
    manifest <- write_manifest_row(
      manifest, NA_character_, role_definition, ipums_source_page,
      "https://api.ipums.org/extracts", extract_json_path,
      format(Sys.Date(), "%Y-%m-%d"),
      paste0(
        "ipums_extract_number=", extract_number_int, "; collection ",
        ipums_collection, "; ASEC ", min(extract_spec$years), "-",
        max(extract_spec$years), " (", length(extract_definition$samples),
        " samples); variables ",
        paste(names(extract_definition$variables), collapse = " "),
        selection_note, "; submitted ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), replaced_note
      )
    )
  }

  # 21) Wait up to 60 minutes; on a timeout, stop and name the extract,
  #     which the next run resumes
  extract_id <- paste0(ipums_collection, ":", extract_number_int)
  log_step(paste0(
    "WAIT: ", extract_id, " (timeout ", wait_timeout_seconds, " seconds)"
  ))
  ready_extract <- tryCatch(
    ipumsr::wait_for_extract(
      extract_id,
      timeout_seconds = wait_timeout_seconds,
      max_delay_seconds = wait_max_delay_seconds,
      verbose = TRUE
    ),
    error = function(e) {
      stop(
        "IPUMS extract ", extract_id, " is not ready: ", conditionMessage(e),
        "\nRe-run this script to resume it; its number is in ",
        file.path("data", "raw", "ipums_cps", "manifest.csv"), ".",
        call. = FALSE
      )
    }
  )

  # 22) Stop before downloading when IPUMS reports a data file larger than
  #     the cap; the extract number stays in the manifest for a later run
  reported_bytes <- ready_extract$download_links$data$bytes
  if (!is.null(reported_bytes) && !is.na(reported_bytes) &&
        reported_bytes > ipums_cap_bytes) {
    stop(
      "IPUMS reports ", format(reported_bytes, big.mark = ","), " bytes ",
      "for the data file of ", extract_id, ", above the cap of ",
      format(ipums_cap_bytes, big.mark = ",", scientific = FALSE),
      ". Nothing was downloaded; ask the owner before raising the cap.",
      call. = FALSE
    )
  }
  log_step(paste0(
    "Data file size reported by IPUMS: ",
    if (is.null(reported_bytes)) {
      "not reported"
    } else {
      paste(format(reported_bytes, big.mark = ","), "bytes")
    }
  ))

  # 23) Download the data and the DDI codebook, then find the data file
  #     that the DDI names (IPUMS serves it gzipped, as <name>.gz)
  download_start <- Sys.time()
  log_step(paste("DOWNLOAD:", extract_id, "->", "data/raw/ipums_cps/"))
  ddi_full_path <- ipumsr::download_extract(
    ready_extract,
    download_dir = path_ipums_raw,
    overwrite = TRUE,
    progress = FALSE
  )
  download_seconds_num <- round(
    as.numeric(difftime(Sys.time(), download_start, units = "secs")), 1L
  )
  ddi_info <- ipumsr::read_ipums_ddi(ddi_full_path)
  data_candidates <- file.path(
    path_ipums_raw, paste0(ddi_info$file_name, c(".gz", ""))
  )
  data_exists_flag <- file.exists(data_candidates)
  if (!any(data_exists_flag)) {
    stop(
      "Downloaded data file not found: ",
      paste(data_candidates, collapse = " or "), call. = FALSE
    )
  }
  data_full_path <- data_candidates[data_exists_flag][1L]

  # 24) Stop if the DDI lacks any expected variable (for REPWTP, each of
  #     REPWTP1-REPWTP160)
  missing_variables <- setdiff(
    extract_spec$ddi_variables, ddi_info$var_info$var_name
  )
  if (length(missing_variables) > 0L) {
    stop(
      "The DDI for ", extract_id, " lacks expected variable(s): ",
      paste(missing_variables, collapse = ", "), call. = FALSE
    )
  }

  # 25) Drop rows for files of an earlier extract, then record both files
  manifest <- manifest[
    !manifest$file_role %in% c(role_data, role_ddi), ,
    drop = FALSE
  ]
  ipums_file_notes <- paste0(
    "ipums_extract_number=", extract_number_int, "; ",
    extract_spec$description, "; downloaded in ", download_seconds_num,
    " seconds"
  )
  manifest <- write_manifest_row(
    manifest, NA_character_, role_data, ipums_source_page,
    ready_extract$download_links$data$url,
    file.path("data", "raw", "ipums_cps", basename(data_full_path)),
    format(Sys.Date(), "%Y-%m-%d"), ipums_file_notes
  )
  manifest <- write_manifest_row(
    manifest, NA_character_, role_ddi, ipums_source_page,
    ready_extract$download_links$ddi_codebook$url,
    file.path("data", "raw", "ipums_cps", basename(ddi_full_path)),
    format(Sys.Date(), "%Y-%m-%d"), ipums_file_notes
  )
  log_step(paste0(
    "Saved ", basename(data_full_path), " (",
    format(file.size(data_full_path), big.mark = ","), " bytes) and ",
    basename(ddi_full_path), " (",
    format(file.size(ddi_full_path), big.mark = ","), " bytes) in ",
    download_seconds_num, " seconds"
  ))
}

################################################################################
###                          9. Verify and summarize                         ###
################################################################################

# 26) Confirm each expected role has one manifest row whose file is on disk
#     with a hash and a size
extract_roles_all <- unlist(lapply(ipums_extracts, function(extract_spec) {
  paste0(extract_spec$role_prefix, c("_data", "_ddi", "_extract_definition"))
}), use.names = FALSE)
expected_roles <- c(census_files$file_role, extract_roles_all)
role_complete_flag <- vapply(expected_roles, function(file_role) {
  role_index <- which(manifest$file_role == file_role)
  length(role_index) == 1L &&
    file.exists(here::here(manifest$local_path[role_index])) &&
    !is.na(manifest$sha256[role_index]) &&
    !is.na(manifest$size_bytes[role_index])
}, logical(1L))
if (!all(role_complete_flag)) {
  stop(
    "Manifest is incomplete for: ",
    paste(expected_roles[!role_complete_flag], collapse = ", "),
    call. = FALSE
  )
}

# 27) Report each extract number, what happened to it, and the run time
elapsed_seconds_num <- round(
  as.numeric(difftime(Sys.time(), run_start, units = "secs")), 1L
)
extract_summary <- paste0(
  names(ipums_extracts), " ", ipums_collection, ":", extract_number_by_name,
  " ", ifelse(extract_complete_flag, "skipped", "downloaded"),
  collapse = "; "
)
census_summary <- paste0(
  sub("^census_", "", names(census_status)), " ",
  ifelse(census_status == "pending", "downloaded", census_status),
  collapse = ", "
)
log_step(paste0(
  "Done: IPUMS extracts ", extract_summary, "; Census tables ",
  census_summary, "; ", elapsed_seconds_num, " seconds; manifest ",
  file.path("data", "raw", "ipums_cps", "manifest.csv")
))
