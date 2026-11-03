# 01_download_scf -- download the SCF waves, 2025 tables, PCE, mortgage rate
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Unit U3 of records/plans/2026-10-11_stage1-scf-pipeline.md (with O5 and O6).
# This script only downloads: it never unzips, opens, loads, or executes a
# downloaded file. Run it from the project root as
#   Rscript code/01_download_scf.R
# or through code/run_all.R. A re-run skips every file whose SHA-256 matches
# data/raw/scf/manifest.csv and stops on any present file that does not match.
# When the manifest lists every expected file and each is on disk, it also
# skips reading the Federal Reserve pages, so the re-run needs no network.

################################################################################
###                             1. Load packages                             ###
################################################################################

# 1) Stop early, naming any package that is not installed; all calls below
#    are qualified as pkg::fn()
required_packages <- c("here", "httr2", "digest")
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

# 2) Stop unless this script sits at code/01_download_scf.R in the project
here::i_am("code/01_download_scf.R")

path_scf_raw <- here::here("data", "raw", "scf")
manifest_path <- file.path(path_scf_raw, "manifest.csv")
dir.create(path_scf_raw, recursive = TRUE, showWarnings = FALSE)

# 3) Waves and the page that lists each wave's files (2025 is on the index)
scf_host <- "https://www.federalreserve.gov"
wave_years <- c(
  1989L, 1992L, 1995L, 1998L, 2001L, 2004L, 2007L,
  2010L, 2013L, 2016L, 2019L, 2022L, 2025L
)
wave_pages <- ifelse(
  wave_years == 2025L,
  paste0(scf_host, "/econres/scfindex.htm"),
  paste0(scf_host, "/econres/scf_", wave_years, ".htm")
)

# 4) The Stata file name for each wave file role; group 1 is the year token,
#    which must be the wave's two- or four-digit year. Every matching link
#    must also say "Stata" in its link text (O6: Stata versions only).
wave_role_patterns <- c(
  extract = "^scfp([0-9]{2}|[0-9]{4})s\\.zip$",
  full_public = "^scf([0-9]{2}|[0-9]{4})s\\.zip$",
  replicate_weights = "^scf([0-9]{2}|[0-9]{4})rw1s\\.zip$"
)

# 5) Files taken once from the 2025 release, as linked on the index page
common_files <- data.frame(
  file_role = c(
    "hist_tables_nominal", "hist_tables_real", "stderr_tables",
    "bulletin_macro"
  ),
  file_name = c(
    "scf2025_tables_public_nominal_historical.xlsx",
    "scf2025_tables_public_real_historical.xlsx",
    "2025_tables1_2_stderrs.xlsx",
    "bulletin.macro.txt"
  ),
  stringsAsFactors = FALSE
)

# 6) Headline PCE chain-type price index, annual, index 2017 = 100: FRED
#    series DPCERG3A086NBEA, which is BEA NIPA Table 1.1.4, line 2. Annual
#    values come straight from BEA (not averaged from the monthly PCEPI).
#    BEA revises the series, so the manifest records the retrieval date.
pce_series_id <- "DPCERG3A086NBEA"
pce_first_year <- 1988L
pce_last_year <- 2025L
pce_url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", pce_series_id,
  "&cosd=", pce_first_year, "-01-01&coed=", pce_last_year, "-01-01"
)
pce_source_page <- paste0("https://fred.stlouisfed.org/series/", pce_series_id)
pce_file_name <- paste0("pce_price_index_", pce_series_id, "_annual.csv")

# 6a) Freddie Mac Primary Mortgage Market Survey 30-year fixed rate, weekly,
#     percent: FRED series MORTGAGE30US, a context series for the draft's
#     mortgage-rate passage (plan records/plans/2026-10-16_draft-v2-replies.md,
#     U1 and O1). The end date is fixed so the file, and its hash, do not
#     change as new weeks are published
mortgage_series_id <- "MORTGAGE30US"
mortgage_first_date <- "2019-01-01"
mortgage_last_date <- "2025-12-31"
mortgage_url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", mortgage_series_id,
  "&cosd=", mortgage_first_date, "&coed=", mortgage_last_date
)
mortgage_source_page <- paste0("https://fred.stlouisfed.org/series/",
                               mortgage_series_id)
mortgage_file_name <- paste0("mortgage_rate_", mortgage_series_id,
                             "_weekly.csv")

# 7) Download limits: a 2 GB cap on pending files (the plan's approval) and
#    at most 2 retries of a transient network or server error per request
size_cap_bytes <- 2e9
max_retries_int <- 2L
transient_status_codes <- c(408L, 429L, 500L, 502L, 503L, 504L)
user_agent_text <- "EIG-SCF-Young-Cohort-Finances 01_download_scf.R (httr2)"

manifest_columns <- c(
  "year", "file_role", "source_page", "url", "local_path",
  "download_date", "size_bytes", "sha256", "notes"
)

run_start <- Sys.time()

################################################################################
###                           3. Define helpers                              ###
################################################################################

# 8) Write one timestamped progress line
log_step <- function(text) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", text)
}

# 9) Perform a request, retrying a transient failure at most max_retries_int
#    times and logging each retry; an HTTP error status is returned, not
#    raised, so the caller names the file in its own stop()
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

# 10) Build a request with the project user agent and a timeout in seconds
build_request <- function(url, timeout_seconds) {
  request <- httr2::request(url)
  request <- httr2::req_user_agent(request, user_agent_text)
  httr2::req_timeout(request, timeout_seconds)
}

################################################################################
###                    4. Discover file links on each page                   ###
################################################################################

# 11) Skip discovery when the manifest already lists every expected file
#     (one row for each wave role and common file, and one PCE row) and each
#     listed file is on disk: the expected files then come from the manifest
#     and no page is read, so a re-run needs no network. The two FRED rows
#     (PCE, mortgage rate) are built from constants in section 5, so they
#     need no discovery; a missing one is downloaded. Sections 6-9 still
#     hash every file and stop on a mismatch. Delete a file or its manifest
#     row to force discovery. Otherwise read every wave page, keep its
#     anchors, and classify the Stata links; a missing or ambiguous link is
#     recorded, and the script stops after all pages are read so every gap
#     is reported at once
expected_role_keys <- c(
  paste(rep(wave_years, each = length(wave_role_patterns)),
        names(wave_role_patterns)),
  paste("2025", common_files$file_role)
)
discovery_needed_flag <- TRUE
if (file.exists(manifest_path)) {
  manifest_listed <- utils::read.csv(
    manifest_path,
    colClasses = "character", na.strings = "", check.names = FALSE
  )
  listed_keys <- paste(manifest_listed$year, manifest_listed$file_role)
  listed_role_flag <- listed_keys %in% expected_role_keys
  listed_pce_flag <- manifest_listed$file_role %in% "pce_index"
  key_counts <- table(factor(listed_keys, levels = expected_role_keys))
  discovery_needed_flag <- !(
    all(key_counts == 1L) && sum(listed_pce_flag) == 1L &&
      all(file.exists(here::here(
        manifest_listed$local_path[listed_role_flag | listed_pce_flag]
      )))
  )
}

anchor_pattern <- "(?is)<a\\s[^>]*?href=\"([^\"]+)\"[^>]*>(.*?)</a>"
discovered_rows <- list()
discovery_problems <- character(0L)
index_links <- NULL
wave_discovery_index <- integer(0L)
common_discovery_index <- integer(0L)
if (discovery_needed_flag) {
  wave_discovery_index <- seq_along(wave_years)
  common_discovery_index <- seq_len(nrow(common_files))
} else {
  log_step(paste(
    "Every manifest file is present; page discovery skipped",
    "(no network needed)"
  ))
  discovered_rows[[1L]] <- manifest_listed[
    listed_role_flag,
    c("year", "file_role", "source_page", "url", "local_path", "notes")
  ]
}

for (wave_index in wave_discovery_index) {
  wave_year <- wave_years[wave_index]
  page_url <- wave_pages[wave_index]
  log_step(paste0("Reading SCF page for ", wave_year, ": ", page_url))

  page_response <- perform_with_retry(
    build_request(page_url, 60L),
    label = paste("SCF page", page_url)
  )
  if (httr2::resp_status(page_response) != 200L) {
    stop(
      "SCF page for ", wave_year, " does not exist or failed: HTTP ",
      httr2::resp_status(page_response), " at ", page_url,
      call. = FALSE
    )
  }
  page_html <- httr2::resp_body_string(page_response)

  # 12) Pull each anchor's href and its visible text (tags stripped)
  anchors <- regmatches(
    page_html, gregexpr(anchor_pattern, page_html, perl = TRUE)
  )[[1]]
  link_href <- sub(anchor_pattern, "\\1", anchors, perl = TRUE)
  link_text <- sub(anchor_pattern, "\\2", anchors, perl = TRUE)
  link_text <- gsub("<[^>]+>|&nbsp;", " ", link_text)
  link_text <- trimws(gsub("\\s+", " ", link_text))

  # 13) Resolve to absolute URLs; only www.federalreserve.gov links are kept
  link_url <- ifelse(
    startsWith(link_href, "/"), paste0(scf_host, link_href), link_href
  )
  on_host_flag <- startsWith(link_url, paste0(scf_host, "/"))
  link_name <- basename(sub("[?#].*$", "", link_url))
  page_links <- data.frame(
    url = link_url[on_host_flag],
    name = link_name[on_host_flag],
    text = link_text[on_host_flag],
    stringsAsFactors = FALSE
  )
  if (wave_year == 2025L) {
    index_links <- page_links
  }

  # 14) Classify: file name pattern, the wave's year token, and "Stata" in
  #     the link text must all hold; exactly one URL per role is required
  year_tokens <- c(substr(as.character(wave_year), 3L, 4L),
                   as.character(wave_year))
  for (file_role in names(wave_role_patterns)) {
    role_pattern <- wave_role_patterns[[file_role]]
    name_match_flag <- grepl(role_pattern, page_links$name)
    year_token <- ifelse(
      name_match_flag, sub(role_pattern, "\\1", page_links$name), ""
    )
    role_valid_flag <- name_match_flag &
      year_token %in% year_tokens &
      grepl("stata", page_links$text, ignore.case = TRUE)
    role_links <- page_links[role_valid_flag, , drop = FALSE]
    role_links <- role_links[!duplicated(role_links$url), , drop = FALSE]

    if (nrow(role_links) == 0L) {
      problem_text <- paste0(
        wave_year, " ", file_role, ": no Stata link found on ", page_url
      )
      log_step(paste("MISSING:", problem_text))
      discovery_problems <- c(discovery_problems, problem_text)
    } else if (nrow(role_links) > 1L) {
      problem_text <- paste0(
        wave_year, " ", file_role, ": ", nrow(role_links),
        " candidate Stata links on ", page_url, " (",
        paste(role_links$url, collapse = ", "), ")"
      )
      log_step(paste("AMBIGUOUS:", problem_text))
      discovery_problems <- c(discovery_problems, problem_text)
    } else {
      log_step(paste0(
        "Found ", wave_year, " ", file_role, ": ", role_links$url
      ))
      discovered_rows[[length(discovered_rows) + 1L]] <- data.frame(
        year = as.character(wave_year),
        file_role = file_role,
        source_page = page_url,
        url = role_links$url,
        local_path = file.path(
          "data", "raw", "scf", wave_year, role_links$name
        ),
        notes = paste0("Stata version; link text: ", role_links$text),
        stringsAsFactors = FALSE
      )
    }
  }
}

# 15) Find the 2025-release common files on the index page by exact name
for (common_index in common_discovery_index) {
  file_name <- common_files$file_name[common_index]
  file_role <- common_files$file_role[common_index]
  name_links <- index_links[index_links$name == file_name, , drop = FALSE]
  name_links <- name_links[!duplicated(name_links$url), , drop = FALSE]

  if (nrow(name_links) != 1L) {
    problem_text <- paste0(
      "2025 ", file_role, ": expected one link to ", file_name, " on ",
      wave_pages[wave_years == 2025L], ", found ", nrow(name_links)
    )
    log_step(paste("MISSING:", problem_text))
    discovery_problems <- c(discovery_problems, problem_text)
  } else {
    log_step(paste0("Found 2025 ", file_role, ": ", name_links$url))
    discovered_rows[[length(discovered_rows) + 1L]] <- data.frame(
      year = "2025",
      file_role = file_role,
      source_page = wave_pages[wave_years == 2025L],
      url = name_links$url,
      local_path = file.path("data", "raw", "scf", "common", file_name),
      notes = paste0("2025 release; link text: ", name_links$text),
      stringsAsFactors = FALSE
    )
  }
}

# 16) Stop, listing every gap, if any wave or common file was not found
if (length(discovery_problems) > 0L) {
  stop(
    "File discovery failed for ", length(discovery_problems), " item(s):\n",
    paste0("  - ", discovery_problems, collapse = "\n"),
    call. = FALSE
  )
}

################################################################################
###                       5. Assemble the expected files                     ###
################################################################################

# 17) Add the PCE price index; its year column is blank because the file
#     covers 1988-2025
pce_row <- data.frame(
  year = NA_character_,
  file_role = "pce_index",
  source_page = pce_source_page,
  url = pce_url,
  local_path = file.path("data", "raw", "scf", "common", pce_file_name),
  notes = paste0(
    "FRED series ", pce_series_id, " (BEA NIPA Table 1.1.4 line 2: ",
    "personal consumption expenditures chain-type price index, headline), ",
    "annual, index 2017=100, ", pce_first_year, "-", pce_last_year,
    "; retrieved on download_date; BEA revises this series"
  ),
  stringsAsFactors = FALSE
)
mortgage_row <- data.frame(
  year = NA_character_,
  file_role = "mortgage_rate",
  source_page = mortgage_source_page,
  url = mortgage_url,
  local_path = file.path("data", "raw", "scf", "common", mortgage_file_name),
  notes = paste0(
    "FRED series ", mortgage_series_id, " (Freddie Mac Primary Mortgage ",
    "Market Survey, 30-year fixed rate mortgage average, percent, weekly), ",
    mortgage_first_date, " to ", mortgage_last_date,
    "; retrieved on download_date"
  ),
  stringsAsFactors = FALSE
)
expected_files <- do.call(rbind,
                          c(discovered_rows, list(pce_row, mortgage_row)))

# 18) Check the count (13 waves x 3 Stata files, 4 common files, the PCE
#     index, and the mortgage rate)
#     and that no two files share a local path
expected_count_int <- length(wave_years) * length(wave_role_patterns) +
  nrow(common_files) + 2L
if (nrow(expected_files) != expected_count_int) {
  stop(
    "Expected ", expected_count_int, " files but assembled ",
    nrow(expected_files), call. = FALSE
  )
}
if (anyDuplicated(expected_files$local_path) > 0L) {
  stop(
    "Two files map to the same local path: ",
    paste(unique(expected_files$local_path[
      duplicated(expected_files$local_path)
    ]), collapse = ", "),
    call. = FALSE
  )
}
log_step(paste("Expected files:", nrow(expected_files)))

################################################################################
###                  6. Compare files on disk with the manifest              ###
################################################################################

# 19) Read the manifest with every column as character, or start an empty one
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

# 20) Classify each expected file: skip (hash matches), mismatch (present,
#     hash differs), record (present, no manifest row), or pending (absent)
expected_files$status <- NA_character_
expected_files$sha256 <- NA_character_
for (file_index in seq_len(nrow(expected_files))) {
  local_path <- expected_files$local_path[file_index]
  full_path <- here::here(local_path)
  manifest_index <- match(local_path, manifest$local_path)

  if (!file.exists(full_path)) {
    expected_files$status[file_index] <- "pending"
    next
  }
  file_hash <- digest::digest(file = full_path, algo = "sha256")
  expected_files$sha256[file_index] <- file_hash
  if (is.na(manifest_index)) {
    expected_files$status[file_index] <- "record"
  } else if (identical(file_hash, manifest$sha256[manifest_index])) {
    expected_files$status[file_index] <- "skip"
    log_step(paste("SKIP (sha256 matches manifest):", local_path))
  } else {
    expected_files$status[file_index] <- "mismatch"
    log_step(paste("HASH MISMATCH:", local_path))
  }
}

# 21) Stop on any mismatch without touching the file or the manifest
mismatch_flag <- expected_files$status == "mismatch"
if (any(mismatch_flag)) {
  stop(
    "SHA-256 of ", sum(mismatch_flag), " present file(s) does not match ",
    "the manifest; nothing was overwritten. Inspect, then move or delete ",
    "the file or its manifest row by hand:\n",
    paste0("  - ", expected_files$local_path[mismatch_flag], collapse = "\n"),
    call. = FALSE
  )
}

# 22) Hash and record any present file that has no manifest row
record_flag <- expected_files$status == "record"
for (file_index in which(record_flag)) {
  local_path <- expected_files$local_path[file_index]
  full_path <- here::here(local_path)
  log_step(paste("RECORD (present, no manifest row):", local_path))
  manifest <- rbind(manifest, data.frame(
    year = expected_files$year[file_index],
    file_role = expected_files$file_role[file_index],
    source_page = expected_files$source_page[file_index],
    url = expected_files$url[file_index],
    local_path = local_path,
    download_date = NA_character_,
    size_bytes = format(file.size(full_path), scientific = FALSE),
    sha256 = expected_files$sha256[file_index],
    notes = paste0(
      expected_files$notes[file_index],
      "; present before a manifest row existed, so hashed from disk ",
      "(download date unknown; file modified ",
      format(file.mtime(full_path), "%Y-%m-%d"), ")"
    ),
    stringsAsFactors = FALSE
  ))
}
if (any(record_flag)) {
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
}

################################################################################
###                       7. Check the size of pending files                 ###
################################################################################

# 23) HEAD every pending file and stop if the total exceeds the 2 GB cap;
#     a file whose server sends no length (the FRED CSV) counts as 0 bytes
pending_index <- which(expected_files$status == "pending")
pending_sizes <- numeric(length(pending_index))
for (position in seq_along(pending_index)) {
  file_index <- pending_index[position]
  file_url <- expected_files$url[file_index]
  head_request <- httr2::req_method(build_request(file_url, 60L), "HEAD")
  head_response <- perform_with_retry(head_request, paste("HEAD", file_url))
  if (httr2::resp_status(head_response) != 200L) {
    stop(
      "HEAD failed for ", file_url, ": HTTP ",
      httr2::resp_status(head_response), call. = FALSE
    )
  }
  length_header <- httr2::resp_header(head_response, "content-length")
  length_bytes <- suppressWarnings(as.numeric(length_header))
  if (length(length_bytes) == 0L || is.na(length_bytes) ||
        length_bytes <= 0) {
    log_step(paste("HEAD size unknown (counted as 0):", file_url))
    length_bytes <- 0
  }
  pending_sizes[position] <- length_bytes
}
pending_total_bytes <- sum(pending_sizes)
log_step(paste0(
  "Pending files: ", length(pending_index), "; HEAD total ",
  format(pending_total_bytes, big.mark = ","), " bytes (cap ",
  format(size_cap_bytes, big.mark = ","), ")"
))
if (pending_total_bytes > size_cap_bytes) {
  stop(
    "Pending downloads total ", format(pending_total_bytes, big.mark = ","),
    " bytes, over the ", format(size_cap_bytes, big.mark = ","),
    "-byte cap; nothing was downloaded", call. = FALSE
  )
}

################################################################################
###                          8. Download pending files                       ###
################################################################################

# 24) Download each file in binary to a .part file, check it, rename it, hash
#     it, and rewrite the manifest after each file so a stopped run resumes
downloaded_bytes <- 0
for (file_index in pending_index) {
  local_path <- expected_files$local_path[file_index]
  file_url <- expected_files$url[file_index]
  full_path <- here::here(local_path)
  part_path <- paste0(full_path, ".part")
  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(part_path)) {
    log_step(paste("Removing leftover temp file:", part_path))
    unlink(part_path)
  }

  log_step(paste("DOWNLOAD:", file_url, "->", local_path))
  file_response <- perform_with_retry(
    build_request(file_url, 1800L),
    label = paste("download", file_url),
    path = part_path
  )

  # 25) Reject an error status, an HTML page, an empty file, or a file whose
  #     size differs from the server's uncompressed Content-Length
  file_status <- httr2::resp_status(file_response)
  content_type <- httr2::resp_header(file_response, "content-type", "")
  content_encoding <- httr2::resp_header(file_response, "content-encoding", "")
  length_bytes <- suppressWarnings(as.numeric(
    httr2::resp_header(file_response, "content-length", NA_character_)
  ))
  part_size_bytes <- file.size(part_path)
  download_problem <- if (file_status != 200L) {
    paste("HTTP", file_status)
  } else if (grepl("text/html", content_type, ignore.case = TRUE)) {
    "server returned an HTML page"
  } else if (is.na(part_size_bytes) || part_size_bytes == 0) {
    "empty file"
  } else if (!is.na(length_bytes) && length_bytes > 0 &&
               content_encoding == "" && part_size_bytes != length_bytes) {
    paste0(
      "size ", part_size_bytes, " differs from Content-Length ", length_bytes
    )
  } else {
    NA_character_
  }
  if (!is.na(download_problem)) {
    unlink(part_path)
    stop("Download of ", file_url, " failed: ", download_problem,
         call. = FALSE)
  }
  if (!file.rename(part_path, full_path)) {
    stop("Could not rename ", part_path, " to ", full_path, call. = FALSE)
  }

  # 26) Replace any stale manifest row for this path, then write the manifest
  file_hash <- digest::digest(file = full_path, algo = "sha256")
  manifest <- manifest[
    is.na(manifest$local_path) | manifest$local_path != local_path, ,
    drop = FALSE
  ]
  manifest <- rbind(manifest, data.frame(
    year = expected_files$year[file_index],
    file_role = expected_files$file_role[file_index],
    source_page = expected_files$source_page[file_index],
    url = file_url,
    local_path = local_path,
    download_date = format(Sys.Date(), "%Y-%m-%d"),
    size_bytes = format(part_size_bytes, scientific = FALSE),
    sha256 = file_hash,
    notes = expected_files$notes[file_index],
    stringsAsFactors = FALSE
  ))
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
  expected_files$status[file_index] <- "downloaded"
  downloaded_bytes <- downloaded_bytes + part_size_bytes
  log_step(paste0(
    "Saved ", local_path, " (", format(part_size_bytes, big.mark = ","),
    " bytes, sha256 ", file_hash, ")"
  ))
}

################################################################################
###                         9. Verify and summarize                          ###
################################################################################

# 27) Sort the manifest by path and confirm every expected file is on disk
#     with a manifest row that has a hash and a size
manifest <- manifest[order(manifest$local_path), , drop = FALSE]
utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")

manifest_index <- match(expected_files$local_path, manifest$local_path)
incomplete_flag <- is.na(manifest_index) |
  !file.exists(here::here(expected_files$local_path))
incomplete_flag[!incomplete_flag] <-
  is.na(manifest$sha256[manifest_index[!incomplete_flag]]) |
  is.na(manifest$size_bytes[manifest_index[!incomplete_flag]])
if (any(incomplete_flag)) {
  stop(
    "Manifest is incomplete for: ",
    paste(expected_files$local_path[incomplete_flag], collapse = ", "),
    call. = FALSE
  )
}

# 28) Report counts by status, bytes downloaded, and the run time
elapsed_seconds_num <- round(
  as.numeric(difftime(Sys.time(), run_start, units = "secs")), 1L
)
status_counts <- table(factor(
  expected_files$status, levels = c("downloaded", "skip", "record")
))
log_step(paste0(
  "Done: ", status_counts[["downloaded"]], " downloaded, ",
  status_counts[["skip"]], " skipped, ", status_counts[["record"]],
  " recorded from disk; ", format(downloaded_bytes, big.mark = ","),
  " bytes downloaded in ", elapsed_seconds_num, " seconds; manifest ",
  file.path("data", "raw", "scf", "manifest.csv")
))
