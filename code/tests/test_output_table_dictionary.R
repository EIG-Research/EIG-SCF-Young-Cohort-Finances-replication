# test_output_table_dictionary -- the column dictionary matches each table
# Author - Ben Glasner
# research title - see README.md
# research question - see README.md

rm(list = ls())
options(scipen = 999)
set.seed(42)

# Checks data/docs/output_tables.md against the CSV headers in output/tables/:
# every table the dictionary documents must exist, and the columns listed in
# its section (the first cell of each "| `column` |" row) must equal the CSV
# header, in the same order. A column added, dropped, renamed, or moved in a
# table fails the check until the dictionary is updated. Tables the
# dictionary does not document are listed but do not fail it.
# Run from the project root as
#   Rscript code/tests/test_output_table_dictionary.R [tables_dir]
# tables_dir defaults to output/tables; pass another folder (for example the
# main checkout's output/tables from a git worktree) to check those files.
# It prints one PASS or FAIL line per table and exits 1 on any failure.

################################################################################
###                        1. Set paths and read inputs                      ###
################################################################################

# 1) Stop unless this script sits at code/tests/; take the tables folder from
#    the command line if one is given
here::i_am("code/tests/test_output_table_dictionary.R")
command_args <- commandArgs(trailingOnly = TRUE)
tables_dir <- if (length(command_args) >= 1L) {
  command_args[1L]
} else {
  here::here("output", "tables")
}
dictionary_path <- here::here("data", "docs", "output_tables.md")
if (!dir.exists(tables_dir) || !file.exists(dictionary_path)) {
  stop("Missing tables folder (", tables_dir, ") or dictionary (",
       dictionary_path, ")", call. = FALSE)
}
dictionary_lines <- readLines(dictionary_path, encoding = "UTF-8")

################################################################################
###                     2. Columns documented per table                      ###
################################################################################

# 2) A table section starts at a "## `name.csv`" heading and runs to the
#    next "## " heading; its columns are the first cells of its table rows
section_flag <- grepl("^## ", dictionary_lines)
table_heading_flag <- grepl("^## `[^`]+\\.csv`\\s*$", dictionary_lines)
section_index <- cumsum(section_flag)
column_row_pattern <- "^\\| `([^`]+)` \\|"
documented <- list()
for (heading_line in which(table_heading_flag)) {
  table_name <- sub("^## `([^`]+)`.*$", "\\1", dictionary_lines[heading_line])
  section_lines <- dictionary_lines[
    section_index == section_index[heading_line]
  ]
  column_lines <- section_lines[grepl(column_row_pattern, section_lines)]
  documented[[table_name]] <- sub(paste0(column_row_pattern, ".*$"), "\\1",
                                  column_lines)
}
if (length(documented) == 0L || anyDuplicated(names(documented)) > 0L) {
  stop("No table sections, or a table documented twice, in ",
       dictionary_path, call. = FALSE)
}

################################################################################
###                     3. Compare with the CSV headers                      ###
################################################################################

# 3) For each documented table: the file exists, no column is listed twice,
#    and the listed columns equal the header in order
failure_count <- 0L
for (table_name in names(documented)) {
  csv_path <- file.path(tables_dir, table_name)
  doc_columns <- documented[[table_name]]
  problems <- character(0L)
  if (!file.exists(csv_path)) {
    problems <- "file not found"
  } else {
    header_columns <- strsplit(
      readLines(csv_path, n = 1L, encoding = "UTF-8"), ",", fixed = TRUE
    )[[1L]]
    header_columns <- gsub('^"|"$', "", header_columns)
    missing_columns <- setdiff(header_columns, doc_columns)
    extra_columns <- setdiff(doc_columns, header_columns)
    if (length(missing_columns) > 0L) {
      problems <- c(problems, paste("undocumented:",
                                    paste(missing_columns, collapse = ", ")))
    }
    if (length(extra_columns) > 0L) {
      problems <- c(problems, paste("not in the CSV:",
                                    paste(extra_columns, collapse = ", ")))
    }
    if (anyDuplicated(doc_columns) > 0L) {
      problems <- c(problems, "a column is documented twice")
    }
    if (length(problems) == 0L && !identical(doc_columns, header_columns)) {
      problems <- "columns are documented in a different order"
    }
  }
  if (length(problems) == 0L) {
    message("PASS: ", table_name, " (", length(doc_columns), " columns)")
  } else {
    failure_count <- failure_count + 1L
    message("FAIL: ", table_name, ": ", paste(problems, collapse = "; "))
  }
}

# 4) List tables in the folder that the dictionary does not cover
undocumented_tables <- setdiff(
  list.files(tables_dir, pattern = "\\.csv$"), names(documented)
)
message("Not in the dictionary (not checked): ",
        if (length(undocumented_tables) == 0L) {
          "none"
        } else {
          paste(undocumented_tables, collapse = ", ")
        })

# 5) Exit non-zero on any failure
message(length(documented) - failure_count, " of ", length(documented),
        " documented tables match their CSV headers")
if (failure_count > 0L) {
  quit(status = 1L, save = "no")
}
