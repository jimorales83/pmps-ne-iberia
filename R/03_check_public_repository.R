# Lightweight integrity checks for the public paper repository.

find_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("--file=", args, fixed = TRUE, value = TRUE)
  if (length(hit) > 0) {
    return(dirname(normalizePath(sub("--file=", "", hit[[1]]), winslash = "/", mustWork = TRUE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_path <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(active_path) && nzchar(active_path)) {
      return(dirname(normalizePath(active_path, winslash = "/", mustWork = TRUE)))
    }
  }
  if (file.exists("R/00_helpers.R")) {
    return(normalizePath("R", winslash = "/", mustWork = TRUE))
  }
  if (file.exists("00_helpers.R")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }
  stop("Cannot find R/00_helpers.R. Open the public repository folder in RStudio or run R/run_reproduction.R.")
}

source(file.path(find_script_dir(), "00_helpers.R"), encoding = "UTF-8")

require_packages(c("dplyr", "readr"))

library(dplyr)
library(readr)

cat("\nRunning public repository checks\n")

errors <- character()
notes <- character()

must_exist <- function(paths, label) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    errors <<- c(errors, paste0(label, " missing: ", paste(missing, collapse = "; ")))
  }
}

corpus_path <- root_path("data", "final", "pmps_ne_iberia_dated_corpus.csv")
must_exist(corpus_path, "Final corpus")

if (file.exists(corpus_path)) {
  corpus <- read_public_corpus()
  if ("source_md" %in% names(corpus)) {
    errors <- c(errors, "The public corpus still contains the internal source_md column.")
  }
  notes <- c(
    notes,
    paste0("Corpus rows: ", nrow(corpus)),
    paste0("Sites: ", dplyr::n_distinct(corpus$site)),
    paste0("14C determinations: ", sum(corpus$method == "14C", na.rm = TRUE))
  )
}

site_files <- list.files(root_path("data", "site_normalized"), pattern = "\\.csv$", full.names = TRUE)
if (length(site_files) == 0) {
  errors <- c(errors, "No site-normalized CSV files found.")
} else {
  notes <- c(notes, paste0("Site-normalized CSV files: ", length(site_files)))
}

oxcal_models <- list.files(root_path("oxcal", "regional_models"), pattern = "\\.oxcal$", full.names = TRUE)
oxcal_outputs <- list.files(root_path("oxcal", "regional_outputs"), pattern = "\\.csv$", full.names = TRUE)
local_models <- list.files(root_path("oxcal", "local_models"), pattern = "\\.oxcal$", full.names = TRUE, recursive = TRUE)
local_outputs <- list.files(root_path("oxcal", "local_outputs"), pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = TRUE)
if (length(oxcal_models) == 0) {
  errors <- c(errors, "No regional OxCal model files found.")
}
if (length(oxcal_outputs) == 0) {
  errors <- c(errors, "No regional OxCal output CSV files found.")
}
if (length(local_models) == 0) {
  errors <- c(errors, "No local OxCal model files found.")
}
notes <- c(
  notes,
  paste0("Regional OxCal models: ", length(oxcal_models)),
  paste0("Regional OxCal outputs: ", length(oxcal_outputs)),
  paste0("Local OxCal models: ", length(local_models)),
  paste0("Local OxCal outputs/posterior exports: ", length(local_outputs))
)

for (output in oxcal_outputs) {
  first_lines <- readLines(output, n = 4, warn = FALSE)
  if (length(first_lines) < 3) {
    errors <- c(errors, paste0("OxCal output appears incomplete: ", output))
  }
}

manifest_path <- root_path("tables", "table_manifest.csv")
must_exist(manifest_path, "Table manifest")

if (file.exists(manifest_path)) {
  manifest <- read_csv(manifest_path, show_col_types = FALSE)
  table_paths <- root_path(manifest$file)
  missing_tables <- manifest$file[!file.exists(table_paths)]
  if (length(missing_tables) > 0) {
    errors <- c(errors, paste0("Manifest files missing: ", paste(missing_tables, collapse = "; ")))
  }
  notes <- c(notes, paste0("Manifest entries: ", nrow(manifest)))
}

main_figs <- list.files(root_path("figures", "main"), pattern = "\\.(pdf|png|tiff)$", full.names = TRUE)
si_figs <- list.files(root_path("figures", "supplementary"), pattern = "\\.(pdf|png|tiff)$", full.names = TRUE)
notes <- c(
  notes,
  paste0("Main figure files: ", length(main_figs)),
  paste0("Supplementary figure files: ", length(si_figs))
)

cat("\nSummary\n")
cat(paste0("- ", notes, collapse = "\n"), "\n", sep = "")

if (length(errors) > 0) {
  cat("\nProblems detected\n")
  cat(paste0("- ", errors, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}

cat("\nAll public repository checks passed.\n")
