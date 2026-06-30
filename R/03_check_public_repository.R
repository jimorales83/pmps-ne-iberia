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

release_files <- c(
  root_path("README.md"),
  root_path("CITATION.cff"),
  root_path("LICENSE.md"),
  root_path("LICENSES", "MIT.txt"),
  root_path("LICENSES", "CC-BY-4.0.txt"),
  root_path("figures", "main", "fig8_chatelperronian_assemblage_foradada.tiff")
)
must_exist(release_files, "Release file")

corpus_path <- root_path("data", "final", "pmps_ne_iberia_dated_corpus.csv")
must_exist(corpus_path, "Final corpus")
must_exist(root_path("R", "04_reproduce_discussion_figure.R"), "Discussion figure script")

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

discussion_figure_sources <- c(
  root_path("tables", "main", "fig_discussion_regional_evidence_bins.csv"),
  root_path("tables", "main", "fig_discussion_regional_modelled_context_spans.csv"),
  root_path("tables", "main", "fig_discussion_regional_modelled_context_spans_manifest.csv"),
  root_path("data", "final", "pmps_ne_iberia_site_coordinates.csv"),
  root_path("tables", "main", "climate", "fig_discussion_climate_ngrip_d18o_50_29ka.csv")
)
must_exist(discussion_figure_sources, "Discussion figure source")
notes <- c(notes, paste0("Discussion figure source files: ", sum(file.exists(discussion_figure_sources))))

site_files <- list.files(root_path("data", "site_normalized"), pattern = "\\.csv$", full.names = TRUE)
if (length(site_files) == 0) {
  errors <- c(errors, "No site-normalized CSV files found.")
} else {
  notes <- c(notes, paste0("Site-normalized CSV files: ", length(site_files)))

  corpus_raw <- read_delim(
    corpus_path,
    delim = ";",
    col_types = cols(.default = col_character()),
    na = character(),
    show_col_types = FALSE
  )
  site_tables <- lapply(
    site_files,
    read_csv,
    col_types = cols(.default = col_character()),
    na = character(),
    show_col_types = FALSE
  )

  invalid_schema <- basename(site_files)[
    !vapply(site_tables, function(x) identical(names(x), names(corpus_raw)), logical(1))
  ]
  if (length(invalid_schema) > 0) {
    errors <- c(
      errors,
      paste0("Site-normalized schema differs from the final corpus: ", paste(invalid_schema, collapse = "; "))
    )
  } else {
    row_signature <- function(x) {
      apply(as.data.frame(x, stringsAsFactors = FALSE), 1, paste, collapse = "\u001f")
    }
    site_corpus <- bind_rows(site_tables)
    if (
      nrow(site_corpus) != nrow(corpus_raw) ||
        !identical(sort(row_signature(site_corpus)), sort(row_signature(corpus_raw)))
    ) {
      errors <- c(errors, "Site-normalized files are not an exact partition of the final corpus.")
    }
  }
}

span_manifest_path <- root_path(
  "tables", "main", "fig_discussion_regional_modelled_context_spans_manifest.csv"
)
if (file.exists(span_manifest_path)) {
  span_manifest <- read_csv(span_manifest_path, show_col_types = FALSE)
  missing_span_sources <- span_manifest$source_file[
    !file.exists(root_path(span_manifest$source_file))
  ]
  if (length(missing_span_sources) > 0) {
    errors <- c(
      errors,
      paste0("Discussion-figure provenance paths missing: ", paste(unique(missing_span_sources), collapse = "; "))
    )
  }
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

for (model in c(oxcal_models, local_models)) {
  model_text <- readLines(model, warn = FALSE, encoding = "UTF-8")
  if (
    length(model_text) < 5 ||
      !any(grepl("Plot\\(\\)", model_text)) ||
      sum(lengths(regmatches(model_text, gregexpr("\\{", model_text)))) !=
        sum(lengths(regmatches(model_text, gregexpr("\\}", model_text))))
  ) {
    errors <- c(errors, paste0("OxCal model appears incomplete or malformed: ", model))
  }
}

for (output in oxcal_outputs) {
  first_lines <- readLines(output, n = 4, warn = FALSE)
  if (length(first_lines) < 3) {
    errors <- c(errors, paste0("OxCal output appears incomplete: ", output))
  }
}

local_manifest_path <- root_path("oxcal", "local_model_manifest.csv")
must_exist(local_manifest_path, "Local OxCal manifest")

if (file.exists(local_manifest_path)) {
  local_manifest <- read_csv(local_manifest_path, show_col_types = FALSE)
  missing_local_files <- local_manifest$file[
    !file.exists(root_path(local_manifest$file))
  ]
  paired_outputs <- local_manifest$paired_output[
    !is.na(local_manifest$paired_output) & local_manifest$paired_output != ""
  ]
  missing_paired_outputs <- paired_outputs[
    !file.exists(root_path(paired_outputs))
  ]
  if (length(missing_local_files) > 0) {
    errors <- c(
      errors,
      paste0("Local OxCal manifest files missing: ", paste(unique(missing_local_files), collapse = "; "))
    )
  }
  if (length(missing_paired_outputs) > 0) {
    errors <- c(
      errors,
      paste0("Local OxCal paired outputs missing: ", paste(unique(missing_paired_outputs), collapse = "; "))
    )
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
