# PMPS NE Iberia: simple reproduction script
#
# How to use in RStudio
# 1. Open this file.
# 2. Click "Source".
# 3. Wait until the final "Finished" message appears in the Console.
#
# The script does not overwrite the paper-ready figures in figures/main/ or
# figures/supplementary/. All new material is written to:
#
#   reproduction_outputs/
#
# This folder is ignored by Git and can be deleted after checking the results.

# -------------------------------------------------------------------------
# 0. Reviewer options
# -------------------------------------------------------------------------

# Keep FALSE to reproduce the CKDE figures with the paper setting.
# Change to TRUE for a faster smoke test.
QUICK_TEST <- FALSE

if (nzchar(Sys.getenv("PMPS_QUICK_TEST"))) {
  QUICK_TEST <- tolower(Sys.getenv("PMPS_QUICK_TEST")) %in% c("1", "true", "yes")
}

# TIFF files are large and slow to write. PDF and PNG are enough for checking.
WRITE_TIFF <- FALSE

# Keep FALSE for the simple guided reproduction. Change to TRUE to also run
# all supplementary CKDE/SPD sensitivity checks. This can take much longer.
RUN_ALL_RCARBON_CHECKS <- FALSE

# Leave as NA to use reproduction_outputs/ inside this repository.
# Advanced users may set an absolute path, for example:
# OUTPUT_DIR <- "C:/Users/YourName/Desktop/pmps_reproduction_check"
OUTPUT_DIR <- NA

# -------------------------------------------------------------------------
# 1. Locate the repository and load helper functions
# -------------------------------------------------------------------------

find_this_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("--file=", args, fixed = TRUE, value = TRUE)
  if (length(hit) > 0) {
    return(normalizePath(sub("--file=", "", hit[[1]]), winslash = "/", mustWork = TRUE))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_path <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(active_path) && nzchar(active_path)) {
      return(normalizePath(active_path, winslash = "/", mustWork = TRUE))
    }
  }

  if (file.exists("R/run_reproduction.R")) {
    return(normalizePath("R/run_reproduction.R", winslash = "/", mustWork = TRUE))
  }

  stop("Cannot locate run_reproduction.R. Open the public repository folder in RStudio.")
}

this_file <- find_this_file()
script_dir <- dirname(this_file)
source(file.path(script_dir, "00_helpers.R"), encoding = "UTF-8")

root <- repo_root()

if (is.na(OUTPUT_DIR) || !nzchar(OUTPUT_DIR)) {
  output_dir <- file.path(root, "reproduction_outputs")
} else {
  output_dir <- OUTPUT_DIR
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)

cat("\nPMPS NE Iberia reproduction run\n")
cat("Repository: ", root, "\n", sep = "")
cat("Output folder: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n\n", sep = "")

# -------------------------------------------------------------------------
# 2. Check required R packages
# -------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "ggplot2",
  "grid",
  "readr",
  "rcarbon",
  "stringr",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please run this in the RStudio Console and then source this script again:\n\n",
    "install.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "),
    "))\n",
    call. = FALSE
  )
}

library(dplyr)
library(readr)

# -------------------------------------------------------------------------
# 3. Inspect the dated corpus
# -------------------------------------------------------------------------

cat("Step 1/5: reading and summarising the dated corpus...\n")

corpus <- read_public_corpus()

corpus_summary <- tibble::tibble(
  measure = c(
    "Rows in public corpus",
    "Sites",
    "Radiocarbon determinations",
    "Included radiocarbon determinations",
    "Human-presence radiocarbon subset",
    "Diagnostic chrono-cultural radiocarbon subset"
  ),
  value = c(
    nrow(corpus),
    dplyr::n_distinct(corpus$site),
    sum(corpus$method == "14C", na.rm = TRUE),
    sum(corpus$method == "14C" & corpus$evaluation_final == "include", na.rm = TRUE),
    sum(corpus$method == "14C" & corpus$use_human_presence == "yes" & corpus$evaluation_final == "include", na.rm = TRUE),
    sum(corpus$method == "14C" & corpus$use_cultural_subset == "yes" & corpus$evaluation_final == "include", na.rm = TRUE)
  )
)

site_summary <- corpus |>
  count(site, site_code, method, name = "n") |>
  arrange(site, method)

cultural_summary <- corpus |>
  filter(method == "14C", evaluation_final == "include") |>
  count(cultural_attribution, use_human_presence, use_cultural_subset, name = "n") |>
  arrange(desc(n), cultural_attribution)

subset_summary <- tibble::tibble(
  subset = c(
    "Comparable radiocarbon base",
    "Human-presence subset",
    "Diagnostic chrono-cultural subset"
  ),
  flag = c("use_14c_primary", "use_human_presence", "use_cultural_subset")
) |>
  rowwise() |>
  mutate(
    n_determinations = sum(
      corpus$method == "14C" &
        corpus[[flag]] == "yes" &
        corpus$evaluation_final == "include" &
        corpus$in_main_14c_window,
      na.rm = TRUE
    ),
    n_sites = dplyr::n_distinct(corpus$site[
      corpus$method == "14C" &
        corpus[[flag]] == "yes" &
        corpus$evaluation_final == "include" &
        corpus$in_main_14c_window
    ]),
    n_bins = dplyr::n_distinct(corpus$bin_id[
      corpus$method == "14C" &
        corpus[[flag]] == "yes" &
        corpus$evaluation_final == "include" &
        corpus$in_main_14c_window &
        !is.na(corpus$bin_id) &
        corpus$bin_id != ""
    ])
  ) |>
  ungroup() |>
  select(-flag)

write_csv(corpus_summary, file.path(output_dir, "tables", "corpus_summary.csv"))
write_csv(site_summary, file.path(output_dir, "tables", "site_summary.csv"))
write_csv(cultural_summary, file.path(output_dir, "tables", "cultural_summary_14c_included.csv"))
write_csv(subset_summary, file.path(output_dir, "tables", "radiocarbon_subset_summary.csv"))

print(corpus_summary)
cat("\nSummary tables written to reproduction_outputs/tables/.\n\n")

# -------------------------------------------------------------------------
# 4. Check repository completeness
# -------------------------------------------------------------------------

cat("Step 2/5: checking repository contents...\n")
source(file.path(script_dir, "03_check_public_repository.R"), encoding = "UTF-8")
cat("\n")

# -------------------------------------------------------------------------
# 5. Generate figures in the reproduction folder
# -------------------------------------------------------------------------

Sys.setenv(PMPS_OUTPUT_DIR = output_dir)
Sys.setenv(PMPS_WRITE_TIFF = ifelse(WRITE_TIFF, "true", "false"))
Sys.setenv(PMPS_KEY_FIGURES_ONLY = ifelse(RUN_ALL_RCARBON_CHECKS, "false", "true"))

if (isTRUE(QUICK_TEST)) {
  Sys.setenv(PMPS_NSIM = "100")
  cat("QUICK_TEST is TRUE: CKDE calculations will use PMPS_NSIM = 100.\n\n")
} else {
  Sys.unsetenv("PMPS_NSIM")
  cat("QUICK_TEST is FALSE: CKDE calculations will use the paper default, PMPS_NSIM = 1000.\n")
  cat("This step can take several minutes.\n\n")
}

cat("Step 3/5: generating OxCal-based figures R2-R4...\n")
source(file.path(script_dir, "02_reproduce_oxcal_figures.R"), encoding = "UTF-8")
cat("\n")

cat("Step 4/5: generating CKDE/SPD figures from the dated corpus...\n")
source(file.path(script_dir, "01_reproduce_rcarbon_figures.R"), encoding = "UTF-8")
cat("\n")

cat("Step 5/5: generating the discussion evidence-structure figure...\n")
source(file.path(script_dir, "04_reproduce_discussion_figure.R"), encoding = "UTF-8")
cat("\n")

# -------------------------------------------------------------------------
# 6. Save session information and finish
# -------------------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(output_dir, "session_info.txt"),
  useBytes = TRUE
)

Sys.unsetenv("PMPS_OUTPUT_DIR")
Sys.unsetenv("PMPS_WRITE_TIFF")
Sys.unsetenv("PMPS_KEY_FIGURES_ONLY")
if (isTRUE(QUICK_TEST)) {
  Sys.unsetenv("PMPS_NSIM")
}

cat("Finished.\n")
cat("Open this folder to inspect the reproduced tables and figures:\n")
cat(normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
