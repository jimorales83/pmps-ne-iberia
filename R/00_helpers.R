# Shared helpers for the PMPS NE Iberia public repository.

options(encoding = "UTF-8")

script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(file_arg, args, fixed = TRUE, value = TRUE)
  if (length(hit) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(sub(file_arg, "", hit[[1]]), winslash = "/", mustWork = TRUE)
}

repo_root <- function() {
  current <- script_path()
  if (basename(dirname(current)) == "R") {
    return(normalizePath(file.path(dirname(current), ".."), winslash = "/", mustWork = TRUE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

root_path <- function(...) {
  file.path(repo_root(), ...)
}

figure_path <- function(section) {
  variant <- Sys.getenv("PMPS_OUTPUT_VARIANT", "")
  if (nzchar(variant)) {
    return(root_path("figures", variant, section))
  }
  root_path("figures", section)
}

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R package(s): ",
      paste(missing, collapse = ", "),
      ". Install them before running the reproduction scripts.",
      call. = FALSE
    )
  }
}

read_public_corpus <- function() {
  require_packages(c("readr", "dplyr", "stringr"))

  path <- root_path("data", "final", "pmps_ne_iberia_dated_corpus.csv")
  if (!file.exists(path)) {
    stop("Missing public corpus: ", path, call. = FALSE)
  }

  readr::read_delim(
    path,
    delim = ";",
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), stringr::str_trim),
      method = as.character(method),
      curve_std = stringr::str_to_lower(as.character(curve)),
      evaluation_final = stringr::str_to_lower(as.character(evaluation_final)),
      use_14c_primary = stringr::str_to_lower(as.character(use_14c_primary)),
      use_human_presence = stringr::str_to_lower(as.character(use_human_presence)),
      use_cultural_subset = stringr::str_to_lower(as.character(use_cultural_subset)),
      age_result_num = as.numeric(age_result),
      error_1sigma_num = as.numeric(error_1sigma),
      in_main_14c_window = !is.na(age_result_num) &
        age_result_num >= 25000 &
        age_result_num <= 45000
    )
}

oxcal_columns <- function() {
  c(
    "name",
    "unmodelled_68_from",
    "unmodelled_68_to",
    "unmodelled_95_from",
    "unmodelled_95_to",
    "modelled_68_from",
    "modelled_68_to",
    "modelled_95_from",
    "modelled_95_to",
    "Acomb",
    "A",
    "L",
    "P",
    "C",
    "unused"
  )
}

read_oxcal_output <- function(file_name) {
  require_packages(c("readr", "dplyr"))

  path <- root_path("oxcal", "regional_outputs", file_name)
  if (!file.exists(path)) {
    stop("Missing OxCal CSV output: ", path, call. = FALSE)
  }

  readr::read_csv(
    path,
    skip = 2,
    col_names = oxcal_columns(),
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) |>
    dplyr::filter(!is.na(name), name != "")
}

get_oxcal_row <- function(oxcal, node_name, source_label = "") {
  require_packages(c("dplyr"))

  out <- oxcal |>
    dplyr::filter(name == node_name) |>
    dplyr::slice(1)

  if (nrow(out) != 1) {
    stop(
      "Missing OxCal node",
      if (nzchar(source_label)) paste0(" in ", source_label) else "",
      ": ",
      node_name,
      call. = FALSE
    )
  }

  out
}

to_ka <- function(x) {
  as.numeric(x) / 1000
}

format_interval <- function(from, to, signed = FALSE) {
  if (signed) {
    return(paste0(sprintf("%+.3f", from), " to ", sprintf("%+.3f", to), " ka"))
  }
  paste0(sprintf("%.3f", from), "-", sprintf("%.3f", to), " ka cal BP")
}

export_figure <- function(plot_obj,
                          path_base,
                          width_mm,
                          height_mm,
                          write_tiff = TRUE,
                          dpi = 600) {
  require_packages(c("ggplot2"))

  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)

  pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf

  ggplot2::ggsave(
    filename = paste0(path_base, ".pdf"),
    plot = plot_obj,
    width = width_mm,
    height = height_mm,
    units = "mm",
    device = pdf_device
  )

  ggplot2::ggsave(
    filename = paste0(path_base, ".png"),
    plot = plot_obj,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = dpi
  )

  if (isTRUE(write_tiff)) {
    ggplot2::ggsave(
      filename = paste0(path_base, ".tiff"),
      plot = plot_obj,
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = dpi,
      compression = "lzw"
    )
  }
}

make_bins_arg <- function(df_subset) {
  bins_candidate <- as.character(df_subset$bin_id)
  bins_candidate[is.na(bins_candidate) | bins_candidate == ""] <- paste0(
    "row_",
    seq_len(nrow(df_subset))
  )

  if (length(unique(bins_candidate)) <= 1) {
    return(NA)
  }

  bins_candidate
}

run_rcarbon_curves <- function(df_subset,
                               label,
                               time_range = c(50000, 25000),
                               nsim = 1000,
                               bw = 200,
                               use_bins = TRUE,
                               run_spd = TRUE,
                               verbose = FALSE) {
  require_packages(c("rcarbon", "dplyr"))

  if (nrow(df_subset) == 0) {
    stop("Empty analytical subset: ", label, call. = FALSE)
  }

  cal_obj <- rcarbon::calibrate(
    x = df_subset$age_result_num,
    errors = df_subset$error_1sigma_num,
    ids = df_subset$lab_code,
    calCurves = df_subset$curve_std,
    normalised = TRUE
  )

  bins_arg <- if (use_bins) make_bins_arg(df_subset) else NA

  sim_obj <- rcarbon::sampleDates(
    x = cal_obj,
    bins = bins_arg,
    nsim = nsim,
    boot = FALSE,
    verbose = verbose
  )

  ckde_obj <- rcarbon::ckde(
    x = sim_obj,
    timeRange = time_range,
    bw = bw,
    normalised = TRUE
  )

  ckde_df <- data.frame(
    time_cal_bp = seq(
      from = ckde_obj$timeRange[1],
      to = ckde_obj$timeRange[2],
      length.out = nrow(ckde_obj$res.matrix)
    ),
    density = rowMeans(ckde_obj$res.matrix),
    label = label,
    curve_type = "CKDE",
    nsim = nsim,
    bw = bw,
    use_bins = use_bins,
    stringsAsFactors = FALSE
  )

  spd_df <- NULL

  if (isTRUE(run_spd)) {
    spd_obj <- rcarbon::spd(
      x = cal_obj,
      timeRange = time_range,
      bins = bins_arg,
      datenormalised = FALSE,
      spdnormalised = TRUE,
      runm = NA,
      verbose = verbose
    )

    spd_df <- data.frame(
      time_cal_bp = spd_obj$grid[, 1],
      density = spd_obj$grid[, 2],
      label = label,
      curve_type = "SPD",
      nsim = NA_integer_,
      bw = NA_real_,
      use_bins = use_bins,
      stringsAsFactors = FALSE
    )
  }

  clean_curve <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    x |>
      dplyr::filter(
        !is.na(time_cal_bp),
        !is.na(density),
        time_cal_bp >= 25000,
        time_cal_bp <= 50000
      ) |>
      dplyr::mutate(age_ka_cal_bp = time_cal_bp / 1000)
  }

  list(
    input = df_subset,
    ckde = clean_curve(ckde_df),
    spd = clean_curve(spd_df),
    calibration = cal_obj,
    simulated_dates = sim_obj
  )
}

write_text_report <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = path, useBytes = TRUE)
}
