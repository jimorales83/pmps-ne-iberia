# Reproduce Figure 7 with its checked climatic reference panel.
#
# The script uses the figure-source tables stored in
# tables/main/ and writes PDF, PNG and TIFF outputs to figures/main/. It is
# intentionally lightweight: the
# calibration and model-output extraction steps are documented in the broader
# analytical workflow, while this script rebuilds the figure from curated
# plotting data. The upper panel uses the NOAA/NCEI GICC05 NGRIP 20-year
# d18O series as a chronological climate reference. GI/GS intervals and
# Heinrich Stadial envelopes are derived in-script from Rasmussen et al. (2014,
# Table 2), with ka b2k converted to ka BP by subtracting 0.05 ka.

options(encoding = "UTF-8")

required_packages <- c("dplyr", "ggplot2", "grid", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them before running this script.",
    call. = FALSE
  )
}

library(dplyr)
library(ggplot2)
library(grid)
library(readr)

find_repo_root <- function() {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    current,
    dirname(current),
    dirname(dirname(current))
  ))

  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "R", "00_helpers.R")) &&
        dir.exists(file.path(candidate, "data")) &&
        dir.exists(file.path(candidate, "figures"))
    ) {
      return(candidate)
    }
  }

  stop(
    "Cannot locate the repository root. Run the script from the public PMPS NE Iberia repository.",
    call. = FALSE
  )
}

read_csv_utf8 <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE
  )
}

repo_root <- find_repo_root()
input_dir <- file.path(repo_root, "tables", "main")
climate_dir <- file.path(input_dir, "climate")
output_root <- Sys.getenv("PMPS_OUTPUT_DIR", "")

if (nzchar(output_root)) {
  if (!grepl("^[A-Za-z]:[/\\\\]|^/", output_root)) {
    output_root <- file.path(repo_root, output_root)
  }
  discussion_output_dir <- file.path(output_root, "figures", "main")
  discussion_control_output_dir <- file.path(output_root, "tables", "climate")
} else {
  discussion_output_dir <- file.path(repo_root, "figures", "main")
  discussion_control_output_dir <- climate_dir
}

dir.create(discussion_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(discussion_control_output_dir, recursive = TRUE, showWarnings = FALSE)

evidence_bins <- read_csv_utf8(file.path(input_dir, "fig_discussion_regional_evidence_bins.csv"))
context_spans <- read_csv_utf8(file.path(input_dir, "fig_discussion_regional_modelled_context_spans.csv")) |>
  filter(include == "yes")
site_coordinates <- read_csv_utf8(
  file.path(repo_root, "data", "final", "pmps_ne_iberia_site_coordinates.csv")
)
ngrip_curve <- read_csv_utf8(file.path(climate_dir, "fig_discussion_climate_ngrip_d18o_50_29ka.csv"))

hs4_definition <- Sys.getenv("PMPS_HS4_DEFINITION", "greenland_envelope")
if (!hs4_definition %in% c("greenland_envelope", "speleothem_broad")) {
  stop(
    "PMPS_HS4_DEFINITION must be 'greenland_envelope' or 'speleothem_broad'.",
    call. = FALSE
  )
}

climate_events_rasmussen_b2k <- data.frame(
  event = c(
    "GI-13", "GS-13", "GI-12", "GS-12", "GI-11", "GS-11",
    "GI-10", "GS-10", "GI-9", "GS-9", "GI-8", "GS-8",
    "GI-7", "GS-7", "GI-6", "GS-6", "GI-5.2", "GS-5.2",
    "GI-5.1", "GS-5.1", "GI-4"
  ),
  onset_ka_b2k = c(
    49.280, 48.340, 46.860, 44.280, 43.340, 42.240,
    41.460, 40.800, 40.160, 39.900, 38.220, 36.580,
    35.480, 34.740, 33.740, 33.360, 32.500, 32.040,
    30.840, 30.600, 28.900
  ),
  stringsAsFactors = FALSE
)

derive_climate_intervals <- function(events) {
  events |>
    mutate(
      type = if_else(grepl("^GS", event), "GS", "GI"),
      start_ka_b2k = onset_ka_b2k,
      end_ka_b2k = lead(onset_ka_b2k),
      start_ka_bp = start_ka_b2k - 0.05,
      end_ka_bp = end_ka_b2k - 0.05
    ) |>
    filter(!is.na(end_ka_b2k)) |>
    mutate(
      older_ka_cal_bp = start_ka_bp,
      younger_ka_cal_bp = end_ka_bp,
      interval_width_ka = start_ka_bp - end_ka_bp,
      label = event,
      type = factor(type, levels = c("GS", "GI")),
      source = "Rasmussen et al. 2014, Quaternary Science Reviews 106, Table 2",
      note = "Onsets are given in ka b2k and converted to ka BP by subtracting 0.05 ka."
    )
}

make_heinrich_bands <- function(climate_intervals, hs4_definition = "greenland_envelope") {
  bands <- data.frame(
    label = c("HS5", "HS4", "HS3"),
    associated_greenland_event = c("GS-13", "GS-9", "GS-5.1"),
    definition = "greenland_envelope",
    stringsAsFactors = FALSE
  ) |>
    left_join(
      climate_intervals |>
        transmute(
          associated_greenland_event = event,
          start_ka_b2k,
          end_ka_b2k,
          start_ka_bp,
          end_ka_bp
        ),
      by = "associated_greenland_event"
    ) |>
    mutate(
      source = "Rasmussen et al. 2014, Quaternary Science Reviews 106, Table 2",
      note = paste(
        "Greenland stadial envelope associated with the named Heinrich Stadial;",
        "not an independently dated duration of Heinrich IRD deposition."
      )
    )

  if (hs4_definition == "speleothem_broad") {
    bands <- bands |>
      mutate(
        associated_greenland_event = if_else(
          label == "HS4",
          "broader speleothem-constrained HS4 interval",
          associated_greenland_event
        ),
        start_ka_bp = if_else(label == "HS4", 40.20, start_ka_bp),
        end_ka_bp = if_else(label == "HS4", 38.34, end_ka_bp),
        start_ka_b2k = if_else(label == "HS4", 40.25, start_ka_b2k),
        end_ka_b2k = if_else(label == "HS4", 38.39, end_ka_b2k),
        definition = if_else(label == "HS4", "speleothem_broad", definition),
        source = if_else(
          label == "HS4",
          "Broad speleothem-constrained interval supplied as an explicit plotting option",
          source
        ),
        note = if_else(
          label == "HS4",
          "Alternative broad interval; caption must distinguish it from the Greenland GS-9 envelope.",
          note
        )
      )
  }

  bands
}

assert_close <- function(actual, expected, label, tolerance = 1e-6) {
  if (isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    return(invisible(TRUE))
  }

  stop(
    label,
    " failed. Observed: ",
    paste(actual, collapse = ", "),
    "; expected: ",
    paste(expected, collapse = ", "),
    call. = FALSE
  )
}

validate_climate_tables <- function(intervals, bands, hs4_definition, x_young_limit = 29.1) {
  assert_close(intervals$start_ka_bp, intervals$start_ka_b2k - 0.05, "b2k to BP conversion for starts")
  assert_close(intervals$end_ka_bp, intervals$end_ka_b2k - 0.05, "b2k to BP conversion for ends")

  if (any(intervals$start_ka_b2k <= intervals$end_ka_b2k)) {
    stop("Every GI/GS interval must run from an older onset to a younger onset.", call. = FALSE)
  }

  expected_intervals <- data.frame(
    event = c("GS-9", "GI-8", "GS-5.1"),
    start_ka_b2k = c(39.900, 38.220, 30.600),
    end_ka_b2k = c(38.220, 36.580, 28.900),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(expected_intervals))) {
    observed <- intervals |> filter(event == expected_intervals$event[i])
    assert_close(observed$start_ka_b2k, expected_intervals$start_ka_b2k[i], paste0(expected_intervals$event[i], " start"))
    assert_close(observed$end_ka_b2k, expected_intervals$end_ka_b2k[i], paste0(expected_intervals$event[i], " end"))
  }

  expected_hs <- data.frame(
    label = c("HS5", "HS4", "HS3"),
    start_ka_b2k = c(48.340, 39.900, 30.600),
    end_ka_b2k = c(46.860, 38.220, 28.900),
    stringsAsFactors = FALSE
  )

  if (hs4_definition == "speleothem_broad") {
    expected_hs[expected_hs$label == "HS4", c("start_ka_b2k", "end_ka_b2k")] <- c(40.25, 38.39)
  }

  for (i in seq_len(nrow(expected_hs))) {
    observed <- bands |> filter(label == expected_hs$label[i])
    assert_close(observed$start_ka_b2k, expected_hs$start_ka_b2k[i], paste0(expected_hs$label[i], " start"))
    assert_close(observed$end_ka_b2k, expected_hs$end_ka_b2k[i], paste0(expected_hs$label[i], " end"))
  }

  hs3 <- bands |> filter(label == "HS3")
  hs3_center_bp <- mean(c(hs3$start_ka_bp, hs3$end_ka_bp))
  if (hs3_center_bp > 31) {
    stop("HS3 appears too old for the GS-5.1 convention.", call. = FALSE)
  }

  if (hs3$end_ka_bp < x_young_limit) {
    message("HS3 extends beyond the plotted young limit; band may be partially visible.")
  }

  invisible(TRUE)
}

climate_intervals <- derive_climate_intervals(climate_events_rasmussen_b2k)
heinrich_bands <- make_heinrich_bands(climate_intervals, hs4_definition)
validate_climate_tables(climate_intervals, heinrich_bands, hs4_definition)

climate_interval_labels <- climate_intervals |>
  filter(interval_width_ka >= 0.85)

component_levels <- c(
  "MP",
  "Chatelperronian",
  "Protoaurignacian",
  "Aurignacian",
  "Gravettian",
  "Non-diagnostic human presence"
)

component_labels <- c(
  "MP",
  "Châtelperronian",
  "Protoaurignacian",
  "Aurignacian",
  "Gravettian",
  "Non-diagnostic human presence"
)

component_colours <- c(
  "MP" = "#1F1F1F",
  "Chatelperronian" = "#C44E52",
  "Protoaurignacian" = "#0072B2",
  "Aurignacian" = "#009E73",
  "Gravettian" = "#CC79A7",
  "Non-diagnostic human presence" = "#7A7A7A"
)

component_shapes <- c(
  "MP" = 16,
  "Chatelperronian" = 15,
  "Protoaurignacian" = 17,
  "Aurignacian" = 18,
  "Gravettian" = 25,
  "Non-diagnostic human presence" = 16
)

component_offsets <- c(
  "MP" = 0.08,
  "Chatelperronian" = -0.08,
  "Protoaurignacian" = 0.13,
  "Aurignacian" = -0.13,
  "Gravettian" = -0.18,
  "Non-diagnostic human presence" = 0
)

site_order <- site_coordinates |>
  distinct(corpus_site_code, site_name, latitude, longitude) |>
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  arrange(desc(latitude), longitude) |>
  mutate(site_y = rev(seq_len(n())))

plot_bins <- evidence_bins |>
  mutate(
    component = factor(component, levels = component_levels),
    across(
      c(
        median_cal_ka,
        cal_95_older_ka,
        cal_95_younger_ka,
        cal_68_older_ka,
        cal_68_younger_ka
      ),
      as.numeric
    )
  ) |>
  left_join(
    site_order,
    by = c("site_code" = "corpus_site_code"),
    suffix = c("", "_order")
  ) |>
  mutate(y_plot = site_y + component_offsets[as.character(component)])

plot_spans <- context_spans |>
  mutate(
    component = factor(component, levels = component_levels),
    across(
      c(
        span_68_older_ka,
        span_68_younger_ka,
        span_95_older_ka,
        span_95_younger_ka,
        span_mid_68_ka
      ),
      as.numeric
    ),
    span_mid_68_ka = ifelse(
      is.na(span_mid_68_ka),
      (span_68_older_ka + span_68_younger_ka) / 2,
      span_mid_68_ka
    )
  ) |>
  left_join(
    site_order,
    by = c("site_code" = "corpus_site_code"),
    suffix = c("", "_order")
  ) |>
  mutate(y_plot = site_y + component_offsets[as.character(component)])

base_theme <- theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey88"),
    panel.grid.major.x = element_line(linewidth = 0.25, colour = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 8.5, colour = "grey15"),
    axis.text.x = element_text(size = 8.5, colour = "grey25"),
    axis.title.x = element_text(size = 9.5, margin = margin(t = 6)),
    plot.margin = margin(3, 6, 3, 3)
  )

common_scales <- list(
  scale_y_continuous(
    breaks = site_order$site_y,
    labels = site_order$site_name,
    limits = c(0.15, nrow(site_order) + 0.75),
    expand = expansion(mult = c(0.02, 0.02))
  ),
  scale_x_reverse(
    breaks = seq(50, 30, by = -2),
    expand = expansion(mult = c(0.01, 0.01))
  ),
  coord_cartesian(xlim = c(51.2, 29.1), clip = "off"),
  scale_colour_manual(
    name = "Evidence class",
    values = component_colours,
    breaks = component_levels,
    labels = component_labels
  ),
  scale_fill_manual(
    name = "Evidence class",
    values = component_colours,
    breaks = component_levels,
    labels = component_labels
  ),
  scale_shape_manual(
    name = "Evidence class",
    values = component_shapes,
    breaks = component_levels,
    labels = component_labels
  )
)

panel_climate <- ggplot(ngrip_curve, aes(x = age_ka_cal_bp, y = d18o_permil)) +
  geom_rect(
    data = climate_intervals,
    aes(
      xmin = older_ka_cal_bp,
      xmax = younger_ka_cal_bp,
      ymin = -Inf,
      ymax = Inf,
      fill = type
    ),
    inherit.aes = FALSE,
    alpha = 0.28,
    colour = NA
  ) +
  geom_rect(
    data = heinrich_bands,
    aes(
      xmin = start_ka_bp,
      xmax = end_ka_bp,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "grey93",
    colour = NA
  ) +
  geom_rect(
    data = climate_intervals,
    aes(
      xmin = older_ka_cal_bp,
      xmax = younger_ka_cal_bp,
      ymin = -47.0,
      ymax = -46.25,
      fill = type
    ),
    inherit.aes = FALSE,
    alpha = 0.85,
    colour = "white",
    linewidth = 0.12
  ) +
  geom_line(linewidth = 0.24, colour = "grey30", lineend = "round") +
  annotate(
    "text",
    x = 51.12,
    y = -36.25,
    label = "A",
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 4.1,
    colour = "grey10"
  ) +
  geom_text(
    data = heinrich_bands,
    aes(
      x = (start_ka_bp + end_ka_bp) / 2,
      y = -36.35,
      label = label
    ),
    inherit.aes = FALSE,
    size = 2.35,
    colour = "grey40"
  ) +
  geom_text(
    data = climate_interval_labels,
    aes(
      x = (older_ka_cal_bp + younger_ka_cal_bp) / 2,
      y = -46.63,
      label = label
    ),
    inherit.aes = FALSE,
    size = 1.75,
    colour = "grey30"
  ) +
  scale_x_reverse(
    breaks = seq(50, 30, by = -2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(breaks = c(-37.5, -40.0, -42.5, -45.0)) +
  scale_fill_manual(
    values = c("GS" = "#E9E9E9", "GI" = "#EEF5F8"),
    guide = "none"
  ) +
  coord_cartesian(xlim = c(51.2, 29.1), ylim = c(-47.1, -36.0), clip = "off") +
  labs(x = NULL, y = expression(delta^18*O~("\u2030"))) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_line(linewidth = 0.25, colour = "grey90"),
    panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey92"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    axis.title.y = element_text(size = 8.2, colour = "grey30", margin = margin(r = 5)),
    axis.text.y = element_text(size = 7.5, colour = "grey35"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(2, 6, 0, 58)
  )

panel_a <- ggplot(plot_bins, aes(y = y_plot, colour = component, fill = component, shape = component)) +
  annotate(
    "text",
    x = 51.12,
    y = nrow(site_order) + 0.68,
    label = "B",
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 4.1,
    colour = "grey10"
  ) +
  geom_segment(
    aes(x = cal_95_older_ka, xend = cal_95_younger_ka, yend = y_plot),
    linewidth = 0.35,
    alpha = 0.45,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = cal_68_older_ka, xend = cal_68_younger_ka, yend = y_plot),
    linewidth = 1.25,
    alpha = 0.9,
    lineend = "round"
  ) +
  geom_point(aes(x = median_cal_ka), size = 2.6, stroke = 0.25) +
  common_scales +
  labs(x = NULL) +
  base_theme +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.margin = margin(2, 6, 2, 3)
  )

panel_b <- ggplot(plot_spans, aes(y = y_plot, colour = component, fill = component, shape = component)) +
  annotate(
    "text",
    x = 51.12,
    y = nrow(site_order) + 0.68,
    label = "C",
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 4.1,
    colour = "grey10"
  ) +
  geom_segment(
    aes(x = span_95_older_ka, xend = span_95_younger_ka, yend = y_plot),
    linewidth = 0.35,
    alpha = 0.45,
    lineend = "round"
  ) +
  geom_segment(
    aes(x = span_68_older_ka, xend = span_68_younger_ka, yend = y_plot),
    linewidth = 1.35,
    alpha = 0.9,
    lineend = "round"
  ) +
  geom_point(aes(x = span_mid_68_ka), size = 2.5, stroke = 0.25) +
  geom_text(
    aes(x = span_mid_68_ka, label = context_short),
    colour = "grey20",
    size = 2.25,
    nudge_y = 0.34,
    show.legend = FALSE
  ) +
  common_scales +
  labs(x = "Age (ka cal BP)") +
  base_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.width = unit(7, "mm"),
    legend.spacing.x = unit(2.5, "mm"),
    legend.box.margin = margin(t = 4, r = 0, b = 8, l = 0),
    plot.margin = margin(2, 6, 10, 3)
  ) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE),
    fill = "none"
  )

draw_combined_figure <- function() {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(0.12, 0.445, 0.435), "null"))))
  print(panel_climate, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(panel_a, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(panel_b, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  popViewport()
}

save_combined_figure <- function(path_base, width_mm = 195, height_mm = 248, dpi = 600) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4

  pdf_path <- paste0(path_base, ".pdf")
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(pdf_path, width = width_in, height = height_in)
  } else {
    grDevices::pdf(pdf_path, width = width_in, height = height_in)
  }
  draw_combined_figure()
  grDevices::dev.off()

  grDevices::png(
    paste0(path_base, ".png"),
    width = width_mm,
    height = height_mm,
    units = "mm",
    res = dpi,
    type = "cairo"
  )
  draw_combined_figure()
  grDevices::dev.off()

  if (tolower(Sys.getenv("PMPS_WRITE_TIFF", "true")) %in% c("1", "true", "yes")) {
    grDevices::tiff(
      paste0(path_base, ".tiff"),
      width = width_mm,
      height = height_mm,
      units = "mm",
      res = dpi,
      compression = "lzw",
      type = "cairo"
    )
    draw_combined_figure()
    grDevices::dev.off()
  }

  grDevices::svg(
    paste0(path_base, ".svg"),
    width = width_in,
    height = height_in
  )
  draw_combined_figure()
  grDevices::dev.off()
}

write_climate_control_outputs <- function(output_dir, climate_intervals, heinrich_bands, hs4_definition) {
  gi_gs_out <- climate_intervals |>
    transmute(
      event,
      start_ka_b2k,
      end_ka_b2k,
      start_ka_bp,
      end_ka_bp,
      source,
      note
    ) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  hs_out <- heinrich_bands |>
    transmute(
      label,
      associated_greenland_event,
      start_ka_b2k,
      end_ka_b2k,
      start_ka_bp,
      end_ka_bp,
      definition,
      source,
      note
    ) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  readr::write_csv(
    gi_gs_out,
    file.path(output_dir, "climate_gi_gs_intervals_rasmussen2014.csv")
  )
  readr::write_csv(
    hs_out,
    file.path(output_dir, "climate_heinrich_bands_used.csv")
  )

  hs_line <- function(target_label) {
    row <- hs_out |> filter(.data$label == target_label)
    paste0(
      "- ", target_label, ": ", row$associated_greenland_event,
      ", ", sprintf("%.3f", row$start_ka_b2k), "-",
      sprintf("%.3f", row$end_ka_b2k), " ka b2k; ",
      sprintf("%.3f", row$start_ka_bp), "-",
      sprintf("%.3f", row$end_ka_bp), " ka BP."
    )
  }

  validation_note <- c(
    "# Climate panel validation notes",
    "",
    "Figure: fig7_discussion_regional_evidence_structure_with_climate_v6",
    "",
    "What changed",
    "- Heinrich Stadial bands are no longer read from approximate visual markers.",
    "- GI/GS intervals are derived from an explicit Rasmussen et al. (2014, Table 2) event table in the plotting script.",
    "- ka b2k values are converted to ka BP by subtracting 0.05 ka.",
    "- The archaeological data plotted in panels B and C were not changed.",
    "",
    "Definition used",
    paste0("- HS4 definition: ", hs4_definition, "."),
    "- HS bands are Greenland stadial envelopes associated with named Heinrich Stadials, not independently dated Heinrich IRD durations.",
    "",
    "Bands used",
    hs_line("HS5"),
    hs_line("HS4"),
    hs_line("HS3"),
    "",
    "Caption caution",
    "- The caption should state that GI/GS boundaries follow Rasmussen et al. (2014, Table 2), converted from ka b2k to ka BP.",
    "- The caption should state that HS5 = GS-13, HS4 = GS-9 and HS3 = GS-5.1 under the Greenland-envelope convention.",
    "- HS3 extends slightly beyond the plotted young limit and is therefore only partly visible near the right edge.",
    "",
    "Suggested caption wording",
    paste(
      "Panel A shows the NGRIP/GICC05 Greenland climatic framework.",
      "Greenland Stadial and Interstadial boundaries follow the INTIMATE event stratigraphy of Rasmussen et al. (2014, Table 2),",
      "converted from ka b2k to ka BP by subtracting 0.05 ka.",
      "Heinrich Stadial bands are shown as the Greenland stadial envelopes commonly associated with HS5, HS4 and HS3",
      "(HS5 = GS-13; HS4 = GS-9; HS3 = GS-5.1), and should not be read as independently dated durations of Heinrich IRD events."
    )
  )

  writeLines(
    validation_note,
    con = file.path(output_dir, "climate_panel_validation_notes.md"),
    useBytes = TRUE
  )
}

write_climate_control_outputs(
  discussion_control_output_dir,
  climate_intervals,
  heinrich_bands,
  hs4_definition
)

output_base <- file.path(
  discussion_output_dir,
  "fig7_discussion_regional_evidence_structure_with_climate_v6"
)
save_combined_figure(output_base)

cat("Discussion figure written to:\n")
cat(normalizePath(discussion_output_dir, winslash = "/", mustWork = FALSE), "\n")
