# Reproduce rcarbon-based Results and supplementary figures.

this_script <- normalizePath(
  sub("--file=", "", grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]),
  winslash = "/",
  mustWork = TRUE
)
source(file.path(dirname(this_script), "00_helpers.R"), encoding = "UTF-8")

require_packages(c("dplyr", "ggplot2", "grid", "rcarbon", "readr", "stringr", "tibble"))

library(dplyr)
library(ggplot2)
library(grid)
library(readr)

script_build <- "public_rcarbon_figures_v1"
cat("\nRunning ", script_build, "\n", sep = "")

write_tiff <- tolower(Sys.getenv("PMPS_WRITE_TIFF", "true")) %in% c("1", "true", "yes")
nsim_ckde <- as.integer(Sys.getenv("PMPS_NSIM", "1000"))
if (is.na(nsim_ckde) || nsim_ckde <= 0) {
  stop("PMPS_NSIM must be a positive integer.", call. = FALSE)
}

set.seed(as.integer(Sys.getenv("PMPS_SEED", "20260525")))

fig_main_dir <- figure_path("main")
fig_si_dir <- figure_path("supplementary")

corpus <- read_public_corpus()

rc_subset <- function(flag) {
  corpus |>
    filter(
      method == "14C",
      .data[[flag]] == "yes",
      evaluation_final == "include",
      in_main_14c_window,
      !is.na(curve_std),
      curve_std != ""
    )
}

df_14c_primary <- rc_subset("use_14c_primary") |>
  mutate(subset = "Comparable radiocarbon base")
df_human <- rc_subset("use_human_presence") |>
  mutate(subset = "Human-presence subset")
df_cultural <- rc_subset("use_cultural_subset") |>
  mutate(subset = "Diagnostic chrono-cultural subset")

strict_lab_codes <- unique(df_cultural$lab_code)
df_residual <- df_human |>
  filter(!(lab_code %in% strict_lab_codes)) |>
  mutate(subset = "Non-diagnostic human-presence layer")

time_range_bp <- c(50000, 25000)
time_range_ka <- c(50, 25)
bw_main <- 200
bw_values <- c(100, 200, 300)

component_name <- function(x) {
  case_when(
    x %in% c("Early Aurignacian", "Late Aurignacian") ~ "Aurignacian",
    x %in% c("Chatelperronian", "Châtelperronian") ~ "Ch\u00e2telperronian",
    TRUE ~ x
  )
}

count_structure <- function(data, grouping_var) {
  data |>
    group_by({{ grouping_var }}) |>
    summarise(
      n_determinations = n(),
      n_bins = n_distinct(bin_id[bin_id != "" & !is.na(bin_id)]),
      n_sites = n_distinct(site),
      n_site_level_units = n_distinct(paste(site, level_unit, sep = " | ")),
      .groups = "drop"
    )
}

base_theme_main <- theme_classic(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot",
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(10, "mm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(b = 1),
    legend.box.margin = margin(b = 1),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.line = element_line(linewidth = 0.35),
    axis.ticks = element_line(linewidth = 0.35),
    plot.margin = margin(3, 4, 3, 4)
  )

base_theme_si <- theme_classic(base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(16, "mm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.line = element_line(linewidth = 0.35),
    axis.ticks = element_line(linewidth = 0.35),
    plot.margin = margin(4, 4, 4, 4)
  )

# Figure R1: human-presence structure

series_order <- c(
  "Human-presence subset",
  "Diagnostic chrono-cultural subset",
  "Non-diagnostic human-presence layer"
)

series_cols <- c(
  "Human-presence subset" = "#000000",
  "Diagnostic chrono-cultural subset" = "#0072B2",
  "Non-diagnostic human-presence layer" = "#7A7A7A"
)

series_linetypes <- c(
  "Human-presence subset" = "solid",
  "Diagnostic chrono-cultural subset" = "longdash",
  "Non-diagnostic human-presence layer" = "dotdash"
)

series_plot_labels <- c(
  "Human-presence subset" = "Human-presence subset",
  "Diagnostic chrono-cultural subset" = "Diagnostic chrono-cultural subset",
  "Non-diagnostic human-presence layer" = "Non-diagnostic human-presence layer"
)

diagnostic_layers <- list(
  "Human-presence subset" = df_human,
  "Diagnostic chrono-cultural subset" = df_cultural,
  "Non-diagnostic human-presence layer" = df_residual
)

diag_curves <- lapply(names(diagnostic_layers), function(label) {
  run_rcarbon_curves(
    df_subset = diagnostic_layers[[label]],
    label = label,
    time_range = time_range_bp,
    nsim = nsim_ckde,
    bw = bw_main,
    use_bins = TRUE,
    run_spd = TRUE
  )
})
names(diag_curves) <- names(diagnostic_layers)

ckde_diag <- bind_rows(lapply(diag_curves, function(x) x$ckde)) |>
  mutate(series = factor(label, levels = series_order))

spd_diag <- bind_rows(lapply(diag_curves, function(x) x$spd)) |>
  mutate(series = factor(label, levels = series_order))

p_fig_a <- ggplot(
  ckde_diag,
  aes(x = age_ka_cal_bp, y = density, colour = series, linetype = series)
) +
  geom_line(linewidth = 0.68, lineend = "round") +
  scale_colour_manual(values = series_cols, labels = series_plot_labels) +
  scale_linetype_manual(values = series_linetypes, labels = series_plot_labels) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  scale_x_reverse(
    limits = time_range_ka,
    breaks = seq(50, 25, by = -5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "A",
    x = "Age (ka cal BP)",
    y = "Area-normalised CKDE density"
  ) +
  base_theme_main

df_cultural_components <- df_cultural |>
  mutate(
    technocomplex_raw = cultural_attribution,
    component = component_name(technocomplex_raw),
    component_type = "Diagnostic chrono-cultural component"
  )

df_residual_component <- df_residual |>
  mutate(
    technocomplex_raw = NA_character_,
    component = "Non-diagnostic human-presence layer",
    component_type = "Non-diagnostic human-presence layer"
  )

preferred_component_order <- c(
  "MP",
  "Ch\u00e2telperronian",
  "Protoaurignacian",
  "Aurignacian",
  "Gravettian",
  "Non-diagnostic human-presence layer"
)

df_components <- bind_rows(df_cultural_components, df_residual_component)
component_order <- preferred_component_order[preferred_component_order %in% unique(df_components$component)]

component_curve_list <- lapply(component_order, function(component_i) {
  run_rcarbon_curves(
    df_subset = df_components |> filter(component == component_i),
    label = component_i,
    time_range = time_range_bp,
    nsim = nsim_ckde,
    bw = bw_main,
    use_bins = TRUE,
    run_spd = FALSE
  )
})
names(component_curve_list) <- component_order

ckde_components <- bind_rows(lapply(component_curve_list, function(x) x$ckde)) |>
  mutate(component = factor(label, levels = component_order)) |>
  left_join(
    df_components |>
      distinct(component, component_type) |>
      mutate(component = factor(component, levels = component_order)),
    by = "component"
  )

amplitude <- 0.70

component_positions <- data.frame(
  component = factor(component_order, levels = component_order),
  y_base = rev(seq_along(component_order)),
  stringsAsFactors = FALSE
) |>
  mutate(
    component_label = case_when(
      as.character(component) == "Non-diagnostic human-presence layer" ~
        "Non-diagnostic human-presence layer",
      TRUE ~ as.character(component)
    )
  )

ckde_components_stacked <- ckde_components |>
  group_by(component) |>
  mutate(
    density_max = max(density, na.rm = TRUE),
    density_scaled = ifelse(density_max > 0, density / density_max, 0)
  ) |>
  ungroup() |>
  left_join(component_positions, by = "component") |>
  mutate(
    y_plot = y_base + amplitude * density_scaled,
    component_type = ifelse(
      component == "Non-diagnostic human-presence layer",
      "Non-diagnostic human-presence layer",
      "Diagnostic chrono-cultural component"
    )
  )

component_cols <- c(
  "Diagnostic chrono-cultural component" = "#000000",
  "Non-diagnostic human-presence layer" = "#7A7A7A"
)

component_linetypes <- c(
  "Diagnostic chrono-cultural component" = "solid",
  "Non-diagnostic human-presence layer" = "longdash"
)

base_theme_components <- theme_classic(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    axis.title.x = element_text(size = 9),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.35),
    axis.ticks.x = element_line(linewidth = 0.35),
    panel.grid = element_blank(),
    plot.margin = margin(4, 10, 4, 4)
  )

p_fig_b <- ggplot() +
  geom_hline(
    data = component_positions,
    aes(yintercept = y_base),
    colour = "#E0E0E0",
    linewidth = 0.30
  ) +
  geom_line(
    data = ckde_components_stacked,
    aes(
      x = age_ka_cal_bp,
      y = y_plot,
      colour = component_type,
      linetype = component_type,
      group = component
    ),
    linewidth = 0.70,
    lineend = "round"
  ) +
  scale_colour_manual(values = component_cols) +
  scale_linetype_manual(values = component_linetypes) +
  scale_x_reverse(
    limits = time_range_ka,
    breaks = seq(50, 25, by = -5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = component_positions$y_base,
    labels = component_positions$component_label,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_cartesian(ylim = c(0.8, max(component_positions$y_base) + amplitude + 0.15)) +
  labs(
    title = "B",
    x = "Age (ka cal BP)"
  ) +
  base_theme_components

export_combined_r1 <- function(plot_a,
                               plot_b,
                               path_base,
                               width_mm = 170,
                               height_mm = 205) {
  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)

  draw_combined <- function() {
    grid.newpage()
    pushViewport(
      viewport(
        layout = grid.layout(
          nrow = 2,
          ncol = 1,
          heights = unit(c(0.40, 0.60), "null")
        )
      )
    )
    print(plot_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(plot_b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  }

  pdf(file = paste0(path_base, ".pdf"), width = width_mm / 25.4, height = height_mm / 25.4, useDingbats = FALSE)
  draw_combined()
  dev.off()

  png(filename = paste0(path_base, ".png"), width = width_mm, height = height_mm, units = "mm", res = 600)
  draw_combined()
  dev.off()

  if (isTRUE(write_tiff)) {
    tiff(filename = paste0(path_base, ".tiff"), width = width_mm, height = height_mm, units = "mm", res = 600, compression = "lzw")
    draw_combined()
    dev.off()
  }
}

export_combined_r1(
  plot_a = p_fig_a,
  plot_b = p_fig_b,
  path_base = file.path(fig_main_dir, "fig_results_R1_human_presence_structure_main")
)

p_spd_diag <- ggplot(
  spd_diag,
  aes(x = age_ka_cal_bp, y = density, colour = series, linetype = series)
) +
  geom_line(linewidth = 0.68, lineend = "round") +
  scale_colour_manual(values = series_cols, labels = series_plot_labels) +
  scale_linetype_manual(values = series_linetypes, labels = series_plot_labels) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  scale_x_reverse(
    limits = time_range_ka,
    breaks = seq(50, 25, by = -5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Age (ka cal BP)",
    y = "Area-normalised SPD density"
  ) +
  base_theme_main

export_figure(
  p_spd_diag,
  file.path(fig_si_dir, "fig_si_spd_human_presence_structure_v3"),
  width_mm = 170,
  height_mm = 82,
  write_tiff = write_tiff
)

# Supplementary SPD decomposition by strict chrono-cultural component.

strict_component_order <- setdiff(component_order, "Non-diagnostic human-presence layer")

strict_spd_curves <- lapply(strict_component_order, function(component_i) {
  run_rcarbon_curves(
    df_subset = df_cultural_components |> filter(component == component_i),
    label = component_i,
    time_range = time_range_bp,
    nsim = nsim_ckde,
    bw = bw_main,
    use_bins = TRUE,
    run_spd = TRUE
  )$spd
}) |>
  bind_rows() |>
  mutate(component = factor(label, levels = strict_component_order))

strict_total_spd <- run_rcarbon_curves(
  df_subset = df_cultural,
  label = "Diagnostic chrono-cultural subset",
  time_range = time_range_bp,
  nsim = nsim_ckde,
  bw = bw_main,
  use_bins = TRUE,
  run_spd = TRUE
)$spd

tech_cols <- c(
  "MP" = "#000000",
  "Ch\u00e2telperronian" = "#CC79A7",
  "Protoaurignacian" = "#0072B2",
  "Aurignacian" = "#D55E00",
  "Gravettian" = "#009E73"
)

p_spd_by_component <- ggplot() +
  geom_line(
    data = strict_total_spd,
    aes(x = time_cal_bp, y = density),
    colour = "#BDBDBD",
    linewidth = 0.55,
    alpha = 0.8
  ) +
  geom_line(
    data = strict_spd_curves,
    aes(x = time_cal_bp, y = density, colour = component),
    linewidth = 0.75,
    lineend = "round"
  ) +
  scale_colour_manual(values = tech_cols[strict_component_order]) +
  scale_x_reverse(
    limits = time_range_bp,
    breaks = seq(50000, 25000, by = -5000),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ component, ncol = 1, scales = "free_y") +
  labs(
    x = "Age (cal BP)",
    y = "Area-normalised SPD density"
  ) +
  base_theme_si +
  theme(legend.position = "none")

export_figure(
  p_spd_by_component,
  file.path(fig_si_dir, "fig_si_spd_strict_cultural_by_technocomplex_v1"),
  width_mm = 170,
  height_mm = 190,
  write_tiff = write_tiff
)

# Supplementary CKDE/SPD checks for the three analytical subsets.

subset_dfs <- list(
  "Comparable radiocarbon base" = df_14c_primary,
  "Human-presence subset" = df_human,
  "Diagnostic chrono-cultural subset" = df_cultural
)

subset_levels <- names(subset_dfs)

subset_cols <- c(
  "Comparable radiocarbon base" = "#9A9A9A",
  "Human-presence subset" = "#000000",
  "Diagnostic chrono-cultural subset" = "#0072B2"
)

subset_linetypes <- c(
  "Comparable radiocarbon base" = "dotdash",
  "Human-presence subset" = "solid",
  "Diagnostic chrono-cultural subset" = "longdash"
)

subset_curves <- lapply(names(subset_dfs), function(label) {
  run_rcarbon_curves(
    df_subset = subset_dfs[[label]],
    label = label,
    time_range = time_range_bp,
    nsim = nsim_ckde,
    bw = bw_main,
    use_bins = TRUE,
    run_spd = TRUE
  )
})
names(subset_curves) <- subset_levels

all_curves <- bind_rows(
  bind_rows(lapply(subset_curves, function(x) x$ckde)),
  bind_rows(lapply(subset_curves, function(x) x$spd))
) |>
  mutate(
    subset = factor(label, levels = subset_levels),
    curve_type = factor(curve_type, levels = c("CKDE", "SPD"), labels = c("A. CKDE", "B. SPD"))
  )

p_ckde_spd <- ggplot(
  all_curves,
  aes(x = time_cal_bp, y = density, colour = subset, linetype = subset)
) +
  geom_line(linewidth = 0.6, lineend = "round") +
  scale_colour_manual(values = subset_cols) +
  scale_linetype_manual(values = subset_linetypes) +
  scale_x_reverse(
    limits = time_range_bp,
    breaks = seq(50000, 25000, by = -5000),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ curve_type, ncol = 1, scales = "free_y") +
  labs(
    x = "Age (cal BP)",
    y = "Density"
  ) +
  base_theme_si

export_figure(
  p_ckde_spd,
  file.path(fig_si_dir, "fig_si_ckde_spd_three_subsets_v1"),
  width_mm = 170,
  height_mm = 170,
  write_tiff = write_tiff
)

binned_unbinned <- lapply(names(subset_dfs), function(label) {
  binned <- run_rcarbon_curves(
    df_subset = subset_dfs[[label]],
    label = label,
    time_range = time_range_bp,
    nsim = nsim_ckde,
    bw = bw_main,
    use_bins = TRUE,
    run_spd = FALSE
  )$ckde |>
    mutate(binning = "Binned")

  unbinned <- run_rcarbon_curves(
    df_subset = subset_dfs[[label]],
    label = label,
    time_range = time_range_bp,
    nsim = nsim_ckde,
    bw = bw_main,
    use_bins = FALSE,
    run_spd = FALSE
  )$ckde |>
    mutate(binning = "Unbinned")

  bind_rows(binned, unbinned)
}) |>
  bind_rows() |>
  mutate(
    subset = factor(label, levels = subset_levels),
    binning = factor(binning, levels = c("Binned", "Unbinned"))
  )

p_binning <- ggplot(
  binned_unbinned,
  aes(x = time_cal_bp, y = density, colour = binning, linetype = binning)
) +
  geom_line(linewidth = 0.6, lineend = "round") +
  scale_colour_manual(values = c("Binned" = "#000000", "Unbinned" = "#999999")) +
  scale_linetype_manual(values = c("Binned" = "solid", "Unbinned" = "dashed")) +
  scale_x_reverse(
    limits = time_range_bp,
    breaks = seq(50000, 25000, by = -5000),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ subset, ncol = 1, scales = "free_y") +
  labs(
    x = "Age (cal BP)",
    y = "Composite kernel density"
  ) +
  base_theme_si

export_figure(
  p_binning,
  file.path(fig_si_dir, "fig_si_ckde_binned_vs_unbinned_v1"),
  width_mm = 170,
  height_mm = 190,
  write_tiff = write_tiff
)

bw_curves <- lapply(names(subset_dfs), function(label) {
  lapply(bw_values, function(bw_i) {
    run_rcarbon_curves(
      df_subset = subset_dfs[[label]],
      label = label,
      time_range = time_range_bp,
      nsim = nsim_ckde,
      bw = bw_i,
      use_bins = TRUE,
      run_spd = FALSE
    )$ckde |>
      mutate(bandwidth = paste0("bw = ", bw_i, " yr"))
  }) |>
    bind_rows()
}) |>
  bind_rows() |>
  mutate(
    subset = factor(label, levels = subset_levels),
    bandwidth = factor(bandwidth, levels = paste0("bw = ", bw_values, " yr"))
  )

p_bandwidth <- ggplot(
  bw_curves,
  aes(x = time_cal_bp, y = density, colour = bandwidth, linetype = bandwidth)
) +
  geom_line(linewidth = 0.55, lineend = "round") +
  scale_colour_manual(
    values = c(
      "bw = 100 yr" = "#666666",
      "bw = 200 yr" = "#000000",
      "bw = 300 yr" = "#BBBBBB"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "bw = 100 yr" = "dotted",
      "bw = 200 yr" = "solid",
      "bw = 300 yr" = "longdash"
    )
  ) +
  scale_x_reverse(
    limits = time_range_bp,
    breaks = seq(50000, 25000, by = -5000),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ subset, ncol = 1, scales = "free_y") +
  labs(
    x = "Age (cal BP)",
    y = "Composite kernel density"
  ) +
  base_theme_si

export_figure(
  p_bandwidth,
  file.path(fig_si_dir, "fig_si_ckde_bandwidth_sensitivity_v1"),
  width_mm = 170,
  height_mm = 190,
  write_tiff = write_tiff
)

cat("\nCompleted ", script_build, "\n", sep = "")
cat("Figures written to:\n")
cat("  ", fig_main_dir, "\n", sep = "")
cat("  ", fig_si_dir, "\n", sep = "")
