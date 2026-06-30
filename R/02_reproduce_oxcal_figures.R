# Reproduce Results figures based on exported regional OxCal outputs.

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

require_packages(c("dplyr", "ggplot2", "grid", "readr", "tibble"))

library(dplyr)
library(ggplot2)
library(grid)

script_build <- "public_oxcal_figures_v5"
cat("\nRunning ", script_build, "\n", sep = "")

write_tiff <- tolower(Sys.getenv("PMPS_WRITE_TIFF", "true")) %in% c("1", "true", "yes")
fig_main_dir <- figure_path("main")

node_interval <- function(oxcal, node, source_label, signed = FALSE) {
  ox_row <- get_oxcal_row(oxcal, node, source_label)
  from_68 <- to_ka(ox_row$modelled_68_from)
  to_68 <- to_ka(ox_row$modelled_68_to)
  from_95 <- to_ka(ox_row$modelled_95_from)
  to_95 <- to_ka(ox_row$modelled_95_to)

  tibble::tibble(
    from_68 = from_68,
    to_68 = to_68,
    from_95 = from_95,
    to_95 = to_95,
    agreement = as.numeric(ox_row$C),
    interval_68 = format_interval(from_68, to_68, signed = signed),
    interval_95 = format_interval(from_95, to_95, signed = signed)
  )
}

base_theme_ranges <- theme_classic(base_size = 9) +
  theme(
    plot.title = element_text(size = 9.5, face = "bold", hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.line = element_line(linewidth = 0.35),
    axis.ticks = element_line(linewidth = 0.35),
    plot.margin = margin(4, 7, 4, 4)
  )

# Figure R2: final Middle Palaeolithic and immediate post-MP field.

ox_m5 <- read_oxcal_output("M5_FPM_S2_regional_PM_end_refined_v1.csv")

r2_events_spec <- tibble::tribble(
  ~group, ~display_label, ~node,
  "Older final MP", "Teixoneres III/II", "=Transition Teixoneres Unit III/II",
  "Terminal MP core", "Abric Roman\u00ed B", "=End AR Level B",
  "Terminal MP core", "L'Arbreda I", "=End ARB Level-I",
  "Late/open final MP", "Cova Gran S1B", "=End S1B",
  "Protoaurignacian", "Abric Roman\u00ed A", "=Start AR Level A",
  "Protoaurignacian", "L'Arbreda H", "=Start ARB Level-H",
  "Ch\u00e2telperronian", "Foradada IV.1-IV", "=Start FO Unit-IV.1-IV",
  "Open comparator", "Cova Gran 497D", "=Start 497D"
)

r2_events <- bind_rows(lapply(seq_len(nrow(r2_events_spec)), function(i) {
  spec <- r2_events_spec[i, ]
  bind_cols(
    spec,
    node_interval(ox_m5, spec$node, "M5-FPM-S2")
  )
}))

r2_diff_spec <- tibble::tribble(
  ~group, ~display_label, ~node,
  "Protoaurignacian comparison", "AR A vs L'Arbreda I", "Difference d_StartARProto_minus_EndARBLevelI",
  "Protoaurignacian comparison", "L'Arbreda H vs AR B", "Difference d_StartARBProto_minus_EndARLevelB",
  "Protoaurignacian comparison", "AR A vs S1B", "Difference d_StartARProto_minus_EndS1B",
  "Protoaurignacian comparison", "L'Arbreda H vs S1B", "Difference d_StartARBProto_minus_EndS1B",
  "Ch\u00e2telperronian comparison", "Foradada IV.1-IV vs L'Arbreda I", "Difference d_StartFOChat_minus_EndARBLevelI",
  "Ch\u00e2telperronian comparison", "Foradada IV.1-IV vs S1B", "Difference d_StartFOChat_minus_EndS1B"
)

r2_differences <- bind_rows(lapply(seq_len(nrow(r2_diff_spec)), function(i) {
  spec <- r2_diff_spec[i, ]
  bind_cols(
    spec,
    node_interval(ox_m5, spec$node, "M5-FPM-S2", signed = TRUE)
  )
}))

r2_events$display_label <- factor(r2_events$display_label, levels = rev(r2_events_spec$display_label))
r2_differences$display_label <- factor(r2_differences$display_label, levels = rev(r2_diff_spec$display_label))

r2_cols <- c(
  "Older final MP" = "#8A8A8A",
  "Terminal MP core" = "#000000",
  "Late/open final MP" = "#4D4D4D",
  "Protoaurignacian" = "#0072B2",
  "Ch\u00e2telperronian" = "#D55E00",
  "Open comparator" = "#6A3D9A",
  "Protoaurignacian comparison" = "#0072B2",
  "Ch\u00e2telperronian comparison" = "#D55E00"
)

p_r2_a <- ggplot(r2_events, aes(y = display_label, colour = group)) +
  geom_segment(aes(x = from_95, xend = to_95, yend = display_label), linewidth = 0.85, lineend = "round", alpha = 0.38) +
  geom_segment(aes(x = from_68, xend = to_68, yend = display_label), linewidth = 2.55, lineend = "round") +
  scale_colour_manual(values = r2_cols) +
  scale_x_reverse(limits = c(45.0, 38.5), breaks = seq(45, 39, by = -1), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "A. Final Middle Palaeolithic and immediate post-MP estimates", x = "Age (ka cal BP)", y = NULL) +
  base_theme_ranges +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

p_r2_b <- ggplot(r2_differences, aes(y = display_label, colour = group)) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "#BDBDBD") +
  geom_segment(aes(x = from_95, xend = to_95, yend = display_label), linewidth = 0.85, lineend = "round", alpha = 0.38) +
  geom_segment(aes(x = from_68, xend = to_68, yend = display_label), linewidth = 2.55, lineend = "round") +
  scale_colour_manual(values = r2_cols) +
  scale_x_continuous(limits = c(-3.5, 4.0), breaks = seq(-3, 4, by = 1), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "B. Selected chronological differences", x = "Difference (ka)", y = NULL) +
  base_theme_ranges +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

export_r2 <- function(path_base, width_mm = 170, height_mm = 175) {
  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)

  draw_legend <- function() {
    category_keys <- c(
      "Terminal MP core",
      "Late/open final MP",
      "Protoaurignacian",
      "Ch\u00e2telperronian",
      "Open comparator"
    )
    category_labels <- c(
      "Terminal MP",
      "Late/open final MP",
      "Protoaurignacian",
      "Ch\u00e2telperronian",
      "Open comparator"
    )
    cols <- r2_cols[category_keys]
    x_start <- unit(c(0.22, 0.33, 0.49, 0.64, 0.80), "npc")
    y <- unit(0.22, "npc")

    grid.text("Probability interval", x = unit(0.035, "npc"), y = unit(0.72, "npc"), just = "left", gp = gpar(fontsize = 6.6, fontface = "bold"))
    grid.lines(x = unit(c(0.22, 0.248), "npc"), y = unit(c(0.72, 0.72), "npc"), gp = gpar(col = "#3A3A3A", lwd = 5.0, lineend = "round"))
    grid.text("68.3%", x = unit(0.255, "npc"), y = unit(0.72, "npc"), just = "left", gp = gpar(fontsize = 6.3))
    grid.lines(x = unit(c(0.34, 0.368), "npc"), y = unit(c(0.72, 0.72), "npc"), gp = gpar(col = adjustcolor("#3A3A3A", alpha.f = 0.38), lwd = 1.8, lineend = "round"))
    grid.text("95.4%", x = unit(0.375, "npc"), y = unit(0.72, "npc"), just = "left", gp = gpar(fontsize = 6.3))
    grid.text("Analytical category", x = unit(0.035, "npc"), y = y, just = "left", gp = gpar(fontsize = 6.6, fontface = "bold"))

    for (i in seq_along(category_keys)) {
      grid.lines(
        x = unit.c(x_start[i], x_start[i] + unit(0.026, "npc")),
        y = unit.c(y, y),
        gp = gpar(col = cols[i], lwd = 5.0, lineend = "round")
      )
      grid.text(category_labels[i], x = x_start[i] + unit(0.033, "npc"), y = y, just = "left", gp = gpar(fontsize = 6.2))
    }
  }

  draw_combined <- function() {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 1, heights = unit(c(0.54, 0.385, 0.075), "null"))))
    print(p_r2_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_r2_b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
    draw_legend()
    popViewport()
  }

  pdf(paste0(path_base, ".pdf"), width = width_mm / 25.4, height = height_mm / 25.4, useDingbats = FALSE)
  draw_combined()
  dev.off()
  png(paste0(path_base, ".png"), width = width_mm, height = height_mm, units = "mm", res = 600)
  draw_combined()
  dev.off()
  if (isTRUE(write_tiff)) {
    tiff(paste0(path_base, ".tiff"), width = width_mm, height = height_mm, units = "mm", res = 600, compression = "lzw")
    draw_combined()
    dev.off()
  }
}

export_r2(file.path(fig_main_dir, "fig4_results_R2_final_mp_post_mp_main"))

# Figure R3: final MP sensitivity.

r3_models <- tibble::tribble(
  ~model_id, ~model_label, ~panel_label, ~file_name,
  "M3", "M3 transition core", "A. Transition-core baseline", "M3_transition_core_baseline_v1.csv",
  "M4-CP-S1", "M4-CP-S1 Proto before Chat", "B. Protoaurignacian before Ch\u00e2telperronian", "M4_CP_S1_ARProto_before_Chat_v1.csv",
  "M5-FPM-S2", "M5-FPM-S2 final-MP model", "C. Final Middle Palaeolithic model", "M5_FPM_S2_regional_PM_end_refined_v1.csv"
)

r3_events_spec <- tibble::tribble(
  ~family, ~display_label, ~node,
  "Terminal MP core", "Abric Roman\u00ed B", "=End AR Level B",
  "Terminal MP core", "L'Arbreda I", "=End ARB Level-I",
  "Late/open final MP", "Cova Gran S1B", "=End S1B",
  "Protoaurignacian", "Abric Roman\u00ed A", "=Start AR Level A",
  "Protoaurignacian", "L'Arbreda H", "=Start ARB Level-H",
  "Ch\u00e2telperronian", "Foradada IV.1-IV", "=Start FO Unit-IV.1-IV",
  "Open comparator", "Cova Gran 497D", "=Start 497D"
)

r3_data <- bind_rows(lapply(seq_len(nrow(r3_models)), function(i) {
  model <- r3_models[i, ]
  ox <- read_oxcal_output(model$file_name)
  bind_rows(lapply(seq_len(nrow(r3_events_spec)), function(j) {
    spec <- r3_events_spec[j, ]
    bind_cols(
      model,
      spec,
      node_interval(ox, spec$node, model$model_id)
    )
  }))
}))

r3_data$display_label <- factor(r3_data$display_label, levels = rev(r3_events_spec$display_label))
r3_data$panel_label <- factor(r3_data$panel_label, levels = r3_models$panel_label)

r3_plot_data <- bind_rows(
  r3_data |>
    transmute(
      across(c(model_id, model_label, panel_label, family, display_label)),
      probability_interval = "95.4% probability",
      interval_from = from_95,
      interval_to = to_95
    ),
  r3_data |>
    transmute(
      across(c(model_id, model_label, panel_label, family, display_label)),
      probability_interval = "68.3% probability",
      interval_from = from_68,
      interval_to = to_68
    )
)

r3_plot_data$probability_interval <- factor(
  r3_plot_data$probability_interval,
  levels = c("68.3% probability", "95.4% probability")
)

r3_cols <- c(
  "Terminal MP core" = "#000000",
  "Late/open final MP" = "#4D4D4D",
  "Protoaurignacian" = "#0072B2",
  "Ch\u00e2telperronian" = "#D55E00",
  "Open comparator" = "#6A3D9A"
)

p_r3 <- ggplot(
  r3_plot_data,
  aes(
    y = display_label,
    colour = family,
    linewidth = probability_interval,
    alpha = probability_interval
  )
) +
  geom_segment(aes(x = interval_from, xend = interval_to, yend = display_label), lineend = "round") +
  facet_wrap(~ panel_label, ncol = 2) +
  scale_colour_manual(
    name = "Analytical category",
    values = r3_cols,
    breaks = names(r3_cols),
    labels = c(
      "Terminal MP",
      "Late/open final MP",
      "Protoaurignacian",
      "Ch\u00e2telperronian",
      "Open comparator"
    )
  ) +
  scale_linewidth_manual(
    name = "Probability interval",
    values = c("68.3% probability" = 2.55, "95.4% probability" = 0.85),
    breaks = c("68.3% probability", "95.4% probability"),
    labels = c("68.3%", "95.4%")
  ) +
  scale_alpha_manual(
    values = c("68.3% probability" = 1, "95.4% probability" = 0.38),
    guide = "none"
  ) +
  scale_x_reverse(limits = c(45.0, 38.7), breaks = seq(45, 39, by = -1), expand = expansion(mult = c(0.01, 0.02))) +
  guides(
    linewidth = guide_legend(
      order = 1,
      override.aes = list(colour = "#3A3A3A", alpha = c(1, 0.38))
    ),
    colour = guide_legend(
      order = 2,
      ncol = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 2.8, alpha = 1)
    )
  ) +
  labs(x = "Age (ka cal BP)", y = NULL) +
  theme_classic(base_size = 9) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9.5, face = "bold", hjust = 0),
    legend.position = "inside",
    legend.position.inside = c(0.77, 0.23),
    legend.justification = c(0.5, 0.5),
    legend.title = element_text(size = 6.8, face = "bold"),
    legend.text = element_text(size = 6.7),
    legend.key.width = unit(9, "mm"),
    legend.key.height = unit(3.2, "mm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(1.2, "mm"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.line = element_line(linewidth = 0.35),
    axis.ticks = element_line(linewidth = 0.35),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing.x = unit(10, "mm"),
    panel.spacing.y = unit(6, "mm"),
    plot.margin = margin(5, 7, 4, 4)
  )

export_figure(
  p_r3,
  file.path(fig_main_dir, "fig5_results_R3_final_mp_sensitivity_main"),
  width_mm = 180,
  height_mm = 140,
  write_tiff = write_tiff
)

# Figure R4: Chatelperronian, Protoaurignacian and Early Aurignacian field.

r4_models <- tibble::tribble(
  ~model_id, ~scenario_label, ~scenario_short, ~file_name,
  "M3", "Transition-core baseline", "Transition-core", "M3_transition_core_baseline_v1.csv",
  "M4-CP-S1", "Protoaurignacian before Ch\u00e2telperronian", "Proto-before-Chat", "M4_CP_S1_ARProto_before_Chat_v1.csv",
  "M4-CP-S2", "Near-contact scenario", "Near-contact", "M4_CP_S2_ARProto_sync_with_Chat_v1.csv"
)

r4_outputs <- lapply(r4_models$file_name, read_oxcal_output)
names(r4_outputs) <- r4_models$model_id

r4_panel_a_spec <- tibble::tribble(
  ~family, ~display_label, ~node,
  "Protoaurignacian", "Start Abric Roman\u00ed A", "=Start AR Level A",
  "Protoaurignacian", "End Abric Roman\u00ed A", "=End AR Level A",
  "Protoaurignacian", "Start L'Arbreda H", "=Start ARB Level-H",
  "Protoaurignacian", "End L'Arbreda H", "=End ARB Level-H",
  "Ch\u00e2telperronian", "Start Foradada IV.1-IV", "=Start FO Unit-IV.1-IV",
  "Ch\u00e2telperronian", "End Foradada IV.1-IV", "=End FO Unit-IV.1-IV",
  "Early Aurignacian", "Start Foradada IIIc", "=Start FO Unit-IIIc"
)

r4_panel_a <- bind_rows(lapply(seq_len(nrow(r4_panel_a_spec)), function(i) {
  spec <- r4_panel_a_spec[i, ]
  bind_cols(
    tibble::tibble(panel = "A", model_id = "M3"),
    spec,
    node_interval(r4_outputs[["M3"]], spec$node, "M3")
  )
}))

r4_panel_b_spec <- tibble::tribble(
  ~comparison_group, ~comparison_label, ~node,
  "Start-start", "AR A start vs Foradada IV.1-IV start", "Difference d_StartARProto_minus_StartFOChat",
  "Start-end", "AR A start vs Foradada IV.1-IV end", "Difference d_StartARProto_minus_EndFOChat"
)

r4_panel_b <- bind_rows(lapply(seq_len(nrow(r4_models)), function(i) {
  model <- r4_models[i, ]
  ox <- r4_outputs[[model$model_id]]
  bind_rows(lapply(seq_len(nrow(r4_panel_b_spec)), function(j) {
    spec <- r4_panel_b_spec[j, ]
    bind_cols(
      tibble::tibble(panel = "B"),
      model,
      spec,
      node_interval(ox, spec$node, model$model_id, signed = TRUE)
    ) |>
      mutate(display_label = paste0(scenario_short, ": ", comparison_label))
  }))
}))

r4_panel_c <- bind_rows(lapply(seq_len(nrow(r4_models)), function(i) {
  model <- r4_models[i, ]
  ox <- r4_outputs[[model$model_id]]
  bind_cols(
    tibble::tibble(
      panel = "C",
      comparison_group = "Early Aurignacian separation",
      comparison_label = "Foradada IIIc start vs AR A end"
    ),
    model,
    node_interval(ox, "Difference d_StartFOAur_minus_EndARProto", model$model_id, signed = TRUE)
  ) |>
    mutate(display_label = paste0(scenario_short, ": Foradada IIIc vs AR A end"))
}))

r4_panel_a$display_label <- factor(r4_panel_a$display_label, levels = rev(r4_panel_a_spec$display_label))
r4_panel_b$display_label <- factor(
  r4_panel_b$display_label,
  levels = rev(c(
    "Transition-core: AR A start vs Foradada IV.1-IV start",
    "Proto-before-Chat: AR A start vs Foradada IV.1-IV start",
    "Near-contact: AR A start vs Foradada IV.1-IV start",
    "Transition-core: AR A start vs Foradada IV.1-IV end",
    "Proto-before-Chat: AR A start vs Foradada IV.1-IV end",
    "Near-contact: AR A start vs Foradada IV.1-IV end"
  ))
)
r4_panel_c$display_label <- factor(
  r4_panel_c$display_label,
  levels = rev(c(
    "Transition-core: Foradada IIIc vs AR A end",
    "Proto-before-Chat: Foradada IIIc vs AR A end",
    "Near-contact: Foradada IIIc vs AR A end"
  ))
)

r4_component_cols <- c(
  "Protoaurignacian" = "#0072B2",
  "Ch\u00e2telperronian" = "#D55E00",
  "Early Aurignacian" = "#009E73"
)

r4_comparison_cols <- c(
  "Start-start" = "#D55E00",
  "Start-end" = "#A84D00",
  "Early Aurignacian separation" = "#009E73"
)

base_theme_r4 <- base_theme_ranges +
  theme(
    plot.title = element_text(size = 9.5, face = "bold", hjust = 0),
    plot.margin = margin(5, 7, 4, 4)
  )

p_r4_a <- ggplot(r4_panel_a, aes(y = display_label, colour = family)) +
  geom_segment(aes(x = from_95, xend = to_95, yend = display_label), linewidth = 0.85, lineend = "round", alpha = 0.38) +
  geom_segment(aes(x = from_68, xend = to_68, yend = display_label), linewidth = 2.55, lineend = "round") +
  scale_colour_manual(values = r4_component_cols) +
  scale_x_reverse(limits = c(44.8, 36.5), breaks = seq(44, 37, by = -1), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "A. Transition-core estimates", x = "Age (ka cal BP)", y = NULL) +
  base_theme_r4 +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

p_r4_b <- ggplot(r4_panel_b, aes(y = display_label, colour = comparison_group)) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "#BDBDBD") +
  geom_segment(aes(x = from_95, xend = to_95, yend = display_label), linewidth = 0.85, lineend = "round", alpha = 0.38) +
  geom_segment(aes(x = from_68, xend = to_68, yend = display_label), linewidth = 2.55, lineend = "round") +
  scale_colour_manual(values = r4_comparison_cols) +
  scale_x_continuous(limits = c(-5.0, 7.2), breaks = seq(-4, 7, by = 2), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "B. Ch\u00e2telperronian/Protoaurignacian tests", x = "Difference (ka)", y = NULL) +
  base_theme_r4 +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

p_r4_c <- ggplot(r4_panel_c, aes(y = display_label, colour = comparison_group)) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "#BDBDBD") +
  geom_segment(aes(x = from_95, xend = to_95, yend = display_label), linewidth = 0.85, lineend = "round", alpha = 0.38) +
  geom_segment(aes(x = from_68, xend = to_68, yend = display_label), linewidth = 2.55, lineend = "round") +
  scale_colour_manual(values = r4_comparison_cols) +
  scale_x_continuous(limits = c(-0.5, 5.3), breaks = seq(0, 5, by = 1), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "C. Early Aurignacian separation", x = "Difference (ka)", y = NULL) +
  base_theme_r4 +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

export_r4 <- function(path_base, width_mm = 180, height_mm = 190) {
  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)

  draw_legend <- function() {
    category_keys <- c("Protoaurignacian", "Ch\u00e2telperronian", "Early Aurignacian")
    cols <- r4_component_cols[category_keys]
    x_start <- unit(c(0.22, 0.46, 0.69), "npc")
    y <- unit(0.22, "npc")

    grid.text("Probability interval", x = unit(0.035, "npc"), y = unit(0.72, "npc"), just = "left", gp = gpar(fontsize = 6.6, fontface = "bold"))
    grid.lines(x = unit(c(0.22, 0.248), "npc"), y = unit(c(0.72, 0.72), "npc"), gp = gpar(col = "#3A3A3A", lwd = 5.0, lineend = "round"))
    grid.text("68.3%", x = unit(0.255, "npc"), y = unit(0.72, "npc"), just = "left", gp = gpar(fontsize = 6.3))
    grid.lines(x = unit(c(0.34, 0.368), "npc"), y = unit(c(0.72, 0.72), "npc"), gp = gpar(col = adjustcolor("#3A3A3A", alpha.f = 0.38), lwd = 1.8, lineend = "round"))
    grid.text("95.4%", x = unit(0.375, "npc"), y = unit(0.72, "npc"), just = "left", gp = gpar(fontsize = 6.3))
    grid.text("Analytical category", x = unit(0.035, "npc"), y = y, just = "left", gp = gpar(fontsize = 6.6, fontface = "bold"))

    for (i in seq_along(category_keys)) {
      grid.lines(
        x = unit.c(x_start[i], x_start[i] + unit(0.028, "npc")),
        y = unit.c(y, y),
        gp = gpar(col = cols[i], lwd = 5.0, lineend = "round")
      )
      grid.text(category_keys[i], x = x_start[i] + unit(0.035, "npc"), y = y, just = "left", gp = gpar(fontsize = 6.3))
    }
  }

  draw_combined <- function() {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 1, heights = unit(c(0.43, 0.325, 0.175, 0.07), "null"))))
    print(p_r4_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(p_r4_b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(p_r4_c, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
    pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
    draw_legend()
    popViewport()
  }

  pdf(paste0(path_base, ".pdf"), width = width_mm / 25.4, height = height_mm / 25.4, useDingbats = FALSE)
  draw_combined()
  dev.off()
  png(paste0(path_base, ".png"), width = width_mm, height = height_mm, units = "mm", res = 600)
  draw_combined()
  dev.off()
  if (isTRUE(write_tiff)) {
    tiff(paste0(path_base, ".tiff"), width = width_mm, height = height_mm, units = "mm", res = 600, compression = "lzw")
    draw_combined()
    dev.off()
  }
}

export_r4(file.path(fig_main_dir, "fig6_results_R4_chat_proto_early_aurignacian_main"))

cat("\nCompleted ", script_build, "\n", sep = "")
cat("Figures written to:\n")
cat("  ", fig_main_dir, "\n", sep = "")
