# R Reproduction Scripts

## Easiest RStudio Workflow

Open `R/run_reproduction.R` in RStudio and click **Source**.

That script guides the whole process, checks the repository, summarises the corpus and writes reproduced figures and tables to:

`reproduction_outputs/`

The paper-ready figures in `figures/main/` and `figures/supplementary/` are not overwritten.

By default, the guided script reproduces the main analytical figures and the key CKDE/SPD check. The full supplementary CKDE/SPD sensitivity set can be run by changing `RUN_ALL_RCARBON_CHECKS <- TRUE` near the top of `R/run_reproduction.R`.

## Individual Scripts

Advanced users can also run the scripts separately:

1. `R/03_check_public_repository.R`
2. `R/02_reproduce_oxcal_figures.R`
3. `R/01_reproduce_rcarbon_figures.R`
4. `R/04_reproduce_discussion_figure.R`

The rcarbon script recalculates CKDE/SPD figures from `data/final/pmps_ne_iberia_dated_corpus.csv`. Analytical subsets are sorted by stable site, level and laboratory-code keys before stochastic sampling, so the fixed seed is independent of the physical row order in the input file. The OxCal script redraws Results figures R2-R4 from `oxcal/regional_outputs/*.csv`. The discussion-figure script redraws Figure 7 from the curated archaeological tables in `tables/main/`, the site coordinates in `data/final/` and the documented NGRIP/GICC05 subset in `tables/main/climate/`.

The main analytical outputs are written as `fig3_results_R1_human_presence_structure_main`, `fig4_results_R2_final_mp_post_mp_main`, `fig5_results_R3_final_mp_sensitivity_main` and `fig6_results_R4_chat_proto_early_aurignacian_main`. Figures 4-6 use compact shared keys for posterior intervals and analytical categories; in Figures 4 and 6 both key rows use the same aligned starting column. Figure 3 retains the distinct CKDE curve grammar while sharing typography and panel-title conventions.

The scripts were tested with R 4.5.2 and these package versions:

- `dplyr` 1.2.1
- `ggplot2` 4.0.3
- `readr` 2.2.0
- `rcarbon` 1.5.2
- `stringr` 1.6.0
- `tibble` 3.3.1

`grid` is used from the standard R installation. A complete `sessionInfo()` is written to `reproduction_outputs/session_info.txt` after each guided run.

Optional environment variables:

- `PMPS_NSIM`: number of simulations for CKDE calculations. Default: `1000`.
- `PMPS_SEED`: random seed for rcarbon sampling. Default: `20260525`.
- `PMPS_WRITE_TIFF`: write TIFF files as well as PDF/PNG. Default: `true`.
- `PMPS_OUTPUT_DIR`: optional output folder. Used by `run_reproduction.R` to avoid overwriting paper-ready figures.
- `PMPS_OUTPUT_VARIANT`: optional figure-output subfolder under `figures/`, useful for tests. Example: `_test`.
- `PMPS_KEY_FIGURES_ONLY`: skip the heavier supplementary CKDE/SPD checks when set to `true`.
