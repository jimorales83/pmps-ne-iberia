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

The rcarbon script recalculates CKDE/SPD figures from `data/final/pmps_ne_iberia_dated_corpus.csv`. The OxCal script redraws Results figures R2-R4 from `oxcal/regional_outputs/*.csv`.

The scripts were tested with R 4.5.2. Required packages are `dplyr`, `ggplot2`, `readr`, `rcarbon`, `stringr` and `tibble`.

Optional environment variables:

- `PMPS_NSIM`: number of simulations for CKDE calculations. Default: `1000`.
- `PMPS_SEED`: random seed for rcarbon sampling. Default: `20260525`.
- `PMPS_WRITE_TIFF`: write TIFF files as well as PDF/PNG. Default: `true`.
- `PMPS_OUTPUT_DIR`: optional output folder. Used by `run_reproduction.R` to avoid overwriting paper-ready figures.
- `PMPS_OUTPUT_VARIANT`: optional figure-output subfolder under `figures/`, useful for tests. Example: `_test`.
- `PMPS_KEY_FIGURES_ONLY`: skip the heavier supplementary CKDE/SPD checks when set to `true`.
