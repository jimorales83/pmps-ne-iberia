# R Reproduction Scripts

Run scripts from the repository root or by passing their full path to `Rscript`.

Recommended order:

1. `R/03_check_public_repository.R`
2. `R/01_reproduce_rcarbon_figures.R`
3. `R/02_reproduce_oxcal_figures.R`

The rcarbon script recalculates CKDE/SPD figures from `data/final/pmps_ne_iberia_dated_corpus.csv`. The OxCal script redraws Results figures R2-R4 from `oxcal/regional_outputs/*.csv`.

The scripts were tested with R 4.5.2. Required packages are `dplyr`, `ggplot2`, `readr`, `rcarbon`, `stringr` and `tibble`.

Optional environment variables:

- `PMPS_NSIM`: number of simulations for CKDE calculations. Default: `1000`.
- `PMPS_SEED`: random seed for rcarbon sampling. Default: `20260525`.
- `PMPS_WRITE_TIFF`: write TIFF files as well as PDF/PNG. Default: `true`.
- `PMPS_OUTPUT_VARIANT`: optional figure-output subfolder under `figures/`, useful for tests. Example: `_test`.
