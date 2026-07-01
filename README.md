# PMPS NE Iberia

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21101127.svg)](https://doi.org/10.5281/zenodo.21101127)

Data, chronological models, figures and tables supporting the paper *The Middle-to-Upper Palaeolithic transition in northeastern Iberia as a non-linear regional system*.

This repository contains the dated corpus used in the analyses, the retained OxCal models and outputs, the R reproduction workflow, and the publication tables and figures that can be redistributed. Private working notes, manuscript drafts and superseded exploratory material are intentionally excluded.

## Contents

- `data/final/`: canonical unified dated corpus used in the paper, in CSV and XLSX formats.
- `data/site_normalized/`: exact site-level partitions of the canonical corpus, retained for traceability.
- `oxcal/`: regional and local OxCal model code, outputs and model inventories.
- `figures/`: redistributable paper-ready main and supplementary figures.
- `tables/`: paper-ready main and supplementary tables, including figure-source tables where a figure is built from curated intermediate data.
- `R/`: minimal scripts for checking the repository and reproducing Figures 3-7 and the supplementary analytical figures.
- `docs/`: data dictionary.
- `LICENSE.md`: licensing scope and third-party material notes.

The semicolon-delimited CSV in `data/final/` is the canonical machine-readable dataset. The XLSX copy and the ten files in `data/site_normalized/` contain the same records.

## Reproduce the analyses

Open `R/run_reproduction.R` in RStudio and click **Source**. The workflow checks repository integrity and writes regenerated figures and summary tables to `reproduction_outputs/` without overwriting the paper-ready files.

The default run uses the paper settings for the main figures and the key CKDE/SPD check. See `R/README.md` for package versions and optional sensitivity runs.

## Reproducibility Scope

The repository supports audit and reproduction from the harmonised publication dataset onward. It does not reproduce the original transcription of published measurements or the full private research workflow.

The OxCal model code and exported outputs are preserved for audit. The R workflow redraws the analytical figures from the final corpus, the OxCal CSV exports and the documented figure-source tables; it does not run OxCal itself. Figure 1 is a GIS composition, Figure 2 is a conceptual workflow diagram and Figure 8 is an author-prepared archaeological composition.

## Licensing

Code in `R/` and OxCal model scripts is released under the MIT License. Data, documentation, tables and original or lawfully adapted visual materials are released under CC BY 4.0, subject to the source-specific notices in `LICENSE.md` and `figures/README.md`.

## Citation and release

Citation metadata are provided in `CITATION.cff`. The archived `v1.0.0`
release is available from Zenodo:

- Version DOI: https://doi.org/10.5281/zenodo.21101127
- Concept DOI for all versions: https://doi.org/10.5281/zenodo.21101126

Use the version DOI when citing the exact materials supporting the paper.
