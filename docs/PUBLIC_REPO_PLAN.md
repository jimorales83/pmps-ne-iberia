# Public Repository Plan

## Publication Model

Recommended model:

- GitHub as the public, navigable repository.
- Zenodo as the archived paper release with DOI.
- A final GitHub release, for example `v1.0.0-paper`, should be deposited in Zenodo.

## Included in the Draft Public Repository

### Data

- `data/final/pmps_ne_iberia_dated_corpus.csv`
- `data/final/pmps_ne_iberia_dated_corpus.xlsx`
- `data/site_normalized/*.csv`

The final corpus is copied from the working repository's unified derived dataset. The site-normalized CSVs are retained for traceability. Internal working-note fields from the private research vault are removed from the public copies.

### OxCal

- `oxcal/regional_models/*.oxcal`
- `oxcal/regional_outputs/*.csv`

The current draft includes regional model code and selected CSV outputs used in the paper. Local OxCal models are not yet copied because the public set should distinguish consolidated models from exploratory or sensitivity variants.

### Figures and Tables

- `figures/main/`
- `figures/supplementary/`
- `tables/main/`
- `tables/supplementary/`
- `tables/table_manifest.csv`

These are copied from the paper-ready `figures_tables` area of the working repository.

## Pending Decisions

1. Decide whether to include source site spreadsheets or only the final unified and normalized datasets.
2. Select local OxCal models for public release.
3. Decide whether selected local OxCal outputs should be included alongside regional outputs.
4. Replace exploratory R scripts with a small set of clean reproduction scripts.
5. Finalize the data dictionary against the submitted manuscript terminology.
6. Add `CITATION.cff` when title, author list and DOI status are final.
7. Add a license after deciding the preferred reuse terms for data, code and figures.
8. Add the final Supplementary Online Material when the document is stable.

## Excluded from Public Release

- Obsidian vault and bibliography notes.
- `_local_archive/`.
- Manuscript `.docx` drafts and local working documents.
- Quarto generated folders, `_site`, `_freeze`, and exploratory rendered files.
- R session files such as `.RData` and `.Rhistory`.
- Internal audit notes and development-only logs.
- Exploratory model variants unless explicitly cited or needed for sensitivity reporting.
