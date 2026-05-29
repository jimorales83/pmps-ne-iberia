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
- `oxcal/local_models/*/*.oxcal`
- `oxcal/local_outputs/**/*`

The current draft includes regional model code, selected regional CSV outputs, and the site-level local models and outputs retained for traceability. Bibliographic reference PDFs from the private working repository are excluded.

### Figures and Tables

- `figures/main/`
- `figures/supplementary/`
- `tables/main/`
- `tables/supplementary/`
- `tables/table_manifest.csv`

These are copied from the paper-ready `figures_tables` area of the working repository.

## Pending Decisions

1. Decide whether to include source site spreadsheets or only the final unified and normalized datasets.
2. Finalize the data dictionary against the submitted manuscript terminology.
3. Add `CITATION.cff` when title, author list and DOI status are final.
4. Add a license after deciding the preferred reuse terms for data, code and figures.
5. Add the final Supplementary Online Material when the document is stable.

## Excluded from Public Release

- Obsidian vault and bibliography notes.
- `_local_archive/`.
- Manuscript `.docx` drafts and local working documents.
- Quarto generated folders, `_site`, `_freeze`, and exploratory rendered files.
- R session files such as `.RData` and `.Rhistory`.
- Internal audit notes and development-only logs.
- Bibliographic PDFs stored beside local OxCal workspaces.
