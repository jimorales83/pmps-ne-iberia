# OxCal Models and Outputs

This folder contains the OxCal material released with the paper.

## Structure

- `regional_models/`: regional Bayesian models used in the Results section.
- `regional_outputs/`: CSV exports from the regional models.
- `local_models/`: site-level OxCal models, organised by site.
- `local_outputs/`: site-level CSV outputs and selected posterior exports, organised by site.
- `local_model_manifest.csv`: inventory of local models and available paired outputs.

## Scope

The local models are included for traceability. They preserve the modelling history used to evaluate site-level sequences before building the regional chronological models. This includes consolidated models, baseline runs, sensitivity variants, candidate runs and replication checks where they were part of the analytical process.

Reference PDFs and bibliographic support files from the private working repository are not included here. The public release keeps the executable model code and exported model results.

## Reading the Model Names

Filenames are preserved from the working analysis to avoid breaking traceability.

- `baseline`: main site-level model at a given stage.
- `consolidated`: preferred or consolidated site-level version.
- `S1`, `S2`, etc.: sensitivity or alternative scenario.
- `candidate`: candidate model retained for comparison.
- `replicacion_publicada`: replication of a published model structure.
- `posterior`: posterior distribution export used for local inspection.

Some consolidated local models do not have a separate paired CSV export in the working archive. In those cases the model code is still retained and the manifest leaves `paired_output` blank.
