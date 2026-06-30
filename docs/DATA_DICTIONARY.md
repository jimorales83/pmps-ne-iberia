# Data Dictionary

The canonical corpus is the semicolon-delimited UTF-8 CSV:

`data/final/pmps_ne_iberia_dated_corpus.csv`

The XLSX file in the same folder contains the same 185 records and 29 fields. Files in `data/site_normalized/` are exact site-level partitions of this corpus, not independent datasets.

Empty cells represent information that is not applicable or was not reported. Analytical inclusion fields use `yes` and `no`.

## Analytical Conventions

- `site_code` is the stable key linking the corpus, site-level files and coordinate table; display names may differ slightly between products.
- Analytical flags are used together with `evaluation_final == "include"`; a `yes` flag alone does not override the final evaluation.
- The exploratory radiocarbon figures use conventional radiocarbon ages from 45-25 ka 14C BP and then plot the calibrated distributions over 50-25 ka cal BP.
- `same_sample_group` identifies repeated measurements of the same sample so that they are not treated as independent evidence.

## Fields

- `site`: site name.
- `site_code`: short site code.
- `sector_area`: sector or excavation area.
- `level_unit`: stratigraphic level or unit.
- `sublevel_phase`: sublevel or phase, when used.
- `cultural_attribution`: chrono-cultural attribution.
- `cultural_attribution_confidence`: confidence assigned to the cultural attribution.
- `sample_id`: project sample identifier.
- `lab_code`: laboratory code.
- `method`: dating method.
- `pretreatment`: pretreatment protocol.
- `sample_type`: dated sample type.
- `taxon_material`: taxon or dated material.
- `age_result`: reported age.
- `error_1sigma`: one-sigma error.
- `age_scale_original`: original reporting scale.
- `curve`: calibration or reference curve.
- `reference`: bibliographic source.
- `year_sample_or_measure`: sample or measurement year, when recorded.
- `relation_to_human_presence`: relationship between the dated sample and human presence.
- `context_role`: analytical role of the context.
- `evaluation_final`: final evaluation category.
- `same_sample_group`: grouping for repeated dates on the same sample.
- `bin_id`: bin identifier used for radiocarbon aggregation.
- `use_14c_primary`: inclusion flag for the primary radiocarbon subset.
- `use_mixed_all_methods`: inclusion flag for the mixed-method subset.
- `use_human_presence`: inclusion flag for the human-presence subset.
- `use_cultural_subset`: inclusion flag for the diagnostic chrono-cultural subset.
- `selection_note`: short selection or exclusion note.
