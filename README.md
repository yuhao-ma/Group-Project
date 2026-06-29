# Group-Project
# UBelong Student Mental Health Project

This project analyses longitudinal student mental health and belonging data from the UBelong dataset.

## Project structure

- scripts/01_clean_ubelong_data.R: imports and cleans the raw dataset, identifies valid response rows, and creates the longitudinal analysis subset.
- outputs/tables/: quality-check and summary tables.
- outputs/figures/: figures used in the report.
- report.Rmd: final analysis report.

## Reproducibility

The raw dataset is not included in this repository due to data access restrictions. To reproduce the analysis, place the original CSV file in data/raw/ and run scripts/01_clean_ubelong_data.R.
