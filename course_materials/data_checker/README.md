# SCR Data Checker

This Shiny app checks `traps` and `detections` csv files before they are used in a `secr` workflow. It proposes column-name mappings, requires the user to approve them, then runs file-level and cross-file checks and produces a downloadable HTML report.

## Run locally

From the `course_materials` R project, run:

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr",
  "stringr", "tidyr", "scales", "leaflet", "sf", "rmarkdown", "htmltools"
))
shiny::runApp("data_checker")
```

You can also run the launcher script from the `course_materials` project:

```r
source("data_checker/run_data_checker.R")
```

## Example files

The `data/` folder contains:

- `traps.csv` and `detections.csv`: cleaned workshop examples
- `traps_raw.csv` and `detections_raw.csv`: examples with non-standard column names for mapping tests
- `traps_bad.csv` and `detections_bad.csv`: examples containing intentional problems for validation tests

## Notes

- The app accepts either an EPSG code such as `EPSG:32644` or a full proj string in the CRS input box.
- If spatial rendering is not possible, the app falls back to a non-spatial scatterplot.
- The downloadable report is HTML and summarizes the same checks shown in the dashboard.
