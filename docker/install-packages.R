options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  Ncpus = max(1L, parallel::detectCores() - 1L)
)

required_packages <- c(
  "DT",
  "dplyr",
  "elevatr",
  "gdistance",
  "ggplot2",
  "here",
  "htmltools",
  "knitr",
  "leaflet",
  "lubridate",
  "patchwork",
  "quarto",
  "readr",
  "rmarkdown",
  "scales",
  "secr",
  "shiny",
  "shinydashboard",
  "sf",
  "stringr",
  "terra",
  "tidyr"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  if (!"pak" %in% installed) {
    install.packages("pak")
  }
  pak::pkg_install(missing, ask = FALSE)
}
