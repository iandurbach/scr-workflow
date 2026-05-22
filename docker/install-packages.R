options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  Ncpus = max(1L, parallel::detectCores() - 1L)
)

required_packages <- c(
  "dplyr",
  "elevatr",
  "gdistance",
  "ggplot2",
  "here",
  "knitr",
  "lubridate",
  "patchwork",
  "quarto",
  "rmarkdown",
  "secr",
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

