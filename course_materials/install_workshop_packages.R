options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  Ncpus = max(1L, parallel::detectCores() - 1L)
)

# Run this script on a personal machine before starting the workshop locally.
# It installs the R packages used across the workshop materials.
#
# Note:
# Some spatial packages such as sf, terra, and gdistance may also need
# system libraries and compilers that are outside R itself. If installation
# fails for one of those packages, the problem is usually missing system
# dependencies rather than the R code below.

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
  "MASS",
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

message("Checking workshop package requirements...")

if (length(missing) == 0) {
  message("All required R packages are already installed.")
} else {
  message("Installing missing packages:")
  message(paste0(" - ", missing, collapse = "\n"))

  if (!"pak" %in% installed) {
    install.packages("pak")
  }

  pak::pkg_install(missing, ask = FALSE)
}

still_missing <- setdiff(required_packages, rownames(installed.packages()))

if (length(still_missing) == 0) {
  message("")
  message("Workshop package installation complete.")
  message("You can now open scr_workshop.Rproj and start the workshop scripts.")
} else {
  message("")
  message("These packages are still missing:")
  message(paste0(" - ", still_missing, collapse = "\n"))
  message("")
  message("If the missing packages include sf, terra, gdistance, or secr,")
  message("you may need to install system libraries first and then rerun this script.")
}
