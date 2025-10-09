packages <- c(
  "shiny", "shinydashboard", "bslib", "tidyverse", "lubridate",
  "DT", "plotly", "leaflet", "sf", "readr", "tsibble", "fable", "fabletools",
  "feasts", "forecast", "scales", "fresh"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(packages, install_if_missing))



