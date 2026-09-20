# 00_setup.R
# Install and load all dependencies for the PGA tournament projection model

packages <- c(
  "tidyverse",    # data wrangling
  "httr",         # GraphQL API calls
  "jsonlite",     # parse JSON responses
  "rvest",        # scrape live API key from pgatour.com
  "glmmTMB",      # binomial hierarchical models (make cut, top 20)
  "lme4",         # mixed effects (scoring model)
  "lubridate",    # date handling
  "zoo",          # rolling averages
  "gt",           # clean projection tables
  "openxlsx",     # Excel export
  "stringi"       # name encoding normalization
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

lapply(packages, install_if_missing)
lapply(packages, library, character.only = TRUE)

message("All packages loaded.")
