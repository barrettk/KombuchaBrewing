# Load the app the way the app loads itself.
#
# `global.R` is sourced by path rather than run from the app directory, so the
# tests do not depend on the working directory testthat happens to start in.

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(DT)
  library(scales)
})

APP_DIR <- normalizePath(testthat::test_path("..", ".."))

# `AppDriver` skips itself on CRAN, and `NOT_CRAN` is only set automatically by
# devtools. This is an app rather than a package, so these tests never run under
# `R CMD check` and the browser tests would otherwise skip everywhere, quietly
# reporting success while testing nothing.
if (!nzchar(Sys.getenv("NOT_CRAN"))) {
  Sys.setenv(NOT_CRAN = "true")
}

for (f in list.files(file.path(APP_DIR, "functions"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
for (f in list.files(file.path(APP_DIR, "modules"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
