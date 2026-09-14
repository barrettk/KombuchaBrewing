suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(DT)
  library(scales)
})

# Helper functions first, then the modules built on them. Nothing depends on
# load order within either folder.
for (file in list.files("functions", pattern = "\\.R$", full.names = TRUE)) source(file)
for (file in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(file)
