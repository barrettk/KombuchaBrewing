source("./global.R")

# UI ----------------------------------------------------------------------

ui <- shinydashboardPlus::dashboardPage(
  title = "Kombucha Brewing",
  skin = "midnight",
  shinydashboardPlus::dashboardHeader(
    title = tagList(
      span(
        class = "logo-lg",
        img(src = "img/logo-mark.png", class = "brand-mark", alt = ""),
        span(class = "brand-text", "Kombucha Brewing")
      ),
      span(
        class = "logo-mini",
        img(src = "img/logo-mark.png", class = "brand-mark", alt = "Kombucha Brewing")
      )
    ),
    titleWidth = "280px",
    leftUi = tagList(
      tags$li(
        class = "dropdown header-note",
        icon("flask"), "Batch planner and cost model"
      )
    )
  ),
  shinydashboardPlus::dashboardSidebar(
    width = "280px",
    sidebarMenu(
      div(
        class = "sidebar-brand",
        img(src = "img/logo-full.png", class = "brand-logo", alt = "")
      )
    ),
    hr(),
    sidebarMenu(
      id = "tabs",
      menuItem("Brew Setup", tabName = "setup_tab", icon = icon("flask"), selected = TRUE),
      menuItem("Ingredient Library", tabName = "library_tab", icon = icon("table-list")),
      menuItem("Cost Analysis", tabName = "cost_tab", icon = icon("chart-line"))
    ),
    hr()
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    tabItems(
      tabItem(
        tabName = "setup_tab",
        fluidRow(
          column(
            12,
            batch_ui("batch"),
            tabBox(
              id = "recipe",
              width = 12,
              tabPanel("F1 Ingredients", f1_ui("f1")),
              tabPanel("F2 Flavoring", f2_ui("f2")),
              tabPanel("Bottling", bottling_ui("bottling"))
            )
          )
        )
      ),
      tabItem(
        tabName = "library_tab",
        fluidRow(column(12, ingredient_library_ui("library")))
      ),
      tabItem(
        tabName = "cost_tab",
        fluidRow(column(12, cost_ui("cost")))
      )
    )
  ),
  footer = shinydashboardPlus::dashboardFooter(
    left = tagList(
      strong("Kombucha Brewing"),
      span(sprintf("Version %s", APP_VERSION))
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  ### Modules ###

  ingredients <- ingredient_library_server("library")
  batch <- batch_server("batch")

  f1 <- f1_server("f1", batch = batch)
  f2 <- f2_server("f2", batch = batch, ingredients = ingredients)
  bottling <- bottling_server("bottling", batch = batch)

  # Each stage returns its costs in one shape, which is what lets a single
  # table render the whole batch.
  batch_lines <- reactive({
    bind_cost_lines(f1(), f2(), bottling$lines())
  })

  cost_server("cost", lines = batch_lines, plan = bottling$plan, batch = batch)
}

shinyApp(ui = ui, server = server)
