#' Batch settings
#'
#' Batch size and starter ratio, which between them set the volume every other
#' module scales its ingredients against.

#' @param id Module id.
batch_ui <- function(id) {  ns <- shiny::NS(id)

  app_box(
    "Batch",
    status = "info",
    collapsible = FALSE,
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::numericInput(
          ns("batch_size"), "Batch size (gallons)",
          value = 15, min = 1, max = 1000, step = 1, width = "100%"
        ),
        field_help("The finished volume, starter included.")
      ),
      shiny::column(
        width = 4,
        shiny::sliderInput(
          ns("starter_ratio"), "Starter ratio",
          value = 0.2, min = 0.05, max = 0.5, step = 0.05, width = "100%"
        ),
        field_help(
          "The share of the batch carried over as fermented kombucha from the",
          "last one. The rest is brewed fresh."
        )
      ),
      shiny::column(
        width = 4,
        shiny::uiOutput(ns("summary"))
      )
    )
  )
}

#' @param id Module id.
#' @return A reactive holding the `batch_size`, `starter_ratio`, and the
#'   `brew_gal` of fresh tea those imply.
batch_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    batch <- shiny::reactive({
      shiny::req(input$batch_size > 0, input$starter_ratio)
      list(
        batch_size = input$batch_size,
        starter_ratio = input$starter_ratio,
        brew_gal = brew_volume(input$batch_size, input$starter_ratio)
      )
    })

    output$summary <- shiny::renderUI({
      settings <- batch()
      stat_row(
        stat_card(
          "Brew volume", format_gallons(settings$brew_gal),
          icon = "mug-hot", accent = "blue",
          note = "Tea brewed fresh for this batch"
        ),
        stat_card(
          "Starter", format_gallons(settings$batch_size - settings$brew_gal),
          icon = "recycle", accent = "green",
          note = "Held back from the last batch"
        )
      )
    })

    batch
  })
}
