#' Bottling
#'
#' What the brew is packaged into. A bottle is part juice, so the count a brew
#' fills is not its volume divided by the bottle size.

#' @param id Module id.
bottling_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        control_group(
          "Bottles",
          accent = "blue",
          shiny::selectInput(
            ns("bottle_size"), "Bottle size",
            choices = stats::setNames(BOTTLE_SIZES, bottle_label(BOTTLE_SIZES)),
            selected = 16, width = "100%"
          ),
          shiny::numericInput(
            ns("juice_per_bottle"), "Ounces of juice per bottle",
            value = 3, min = 0, step = 0.5, width = "100%"
          ),
          field_help("Kombucha makes up the rest of the bottle.")
        )
      ),
      shiny::column(
        width = 6,
        control_group(
          "Packaging costs",
          accent = "green",
          money_input(ns("case_price"), "Case of bottles", 6.55),
          field_help(sprintf("A case holds %d bottles.", BOTTLES_PER_CASE)),
          money_input(ns("cap_price"), "Cap", 0.09),
          shiny::tags$a(
            class = "btn btn-app-link",
            href = "https://www.fillmorecontainer.com/",
            target = "_blank", rel = "noopener noreferrer",
            shiny::icon("wine-bottle"), " Buy bottles and caps"
          )
        )
      )
    ),
    shiny::uiOutput(ns("summary")),
    shiny::uiOutput(ns("costs"))
  )
}

#' @param id Module id.
#' @param batch Reactive batch settings from [batch_server()].
#' @return The bottling `plan` and the bottling cost `lines`, both reactive.
bottling_server <- function(id, batch) {
  shiny::moduleServer(id, function(input, output, session) {
    # A bottle with no room left for kombucha is something a brewer can type
    # in, so it is reported rather than left to blank the page.
    plan <- shiny::reactive({
      size <- as.numeric(input$bottle_size)
      shiny::req(size, input$juice_per_bottle)
      shiny::validate(shiny::need(
        input$juice_per_bottle < size,
        sprintf(
          "A %s bottle cannot hold %s oz of juice. Lower the juice per bottle.",
          bottle_label(size), format_quantity(input$juice_per_bottle)
        )
      ))
      bottling_plan(batch()$brew_gal, size, input$juice_per_bottle)
    })

    lines <- shiny::reactive({
      shiny::req(input$case_price, input$cap_price)
      bottling_lines(plan(), input$case_price, input$cap_price)
    })

    output$summary <- shiny::renderUI({
      filled <- plan()
      stat_row(
        stat_card(
          "Kombucha per bottle", paste(format_quantity(filled$booch_oz), "oz"),
          icon = "wine-bottle", accent = "blue",
          note = sprintf(
            "%s bottle, %s oz juice",
            bottle_label(as.numeric(input$bottle_size)),
            format_quantity(input$juice_per_bottle)
          )
        ),
        stat_card(
          "Bottles filled", format_quantity(filled$sellable),
          icon = "boxes-stacked", accent = "amber",
          note = sprintf("%s cases to buy", format_quantity(filled$cases))
        )
      )
    })

    output$costs <- shiny::renderUI({
      cost_table(lines(), total_label = "Bottling total")
    })

    list(plan = plan, lines = lines)
  })
}
