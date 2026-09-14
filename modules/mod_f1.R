#' F1 ingredients
#'
#' The first ferment: sweet tea and whatever brings it down to pitching
#' temperature. Recipe amounts are set per gallon, and rates are derived from
#' what a purchase actually cost rather than typed in twice.

#' @param id Module id.
f1_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    control_group(
      "Tea",
      accent = "brown",
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::sliderInput(
            ns("bags_per_gal"), "Tea bags per gallon",
            value = 10, min = 1, max = 15, step = 1, width = "100%"
          ),
          shiny::sliderInput(
            ns("black_share"), "Black tea",
            value = 70, min = 0, max = 100, step = 5, post = "%", width = "100%"
          ),
          field_help("The remainder is green tea.")
        ),
        shiny::column(
          width = 4,
          money_input(ns("black_price"), "Black tea purchase", 21.00),
          count_input(ns("black_bags"), "Black tea bags bought", 600)
        ),
        shiny::column(
          width = 4,
          money_input(ns("green_price"), "Green tea purchase", 3.00),
          count_input(ns("green_bags"), "Green tea bags bought", 40)
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        control_group(
          "Sugar",
          accent = "amber",
          shinyWidgets::sliderTextInput(
            ns("sugar_per_gal"), "Cups of sugar per gallon",
            choices = SUGAR_PER_GAL_CHOICES, selected = "1", grid = TRUE,
            width = "100%"
          ),
          money_input(ns("sugar_price"), "Sugar purchase", 5.00),
          count_input(ns("sugar_cups"), "Cups bought", 14)
        )
      ),
      shiny::column(
        width = 6,
        control_group(
          "Cooling agent",
          accent = "blue",
          shinyWidgets::radioGroupButtons(
            ns("cooling_agent"), "Cool the tea with",
            choices = names(COOLING_AGENTS), selected = "Water",
            justified = TRUE, width = "100%"
          ),
          money_input(ns("cooling_price"), "Cooling agent purchase", 3.00),
          shiny::conditionalPanel(
            condition = "input.cooling_agent == 'Water'",
            ns = ns,
            count_input(ns("water_jugs"), "Gallons of water bought", 5)
          ),
          shiny::conditionalPanel(
            condition = "input.cooling_agent == 'Ice'",
            ns = ns,
            count_input(ns("ice_bags"), "Bags of ice bought", 1)
          )
        )
      )
    ),
    shiny::uiOutput(ns("costs"))
  )
}

#' @param id Module id.
#' @param batch Reactive batch settings from [batch_server()].
#' @return A reactive of the F1 cost lines.
f1_server <- function(id, batch) {
  shiny::moduleServer(id, function(input, output, session) {
    cooling_quantity <- shiny::reactive({
      if (input$cooling_agent == "Water") input$water_jugs else input$ice_bags
    })

    lines <- shiny::reactive({
      brew_gal <- batch()$brew_gal
      shiny::req(
        input$bags_per_gal, input$black_price, input$black_bags,
        input$green_price, input$green_bags, input$sugar_price,
        input$sugar_cups, input$cooling_price, cooling_quantity()
      )

      bind_cost_lines(
        tea_lines(
          brew_gal = brew_gal,
          bags_per_gal = input$bags_per_gal,
          black_share = input$black_share / 100,
          black_rate = unit_rate(input$black_price, input$black_bags),
          green_rate = unit_rate(input$green_price, input$green_bags)
        ),
        sugar_lines(
          brew_gal = brew_gal,
          cups_per_gal = parse_mixed_number(input$sugar_per_gal),
          rate = unit_rate(input$sugar_price, input$sugar_cups)
        ),
        cooling_lines(
          agent = input$cooling_agent,
          quantity = cooling_quantity(),
          rate = unit_rate(input$cooling_price, cooling_quantity())
        )
      )
    })

    output$costs <- shiny::renderUI({
      cost_table(lines(), total_label = "First ferment total")
    })

    lines
  })
}
