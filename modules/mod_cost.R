#' Cost analysis
#'
#' What the batch costs, where the cost went, and what it returns at a given
#' shelf price. No arithmetic happens here: this renders the lines the
#' ingredient modules produced and the summary derived from them.

#' @param id Module id.
cost_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("headline")),
    app_box(
      "Cost breakdown",
      status = "danger",
      collapsible = FALSE,
      shiny::uiOutput(ns("breakdown"))
    ),
    app_box(
      "Pricing",
      status = "warning",
      collapsible = FALSE,
      shiny::fluidRow(
        shiny::column(
          width = 4,
          money_input(ns("sell_price"), "Sell price per bottle", 2.50),
          field_help("Set what a bottle sells for to see the return.")
        ),
        shiny::column(
          width = 8,
          shiny::uiOutput(ns("returns"))
        )
      )
    )
  )
}

#' @param id Module id.
#' @param lines Reactive cost lines for the whole batch.
#' @param plan Reactive bottling plan from [bottling_server()].
#' @param batch Reactive batch settings from [batch_server()].
cost_server <- function(id, lines, plan, batch) {
  shiny::moduleServer(id, function(input, output, session) {
    summary <- shiny::reactive({
      shiny::req(input$sell_price)
      pricing_summary(cost_total(lines()), plan()$sellable, input$sell_price)
    })

    output$headline <- shiny::renderUI({
      totals <- summary()
      stat_row(
        stat_card(
          "Batch cost", format_currency(cost_total(lines())),
          icon = "receipt", accent = "red",
          note = sprintf("For %s brewed", format_gallons(batch()$brew_gal))
        ),
        stat_card(
          "Cost per bottle", format_currency(totals$cost_per_bottle),
          icon = "wine-bottle", accent = "amber",
          note = sprintf(
            "%s %s bottles",
            format_quantity(totals$sellable),
            bottle_label(plan()$bottle_size)
          )
        ),
        stat_card(
          "Break-even", sprintf("%s bottles", format_quantity(totals$break_even)),
          icon = "scale-balanced", accent = "blue",
          note = "Sold at the current price"
        ),
        stat_card(
          "Profit", format_currency(totals$profit),
          icon = "coins", accent = if (isTRUE(totals$profit >= 0)) "green" else "red",
          note = sprintf("%s margin", scales::percent(totals$margin, accuracy = 0.1))
        )
      )
    })

    output$breakdown <- shiny::renderUI({
      cost_table(
        lines(),
        empty_message = "Set up a batch to see what it costs."
      )
    })

    output$returns <- shiny::renderUI({
      totals <- summary()
      stat_row(
        stat_card(
          "Revenue", format_currency(totals$revenue),
          icon = "cash-register", accent = "green",
          note = sprintf("%s bottles sold", format_quantity(totals$sellable))
        ),
        stat_card(
          "Profit per bottle", format_currency(totals$profit_per_bottle),
          icon = "hand-holding-dollar",
          accent = if (isTRUE(totals$profit >= 0)) "green" else "red",
          note = sprintf(
            "%s cost, %s price",
            format_currency(totals$cost_per_bottle),
            format_currency(input$sell_price)
          )
        )
      )
    })
  })
}
