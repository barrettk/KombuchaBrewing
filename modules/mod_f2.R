#' F2 flavoring
#'
#' The second ferment, where the batch is split between the two styles and each
#' is flavored from the ingredient library.

#' @param id Module id.
f2_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    control_group(
      "Styles",
      accent = "amber",
      shiny::fluidRow(
        shiny::column(
          width = 5,
          shinyWidgets::checkboxGroupButtons(
            ns("styles"), "What are you making?",
            choices = BOOCH_STYLES, selected = "Regular",
            justified = TRUE, width = "100%",
            checkIcon = list(yes = shiny::icon("check"))
          ),
          shiny::conditionalPanel(
            condition = "input.styles.indexOf('Regular') > -1 &&
                         input.styles.indexOf('Hard') > -1",
            ns = ns,
            shinyWidgets::sliderTextInput(
              ns("percent_regular"), "Share of the batch that is regular",
              choices = seq(0, 100, by = 5), selected = 50, post = "%",
              width = "100%"
            )
          )
        ),
        shiny::column(
          width = 7,
          shiny::uiOutput(ns("split"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::conditionalPanel(
          condition = "input.styles.indexOf('Regular') > -1",
          ns = ns,
          control_group(
            "Regular flavorings",
            accent = "brown",
            flavor_picker(ns("regular"))
          )
        )
      ),
      shiny::column(
        width = 6,
        shiny::conditionalPanel(
          condition = "input.styles.indexOf('Hard') > -1",
          ns = ns,
          control_group(
            "Hard flavorings",
            accent = "green",
            flavor_picker(ns("hard"))
          )
        )
      )
    ),
    shiny::uiOutput(ns("costs"))
  )
}

#' The flavoring picker
#'
#' Built empty and filled by the server, so it follows edits to the ingredient
#' library without being rebuilt.
#'
#' @param inputId Namespaced input id.
flavor_picker <- function(inputId) {
  shinyWidgets::pickerInput(
    inputId, "Ingredients",
    choices = character(0), multiple = TRUE, width = "100%",
    options = shinyWidgets::pickerOptions(
      actionsBox = TRUE, liveSearch = TRUE,
      noneSelectedText = "No ingredients selected",
      selectedTextFormat = "count > 2",
      countSelectedText = "{0} ingredients selected"
    )
  )
}

#' @param id Module id.
#' @param batch Reactive batch settings from [batch_server()].
#' @param ingredients Reactive ingredient library from
#'   [ingredient_library_server()].
#' @return A reactive of the flavoring cost lines for both styles.
f2_server <- function(id, batch, ingredients) {
  shiny::moduleServer(id, function(input, output, session) {
    style_inputs <- stats::setNames(tolower(BOOCH_STYLES), BOOCH_STYLES)

    # A plain variable rather than a `reactiveVal`, because the observe below
    # both reads and writes it: as a reactive it would invalidate itself and
    # clear the defaults it had just set.
    filled <- FALSE

    # Refreshing rather than setting once keeps a selection through an edit
    # that leaves the ingredient in place, and drops it with one that removes
    # the ingredient.
    shiny::observe({
      choices <- ingredients()$ingredient
      for (style in BOOCH_STYLES) {
        current <- if (filled) {
          shiny::isolate(input[[style_inputs[[style]]]])
        } else {
          DEFAULT_FLAVORINGS[[style]]
        }
        shinyWidgets::updatePickerInput(
          session, style_inputs[[style]],
          choices = choices, selected = intersect(current, choices)
        )
      }
      filled <<- TRUE
    })

    split <- shiny::reactive({
      shiny::req(input$percent_regular)
      brew_split(input$styles, as.numeric(input$percent_regular))
    })

    volumes <- shiny::reactive({
      lapply(split(), function(share) share * batch()$brew_gal)
    })

    lines <- shiny::reactive({
      library_data <- ingredients()
      gallons <- volumes()
      bind_cost_lines(
        flavoring_lines(library_data, input$regular, gallons$regular, "Regular"),
        flavoring_lines(library_data, input$hard, gallons$hard, "Hard")
      )
    })

    output$split <- shiny::renderUI({
      gallons <- volumes()
      stat_row(
        stat_card(
          "Regular", format_gallons(gallons$regular),
          icon = "glass-water", accent = "brown"
        ),
        stat_card(
          "Hard", format_gallons(gallons$hard),
          icon = "beer-mug-empty", accent = "green"
        )
      )
    })

    output$costs <- shiny::renderUI({
      cost_table(
        lines(),
        total_label = "Second ferment total",
        empty_message = "Pick a style and its flavorings to see what they cost."
      )
    })

    lines
  })
}
