#' The F2 ingredient library
#'
#' The editable table behind both flavoring pickers. Edits take effect
#' immediately; saving is what makes them outlast the session.

#' @param id Module id.
ingredient_library_ui <- function(id) {
  ns <- shiny::NS(id)

  app_box(
    "F2 Ingredient Library",
    status = "primary",
    collapsible = FALSE,
    field_help(
      "Double-click a cell to edit it. Changes re-price the batch straight",
      "away; save to keep them for next time."
    ),
    shiny::div(
      class = "toolbar",
      shiny::actionButton(
        ns("add"), "Add ingredient",
        icon = shiny::icon("plus"), class = "btn-app btn-app-primary"
      ),
      shiny::actionButton(
        ns("remove"), "Delete selected",
        icon = shiny::icon("trash"), class = "btn-app btn-app-danger"
      ),
      shiny::actionButton(
        ns("restore"), "Restore defaults",
        icon = shiny::icon("rotate-left"), class = "btn-app btn-app-muted"
      ),
      shiny::actionButton(
        ns("save"), "Save library",
        icon = shiny::icon("floppy-disk"), class = "btn-app btn-app-success"
      ),
      shiny::downloadButton(
        ns("download"), "Download CSV",
        class = "btn-app btn-app-muted"
      )
    ),
    DT::DTOutput(ns("table"))
  )
}

#' @param id Module id.
#' @param path CSV file the library is read from and saved to.
#' @return A reactive of the current library, saved or not.
ingredient_library_server <- function(id, path = ingredients_path()) {
  shiny::moduleServer(id, function(input, output, session) {
    library_data <- shiny::reactiveVal(load_ingredients(path))

    output$table <- DT::renderDT(
      {
        DT::datatable(
          shiny::isolate(library_data()),
          rownames = FALSE,
          colnames = c(
            "Ingredient", "Amount per gallon", "Unit", "Cost per unit"
          ),
          selection = "multiple",
          editable = list(target = "cell"),
          class = "compact stripe hover",
          options = list(
            dom = "tp",
            pageLength = 10,
            columnDefs = list(list(className = "dt-right", targets = c(1, 3)))
          )
        ) |>
          DT::formatCurrency("cost_per_unit")
      },
      server = TRUE
    )

    # Redrawing through a proxy rather than re-rendering keeps the page and
    # selection the brewer is working in.
    proxy <- DT::dataTableProxy("table")
    redraw <- function(data) {
      DT::replaceData(proxy, data, resetPaging = FALSE, rownames = FALSE)
    }
    shiny::observeEvent(library_data(), redraw(library_data()), ignoreInit = TRUE)

    # A rejected edit is put back, so what is displayed is always what the rest
    # of the app is costing against.
    shiny::observeEvent(input$table_cell_edit, {
      tryCatch(
        {
          edited <- DT::editData(
            library_data(), input$table_cell_edit,
            rownames = FALSE
          )
          library_data(validate_ingredients(edited))
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          redraw(library_data())
        }
      )
    })

    shiny::observeEvent(input$add, {
      library_data(rbind(library_data(), new_ingredient_row(library_data())))
    })

    shiny::observeEvent(input$remove, {
      rows <- input$table_rows_selected
      if (length(rows) == 0) {
        shiny::showNotification("Select the rows to delete first.", type = "warning")
        return()
      }
      library_data(library_data()[-rows, , drop = FALSE])
    })

    shiny::observeEvent(input$restore, {
      library_data(default_ingredients())
      shiny::showNotification("Restored the default ingredients.", type = "message")
    })

    # Deployments often run from a read-only image, where the library is still
    # usable for the session and only saving is unavailable.
    shiny::observeEvent(input$save, {
      tryCatch(
        {
          save_ingredients(library_data(), path)
          shiny::showNotification("Ingredient library saved.", type = "message")
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not save the library:", conditionMessage(e)),
            type = "error"
          )
        }
      )
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        sprintf("kombucha-f2-ingredients-%s.csv", Sys.Date())
      },
      content = function(file) {
        utils::write.csv(library_data(), file, row.names = FALSE)
      }
    )

    shiny::reactive(library_data())
  })
}
