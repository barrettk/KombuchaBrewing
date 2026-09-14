#' Shared UI pieces
#'
#' The building blocks every module renders through, so a cost, a headline
#' number, and a price field look the same wherever they appear.

#' A dollar amount field
#'
#' @param inputId Input id.
#' @param label Field label, without the unit.
#' @param value Starting value.
#' @param width CSS width.
money_input <- function(inputId, label, value, width = "100%") {
  shiny::div(
    class = "money-input",
    shiny::numericInput(
      inputId, paste0(label, " ($)"),
      value = value, min = 0, step = 0.01, width = width
    )
  )
}

#' A whole-number field
#'
#' @param inputId Input id.
#' @param label Field label.
#' @param value Starting value.
#' @param min Smallest value accepted.
#' @param width CSS width.
count_input <- function(inputId, label, value, min = 1, width = "100%") {
  shiny::numericInput(inputId, label, value = value, min = min, step = 1, width = width)
}

#' A headline number
#'
#' @param label What the number is.
#' @param value The number, already formatted.
#' @param icon Font Awesome icon name.
#' @param accent Accent class suffix: "blue", "amber", "green", or "red".
#' @param note Optional line of context under the value.
stat_card <- function(label, value, icon, accent = "blue", note = NULL) {
  shiny::div(
    class = paste0("stat-card stat-card-", accent),
    shiny::div(class = "stat-card-icon", shiny::icon(icon)),
    shiny::div(
      class = "stat-card-body",
      shiny::div(class = "stat-card-label", label),
      shiny::div(class = "stat-card-value", value),
      if (!is.null(note)) shiny::div(class = "stat-card-note", note)
    )
  )
}

#' A row of headline numbers
#'
#' @param ... [stat_card()] calls.
stat_row <- function(...) {
  shiny::div(class = "stat-row", ...)
}

#' A short explanation under a control
#'
#' @param ... Text.
field_help <- function(...) {
  shiny::p(class = "field-help", ...)
}

#' A titled group of controls
#'
#' @param title Group heading.
#' @param ... Controls.
#' @param accent Accent class suffix used for the heading rule.
control_group <- function(title, ..., accent = "blue") {
  shiny::div(
    class = paste0("control-group control-group-", accent),
    shiny::h4(class = "control-group-title", title),
    ...
  )
}

#' Render cost lines as a table
#'
#' One table renders every cost in the app, since every cost is the same shape.
#' Group headings are rows in that table rather than separate tables, so the
#' totals line up down the page.
#'
#' @param lines A data frame of cost lines.
#' @param total_label Label for the footer total, or `NULL` for no footer.
#' @param empty_message Shown in place of the table when there are no lines.
cost_table <- function(lines, total_label = "Batch total",
                       empty_message = "Nothing selected yet.") {
  if (nrow(lines) == 0) {
    return(shiny::div(class = "cost-empty", empty_message))
  }

  groups <- unique(lines$group)
  body <- lapply(groups, function(group) {
    rows <- lines[lines$group == group, , drop = FALSE]
    shiny::tagList(
      shiny::tags$tr(
        class = "cost-group",
        shiny::tags$th(colspan = 3, group),
        shiny::tags$th(class = "cost-numeric", format_currency(cost_total(rows)))
      ),
      lapply(seq_len(nrow(rows)), function(i) {
        shiny::tags$tr(
          shiny::tags$td(class = "cost-item", rows$item[i]),
          shiny::tags$td(
            class = "cost-numeric",
            format_quantity(rows$quantity[i]), " ",
            pluralize_unit(rows$unit[i], rows$quantity[i])
          ),
          shiny::tags$td(
            class = "cost-numeric cost-rate",
            format_currency(rows$rate[i]), " / ", rows$unit[i]
          ),
          shiny::tags$td(class = "cost-numeric", format_currency(rows$cost[i]))
        )
      })
    )
  })

  shiny::tags$table(
    class = "cost-table",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Item"),
        shiny::tags$th(class = "cost-numeric", "Amount"),
        shiny::tags$th(class = "cost-numeric", "Rate"),
        shiny::tags$th(class = "cost-numeric", "Cost")
      )
    ),
    shiny::tags$tbody(body),
    if (!is.null(total_label)) {
      shiny::tags$tfoot(
        shiny::tags$tr(
          shiny::tags$th(colspan = 3, total_label),
          shiny::tags$th(class = "cost-numeric", format_currency(cost_total(lines)))
        )
      )
    }
  )
}

#' A standard app box
#'
#' @param title Box title.
#' @param ... Box contents.
#' @param status Header color, as understood by shinydashboardPlus.
#' @param width Bootstrap column width.
#' @param collapsible Whether the box can be collapsed.
app_box <- function(title, ..., status = "primary", width = 12, collapsible = TRUE) {
  shinydashboardPlus::box(
    title = title,
    status = status,
    width = width,
    solidHeader = TRUE,
    closable = FALSE,
    collapsible = collapsible,
    ...
  )
}
