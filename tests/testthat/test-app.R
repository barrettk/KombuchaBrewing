# End-to-end checks in a real browser.
#
# These cover what `testServer` cannot see: whether a rendered table actually
# contains the number the reactive graph computed, and whether a control the
# server writes back reaches the page. Styling is deliberately not asserted --
# snapshot images would fail on every palette change without saying anything
# about correctness.

library(shinytest2)

#' Start the app against a fixture ingredient library
#'
#' The library is read at startup from `KOMBUCHA_INGREDIENTS`, so the variable
#' has to be set before the process launches. Tied to the calling test's frame,
#' so both the file and the browser are cleaned up with it.
#'
#' @param ingredients Path to the library to run against. Tests that check what
#'   saving wrote pass the path they built, so they can read it back.
launch_app <- function(ingredients = NULL, ...) {
  # Chrome is the real precondition; without it these cannot run anywhere.
  testthat::skip_if_not(
    nzchar(Sys.getenv("CHROMOTE_CHROME", unset = "")) ||
      !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
    "no Chrome available for shinytest2"
  )
  if (is.null(ingredients)) {
    ingredients <- local_ingredients_file(envir = parent.frame())
  }
  withr::local_envvar(
    c(KOMBUCHA_INGREDIENTS = ingredients),
    .local_envir = parent.frame()
  )

  app <- shinytest2::AppDriver$new(
    app_dir = APP_DIR,
    name = "kombucha",
    width = 1500, height = 1000,
    load_timeout = 60 * 1000,
    ...
  )
  withr::defer(app$stop(), envir = parent.frame())
  app
}

# Only the open tab is laid out, so anything read from another one comes back
# empty and every assertion about it passes for the wrong reason.
show_tab <- function(app, tab) {
  app$set_inputs(tabs = tab)
  app$wait_for_idle()
}

visible_text <- function(app, selector) {
  app$get_js(sprintf(
    "(function(){var e=document.querySelector('%s');return e?e.innerText:'';})()",
    selector
  ))
}

#' Read a cost table's footer total back off the page as a number
#'
#' Comparing the totals the app actually rendered is the only way to catch a
#' page that agrees with itself but not with the rest of the app.
#'
#' @param app An `AppDriver`.
#' @param selector Container holding the cost table.
shown_total <- function(app, selector) {
  text <- visible_text(app, sprintf("%s .cost-table tfoot th.cost-numeric", selector))
  as.numeric(gsub("[$,]", "", text))
}

#' Whether an element is laid out
#'
#' `conditionalPanel` hides rather than removes, so the element is still in the
#' document when the condition is false.
# DataTables reads the modifier keys on mousedown and only then acts on the
# click, so a click on its own selects nothing.
select_row <- function(app, row = 1) {
  app$run_js(sprintf(
    "var td = $('#library-table tbody tr').eq(%d).find('td').eq(0);
     td.trigger('mousedown'); td.trigger('click');",
    row - 1
  ))
  app$wait_for_idle()
}

is_visible <- function(app, selector) {
  app$get_js(sprintf(
    "(function(){var e=document.querySelector('%s');
       return !!(e && e.offsetParent !== null);})()",
    selector
  ))
}

#' Edit a cell the way DT does
#'
#' Double-clicking a cell is not reachable through `set_inputs`, so the edit is
#' sent as the input DT sends. The `DT.cellInfo` suffix is the input handler
#' that turns the payload into the data frame `DT::editData()` reads; without
#' it the value arrives as a bare list and the edit fails.
#'
#' @param app An `AppDriver`.
#' @param row,col Cell to edit, with `col` zero-indexed as DT reports it.
#' @param value New value, as typed.
edit_cell <- function(app, row, col, value) {
  app$run_js(sprintf(
    "Shiny.setInputValue('library-table_cell_edit:DT.cellInfo',
       [{row: %d, col: %d, value: '%s'}], {priority: 'event'});",
    row, col, value
  ))
  app$wait_for_idle()
}

test_that("the app starts on the brew setup tab with a costed batch", {
  app <- launch_app()

  values <- app$get_values(input = TRUE)$input
  expect_identical(values$tabs, "setup_tab")
  expect_equal(values$`batch-batch_size`, 15)
  expect_equal(values$`batch-starter_ratio`, 0.2)

  # 15 gallons at a fifth starter is 12 gallons brewed, and the number the
  # brewer reads has to be that rather than the batch size.
  expect_match(visible_text(app, "#batch-summary"), "12 gallons")

  # The F1 tab is open at startup and its costs are already on the page.
  expect_match(visible_text(app, "#f1-costs"), "Black tea")
  expect_match(visible_text(app, "#f1-costs"), "First ferment total")
})

test_that("changing the batch re-costs the ingredients", {
  app <- launch_app()

  before <- visible_text(app, "#f1-costs")
  app$set_inputs(`batch-batch_size` = 30)
  app$wait_for_idle()

  expect_match(visible_text(app, "#batch-summary"), "24 gallons")
  # Twice the brew is twice the tea: 10 bags a gallon at 70% black.
  expect_match(visible_text(app, "#f1-costs"), "168")
  expect_false(identical(before, visible_text(app, "#f1-costs")))
})

test_that("the flavoring pickers are filled from the ingredient library", {
  app <- launch_app()

  app$set_inputs(recipe = "F2 Flavoring")
  app$wait_for_idle()

  # The pickers are built empty and filled by the server, so this is the
  # round trip `testServer` cannot observe.
  choices <- app$get_js(
    "(function(){return Array.from(
       document.querySelectorAll('#f2-regular option')
     ).map(function(o){return o.value;});})()"
  )
  expect_setequal(unlist(choices), fixture_ingredients()$ingredient)

  # Only the defaults the fixture library actually holds are selected.
  expect_setequal(app$get_value(input = "f2-regular"), "Ginger")
})

test_that("selecting both styles reveals the split and costs each side", {
  app <- launch_app()
  app$set_inputs(recipe = "F2 Flavoring")
  app$wait_for_idle()

  app$set_inputs(`f2-styles` = c("Regular", "Hard"))
  app$wait_for_idle()

  split <- visible_text(app, "#f2-split")
  expect_match(split, "REGULAR")
  expect_match(split, "6 gallons")

  costs <- visible_text(app, "#f2-costs")
  # Group headings are uppercased by the stylesheet, so this is the text a
  # reader actually sees rather than the string the server built.
  expect_match(costs, "FLAVORING \u2014 REGULAR")
  expect_match(costs, "FLAVORING \u2014 HARD")
})

test_that("the cost page totals every stage of the brew", {
  app <- launch_app()
  show_tab(app, "cost_tab")

  breakdown <- visible_text(app, "#cost-breakdown")
  for (group in c("TEA", "SUGAR", "COOLING", "BOTTLING")) {
    expect_match(breakdown, group)
  }

  # A 12 gallon brew into 16 oz bottles holding 13 oz of kombucha fills 118
  # sellable bottles, and the headline has to agree with the bottling tab.
  expect_match(visible_text(app, "#cost-headline"), "118")
})

test_that("the sell price moves profit without moving cost", {
  app <- launch_app()
  show_tab(app, "cost_tab")

  cost_before <- visible_text(app, "#cost-breakdown")
  app$set_inputs(`cost-sell_price` = 10)
  app$wait_for_idle()

  expect_identical(visible_text(app, "#cost-breakdown"), cost_before)
  expect_match(visible_text(app, "#cost-returns"), "\\$1,180.00")
})

test_that("a price below cost reports a loss", {
  app <- launch_app()
  show_tab(app, "cost_tab")

  app$set_inputs(`cost-sell_price` = 0.05)
  app$wait_for_idle()

  expect_match(visible_text(app, "#cost-headline"), "-\\$")
})

test_that("editing the library re-prices the batch", {
  app <- launch_app()

  # Ginger is selected for regular kombucha by default and costs $2.00/lb in
  # the fixture library.
  show_tab(app, "cost_tab")
  before <- visible_text(app, "#cost-breakdown")
  expect_match(before, "Ginger")

  show_tab(app, "library_tab")
  edit_cell(app, row = 1, col = 3, value = "20")

  show_tab(app, "cost_tab")
  expect_false(identical(before, visible_text(app, "#cost-breakdown")))
})

test_that("an invalid edit is refused and the library is left alone", {
  app <- launch_app()
  show_tab(app, "library_tab")

  edit_cell(app, row = 1, col = 3, value = "-5")

  expect_match(visible_text(app, "#shiny-notification-panel"), "at least zero")
  # The table still shows the price the rest of the app is costing against.
  expect_match(visible_text(app, "#library-table"), "\\$2\\.00")
})

test_that("a bottle with no room for kombucha says so", {
  app <- launch_app()
  app$set_inputs(recipe = "Bottling")
  app$wait_for_idle()

  app$set_inputs(`bottling-juice_per_bottle` = 16)
  app$wait_for_idle()

  # Blanking the panel would leave a brewer with no idea which field to fix.
  expect_match(visible_text(app, "#bottling-summary"), "cannot hold")
})

test_that("the cost page total agrees with each stage's own total", {
  # The stage tabs and the cost page each render their own table from the same
  # lines. A refactor that changed how the lines are combined could leave the
  # two disagreeing while both still looked right on their own.
  app <- launch_app()

  app$set_inputs(`f2-styles` = c("Regular", "Hard"))
  app$wait_for_idle()

  app$set_inputs(recipe = "F1 Ingredients")
  app$wait_for_idle()
  f1 <- shown_total(app, "#f1-costs")

  app$set_inputs(recipe = "F2 Flavoring")
  app$wait_for_idle()
  f2 <- shown_total(app, "#f2-costs")

  app$set_inputs(recipe = "Bottling")
  app$wait_for_idle()
  bottling <- shown_total(app, "#bottling-costs")

  show_tab(app, "cost_tab")
  total <- shown_total(app, "#cost-breakdown")

  expect_true(all(is.finite(c(f1, f2, bottling, total))))
  expect_gt(f2, 0)
  # Rendered to the cent, so the parts can round a penny away from the whole.
  expect_equal(total, f1 + f2 + bottling, tolerance = 0.02)
})

test_that("switching the cooling agent swaps which purchase is asked for", {
  app <- launch_app()

  expect_true(is_visible(app, "#f1-water_jugs"))
  expect_false(is_visible(app, "#f1-ice_bags"))

  app$set_inputs(`f1-cooling_agent` = "Ice")
  app$wait_for_idle()

  expect_false(is_visible(app, "#f1-water_jugs"))
  expect_true(is_visible(app, "#f1-ice_bags"))
  expect_match(visible_text(app, "#f1-costs"), "Ice")
})

test_that("the split slider is only offered when both styles are made", {
  app <- launch_app()
  app$set_inputs(recipe = "F2 Flavoring")
  app$wait_for_idle()

  # One style takes the whole batch, so a split would be a control that does
  # nothing.
  expect_false(is_visible(app, "#f2-percent_regular"))
  expect_match(visible_text(app, "#f2-split"), "12 gallons")

  app$set_inputs(`f2-styles` = c("Regular", "Hard"))
  app$wait_for_idle()
  expect_true(is_visible(app, "#f2-percent_regular"))

  app$set_inputs(`f2-styles` = "Hard")
  app$wait_for_idle()
  expect_false(is_visible(app, "#f2-percent_regular"))
  # The hard card now holds the whole brew.
  expect_match(visible_text(app, "#f2-split"), "12 gallons")
})

test_that("adding an ingredient offers it to the flavoring pickers", {
  app <- launch_app()
  show_tab(app, "library_tab")

  app$click("library-add")
  app$wait_for_idle()
  expect_match(visible_text(app, "#library-table"), "New ingredient")

  # The point of the library is that it feeds the pickers, and a row that only
  # reaches the table is not usable.
  show_tab(app, "setup_tab")
  app$set_inputs(recipe = "F2 Flavoring")
  app$wait_for_idle()

  choices <- app$get_js(
    "(function(){return Array.from(
       document.querySelectorAll('#f2-regular option')
     ).map(function(o){return o.value;});})()"
  )
  expect_true("New ingredient" %in% unlist(choices))
})

test_that("deleting an ingredient drops it from the pickers and the costing", {
  app <- launch_app()

  show_tab(app, "cost_tab")
  expect_match(visible_text(app, "#cost-breakdown"), "Ginger")

  show_tab(app, "library_tab")
  select_row(app, 1)
  app$click("library-remove")
  app$wait_for_idle()

  expect_no_match(visible_text(app, "#library-table"), "Ginger")
  show_tab(app, "cost_tab")
  expect_no_match(visible_text(app, "#cost-breakdown"), "Ginger")
})

test_that("deleting with nothing selected says so rather than guessing", {
  app <- launch_app()
  show_tab(app, "library_tab")

  app$click("library-remove")
  app$wait_for_idle()

  expect_match(visible_text(app, "#shiny-notification-panel"), "Select the rows")
  expect_match(visible_text(app, "#library-table"), "Ginger")
})

test_that("saving writes the library to the file it was read from", {
  path <- local_ingredients_file()
  app <- launch_app(ingredients = path)
  show_tab(app, "library_tab")

  app$click("library-add")
  app$wait_for_idle()
  # Unsaved edits stay in the session.
  expect_equal(nrow(load_ingredients(path)), nrow(fixture_ingredients()))

  app$click("library-save")
  app$wait_for_idle()

  saved <- load_ingredients(path)
  expect_equal(nrow(saved), nrow(fixture_ingredients()) + 1)
  expect_true("New ingredient" %in% saved$ingredient)
})

test_that("restoring defaults replaces the fixture library in the page", {
  app <- launch_app()
  show_tab(app, "library_tab")

  app$click("library-restore")
  app$wait_for_idle()

  shown <- visible_text(app, "#library-table")
  expect_match(shown, "Apricot Puree")
  expect_match(visible_text(app, "#shiny-notification-panel"), "Restored")
})

test_that("the download hands back the library currently in the page", {
  app <- launch_app()
  show_tab(app, "library_tab")

  app$click("library-add")
  app$wait_for_idle()

  file <- app$get_download("library-download")
  downloaded <- load_ingredients(file)

  expect_identical(names(downloaded), INGREDIENT_COLUMNS)
  expect_true("New ingredient" %in% downloaded$ingredient)
})

test_that("a walkthrough of every page logs no errors", {
  # A broken renderUI reaches the browser as an error in the log and an empty
  # box on the page, which every targeted assertion above can still pass over.
  app <- launch_app()

  app$set_inputs(`batch-batch_size` = 30, `batch-starter_ratio` = 0.35)
  app$wait_for_idle()
  app$set_inputs(`f1-cooling_agent` = "Ice", `f1-sugar_per_gal` = "5/4")
  app$wait_for_idle()
  app$set_inputs(recipe = "F2 Flavoring")
  app$wait_for_idle()
  app$set_inputs(`f2-styles` = c("Regular", "Hard"), `f2-percent_regular` = 25)
  app$wait_for_idle()
  app$set_inputs(recipe = "Bottling")
  app$wait_for_idle()
  app$set_inputs(`bottling-bottle_size` = "128", `bottling-juice_per_bottle` = 12)
  app$wait_for_idle()
  show_tab(app, "library_tab")
  show_tab(app, "cost_tab")
  app$set_inputs(`cost-sell_price` = 14)
  app$wait_for_idle()

  logs <- app$get_logs()
  errors <- logs[!is.na(logs$level) & logs$level == "error", ]
  expect_equal(nrow(errors), 0)

  # And the page still reports a batch rather than a row of dashes.
  expect_match(visible_text(app, "#cost-headline"), "\\$")
  expect_no_match(visible_text(app, "#cost-headline"), "NA")
})
