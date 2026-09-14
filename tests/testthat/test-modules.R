# The module servers, driven directly rather than through a browser. These
# cover the reactive graph: what each module returns for a given set of inputs,
# and what it does with an input the calculations would refuse.

test_that("the batch module reports the volume brewed fresh", {
  testServer(batch_server, {
    session$setInputs(batch_size = 15, starter_ratio = 0.2)

    expect_equal(batch()$brew_gal, 12)
    expect_equal(batch()$batch_size, 15)
  })
})

test_that("the batch module waits rather than reporting on an empty size", {
  # Clearing the field leaves the input NA mid-edit, and a batch of NA gallons
  # would carry through every cost on the page.
  testServer(batch_server, {
    session$setInputs(batch_size = NA_real_, starter_ratio = 0.2)

    expect_error(batch(), class = "shiny.silent.error")
  })
})

test_that("the F1 module costs tea, sugar, and cooling together", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))

  testServer(f1_server, args = list(batch = batch), {
    session$setInputs(
      bags_per_gal = 10, black_share = 70,
      black_price = 21, black_bags = 600,
      green_price = 3, green_bags = 40,
      sugar_per_gal = "1", sugar_price = 5, sugar_cups = 14,
      cooling_agent = "Water", cooling_price = 3, water_jugs = 5, ice_bags = 1
    )

    lines <- session$getReturned()()
    expect_setequal(unique(lines$group), c("Tea", "Sugar", "Cooling"))
    expect_equal(lines$quantity[lines$item == "Black tea"], 84)
    expect_equal(lines$quantity[lines$item == "Sugar"], 12)
    expect_equal(cost_total(lines), sum(lines$cost))
  })
})

test_that("switching the cooling agent costs the other purchase", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))

  testServer(f1_server, args = list(batch = batch), {
    session$setInputs(
      bags_per_gal = 10, black_share = 70,
      black_price = 21, black_bags = 600,
      green_price = 3, green_bags = 40,
      sugar_per_gal = "1", sugar_price = 5, sugar_cups = 14,
      cooling_agent = "Water", cooling_price = 6, water_jugs = 5, ice_bags = 2
    )
    water <- session$getReturned()()
    expect_identical(water$item[water$group == "Cooling"], "Water")
    expect_equal(water$quantity[water$group == "Cooling"], 5)

    session$setInputs(cooling_agent = "Ice")
    ice <- session$getReturned()()
    expect_identical(ice$item[ice$group == "Cooling"], "Ice")
    expect_equal(ice$quantity[ice$group == "Cooling"], 2)
    # The purchase is what it is; buying it as two bags rather than five jugs
    # does not change what the batch is charged for cooling.
    expect_equal(
      cost_total(ice[ice$group == "Cooling", ]),
      cost_total(water[water$group == "Cooling", ])
    )
  })
})

test_that("the F2 module splits the batch and costs each style", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))
  ingredients <- reactiveVal(fixture_ingredients())

  testServer(f2_server, args = list(batch = batch, ingredients = ingredients), {
    session$setInputs(
      styles = c("Regular", "Hard"), percent_regular = 50,
      regular = "Ginger", hard = "Citra Hops"
    )

    expect_equal(volumes(), list(regular = 6, hard = 6))
    lines <- session$getReturned()()
    expect_setequal(
      unique(lines$group),
      c("Flavoring \u2014 Regular", "Flavoring \u2014 Hard")
    )
    expect_equal(lines$quantity[lines$item == "Ginger"], 0.125 * 6)
  })
})

test_that("one style takes the whole batch whatever the split says", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))
  ingredients <- reactiveVal(fixture_ingredients())

  testServer(f2_server, args = list(batch = batch, ingredients = ingredients), {
    session$setInputs(
      styles = "Hard", percent_regular = 50,
      regular = "Ginger", hard = "Citra Hops"
    )

    expect_equal(volumes(), list(regular = 0, hard = 12))
    lines <- session$getReturned()()
    # The regular selection is still made, but there is no regular kombucha to
    # flavor, so it costs nothing rather than half a batch.
    expect_equal(cost_total(lines[lines$group == "Flavoring \u2014 Regular", ]), 0)
    expect_gt(cost_total(lines), 0)
  })
})

test_that("an ingredient deleted from the library leaves the selection", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))
  ingredients <- reactiveVal(fixture_ingredients())

  testServer(f2_server, args = list(batch = batch, ingredients = ingredients), {
    session$setInputs(
      styles = "Regular", percent_regular = 100,
      regular = c("Ginger", "Mango Juice"), hard = character(0)
    )
    expect_equal(nrow(session$getReturned()()), 2)

    ingredients(fixture_ingredients()[-1, , drop = FALSE])
    session$flushReact()

    lines <- session$getReturned()()
    expect_identical(lines$item, "Mango Juice")
  })
})

test_that("the bottling module plans and costs the packaging", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))

  testServer(bottling_server, args = list(batch = batch), {
    session$setInputs(
      bottle_size = "16", juice_per_bottle = 3,
      case_price = 6.55, cap_price = 0.09
    )

    returned <- session$getReturned()
    expect_equal(returned$plan()$booch_oz, 13)
    expect_equal(returned$plan()$sellable, 118)
    expect_equal(returned$lines()$quantity, c(10, 118))
  })
})

test_that("a bottle with no room for kombucha is reported rather than blanked", {
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))

  testServer(bottling_server, args = list(batch = batch), {
    session$setInputs(
      bottle_size = "8", juice_per_bottle = 8,
      case_price = 6.55, cap_price = 0.09
    )

    # A brewer can type this in, so it is a message on the page rather than a
    # silent hold that leaves the cost tab looking empty for no stated reason.
    expect_error(session$getReturned()$plan(), "cannot hold")
  })
})

test_that("the cost module prices the batch it is handed", {
  lines <- reactiveVal(bind_cost_lines(
    tea_lines(12, 10, 0.7, unit_rate(21, 600), unit_rate(3, 40)),
    bottling_lines(bottling_plan(12, 16, 3), 6.55, 0.09)
  ))
  plan <- reactiveVal(bottling_plan(12, 16, 3))
  batch <- reactiveVal(list(batch_size = 15, starter_ratio = 0.2, brew_gal = 12))

  testServer(
    cost_server,
    args = list(lines = lines, plan = plan, batch = batch),
    {
      session$setInputs(sell_price = 2.50)

      totals <- summary()
      expect_equal(totals$sellable, 118)
      expect_equal(totals$cost_per_bottle, cost_total(lines()) / 118)
      expect_equal(totals$revenue, 2.50 * 118)
      expect_equal(totals$profit, totals$revenue - cost_total(lines()))
    }
  )
})

test_that("the ingredient library module edits, adds, and deletes rows", {
  path <- local_ingredients_file()

  testServer(ingredient_library_server, args = list(path = path), {
    expect_identical(session$getReturned()(), fixture_ingredients())

    session$setInputs(add = 1)
    expect_equal(nrow(session$getReturned()()), 4)
    expect_identical(session$getReturned()()$ingredient[4], "New ingredient")

    session$setInputs(table_rows_selected = 4, remove = 1)
    expect_identical(session$getReturned()(), fixture_ingredients())
  })
})

test_that("a rejected edit leaves the library as it was", {
  path <- local_ingredients_file()

  testServer(ingredient_library_server, args = list(path = path), {
    # Column 3 is `cost_per_unit`, zero-indexed as DT reports it.
    session$setInputs(
      table_cell_edit = data.frame(row = 1L, col = 3L, value = "-5")
    )

    expect_identical(session$getReturned()(), fixture_ingredients())
  })
})

test_that("an accepted edit re-prices the library without saving it", {
  path <- local_ingredients_file()

  testServer(ingredient_library_server, args = list(path = path), {
    session$setInputs(
      table_cell_edit = data.frame(row = 1L, col = 3L, value = "9.99")
    )
    expect_equal(session$getReturned()()$cost_per_unit[1], 9.99)

    # Unsaved: the file still holds what it did.
    expect_equal(load_ingredients(path)$cost_per_unit[1], 2.00)

    session$setInputs(save = 1)
    expect_equal(load_ingredients(path)$cost_per_unit[1], 9.99)
  })
})

test_that("deleting with nothing selected changes nothing", {
  path <- local_ingredients_file()

  testServer(ingredient_library_server, args = list(path = path), {
    session$setInputs(remove = 1)

    expect_identical(session$getReturned()(), fixture_ingredients())
  })
})

test_that("restoring defaults replaces the library", {
  path <- local_ingredients_file()

  testServer(ingredient_library_server, args = list(path = path), {
    session$setInputs(restore = 1)

    expect_identical(session$getReturned()(), default_ingredients())
  })
})
