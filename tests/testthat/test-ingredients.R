# The ingredient library: what the app will accept into it, and what survives
# a round trip through the file.

test_that("the shipped library passes its own validation", {
  expect_identical(validate_ingredients(default_ingredients()), default_ingredients())
})

test_that("a library survives a round trip through the file", {
  path <- local_ingredients_file()

  expect_identical(load_ingredients(path), fixture_ingredients())
})

test_that("a missing file falls back to the shipped defaults", {
  # A fresh checkout has no data file, and the app has to start anyway.
  expect_identical(
    load_ingredients(file.path(tempdir(), "does-not-exist.csv")),
    default_ingredients()
  )
})

test_that("numbers read from a file come back as numbers", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "ingredient,amount_per_gal,unit,cost_per_unit",
      "Ginger,0.125,lb,2.98"
    ),
    path
  )

  loaded <- load_ingredients(path)
  expect_type(loaded$amount_per_gal, "double")
  expect_type(loaded$cost_per_unit, "double")
})

test_that("a library missing a column is refused", {
  expect_error(
    validate_ingredients(data.frame(ingredient = "Ginger", unit = "lb")),
    "missing"
  )
  expect_error(validate_ingredients(list(ingredient = "Ginger")), "data frame")
})

test_that("an unnamed ingredient is refused rather than dropped", {
  # Dropping the row would take the selection made from it with it, and the
  # batch would quietly re-price.
  bad <- fixture_ingredients()
  bad$ingredient[2] <- "  "

  expect_error(validate_ingredients(bad), "needs a name")
})

test_that("an amount that is not a number is refused", {
  bad <- fixture_ingredients()
  bad$cost_per_unit[1] <- "free"

  expect_error(validate_ingredients(bad), "must be numbers")
})

test_that("a negative price is refused", {
  bad <- fixture_ingredients()
  bad$cost_per_unit[1] <- -1

  expect_error(validate_ingredients(bad), "at least zero")
})

test_that("a duplicated ingredient is refused", {
  # Selections are keyed on the name, so two rows sharing one would cost the
  # first and ignore the second.
  bad <- rbind(fixture_ingredients(), fixture_ingredients()[1, ])

  expect_error(validate_ingredients(bad), "only appear once")
})

test_that("names and units are trimmed on the way in", {
  padded <- fixture_ingredients()
  padded$ingredient[1] <- "  Ginger  "
  padded$unit[1] <- " lb "

  cleaned <- validate_ingredients(padded)
  expect_identical(cleaned$ingredient[1], "Ginger")
  expect_identical(cleaned$unit[1], "lb")
})

test_that("extra columns are dropped rather than carried", {
  extra <- fixture_ingredients()
  extra$note <- "from the old spreadsheet"

  expect_identical(names(validate_ingredients(extra)), INGREDIENT_COLUMNS)
})

test_that("a new row is named so it cannot collide with an existing one", {
  library_data <- fixture_ingredients()
  first <- new_ingredient_row(library_data)
  expect_identical(first$ingredient, "New ingredient")

  library_data <- rbind(library_data, first)
  second <- new_ingredient_row(library_data)
  expect_identical(second$ingredient, "New ingredient 2")

  # The whole point: adding two blank rows in a row still validates.
  expect_no_error(validate_ingredients(rbind(library_data, second)))
})

test_that("saving creates the folder it writes into", {
  path <- file.path(withr::local_tempdir(), "nested", "ingredients.csv")

  save_ingredients(fixture_ingredients(), path)
  expect_true(file.exists(path))
})

test_that("an invalid library is refused before it overwrites a good one", {
  path <- local_ingredients_file()
  bad <- fixture_ingredients()
  bad$cost_per_unit[1] <- -5

  expect_error(save_ingredients(bad, path))
  expect_identical(load_ingredients(path), fixture_ingredients())
})
