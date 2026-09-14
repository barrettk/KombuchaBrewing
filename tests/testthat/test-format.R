# The display layer: what a number looks like once it reaches the page.

test_that("currency is formatted to the cent", {
  expect_identical(format_currency(1234.5), "$1,234.50")
  expect_identical(format_currency(0.035), "$0.04")
})

test_that("a missing value prints as a dash rather than NA", {
  # A half-filled form should read as incomplete, not broken.
  expect_identical(format_currency(NA_real_), "\u2014")
  expect_identical(format_quantity(NA_real_), "\u2014")
})

test_that("quantities drop trailing zeros", {
  expect_identical(format_quantity(12), "12")
  expect_identical(format_quantity(0.533), "0.533")
  expect_identical(format_quantity(1500), "1,500")
})

test_that("gallons agree with their noun", {
  expect_identical(format_gallons(1), "1 gallon")
  expect_identical(format_gallons(12), "12 gallons")
  expect_identical(format_gallons(0.5), "0.5 gallons")
})

test_that("units pluralize against the amount measured in them", {
  expect_identical(pluralize_unit("bag", 1), "bag")
  expect_identical(pluralize_unit("bag", 2), "bags")
  expect_identical(pluralize_unit(c("cup", "case"), c(1, 3)), c("cup", "cases"))
})

test_that("unit symbols are left alone", {
  # "3.198 ozs" is the kind of thing a reader notices.
  expect_identical(pluralize_unit("oz", 3.198), "oz")
  expect_identical(pluralize_unit("gal", 5), "gal")
  # Only the symbol itself, not a unit that merely contains it.
  expect_identical(pluralize_unit("32oz jar", 4.5), "32oz jars")
})

test_that("a gallon jug is labelled as one", {
  expect_identical(bottle_label(c(8, 16, 128)), c("8 oz", "16 oz", "1 gal"))
})

test_that("recipe fractions are read as numbers", {
  expect_equal(parse_mixed_number("1"), 1)
  expect_equal(parse_mixed_number("1/2"), 0.5)
  expect_equal(parse_mixed_number("5/4"), 1.25)
  expect_equal(parse_mixed_number("1 1/2"), 1.5)
  expect_equal(parse_mixed_number(SUGAR_PER_GAL_CHOICES), c(0.5, 0.75, 1, 1.25))
})

test_that("something that is not a number is refused", {
  expect_error(parse_mixed_number("a lot"), "Not a number")
  expect_error(parse_mixed_number(""), "Not a number")
})
