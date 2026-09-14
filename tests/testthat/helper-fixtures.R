# Fixtures shared by the unit and browser tests.

#' A small ingredient library
#'
#' Deliberately not the shipped defaults: a test that asserts against the
#' library the app ships with breaks whenever a price changes, without either
#' one being wrong.
fixture_ingredients <- function() {
  data.frame(
    ingredient = c("Ginger", "Mango Juice", "Citra Hops"),
    amount_per_gal = c(0.125, 0.75, 0.2),
    unit = c("lb", "32oz jar", "1oz bag"),
    cost_per_unit = c(2.00, 4.00, 5.00),
    stringsAsFactors = FALSE
  )
}

#' Write the fixture library to a temporary file
#'
#' Tied to the calling test's frame, so the file is cleaned up with it and no
#' test can read the library another one left behind.
#'
#' @param ingredients Library to write.
#' @param envir Frame the file's lifetime is tied to.
local_ingredients_file <- function(ingredients = fixture_ingredients(),
                                   envir = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = envir)
  save_ingredients(ingredients, path)
  path
}
