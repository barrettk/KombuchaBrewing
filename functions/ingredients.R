#' The F2 ingredient library
#'
#' The flavorings a batch can be finished with, and what each costs. Stored as
#' CSV rather than in the code, so a new ingredient does not need a release.

INGREDIENT_COLUMNS <- c("ingredient", "amount_per_gal", "unit", "cost_per_unit")

#' The library the app ships with
#'
#' Also what "Restore defaults" restores.
default_ingredients <- function() {
  data.frame(
    ingredient = c(
      "Ginger", "Pomegranate Juice", "Mango Juice", "Apricot Puree",
      "Citra Hops", "Blue Butterfly Pea Flower", "EC-1118 Champagne Yeast"
    ),
    amount_per_gal = c(0.125, 0.75, 0.75, 0.86, 0.2, 0.533, 0.2),
    unit = c("lb", "32oz jar", "32oz jar", "28oz jar", "1oz bag", "oz", "5g bag"),
    cost_per_unit = c(2.98, 4.49, 3.59, 4.20, 5.48, 2.37, 1.40),
    stringsAsFactors = FALSE
  )
}

#' Check and clean a library
#'
#' Applied on the way in and on the way out, so a file edited by hand and a row
#' typed into the table are held to the same standard. Nothing is dropped: a
#' row that quietly disappears takes the flavoring selected from it with it.
#'
#' @param x A data frame holding [INGREDIENT_COLUMNS].
#' @return The same rows, typed and trimmed.
validate_ingredients <- function(x) {
  if (!is.data.frame(x)) {
    stop("The ingredient library must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(INGREDIENT_COLUMNS, names(x))
  if (length(missing) > 0) {
    stop(
      "The ingredient library is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x <- x[, INGREDIENT_COLUMNS, drop = FALSE]
  x$ingredient <- trimws(as.character(x$ingredient))
  x$unit <- trimws(as.character(x$unit))
  x$amount_per_gal <- suppressWarnings(as.numeric(x$amount_per_gal))
  x$cost_per_unit <- suppressWarnings(as.numeric(x$cost_per_unit))

  if (any(!nzchar(x$ingredient))) {
    stop("Every ingredient needs a name.", call. = FALSE)
  }

  invalid <- which(
    is.na(x$amount_per_gal) | is.na(x$cost_per_unit) |
      x$amount_per_gal < 0 | x$cost_per_unit < 0
  )
  if (length(invalid) > 0) {
    stop(
      "Amounts and costs must be numbers of at least zero: ",
      paste(sQuote(x$ingredient[invalid]), collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_names <- unique(x$ingredient[duplicated(x$ingredient)])
  if (length(duplicated_names) > 0) {
    stop(
      "Each ingredient may only appear once: ",
      paste(sQuote(duplicated_names), collapse = ", "),
      call. = FALSE
    )
  }

  rownames(x) <- NULL
  x
}

#' Load the ingredient library
#'
#' Falls back to the shipped defaults when there is no file yet, so a fresh
#' checkout and a first run both start from a working library.
#'
#' @param path CSV file to read.
load_ingredients <- function(path = ingredients_path()) {
  if (!file.exists(path)) {
    return(default_ingredients())
  }
  validate_ingredients(utils::read.csv(path, stringsAsFactors = FALSE))
}

#' Save the ingredient library
#'
#' @param x A data frame holding [INGREDIENT_COLUMNS].
#' @param path CSV file to write.
#' @return The validated library, invisibly.
save_ingredients <- function(x, path = ingredients_path()) {
  x <- validate_ingredients(x)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(x, path, row.names = FALSE)
  invisible(x)
}

#' A blank row to fill in
#'
#' Named rather than empty, since the picker and the library are keyed on the
#' name and two blank rows would collide.
#'
#' @param x The library the row is being added to.
new_ingredient_row <- function(x) {
  taken <- x$ingredient
  name <- "New ingredient"
  i <- 2
  while (name %in% taken) {
    name <- paste("New ingredient", i)
    i <- i + 1
  }
  data.frame(
    ingredient = name,
    amount_per_gal = 1,
    unit = "unit",
    cost_per_unit = 0,
    stringsAsFactors = FALSE
  )
}
