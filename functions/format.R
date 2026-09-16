#' Formatting helpers
#'
#' The display layer. Everything here turns a number the calculations produced
#' into the string the UI shows, and nothing here does arithmetic that a result
#' depends on.

#' Format a dollar amount
#'
#' Missing values print as a dash rather than "NA", so a half-filled form reads
#' as incomplete instead of broken.
#'
#' @param x Numeric vector of dollar amounts.
format_currency <- function(x) {
  out <- scales::dollar(x, accuracy = 0.01)
  out[is.na(x)] <- "\u2014"
  out
}

#' Format a quantity for display
#'
#' Trailing zeros are dropped, so a count reads as "12" and a measured amount
#' as "0.533", without a per-call decision about how many digits to show.
#'
#' @param x Numeric vector.
format_quantity <- function(x) {
  out <- formatC(
    x,
    format = "f", digits = 3, big.mark = ",", drop0trailing = TRUE
  )
  out[is.na(x)] <- "\u2014"
  out
}

#' Format a volume in gallons
#'
#' @param x Numeric vector of gallons.
format_gallons <- function(x) {
  paste(format_quantity(x), ifelse(x == 1, "gallon", "gallons"))
}

#' Pluralize a unit against the quantity measured in it
#'
#' Units are free text from the ingredient library ("32oz jar", "5g bag"), so
#' this only ever appends an "s". The symbols that do not take one are listed,
#' since "3.198 ozs" is the kind of thing a reader notices.
#'
#' @param unit Character vector of unit names.
#' @param quantity Numeric vector of quantities.
pluralize_unit <- function(unit, quantity) {
  invariant <- c("oz", "g", "kg", "ml", "l", "gal", "tsp", "tbsp")
  ifelse(
    quantity == 1 | tolower(unit) %in% invariant,
    unit,
    paste0(unit, "s")
  )
}

#' Label a bottle size
#'
#' @param size Numeric vector of bottle sizes in fluid ounces.
bottle_label <- function(size) {
  ifelse(size == OZ_PER_GAL, "1 gal", paste(size, "oz"))
}

#' Read a whole number, fraction, or mixed number
#'
#' Sugar is chosen as "1/2", "3/4", "5/4" -- the way a recipe is written -- and
#' has to reach the arithmetic as a number.
#'
#' @param x Character vector such as "1", "3/4", or "1 1/2".
#' @return Numeric vector of the same length.
parse_mixed_number <- function(x) {
  x <- trimws(as.character(x))
  valid <- grepl("^\\d+( \\d+/\\d+|/\\d+)?$", x)
  if (!all(valid)) {
    stop(
      "Not a number, fraction, or mixed number: ",
      paste(sQuote(x[!valid]), collapse = ", "),
      call. = FALSE
    )
  }

  parts <- strsplit(x, "[ /]")
  vapply(parts, function(p) {
    p <- as.numeric(p)
    switch(as.character(length(p)),
      "1" = p[1],
      "2" = p[1] / p[2],
      "3" = p[1] + p[2] / p[3]
    )
  }, numeric(1))
}
