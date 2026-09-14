#' Batch calculations
#'
#' Every number the app reports is produced here, from plain arguments and with
#' no reference to `input` or a session. A cost is always expressed as a line:
#' a quantity of some unit at a rate, which is what makes tea, hops, and bottle
#' caps summable and lets one table render all of them.

COST_LINE_COLUMNS <- c("group", "item", "quantity", "unit", "rate", "cost")

#' Build cost lines
#'
#' @param group Cost group the lines belong to, one of [COST_GROUPS].
#' @param item Character vector naming what is being bought.
#' @param quantity Numeric vector, the amount needed for the batch.
#' @param unit Character vector, the unit `quantity` and `rate` are in.
#' @param rate Numeric vector, dollars per unit.
#' @return A data frame holding [COST_LINE_COLUMNS].
cost_lines <- function(group, item, quantity, unit, rate) {
  if (length(item) == 0) {
    return(empty_cost_lines())
  }
  data.frame(
    group = as.character(group),
    item = as.character(item),
    quantity = as.numeric(quantity),
    unit = as.character(unit),
    rate = as.numeric(rate),
    cost = as.numeric(quantity) * as.numeric(rate),
    stringsAsFactors = FALSE
  )
}

#' Cost lines with no rows
#'
#' Typed, so it binds with real lines rather than collapsing the frame.
empty_cost_lines <- function() {
  data.frame(
    group = character(0), item = character(0), quantity = numeric(0),
    unit = character(0), rate = numeric(0), cost = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Combine cost lines into one batch
#'
#' @param ... Data frames of cost lines.
#' @return All lines, ordered by [COST_GROUPS].
bind_cost_lines <- function(...) {
  lines <- do.call(rbind, c(list(empty_cost_lines()), list(...)))
  lines[order(match(lines$group, COST_GROUPS)), , drop = FALSE]
}

#' Total the cost lines
#'
#' @param lines A data frame of cost lines.
cost_total <- function(lines) {
  sum(lines$cost)
}

#' Cost per unit implied by a purchase
#'
#' Prices are entered the way they are paid -- a box of 600 tea bags for
#' $21.00 -- and the recipe needs dollars per bag.
#'
#' @param price Total paid.
#' @param count Number of units that bought.
#' @return Dollars per unit, or `NA` when the count cannot support a rate.
unit_rate <- function(price, count) {
  if (!isTRUE(count > 0)) {
    return(NA_real_)
  }
  price / count
}

#' Volume of fresh tea a batch needs
#'
#' A batch is part starter -- kombucha held back from the last one -- so only
#' the remainder is brewed, and it is that volume every ingredient scales with.
#'
#' @param batch_size Finished batch size in gallons.
#' @param starter_ratio Fraction of the batch carried over as starter.
brew_volume <- function(batch_size, starter_ratio) {
  (1 - starter_ratio) * batch_size
}

#' How a batch splits between the two styles
#'
#' The split slider only means anything when both styles are being made; with
#' one style selected the batch is all of it, whatever the slider says.
#'
#' @param styles Selected styles, a subset of [BOOCH_STYLES].
#' @param percent_regular Slider position, 0-100.
#' @return Fractions of the batch that are `regular` and `hard`.
brew_split <- function(styles, percent_regular) {
  regular <- "Regular" %in% styles
  hard <- "Hard" %in% styles

  share <- if (regular && hard) {
    percent_regular / 100
  } else if (regular) {
    1
  } else if (hard) {
    0
  } else {
    return(list(regular = 0, hard = 0))
  }

  list(regular = share, hard = 1 - share)
}

#' Tea needed for a brew
#'
#' Bags are whole things, so the split is rounded rather than carried as a
#' fraction of a bag into the cost.
#'
#' @param brew_gal Gallons of tea being brewed.
#' @param bags_per_gal Bags of tea per gallon.
#' @param black_share Fraction of the bags that are black tea; the rest green.
#' @param black_rate,green_rate Dollars per bag.
tea_lines <- function(brew_gal, bags_per_gal, black_share, black_rate, green_rate) {
  bags <- round(bags_per_gal * brew_gal * c(black_share, 1 - black_share))
  cost_lines(
    group = "Tea",
    item = c("Black tea", "Green tea"),
    quantity = bags,
    unit = "bag",
    rate = c(black_rate, green_rate)
  )
}

#' Sugar needed for a brew
#'
#' @param brew_gal Gallons of tea being brewed.
#' @param cups_per_gal Cups of sugar per gallon.
#' @param rate Dollars per cup.
sugar_lines <- function(brew_gal, cups_per_gal, rate) {
  cost_lines("Sugar", "Sugar", cups_per_gal * brew_gal, "cup", rate)
}

#' Cooling agent needed for a brew
#'
#' Hot tea is brought down to pitching temperature by dilution, with either
#' water or ice. The amount is what is bought, not what the volume implies:
#' water comes by the jug and ice by the bag.
#'
#' @param agent One of `names(COOLING_AGENTS)`.
#' @param quantity Jugs of water or bags of ice bought.
#' @param rate Dollars per jug or bag.
cooling_lines <- function(agent, quantity, rate) {
  agent <- match.arg(agent, names(COOLING_AGENTS))
  cost_lines("Cooling", agent, quantity, COOLING_AGENTS[[agent]], rate)
}

#' Flavorings needed for one style
#'
#' Ingredients are matched by name against the library, so an ingredient
#' deleted from the library drops out of the costing rather than matching a
#' neighbour.
#'
#' @param ingredients The library, as returned by [load_ingredients()].
#' @param chosen Ingredient names selected for this style.
#' @param volume_gal Gallons of this style being made.
#' @param style One of [BOOCH_STYLES], naming the cost group.
flavoring_lines <- function(ingredients, chosen, volume_gal, style) {
  style <- match.arg(style, BOOCH_STYLES)
  picked <- ingredients[match(chosen, ingredients$ingredient), , drop = FALSE]
  picked <- picked[!is.na(picked$ingredient), , drop = FALSE]

  cost_lines(
    group = paste("Flavoring", style, sep = " \u2014 "),
    item = picked$ingredient,
    quantity = picked$amount_per_gal * volume_gal,
    unit = picked$unit,
    rate = picked$cost_per_unit
  )
}

#' What a brew fills
#'
#' Bottles are part juice, so a 16 oz bottle holds less than 16 oz of
#' kombucha and the brew fills more bottles than its volume divided by the
#' bottle size. Bottles are bought by the case and only whole bottles are
#' sellable, which is why the three counts differ.
#'
#' @param brew_gal Gallons of kombucha to bottle.
#' @param bottle_size Bottle size in fluid ounces.
#' @param juice_per_bottle Fluid ounces of juice added to each bottle.
#' @return The `bottle_size` used, the `booch_oz` each bottle holds, the
#'   `bottles` the brew fills, the `sellable` whole bottles among them, and the
#'   `cases` and `caps` to buy.
bottling_plan <- function(brew_gal, bottle_size, juice_per_bottle) {
  booch_oz <- bottle_size - juice_per_bottle
  if (!isTRUE(booch_oz > 0)) {
    stop("A bottle must hold more than its juice.", call. = FALSE)
  }

  bottles <- OZ_PER_GAL * brew_gal / booch_oz
  list(
    bottle_size = bottle_size,
    booch_oz = booch_oz,
    bottles = bottles,
    sellable = floor(bottles),
    cases = ceiling(bottles / BOTTLES_PER_CASE),
    caps = floor(bottles)
  )
}

#' Bottles and caps a brew needs
#'
#' @param plan A plan from [bottling_plan()].
#' @param case_price Dollars per case of bottles.
#' @param cap_price Dollars per cap.
bottling_lines <- function(plan, case_price, cap_price) {
  cost_lines(
    group = "Bottling",
    item = c("Bottles", "Caps"),
    quantity = c(plan$cases, plan$caps),
    unit = c("case", "cap"),
    rate = c(case_price, cap_price)
  )
}

#' What a batch costs and returns
#'
#' Cost is spread over the bottles that can be sold rather than over the volume
#' brewed, so the part-bottle at the end of a batch is carried by the rest.
#'
#' @param total_cost Cost of the batch.
#' @param sellable Whole bottles the batch fills.
#' @param sell_price Dollars per bottle.
pricing_summary <- function(total_cost, sellable, sell_price) {
  per_bottle <- if (sellable > 0) total_cost / sellable else NA_real_
  revenue <- sell_price * sellable
  profit <- revenue - total_cost

  list(
    sellable = sellable,
    cost_per_bottle = per_bottle,
    revenue = revenue,
    profit = profit,
    profit_per_bottle = if (sellable > 0) profit / sellable else NA_real_,
    margin = if (revenue > 0) profit / revenue else NA_real_,
    break_even = if (isTRUE(sell_price > 0)) ceiling(total_cost / sell_price) else NA_real_
  )
}
