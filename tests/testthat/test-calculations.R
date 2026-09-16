# The batch arithmetic every reported number rests on.

test_that("brew volume is the batch less its starter", {
  expect_equal(brew_volume(15, 0.2), 12)
  expect_equal(brew_volume(10, 0), 10)
})

test_that("a purchase implies a rate per unit", {
  expect_equal(unit_rate(21, 600), 0.035)
  # A price with nothing bought does not imply a rate. Dividing by zero would
  # hand `Inf` to the cost table and print a batch total of infinity.
  expect_true(is.na(unit_rate(21, 0)))
  expect_true(is.na(unit_rate(21, NA)))
})

test_that("cost lines carry quantity times rate", {
  lines <- cost_lines("Tea", c("Black tea", "Green tea"), c(84, 36), "bag", c(0.035, 0.075))

  expect_identical(names(lines), COST_LINE_COLUMNS)
  expect_equal(lines$cost, c(84 * 0.035, 36 * 0.075))
  expect_equal(cost_total(lines), 84 * 0.035 + 36 * 0.075)
})

test_that("empty cost lines bind with real ones", {
  lines <- bind_cost_lines(
    empty_cost_lines(),
    cost_lines("Sugar", "Sugar", 12, "cup", 0.5)
  )

  expect_identical(names(lines), COST_LINE_COLUMNS)
  expect_equal(nrow(lines), 1)
  expect_equal(cost_total(lines), 6)
})

test_that("combined lines are ordered by the brew sequence", {
  lines <- bind_cost_lines(
    bottling_lines(bottling_plan(12, 16, 3), 6.55, 0.09),
    sugar_lines(12, 1, 0.5),
    tea_lines(12, 10, 0.7, 0.035, 0.075)
  )

  expect_identical(unique(lines$group), c("Tea", "Sugar", "Bottling"))
})

test_that("tea is costed in whole bags", {
  lines <- tea_lines(12, 10, 0.7, 0.035, 0.075)

  expect_equal(lines$quantity, c(84, 36))
  expect_equal(sum(lines$quantity), 120)
})

test_that("a lopsided black tea share still rounds to whole bags", {
  lines <- tea_lines(7, 10, 0.35, 0.035, 0.075)

  expect_equal(lines$quantity, round(lines$quantity))
  expect_true(all(lines$quantity >= 0))
})

test_that("sugar scales with the brew volume", {
  lines <- sugar_lines(12, parse_mixed_number("3/4"), 0.5)

  expect_equal(lines$quantity, 9)
  expect_equal(lines$cost, 4.5)
})

test_that("the cooling agent is costed in the unit it is bought in", {
  water <- cooling_lines("Water", 5, 0.6)
  ice <- cooling_lines("Ice", 2, 3)

  expect_identical(water$unit, "gallon")
  expect_identical(ice$unit, "bag")
  expect_equal(ice$cost, 6)
  expect_error(cooling_lines("Snow", 1, 1))
})

test_that("a style's flavorings scale with that style's volume", {
  lines <- flavoring_lines(fixture_ingredients(), c("Ginger", "Citra Hops"), 6, "Regular")

  expect_identical(lines$group, rep("Flavoring \u2014 Regular", 2))
  expect_equal(lines$quantity, c(0.125 * 6, 0.2 * 6))
  expect_equal(cost_total(lines), 0.125 * 6 * 2 + 0.2 * 6 * 5)
})

test_that("an ingredient no longer in the library drops out of the costing", {
  # The earlier app matched selections against the library with a regex over
  # the names joined by "|", so a deleted ingredient could match a neighbour
  # and be costed as it.
  lines <- flavoring_lines(fixture_ingredients(), c("Ginger", "Elderflower"), 6, "Hard")

  expect_identical(lines$item, "Ginger")
})

test_that("no flavorings selected costs nothing rather than erroring", {
  lines <- flavoring_lines(fixture_ingredients(), character(0), 6, "Regular")

  expect_equal(nrow(lines), 0)
  expect_equal(cost_total(lines), 0)
})

test_that("the split follows the styles selected, not the slider", {
  expect_equal(brew_split(c("Regular", "Hard"), 60), list(regular = 0.6, hard = 0.4))
  # With one style selected the batch is all of it, whatever the slider says.
  expect_equal(brew_split("Regular", 60), list(regular = 1, hard = 0))
  expect_equal(brew_split("Hard", 60), list(regular = 0, hard = 1))
  expect_equal(brew_split(character(0), 60), list(regular = 0, hard = 0))
})

test_that("bottles hold less kombucha than their size", {
  plan <- bottling_plan(12, 16, 3)

  expect_equal(plan$booch_oz, 13)
  expect_equal(plan$bottles, 128 * 12 / 13)
  # Whole bottles are sold, whole cases are bought, and the part-bottle at the
  # end of the batch is neither.
  expect_equal(plan$sellable, 118)
  expect_equal(plan$caps, 118)
  expect_equal(plan$cases, 10)
})

test_that("a bottle that is all juice is refused", {
  expect_error(bottling_plan(12, 16, 16), "more than its juice")
  expect_error(bottling_plan(12, 8, 12), "more than its juice")
})

test_that("bottling is costed by the case and the cap", {
  lines <- bottling_lines(bottling_plan(12, 16, 3), 6.55, 0.09)

  expect_identical(lines$item, c("Bottles", "Caps"))
  expect_equal(lines$quantity, c(10, 118))
  expect_equal(cost_total(lines), 10 * 6.55 + 118 * 0.09)
})

test_that("pricing spreads cost over the bottles that can be sold", {
  totals <- pricing_summary(total_cost = 100, sellable = 50, sell_price = 4)

  expect_equal(totals$cost_per_bottle, 2)
  expect_equal(totals$revenue, 200)
  expect_equal(totals$profit, 100)
  expect_equal(totals$profit_per_bottle, 2)
  expect_equal(totals$margin, 0.5)
  expect_equal(totals$break_even, 25)
})

test_that("a batch sold below cost reports a loss rather than a small profit", {
  totals <- pricing_summary(total_cost = 100, sellable = 50, sell_price = 1)

  expect_equal(totals$profit, -50)
  expect_equal(totals$profit_per_bottle, -1)
  expect_lt(totals$margin, 0)
})

test_that("break-even rounds up to a whole bottle", {
  # 33.3 bottles does not break even; 34 does.
  expect_equal(pricing_summary(100, 50, 3)$break_even, 34)
})

test_that("pricing with nothing to sell reports no per-bottle figure", {
  totals <- pricing_summary(total_cost = 100, sellable = 0, sell_price = 4)

  expect_true(is.na(totals$cost_per_bottle))
  expect_true(is.na(totals$profit_per_bottle))
  expect_equal(totals$revenue, 0)
  expect_equal(totals$profit, -100)
})

test_that("a batch totals its stages", {
  # The whole pipeline, from settings to a total, at figures a brewer would
  # recognize: 15 gallons at a fifth starter, half regular and half hard.
  brew_gal <- brew_volume(15, 0.2)
  split <- brew_split(c("Regular", "Hard"), 50)
  plan <- bottling_plan(brew_gal, 16, 3)

  lines <- bind_cost_lines(
    tea_lines(brew_gal, 10, 0.7, unit_rate(21, 600), unit_rate(3, 40)),
    sugar_lines(brew_gal, 1, unit_rate(5, 14)),
    cooling_lines("Water", 5, unit_rate(3, 5)),
    flavoring_lines(fixture_ingredients(), "Ginger", split$regular * brew_gal, "Regular"),
    flavoring_lines(fixture_ingredients(), "Citra Hops", split$hard * brew_gal, "Hard"),
    bottling_lines(plan, 6.55, 0.09)
  )

  expect_setequal(
    unique(lines$group),
    c("Tea", "Sugar", "Cooling", "Flavoring \u2014 Regular", "Flavoring \u2014 Hard", "Bottling")
  )
  # The total is the sum of the lines, and the per-bottle figure divides it.
  expect_equal(cost_total(lines), sum(lines$cost))
  totals <- pricing_summary(cost_total(lines), plan$sellable, 2.50)
  expect_equal(totals$cost_per_bottle * plan$sellable, cost_total(lines))
})
