#' Application constants and configuration

APP_VERSION <- "2.0.0"

OZ_PER_GAL <- 128
BOTTLES_PER_CASE <- 12

# Bottle sizes in fluid ounces; 128 is a gallon jug, labelled as one
BOTTLE_SIZES <- c(8, 12, 16, 24, 32, 64, 128)

SUGAR_PER_GAL_CHOICES <- c("1/2", "3/4", "1", "5/4")

# Cooling agents, and the unit each is bought in
COOLING_AGENTS <- c(Water = "gallon", Ice = "bag")

BOOCH_STYLES <- c("Regular", "Hard")

DEFAULT_FLAVORINGS <- list(
  Regular = c("Ginger", "Pomegranate Juice", "Blue Butterfly Pea Flower"),
  Hard = c("Mango Juice", "Citra Hops", "EC-1118 Champagne Yeast")
)

# Cost groups in the order a batch is built, which is the order the breakdown
# table renders them in.
COST_GROUPS <- c(
  "Tea", "Sugar", "Cooling",
  "Flavoring \u2014 Regular", "Flavoring \u2014 Hard",
  "Bottling"
)

#' Path to the editable ingredient library
#'
#' Overridable by environment variable so tests, and a read-only deployment,
#' can point it at a copy.
ingredients_path <- function() {
  Sys.getenv("KOMBUCHA_INGREDIENTS", unset = "data/ingredients.csv")
}
