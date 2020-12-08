mixedToFloat <- function(x){
  is.integer  <- grepl("^\\d+$", x)
  is.fraction <- grepl("^\\d+\\/\\d+$", x)
  is.mixed    <- grepl("^\\d+ \\d+\\/\\d+$", x)
  stopifnot(all(is.integer | is.fraction | is.mixed))
  
  numbers <- strsplit(x, "[ /]")
  
  ifelse(is.integer,  as.numeric(sapply(numbers, `[`, 1)),
         ifelse(is.fraction, as.numeric(sapply(numbers, `[`, 1)) /
                  as.numeric(sapply(numbers, `[`, 2)),
                as.numeric(sapply(numbers, `[`, 1)) +
                  as.numeric(sapply(numbers, `[`, 2)) /
                  as.numeric(sapply(numbers, `[`, 3))))
}
