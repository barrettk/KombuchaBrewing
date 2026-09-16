# Test runner.
#
# This is a Shiny app rather than a package, so there is no library() to load;
# `setup.R` sources the app's functions and modules instead.
#
#   Rscript tests/testthat.R
#   testthat::test_dir("tests/testthat")

testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
