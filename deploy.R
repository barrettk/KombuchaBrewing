# Deployment to shinyapps.io. Not part of the app; source it and call redeploy().
#
#   source("deploy.R")
#   redeploy()

DEPLOY_ACCOUNT <- "kylebarrett"
DEPLOY_APP <- "Kombucha"
DEPLOY_SERVER <- "shinyapps.io"

# Listed by directory rather than by name so a new module or stylesheet ships
# without anyone remembering to add it here. Tests and the README screenshot are
# left out: they are not needed to run the app.
deploy_files <- function() {
  c(
    "app.R",
    "global.R",
    "data/ingredients.csv",
    list.files("functions", pattern = "[.]R$", full.names = TRUE),
    list.files("modules", pattern = "[.]R$", full.names = TRUE),
    list.files("www/css", full.names = TRUE),
    list.files("www/img", full.names = TRUE)
  )
}

# rsconnect reads the project's renv lockfile whenever it finds one, which would
# send testthat, shinytest2 and chromote to the server along with the app.
# Copying the app to a directory with no renv files leaves rsconnect to read the
# library() calls instead, and it still resolves versions from the renv library
# this runs in, so what deploys is what the tests ran against.
stage_bundle <- function(files) {
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("Missing from the bundle: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  stage <- file.path(tempdir(), "kombucha-bundle")
  unlink(stage, recursive = TRUE)
  for (f in files) {
    dest <- file.path(stage, f)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(f, dest)
  }
  stage
}

#' Deploy the app over the existing shinyapps.io instance.
#'
#' @param test Run the test suite first and stop on failure.
#' @return The deployed app's URL, invisibly.
redeploy <- function(account = DEPLOY_ACCOUNT, app_name = DEPLOY_APP, test = TRUE) {
  if (!file.exists("app.R")) {
    stop("Run this from the project root.", call. = FALSE)
  }

  registered <- rsconnect::accounts()
  if (is.null(registered) || !account %in% registered$name) {
    stop(
      "No credentials for '", account, "'. Get a token from ",
      "https://www.shinyapps.io/admin/#/tokens and run:\n",
      '  rsconnect::setAccountInfo(name = "', account, '", token = "...", secret = "...")',
      call. = FALSE
    )
  }

  # Deploying is public and immediate, so a broken suite should stop it here
  # rather than after the fact.
  if (test) {
    message("Running tests before deploying...")
    testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
  }

  stage <- stage_bundle(deploy_files())

  # Carrying the deployment record in and back out keeps the app id and bundle
  # history attached to the app rather than left behind in a temp directory.
  record <- file.path("rsconnect", DEPLOY_SERVER, account, paste0(app_name, ".dcf"))
  if (file.exists(record)) {
    dir.create(file.path(stage, dirname(record)), recursive = TRUE, showWarnings = FALSE)
    file.copy(record, file.path(stage, record))
  }

  rsconnect::deployApp(
    appDir = stage,
    appName = app_name,
    appTitle = app_name,
    account = account,
    server = DEPLOY_SERVER,
    forceUpdate = TRUE,
    launch.browser = FALSE
  )

  if (file.exists(file.path(stage, record))) {
    dir.create(dirname(record), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(stage, record), record, overwrite = TRUE)
  }

  url <- sprintf("https://%s.shinyapps.io/%s/", account, app_name)
  message("Deployed: ", url)
  invisible(url)
}
