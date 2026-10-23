#' Launch Hypothesis Testing  Shiny app
#'
#' @return Launches Hypothesis Testing Shiny application.
#' @export
run_hypothesis_app <- function() {

  app_dir <- system.file(
    "shiny-apps",
    "hypothesis",
    package = "statshinyapps"
  )

  shiny::runApp(app_dir)
}
