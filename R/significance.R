#' Launch Hypothesis Testing  Shiny app
#'
#' @return Launches Hypothesis Testing Shiny application.
#' @export
run_significance_app <- function() {

  app_dir <- system.file(
    "shiny-apps",
    "significance",
    package = "statshinyapps"
  )

  shiny::runApp(app_dir)
}
