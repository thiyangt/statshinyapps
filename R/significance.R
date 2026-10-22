#' Launch Hypothesis Testing  Shiny app
#'
#' @return Launches Hypothesis Testing Shiny application.
#' @export
run_die_app <- function() {

  app_dir <- system.file(
    "shiny-apps",
    "significance",
    package = "statshinyapps"
  )

  shiny::runApp(app_dir)
}
