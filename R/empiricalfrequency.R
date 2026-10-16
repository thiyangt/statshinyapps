#' Launch the die tossing Shiny app
#'
#' @return Launches a Shiny application.
#' @export
run_die_app <- function() {

  app_dir <- system.file(
    "shiny-apps",
    "empiricalfrequency",
    package = "statshinyapps"
  )

  shiny::runApp(app_dir)
}
