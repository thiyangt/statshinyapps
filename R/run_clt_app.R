#' Launch the Central Limit Theorem Shiny app
#'
#' @return Launches a Shiny application.
#' @export
run_clt_app <- function() {

  app_dir <- system.file(
    "shiny-apps",
    "CLT",
    package = "statshinyapps"
  )

  shiny::runApp(app_dir)
}
