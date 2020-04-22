#' @export
dashboard <- function() {
  app_dir <- system.file("apps", "covid-19", package = "covid19")
  shiny::runApp(app_dir, display.mode = "normal")
}
