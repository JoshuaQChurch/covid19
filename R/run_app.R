#' @export
dashboard <- function() {
  app_dir <- system.file("apps", "dashboard", package = "covid19")
  shiny::runApp(app_dir, display.mode = "normal")
}
