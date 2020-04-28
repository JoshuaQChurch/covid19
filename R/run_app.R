#' Launch the COVID-19 dashboard
#'
#' @param update Fetch the latest data.
#'
#' @export
dashboard <- function(update = TRUE) {

  if (update) {
    message("Fetching latest data...")
    download_all_data()
  }
  message("Launching dashboard...")

  app_dir <- system.file("apps", "covid-19", package = "covid19")
  shiny::runApp(app_dir, display.mode = "normal")
}
