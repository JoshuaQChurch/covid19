#' Helper function to deploy up-to-date data 
#' 
#' @description Download and deploy most recent data. 
#'     Will only work if \link{rsconnect} information has been set up. 
#'     
#' @export
deploy <- function() {
  download_all_data()
  
  rsconnect::deployApp(
    appDir = system.file("apps", "covid-19", package = "covid19"),
    forceUpdate = TRUE
  )
}
