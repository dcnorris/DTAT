#' Run Shiny apps included in package:DTAT
#'
#' @param app Character vector of length 1. Name off app to run.
#'
#' @return Invoked for side effect. Runs the named Shiny app.
#' @importFrom shiny runApp
#' @export
#'
#' @examples
#' \donttest{
#' runApp("Sim33PC")
#' }
runApp <- function(app) {
  # locate all the shiny apps distributed with package:DTAT
  validApps <- list.files(system.file("shiny-apps", package = "DTAT"))
  
  validAppsMsg <-
    paste0(
      "Shiny apps available in DTAT package: '",
      paste(validApps, collapse = "', '"),
      "'")
  
  # if an invalid app is given, throw an error
  if (missing(app) || !nzchar(app) ||
      !app %in% validApps) {
    stop(
      'Please run `runApp()` with a valid app name as an argument.\n',
      validAppsMsg,
      call. = FALSE)
  }
  
  # find and launch the app
  appDir <- system.file("shiny-apps", app, package = "DTAT")
  shiny::runApp(appDir, display.mode = "normal")
}