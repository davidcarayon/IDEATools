#' Run the shiny App
#'
#' @importFrom magrittr %>%
#' @importFrom shiny runApp
#' @export

runIDEATool <- function() {
  appDir <- system.file("IDEAToolsApp", package = "IDEATools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IDEATools`.", call. = FALSE)
  }
  runApp(appDir, display.mode = "normal")
}
