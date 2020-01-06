#' @importFrom magrittr %>%
#' @export
runIDEATool <- function() {
  appDir <- system.file("myApp", package = "IDEATools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IDEATools`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
