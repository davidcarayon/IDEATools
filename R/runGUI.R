#' Minimal Shiny app for IDEATools
#'
#' This function loads a graphical user interface (GUI) to use IDEATools.
#' @export
#' @examples
#' library(IDEATools)
#' \dontrun{
#' runGUI()
#' }
#' @importFrom rlang check_installed
#' @importFrom shiny runApp
runGUI <- function() {
  rlang::check_installed("shiny", reason = "to use `runGUI()`")
  appDir <- system.file("ShinyApp", package = "IDEATools")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `IDEATools`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
