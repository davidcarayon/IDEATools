#' Run the shiny App for Web IDEA
#'
#' @importFrom magrittr %>%
#' @export
runWebIDEA <- function() {
  appDir <- system.file("AppWeb", package = "IDEATools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IDEATools`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
