####################
#### SHOWING RULES
####################


#' Show decision rules
#' @param directory the directory where to output the decision rules
#'
#' @export
show_decision_rules <- function(directory = getwd()) {

  if(!requireNamespace("openxlsx", quietly = TRUE)){stop("Package {openxlsx} is required to output excel files. Please use `install.packages('openxlsx')`")}

  if (!dir.exists(directory)) (dir.create(directory))
  openxlsx::write.xlsx(decision_rules_total, file.path(directory, "IDEATools_decision_rules.xlsx"))
}



#' Show the blank tree canvas
#' @param directory the directory where to output the blank canvas
#'
#' @export
show_canvas <- function(directory = getwd()) {
  if (!dir.exists(file.path(directory, "canvas"))) (dir.create(file.path(directory, "canvas")))
  for (i in names(canvas)) {
    writeLines(canvas[[i]], file.path(directory, "canvas", paste0(i, ".svg")))
  }
}
