####################
#### SHOWING RULES
####################

#' Show decision rules
#' @param directory the directory where to output the decision rules
#' @return Exports an excel file in the desired directory
#' @examples
#' library(IDEATools)
#' show_decision_rules(tempdir())
#' 
#' @export
show_decision_rules <- function(directory) {
  rlang::check_installed("openxlsx", reason = "to use `show_decision_rules()`")

  if (!dir.exists(directory)) (dir.create(directory))
  openxlsx::write.xlsx(decision_rules_total, file.path(directory, "IDEATools_decision_rules.xlsx"))
}


#' Show the reference table used for building colored trees
#' @param directory the directory where to output the reference tables
#' @return Exports an excel file in the desired directory
#' @examples
#' library(IDEATools)
#' show_tree_structure(tempdir())
#' 
#' @export
show_tree_structure <- function(directory) {
  rlang::check_installed("openxlsx", reason = "to use `show_tree_structure()`")

  if (!dir.exists(file.path(directory, "tree_structure"))) (dir.create(file.path(directory, "tree_structure")))

  for (i in names(tree_structure)) {
    openxlsx::write.xlsx(tree_structure[[i]], file.path(directory, "tree_structure", paste0(i, ".xlsx")))
  }
}
