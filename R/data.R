#' Table used to convert unscaled values to categories
#'
#' This table lists, for each indicator, the thresholds used to categorise them.
#'
#' @format A data frame with 53 rows and 6 variables:
#' \describe{
#'   \item{indicateur}{code of the indicator}
#'   \item{nom_indicateur}{full name of the indicator}
#'   \item{TD}{threshold for très défavorable}
#'   \item{D}{threshold for défavorable}
#'   \item{I}{threshold for intermédiaire}
#'   \item{F}{threshold for favorable}
#' }
"categ"

#' List used to convert indicators to properties
#'
#' This list contains, for each node, the decision table created by the authors
#'
#' @format A list of length 47
"decision_rules_total"

#' Table gathering all metadata on IDEA variables
#'
#' This table contains info for each IDEA variable about the full label, heuristic map level, dimension, etc.
#'
#' @format A data frame with 99 rows and 7 variables:
#' \describe{
#'   \item{code_indicateur}{code of the indicator}
#'   \item{nom_indicateur}{full name of the indicator}
#'   \item{nom_complet}{the code and full name of the indicator collapsed together}
#'   \item{level}{The levels of the given indicator in the heuristic map}
#'   \item{dimension}{Dimension of the indicator}
#'   \item{dim}{Abbreviated dimension}
#'   \item{composante}{compound of the indicator}
#' }
"label_nodes"
