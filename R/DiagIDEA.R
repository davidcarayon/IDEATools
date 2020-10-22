#' Wrapper function for IDEATools
#'
#' @param input a system path leading either to a single file or a directory. If the input is a single file, accepted formats are : .xls, .xlsx and .json.
#' @param output the output directory
#' @param dimensions Should the diagnosis feature the dimension plots ?
#' @param trees Should the diagnosis feature the enlightened trees ?
#' @param radar Should the diagnosis feature the radar plots ?
#' @param meta Should the diagnosis feature the meta-analysis plots ?
#' @param export Should the result be exported as png/pdf files, or as an R object ?
#'
#' @return a named list, containing :
#' \describe{
#'   \item{dimensions}{a string which can be "single" or "folder" depending on the input type.}
#'   \item{trees}{a tibble dataframe with the extracted data}
#'   \item{radars}{results of the property analysis}
#'   \item{meta}{metadata extracted from the input}
#' }
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' Diag <- DiagIDEA(path,"tmp", meta = FALSE, export = "NULL")
DiagIDEA <- function(input, output = paste0("RES_", Sys.Date()), dimensions = TRUE, trees = TRUE, radar = TRUE,meta = TRUE, export = "NULL") {

  IDEAdata <- importIDEA(input)

  dim <- NULL
  tree <- NULL
  rad <- NULL
  met <- NULL

  if(export == "Report") {

    MakeReport(IDEAdata, silent = FALSE)

  }

  if(export == "Excel") {

    MakeExcel(input, silent = FALSE)

  }


  if(export%in% c("Images","NULL")) {

    if(dimensions) {
      dim <- dimensionsPlots(IDEAdata)
      if(export == "Images"){exportIDEA(dim,output)}
    }

    if(trees) {
      tree <- MakeTrees(IDEAdata)
      if(export == "Images"){exportIDEA(tree,output)}
    }


    if(radar) {
      rad <- radarPlots(IDEAdata)
      if(export == "Images"){exportIDEA(rad,output)}
    }


    if(meta) {
      met <- metaIDEA(IDEAdata)
      if(export == "Images"){exportIDEA(met,output)}
    }

  }

  if(export == "NULL"){return(list(dimensions=dim,trees = tree,radars = rad,meta = met))}

}
