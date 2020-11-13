#' Render a PDF report of an IDEA diagnosis
#'
#' @param input a system path leading either to a single file or a directory. If the input is a single file, accepted formats are : .xls, .xlsx and .json.
#' @param output_dir the output directory
#' @param silent Should the algorithm be silent ?
#'
#' @return A PDF report
#' @import rmarkdown
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import tibble
#' @import knitr
#' @import ggplot2
#' @import cli
#' @import officer
#' @import officedown
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' MakeWordReport(path)
MakeWordReport <- function(input, output_dir = getwd(), silent = FALSE) {

  Encoding(input) <- "UTF-8"
  Encoding(list_max_compo$composante) <- "UTF-8"

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  extension <- tools::file_ext(input)

  if(!extension %in% c("xls","json","xlsx")) {
    stop("Invalid file extension. Please use .xls(x) or .json files")
  }

  IDEAdata <- importIDEA(input)
  outdir <- tempdir()

  template <- system.file("report/rapport_individuel_word.Rmd", package = "IDEATools")
  style_folder <- system.file("report/bandeau.png", package = "IDEATools")
  template_folder <- system.file("report/template.docx", package = "IDEATools")

  # Définition des paramètres pour le rendu
  params <- list(data = IDEAdata,
                 outdir = outdir,
                 anon = FALSE)


  file = "Rapport_individuel"

  output_file <- paste0(file,"_",basename(tools::file_path_sans_ext(input)),".docx")


  # Rendu du document dans un sous-environnement isolé
  suppressWarnings(render(template, output_file = output_file, output_dir = output_dir,
                          params = params,
                          envir = new.env(parent = globalenv()), quiet = FALSE))


  if(!silent){
    cat_bullet(paste0("Le rapport a été exporté à l'adresse '",file.path(output_dir,output_file)), bullet = "info", bullet_col = "green")
  }


}
