#' Render a HTML/PDF report of an IDEA diagnosis
#'
#' @param input a system path leading either to a single file or a directory. If the input is a single file, accepted formats are : .xls, .xlsx and .json.
#' @param output_dir the output directory
#' @param silent Should the algorithm be silent ?
#'
#' @return A ODT report
#' @import rmarkdown
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import tibble
#' @import knitr
#' @import ggplot2
#' @import cli
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("", package = "IDEATools")
#' MakePagedownGroupReport(path)
MakePagedownGroupReport <- function(input, output_dir = getwd(), file = "Rapport_groupe", silent = FALSE) {


  Encoding(input) <- "UTF-8"

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  IDEAdata <- importIDEA(input)
  outdir <- tempdir()

  template <- system.file("report/rapport_groupe_pagedown.Rmd", package = "IDEATools")
  style_folder <- system.file("report/bandeau.png", package = "IDEATools")
  template_folder <- system.file("report/style", package = "IDEATools")

  output_file <- paste0(file,"_",Sys.Date(),".html")

  # Définition des paramètres pour le rendu
  params <- list(data = IDEAdata,
                 outdir = outdir,
                 anon = FALSE)

  # Rendu du document dans un sous-environnement isolé
  suppressWarnings(render(template, output_file = output_file, output_dir = output_dir,
                                 params = params,
                                 envir = new.env(parent = globalenv()), quiet = FALSE))


  if(!silent){
    cat_bullet(paste0("Le rapport a été exporté à l'adresse '",file.path(output_dir,output_file)), bullet = "info", bullet_col = "green")
  }

}
