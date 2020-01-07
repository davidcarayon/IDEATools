#' Export IDEA related plots
#'
#' @param IDEAres output of IDEA plotting functions
#' @param outdir the output directory
#'
#' @return Exports plots in png/pdf format in the output directory
#' @importFrom magrittr %>%
#' @export
exportIDEA <- function(IDEAres, outdir = paste0("RES_",Sys.Date())) {

  # On va toujours créer un dossier par exploitation
  # On stockera les graphes "meta" à la racine

  if (!dir.exists(outdir)){dir.create(outdir)}

  # Heuristic maps ----------------------------------------------------------
  if (IDEAres$plot.type == "tree") {

tab_res <- tibble::tibble(name = names(IDEAres), itemlist = IDEAres) %>%
      dplyr::filter(!name %in% c("analysis.type","plot.type")) %>%
      dplyr::mutate(prop = purrr::map(itemlist, names)) %>%
      tidyr::unnest(c(itemlist,prop)) %>%
      # dplyr::mutate(prop = stringi::stri_trans_general(prop,"Latin-ASCII")) %>%
      dplyr::mutate(name = stringr::str_replace(name," ","_")) %>%
      dplyr::mutate(folder = file.path(outdir,name,"Propriétés","Cartes_heuristiques")) %>%
      dplyr::mutate(path = file.path(outdir,name,"Propriétés","Cartes_heuristiques",prop)) %>%
      dplyr::mutate(png_path = glue::glue("{path}.png"),
                    pdf_path = glue::glue("{path}.pdf"))

export_heuristic_map <- function(prop,itemlist,folder,png_path,pdf_path){

  heuristic_res <- list(
    Robustesse = c(1439,951),
    Capacité = c(1596,934),
    Autonomie = c(1439,951),
    Responsabilité=c(1439,951),
    Ancrage= c(1439,951),
    Global=c(1984,1403)
  )

  dim = heuristic_res[[prop]]
  ff <- charToRaw(itemlist)


  if (!dir.exists(folder)){dir.create(folder, recursive = TRUE)}

  rsvg::rsvg_png(ff,png_path, width = dim[1], height = dim[2])
  rsvg::rsvg_pdf(ff, pdf_path)
}

purrr::pwalk(.l = list(tab_res$prop,tab_res$itemlist,tab_res$folder,tab_res$png_path,tab_res$pdf_path),.f = export_heuristic_map)


}

  # Dimension plots ---------------------------------------------------------
  if (IDEAres$plot.type == "dim") {


    dimension_res <- list(
dimensions = c(9.11,5.6),
composantes = c(13.69,10.5),
`indic_Socio-Territoriale` = c(10.69,13.5),
indic_Economique = c(10.69,9),
indic_Agroécologique = c(10.69,12),
`Responsabilité globale` = c(6,7),
`Ancrage Territorial` = c(6,7),
Autonomie = c(6,7),
Robustesse = c(6,7),
`Capacité productive et reproductive \nde biens et de services` = c(6,7)
)




n_exploit <- dplyr::n_distinct(names(IDEAres))-2

tab_res <- tibble::tibble(name = names(IDEAres), plot = IDEAres) %>%
  dplyr::filter(!name %in% c("analysis.type","plot.type")) %>%
  dplyr::mutate(plotname = purrr::map(plot, names)) %>%
  tidyr::unnest(c(plot,plotname)) %>%
  dplyr::mutate(widths = rep(c(9.11,13.69,10.69,10.69,10.69,6,6,6,6,6),n_exploit),
                heights = rep(c(5.6,10.5,13.5,9,12,7,7,7,7,7),n_exploit)) %>%
  dplyr::mutate(name = str_replace(name," ","_")) %>%
  dplyr::mutate(folder = file.path(outdir,name,"Dimensions")) %>%
  dplyr::mutate(path = file.path(outdir,name,"Dimensions",plotname)) %>%
  dplyr::mutate(png_path = glue::glue("{path}.png"))

export_dimplot <- function(plotname,plot,widths, heights, folder, png_path) {

  if (!dir.exists(folder)){dir.create(folder, recursive = TRUE)}

  print(plot) %>%
    ggplot2::ggsave(filename=png_path,dpi = "retina", width = widths, height = heights)
}

purrr::pwalk(.l = list(tab_res$plotname,tab_res$plot,tab_res$widths,tab_res$heights, tab_res$folder,tab_res$png_path), .f = export_dimplot)



}




}

