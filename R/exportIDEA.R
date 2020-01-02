#' Export plots
#'
#' @param IDEAdata output of the `importIDEA()` function
#'
#' @return a named list of plots
#' @importFrom magrittr %>%
#' @export
exportIDEA <- function(IDEAres, outdir = "resultats") {

  # Heuristic maps ----------------------------------------------------------
  if (IDEAres$plot.type == "propertyplots") {


export_heuristic_map <- function(prop,union,itemlist, outdir){

  heuristic_res <- list(
    Robustesse = c(1439,951),
    Capacité = c(1596,934),
    Autonomie = c(1439,951),
    Responsabilité=c(1439,951),
    Ancrage= c(1439,951),
    Global=c(1984,1403)
  )

  if (!dir.exists(outdir)){dir.create(outdir)}

  dim = heuristic_res[[prop]]

  ff <- charToRaw(itemlist)

  pngpath <- paste0(outdir,"/",union,".png")

  pdfpath <- paste0(outdir,"/",union,".pdf")

  rsvg::rsvg_png(ff,pngpath, width = dim[1], height = dim[2])

  rsvg::rsvg_pdf(ff, pdfpath)
}


if(IDEAres$analysis.type == "group"){
tab_res <- tibble::tibble(name = names(IDEAres), itemlist = IDEAres) %>%
  dplyr::filter(!name %in% c("analysis.type","plot.type")) %>%
  dplyr::mutate(prop = purrr::map(itemlist, names)) %>%
  tidyr::unnest(c(itemlist,prop)) %>%
  dplyr::mutate(union = paste(name,prop,sep="_")) %>%
  dplyr::mutate(union = stringr::str_replace(union," ","_"))
purrr::pwalk(.l = list(tab_res$prop,tab_res$union,tab_res$itemlist),.f = export_heuristic_map, outdir = outdir)
}


if(IDEAres$analysis.type == "single"){
tab_res <- tibble::tibble(prop = names(IDEAres),itemlist = unlist(IDEAres)) %>% dplyr::filter(!prop %in% c("analysis.type","plot.type"))
purrr::pwalk(.l = list(tab_res$prop,tab_res$prop,tab_res$itemlist),.f = export_heuristic_map, outdir = outdir)
}



}

  # Dimension plots ---------------------------------------------------------
  if (IDEAres$plot.type == "dimensionplots") {
dimension_res <- list(
dimensions = c(9.11,5.6),
composantes = c(13.69,10.5),
`indic_Socio-Territoriale` = c(10.69,13.5),
indic_Economique = c(10.69,9),
indic_Agroécologique = c(10.69,12)
)


export_dimplot <- function(plotname,union,plot,w,h,outdir) {
  filename = paste0(outdir,"/",union,".png")
  print(plot) %>%
    ggplot2::ggsave(filename=filename,dpi = "retina", width = w, height = h)
}



if(IDEAres$analysis.type == "group"){

n_exploit <- dplyr::n_distinct(names(IDEAres))-2

tab_res <- tibble::tibble(name = names(IDEAres), plot = IDEAres) %>%
    dplyr::filter(!name %in% c("analysis.type","plot.type")) %>%
    dplyr::mutate(plotname = purrr::map(plot, names)) %>%
    tidyr::unnest(c(plot,plotname)) %>%
    dplyr::mutate(widths = rep(c(9.11,13.69,10.69,10.69,10.69),n_exploit),
         heights = rep(c(5.6,10.5,13.5,9,12),n_exploit))%>%
  dplyr::mutate(union = paste(name,plotname,sep="_")) %>%
  dplyr::mutate(union = stringr::str_replace(union," ","_"))
purrr::pwalk(.l = list(tab_res$plotname,tab_res$union,tab_res$plot,tab_res$widths, tab_res$heights,outdir), .f = export_dimplot)
}


if(IDEAres$analysis.type == "single"){
  tab_res <- tibble::tibble(plotname = names(IDEAres), plot = IDEAres) %>% dplyr::filter(!plotname %in% c("analysis.type","plot.type")) %>% dplyr::mutate(widths = c(9.11,13.69,10.69,10.69,10.69), heights = c(5.6,10.5,13.5,9,12))
  purrr::pwalk(.l = list(tab_res$plotname,tab_res$plotname,tab_res$plot,tab_res$widths, tab_res$heights,outdir), .f = export_dimplot)
  }

}




}

