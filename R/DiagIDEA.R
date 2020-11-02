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
#' @import cli
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' Diag <- DiagIDEA(path,"tmp", meta = FALSE, export = FALSE)
DiagIDEA <- function(input, output = paste0("RES_", Sys.Date()), dimensions = TRUE, polar = TRUE, trees = TRUE, radar = TRUE,meta = TRUE, export = FALSE) {

  cli_h1("Nouveau diagnostic IDEAv4 (Version 1.1)")

  cli_h2("Import des données")

  IDEAdata <- importIDEA(input)

  cat_bullet("Données importées pour ",nrow(IDEAdata$metadata), " Exploitation(s)\n", bullet = "tick",bullet_col = "green")

  dim <- NULL
  tree <- NULL
  rad <- NULL
  met <- NULL
  pol <- NULL

  cli_h2("Réalisation des graphiques")

  if(dimensions) {
    dim <- dimensionsPlots(IDEAdata)
    cat_bullet("Graphiques des dimensions")
    if(export){
      exportIDEA(dim,output)
      cat_bullet("Les graphiques dimensions ont été exportés dans '",output,"'\n", bullet = "tick", bullet_col = "green")}
  } else {
    cat_bullet("Graphiques des dimensions non demandés par l'utilisateur\n", bullet = "info", bullet_col = "blue")
  }

  if(trees) {
    tree <- MakeTrees(IDEAdata)
    cat_bullet("Arbres éclairés")
    if(export){
      exportIDEA(tree,output)
      cat_bullet("Les arbres éclairés ont été exportés dans '",output,"'\n", bullet = "tick", bullet_col = "green")}
  } else {
    cat_bullet("Arbres éclairés non demandés par l'utilisateur\n", bullet = "info", bullet_col = "blue")
  }


  if(radar) {
    rad <- radarPlots(IDEAdata)
    cat_bullet("Diagrammes Radars")
    if(export){
      exportIDEA(rad,output)
      cat_bullet("Les diagrammes radars ont été exportés dans '",output,"'\n", bullet = "tick", bullet_col = "green")}
  } else {
    cat_bullet("Diagrammes Radars non demandés par l'utilisateur\n", bullet = "info", bullet_col = "blue")
  }


  if(meta) {
    met <- metaIDEA(IDEAdata)
    cat_bullet("Analyse de groupe")
    if(export){
      exportIDEA(met,output)
      cat_bullet("Les graphes de groupe ont été exportés dans '",output,"'\n", bullet = "tick", bullet_col = "green")}
  } else {
    cat_bullet("Graphes de groupe non demandés par l'utilisateur\n", bullet = "info", bullet_col = "blue")
  }


  if(polar) {
    pol <- PolarComponent(IDEAdata)
    cat_bullet("Diagrammes polarisés")
    if(export){
      exportIDEA(pol,output)
      cat_bullet("Les Diagrammes polarisés ont été exportés dans '",output,"'\n", bullet = "tick", bullet_col = "green")}
  } else {
    cat_bullet("Diagrammes polarisés non demandés par l'utilisateur\n", bullet = "info", bullet_col = "blue")
  }

  cli_h1("Fin de l'algorithme IDEAv4")

  if(!export){return(list(dimensions=dim,trees = tree,radars = rad,meta = met, polar = pol))}

}



