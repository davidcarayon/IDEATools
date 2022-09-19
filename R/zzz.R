####################
#### MISC FUNCTIONS
####################


# Global variables (to avoid CRAN NOTE)
utils::globalVariables(c("Code", "Valeur", "...1", "item", "A Exporter", "x1", "x6", "x5", "value", "x6", "DEF", "FAV", "INT", "TDEF", "component", "component_code", "component_value", "dimension", "dimension_code", "dimension_value", "indic", "indic_code", "level", "max_compo", "max_indic", "scaled_value", "score_category", "unscaled_value", "data", "i_donnees_generales_et_inventaires_de_lexploitation", "x2", "x7", "x8", "category", "full_name", "head", "hjust", "id", "indic_name", "indic_number", "max_dim", "path", "rect_end", "rect_id", "rect_number_c", "rect_number_p", "result", "score", "score_indic", "ymax", "ymin", "Indicateur", "indicateur", "R\u00e9sultat", "Niveau", "plotname", "name","Exploitation","Mean","Statistique","farm_id","label","min_compo","n","prop","quantile","resultat","cairo_pdf","result_ascii","node_name","component_max","node_code","node_name","Resultat","prop_code","code","x","y","xend","yend","size","id_exploit","orga","id_number","A1","C11","item_name","item_no","facet_label"))


# Adds a line break for too long strings
wrapit <- function(text, width = 75) {
  wtext <- paste(strwrap(text, width = width), collapse = " \n ")
  return(wtext)
}
