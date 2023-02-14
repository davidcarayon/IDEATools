######################
#### EXCEL REPORTING #
######################

## Function to produce Excel single reports
excel_report <- function(IDEAdata, output_dir, outdir, output_file, prefix, dpi, append = FALSE, input_file_append = NULL) {
  # Check if openxlsx installed
  rlang::check_installed("openxlsx", reason = "to make excel reports`")

  # Creating plots
  write_idea(IDEAdata, output_directory = outdir, type = "local", prefix = prefix, dpi = dpi, quiet = TRUE)

  # Creating workbook
  if (append == TRUE) {
    wb <- openxlsx::loadWorkbook(input_file_append)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  ## Setting styles

  ## Custom cells
  hs1 <- openxlsx::createStyle(
    fgFill = "#C0504D", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "white"
  )
  standard <- openxlsx::createStyle(
    fontColour = "#000000", halign = "CENTER",
    borderStyle = "medium", border = "TopBottomLeftRight"
  )

  ## Dimensions
  AEStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#2e9c15")
  STStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#5077FE")
  ECStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FE962B")

  ## Properties
  FStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#33FF00")
  TFStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#008B00")
  IStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FCD400")
  DStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF6347")
  TDStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#CD0000")
  NCStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#cecece")

  ## Beginning the openxlsx sequence

  # Dimensions --------------------------------------------------------------

  ## Add worksheets
  openxlsx::addWorksheet(wb, "Dimensions", gridLines = FALSE, tabColour = "#cecece")
  openxlsx::addWorksheet(wb, "Composantes", gridLines = FALSE, tabColour = "#cecece")
  openxlsx::addWorksheet(wb, "Indicateurs", gridLines = FALSE, tabColour = "#cecece")



  df <- unique(merge(IDEAdata$data$dataset, reference_list$indic_dim, by.x = "indic", by.y = "indic_code")[, .(Dimension = dimension, Score = as.numeric(dimension_value))])

  ## Write the data
  openxlsx::writeData(wb, "Dimensions", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Colwidths
  openxlsx::setColWidths(wb, "Dimensions", cols = 4, widths = 2)
  openxlsx::setColWidths(wb, "Dimensions", cols = 1:3, widths = 26)

  ## Add dimension plot
  img <- file.path(outdir, Sys.Date(), prefix, "Dimensions", paste0(prefix, "_", "Dimensions.png"))
  openxlsx::insertImage(wb, "Dimensions", file = img, startRow = 2, startCol = "E", width = 16.61, height = 10.21, units = "cm")


  ## Components

  ## Create dataset
  df <- unique(merge(IDEAdata$data$dataset, reference_list$indic_dim, by.x = "indic", by.y = "indic_code")[, .(Dimension = dimension, Composante = component, Score = as.numeric(component_value), `Max possible` = component_max)])

  ## Write the data
  openxlsx::writeData(wb, "Composantes", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to dimensions
  openxlsx::conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Agro\u00e9cologique", style = AEStyle)
  openxlsx::conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-", style = STStyle)
  openxlsx::conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique", style = ECStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Composantes", cols = c(1, 2, 4, 5), widths = "auto")
  openxlsx::setColWidths(wb, "Composantes", cols = 3, widths = 60)
  openxlsx::setColWidths(wb, "Composantes", cols = 6, widths = 2)

  ## Add component plot
  img <- file.path(outdir, Sys.Date(), prefix, "Dimensions", paste0(prefix, "_", "Composantes.png"))
  openxlsx::insertImage(wb, "Composantes", file = img, startRow = 2, startCol = "G", width = 16.32, height = 12.52, units = "cm")

  ## Add polarised component plot
  img <- file.path(outdir, Sys.Date(), prefix, "Dimensions", paste0(prefix, "_", "Composantes polaris\u00e9es.png"))
  openxlsx::insertImage(wb, "Composantes", file = img, startRow = 17, startCol = "B", width = 21.59, height = 21.59, units = "cm")

  ## Indicators

  ## Create dataset
  df <- unique(merge(IDEAdata$data$dataset, reference_list$indic_dim, by.x = "indic", by.y = "indic_code")[, indic_number := as.numeric(regmatches(indic, regexpr("[[:digit:]]+", indic)))][order(dimension, indic_number), ][, .(Dimension = dimension, Composante = component, Indicateur = paste0(indic, " - ", indic_name), Score = as.numeric(scaled_value), `Max possible` = max_indic)])


  ## Write the data
  openxlsx::writeData(wb, "Indicateurs", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to dimensions
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Agro\u00e9cologique", style = AEStyle)
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Socio-", style = STStyle)
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Economique", style = ECStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Indicateurs", cols = 7, widths = 2)
  openxlsx::setColWidths(wb, "Indicateurs", cols = 4, widths = 75)

  ## Add dimension A indicators plot
  img <- file.path(outdir, Sys.Date(), prefix, "Dimensions", paste0(prefix, "_", "Indicateurs Agro\u00e9cologiques.png"))
  openxlsx::insertImage(wb, "Indicateurs", file = img, startRow = 2, startCol = "H", width = 14.31, height = 16.07, units = "cm")

  ## Add dimension B indicators plot
  img <- file.path(outdir, Sys.Date(), prefix, "Dimensions", paste0(prefix, "_", "Indicateurs Socio-Territoriaux.png"))
  openxlsx::insertImage(wb, "Indicateurs", file = img, startRow = 39, startCol = "H", width = 14.31, height = 18.08, units = "cm")

  ## Add dimension C indicators plot
  img <- file.path(outdir, Sys.Date(), prefix, "Dimensions", paste0(prefix, "_", "Indicateurs Economiques.png"))
  openxlsx::insertImage(wb, "Indicateurs", file = img, startRow = 57, startCol = "D", width = 14.31, height = 12.09, units = "cm")

  # Properties --------------------------------------------------------------

  ## Create worksheet
  openxlsx::addWorksheet(wb, "Synth\u00e8se Proprietes", gridLines = FALSE, tabColour = "yellow")

  ## Create list of properties names
  props <- data.table::as.data.table(reference_list$properties_nodes)[level == "propriete", node_code]

  ## Create the dataset
  df <- IDEAdata$data$nodes$Global[, index := 1][, data.table::melt(.SD, id.vars = "index")][variable %in% props, ][, .(Propriete = variable, "Resultat" = value)]


  # Extract results for coloring
  to_col <- df$"Resultat"
  names(to_col) <- df$"Propriete"


  ## Custom function with suited colors
  colorise <- function(res) {
    with(as.data.frame(res), ifelse(res == "favorable", "#33FF00",
      ifelse(res == "d\u00e9favorable", "#FF6347",
        ifelse(res == "tr\u00e8s favorable", "#008B00",
          ifelse(res == "tr\u00e8s d\u00e9favorable", "#CD0000", NA)
        )
      )
    ))
  }

  ## Add sheets with conditional tab color
  openxlsx::addWorksheet(wb, "Robustesse", gridLines = FALSE, tabColour = colorise(to_col["R10"]))
  openxlsx::addWorksheet(wb, "Ancrage Territorial", gridLines = FALSE, tabColour = colorise(to_col["AN5"]))
  openxlsx::addWorksheet(wb, "Capacit\u00e9", gridLines = FALSE, tabColour = colorise(to_col["CP10"]))
  openxlsx::addWorksheet(wb, "Autonomie", gridLines = FALSE, tabColour = colorise(to_col["AU6"]))
  openxlsx::addWorksheet(wb, "Responsabilit\u00e9 globale", gridLines = FALSE, tabColour = colorise(to_col["RG15"]))

  ## Global synthesis

  ## Write the data
  df <- merge(df, reference_list$properties_nodes, by.x = "Propriete", by.y = "node_code")[, .(Propriete = node_name, Resultat)]

  openxlsx::writeData(wb, "Synth\u00e8se Proprietes", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 4, rows = 1:300, type = "contains", rule = "NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Synth\u00e8se Proprietes", cols = 4, widths = 2)
  openxlsx::setColWidths(wb, "Synth\u00e8se Proprietes", cols = 1:3, widths = "auto")

  ## Add global tree
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Arbre global.png"))
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = img, startRow = 2, startCol = "E", width = 8.38, height = 5.21, units = "in")


  ## Robustesse

  ## Create the dataset
  pivoted_df <- IDEAdata$data$nodes[["Robustesse"]][, index := 1][, data.table::melt(.SD, id.vars = "index")][, .(indicateur = variable, Resultat = value)]

  merge_1 <- merge(pivoted_df, reference_list[["indic_dim"]], by.x = "indicateur", by.y = "indic_code", all.x = TRUE)
  merge_2 <- merge(merge_1, reference_list[["properties_nodes"]], by.x = "indicateur", by.y = "node_code", all.x = TRUE)

  merge_2_modif <- merge_2[, indic_name := ifelse(is.na(indic_name), yes = node_name, no = indic_name)][, level := ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")][, .(indicateur, Indicateur = indic_name, Niveau = level, Resultat)]

  df <- merge(merge_2_modif, IDEAdata$data$dataset, by.x = "indicateur", by.y = "indic", all.x = TRUE)[, .(Indicateur, Niveau, "Score_brut" = unscaled_value, Resultat)]

  ## Write the data
  openxlsx::writeData(wb, "Robustesse", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Robustesse", cols = 4, rows = 1:300, type = "contains", rule = "NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Robustesse", cols = 6, widths = 2)
  openxlsx::setColWidths(wb, "Robustesse", cols = 1:5, widths = "auto")

  ## Add plots
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Robustesse.png"))
  openxlsx::insertImage(wb, "Robustesse", file = img, startRow = 2, startCol = "G", width = 8.74, height = 5.66, units = "in")


  ## Ancrage

  ## Create the dataset
  pivoted_df <- IDEAdata$data$nodes[["Ancrage"]][, index := 1][, data.table::melt(.SD, id.vars = "index")][, .(indicateur = variable, Resultat = value)]

  merge_1 <- merge(pivoted_df, reference_list[["indic_dim"]], by.x = "indicateur", by.y = "indic_code", all.x = TRUE)
  merge_2 <- merge(merge_1, reference_list[["properties_nodes"]], by.x = "indicateur", by.y = "node_code", all.x = TRUE)

  merge_2_modif <- merge_2[, indic_name := ifelse(is.na(indic_name), yes = node_name, no = indic_name)][, level := ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")][, .(indicateur, Indicateur = indic_name, Niveau = level, Resultat)]

  df <- merge(merge_2_modif, IDEAdata$data$dataset, by.x = "indicateur", by.y = "indic", all.x = TRUE)[, .(Indicateur, Niveau, "Score_brut" = unscaled_value, Resultat)]


  ## Write the data
  openxlsx::writeData(wb, "Ancrage Territorial", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Ancrage Territorial", cols = 4, rows = 1:300, type = "contains", rule = "NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Ancrage Territorial", cols = 6, widths = 2)
  openxlsx::setColWidths(wb, "Ancrage Territorial", cols = 1:5, widths = "auto")

  ## Add plots
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Ancrage Territorial.png"))
  openxlsx::insertImage(wb, "Ancrage Territorial", file = img, startRow = 2, startCol = "G", width = 6.66, height = 3.97, units = "in")


  ## Capacit\u00e9 productive

  ## Create the dataset
  pivoted_df <- IDEAdata$data$nodes[["Capacite"]][, index := 1][, data.table::melt(.SD, id.vars = "index")][, .(indicateur = variable, Resultat = value)]

  merge_1 <- merge(pivoted_df, reference_list[["indic_dim"]], by.x = "indicateur", by.y = "indic_code", all.x = TRUE)
  merge_2 <- merge(merge_1, reference_list[["properties_nodes"]], by.x = "indicateur", by.y = "node_code", all.x = TRUE)

  merge_2_modif <- merge_2[, indic_name := ifelse(is.na(indic_name), yes = node_name, no = indic_name)][, level := ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")][, .(indicateur, Indicateur = indic_name, Niveau = level, Resultat)]

  df <- merge(merge_2_modif, IDEAdata$data$dataset, by.x = "indicateur", by.y = "indic", all.x = TRUE)[, .(Indicateur, Niveau, "Score_brut" = unscaled_value, Resultat)]


  ## Write the data
  openxlsx::writeData(wb, "Capacit\u00e9", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  openxlsx::conditionalFormatting(wb, "Capacit\u00e9", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Capacit\u00e9", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Capacit\u00e9", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Capacit\u00e9", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Capacit\u00e9", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Capacit\u00e9", cols = 4, rows = 1:300, type = "contains", rule = "NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Capacit\u00e9", cols = 6, widths = 2)
  openxlsx::setColWidths(wb, "Capacit\u00e9", cols = 1:5, widths = "auto")

  ## Add plots
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Capacit\u00e9 productive et reproductive de biens et de services.png"))
  openxlsx::insertImage(wb, "Capacit\u00e9", file = img, startRow = 2, startCol = "G", width = 8.74, height = 5.35, units = "in")

  ## Autonomie

  ## Create the dataset
  pivoted_df <- IDEAdata$data$nodes[["Autonomie"]][, index := 1][, data.table::melt(.SD, id.vars = "index")][, .(indicateur = variable, Resultat = value)]

  merge_1 <- merge(pivoted_df, reference_list[["indic_dim"]], by.x = "indicateur", by.y = "indic_code", all.x = TRUE)
  merge_2 <- merge(merge_1, reference_list[["properties_nodes"]], by.x = "indicateur", by.y = "node_code", all.x = TRUE)

  merge_2_modif <- merge_2[, indic_name := ifelse(is.na(indic_name), yes = node_name, no = indic_name)][, level := ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")][, .(indicateur, Indicateur = indic_name, Niveau = level, Resultat)]

  df <- merge(merge_2_modif, IDEAdata$data$dataset, by.x = "indicateur", by.y = "indic", all.x = TRUE)[, .(Indicateur, Niveau, "Score_brut" = unscaled_value, Resultat)]


  ## Write the data
  openxlsx::writeData(wb, "Autonomie", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Autonomie", cols = 4, rows = 1:300, type = "contains", rule = "NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Autonomie", cols = 6, widths = 2)
  openxlsx::setColWidths(wb, "Autonomie", cols = 1:5, widths = "auto")

  ## Add plots
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Autonomie.png"))
  openxlsx::insertImage(wb, "Autonomie", file = img, startRow = 2, startCol = "G", width = 7.78, height = 4.82, units = "in")


  ## Responsabilit\u00e9 globale

  ## Create the dataset
  pivoted_df <- IDEAdata$data$nodes[["Responsabilite"]][, index := 1][, data.table::melt(.SD, id.vars = "index")][, .(indicateur = variable, Resultat = value)]

  merge_1 <- merge(pivoted_df, reference_list[["indic_dim"]], by.x = "indicateur", by.y = "indic_code", all.x = TRUE)
  merge_2 <- merge(merge_1, reference_list[["properties_nodes"]], by.x = "indicateur", by.y = "node_code", all.x = TRUE)

  merge_2_modif <- merge_2[, indic_name := ifelse(is.na(indic_name), yes = node_name, no = indic_name)][, level := ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")][, .(indicateur, Indicateur = indic_name, Niveau = level, Resultat)]

  df <- merge(merge_2_modif, IDEAdata$data$dataset, by.x = "indicateur", by.y = "indic", all.x = TRUE)[, .(Indicateur, Niveau, "Score_brut" = unscaled_value, Resultat)]


  ## Write the data
  openxlsx::writeData(wb, "Responsabilit\u00e9 globale", df,
    colNames = TRUE, rowNames = TRUE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Responsabilit\u00e9 globale", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Responsabilit\u00e9 globale", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Responsabilit\u00e9 globale", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Responsabilit\u00e9 globale", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Responsabilit\u00e9 globale", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Responsabilit\u00e9 globale", cols = 4, rows = 1:300, type = "contains", rule = "NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Responsabilit\u00e9 globale", cols = 6, widths = 2)
  openxlsx::setColWidths(wb, "Responsabilit\u00e9 globale", cols = 1:5, widths = "auto")

  ## Add plots
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Responsabilit\u00e9 globale.png"))
  openxlsx::insertImage(wb, "Responsabilit\u00e9 globale", file = img, startRow = 2, startCol = "G", width = 10.20, height = 7.75, units = "in")


  # Appendix ------------------------------------------------------------------

  # Create the worksheet
  openxlsx::addWorksheet(wb, "Annexe", gridLines = FALSE)

  ## Now add all "radar" charts

  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Radars", paste0(prefix, "_", "Robustesse.png"))
  openxlsx::insertImage(wb, "Annexe", file = img, startRow = 2, startCol = "B", width = 23.42, height = 11.07, units = "cm")

  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Radars", paste0(prefix, "_", "Ancrage Territorial.png"))
  openxlsx::insertImage(wb, "Annexe", file = img, startRow = 2, startCol = "M", width = 23.42, height = 11.07, units = "cm")

  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Radars", paste0(prefix, "_", "Capacit\u00e9 productive et reproductive de biens et de services.png"))
  openxlsx::insertImage(wb, "Annexe", file = img, startRow = 26, startCol = "B", width = 23.42, height = 11.07, units = "cm")

  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Radars", paste0(prefix, "_", "Autonomie.png"))
  openxlsx::insertImage(wb, "Annexe", file = img, startRow = 26, startCol = "M", width = 23.42, height = 11.07, units = "cm")

  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Radars", paste0(prefix, "_", "Responsabilit\u00e9 globale.png"))
  openxlsx::insertImage(wb, "Annexe", file = img, startRow = 49, startCol = "B", width = 23.42, height = 11.07, units = "cm")

  ## Save workbook
  openxlsx::saveWorkbook(wb, file.path(output_dir, output_file), overwrite = TRUE)
}

## Function to produce Excel group reports
excel_group_report <- function(IDEAdata, output_dir, outdir, output_file, dpi) {
  # Check if openxlsx installed
  rlang::check_installed("openxlsx", reason = "to make excel reports")

  # Number of farms
  n_farm <- nrow(IDEAdata$data$metadata)

  # Creating plots
  write_idea(IDEAdata, output_directory = outdir, type = "local", dpi = dpi, quiet = TRUE)

  # Creating workbook
  wb <- openxlsx::createWorkbook()

  ## Setting styles
  hs1 <- openxlsx::createStyle(
    fgFill = "#C0504D", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "white"
  )
  hs2 <- openxlsx::createStyle(
    halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight"
  )

  bold.style <- openxlsx::createStyle(textDecoration = "Bold")

  # Dimensions
  header_ae <- openxlsx::createStyle(
    fgFill = "#2e9c15", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "#000000", wrapText = TRUE
  )

  header_st <- openxlsx::createStyle(
    fgFill = "#5077FE", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "#000000", wrapText = TRUE
  )

  header_ec <- openxlsx::createStyle(
    fgFill = "#FE962B", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "#000000", wrapText = TRUE
  )

  AEStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#2e9c15", wrapText = TRUE)
  STStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#5077FE", wrapText = TRUE)
  ECStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FE962B", wrapText = TRUE)
  standard <- openxlsx::createStyle(fontColour = "#000000", halign = "CENTER", borderStyle = "medium", border = "TopBottomLeftRight")

  # Properties
  FStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#33FF00")
  TFStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#008B00")
  IStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FCD400")
  DStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF6347")
  TDStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#CD0000")
  NCStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#cecece")



  # Beggining the xlsx sequence ---------------------------------------------

  ## Metadata

  ## Creating worksheet
  openxlsx::addWorksheet(wb, "Donn\u00e9es structurelles")

  metadata <- IDEAdata$data$metadata

  data.table::setDT(metadata)

  metadata <- metadata[, .(MTD_02 = round(MTD_02, 1), MTD_15 = round(MTD_15, 1), MTD_03 = round(MTD_03, 1), MTD_08 = round(MTD_08, 0), MTD_09 = round(MTD_09, 0), MTD_10 = round(MTD_10, 0))]

  metadata <- data.table::melt(metadata, id.vars = NULL, measure.vars = names(metadata)) |>
    merge(reference_list$metadata, by.x = "variable", by.y = "metadata_code")

  metadata <- metadata[, .(Variable = metadata_name, value = value)][, .(value = as.numeric(value)), by = Variable]

  num_data <- metadata[, .(
    "Percentile 5" = stats::quantile(value, 0.05, na.rm = TRUE),
    "Quartile 1" = stats::quantile(value, 0.25, na.rm = TRUE),
    "Moyenne" = round(mean(value, na.rm = TRUE)),
    "Mediane" = stats::median(value, na.rm = TRUE),
    "Quartile 3" = stats::quantile(value, 0.75, na.rm = TRUE),
    "Percentile 95" = stats::quantile(value, 0.95, na.rm = TRUE)
  ), by = Variable] |>
    transform(Variable = c("SAU (ha)", "UTH", "Capital d'exploitation (\u20ac)", "EBE (\u20ac)", "R\u00e9sultat courant (\u20ac)", "Part des PP dans la SAU (%)"))

  metadata <- IDEAdata$data$metadata

  unique_age <- unique(metadata$MTD_05)
  unique_atelier <- data.frame(MTD = unique(metadata$MTD_12)) |>
    transform(MTD = ifelse(MTD == "1", yes = "oui", no = "non")) |>
    unlist(use.names = FALSE)
  unique_elevage <- data.table::data.table(MTD = metadata$MTD_14)[, MTD := data.table::fcase(
    MTD == 0, "Pas d'\u00e9levage",
    MTD == 1, "Monogastrique",
    TRUE, "Herbivore"
  )][, .(MTD)][, unique(MTD)]

  possible_age <- data.table::data.table(
    metadata_name = "Tranche d'\u00e2ge du chef d'exploitation",
    value = c("-25", "26-35", "36-45", "46-55", "56-65", "65+"),
    n = 0
  ) |>
    subset(!value %in% unique_age)

  possible_atelier <- data.table::data.table(
    metadata_name = "Atelier hors sol",
    value = c("oui", "non"),
    n = 0
  ) |>
    subset(!value %in% unique_atelier)

  possible_elevage <- data.table::data.table(
    metadata_name = "Type d'\u00e9levage",
    value = c("Pas d'\u00e9levage", "Monogastrique", "Herbivore"),
    n = 0
  ) |>
    subset(!value %in% unique_elevage)


  metadata_selected <- data.table::as.data.table(metadata)[, .(MTD_05, MTD_06, MTD_11, MTD_12, MTD_13, MTD_14)]

  metadata_selected[, MTD_12 := ifelse(MTD_12 == "1", yes = "oui", no = "non")]

  metadata_selected[, MTD_14 := data.table::fcase(
    MTD_14 == 0, "Pas d'\u00e9levage",
    MTD_14 == 1, "Monogastrique",
    TRUE, "Herbivore"
  )]

  reference_list$metadata <- data.table::as.data.table(reference_list$metadata)

  metadata_long <- data.table::melt(metadata_selected, measure.vars = names(metadata_selected)) |>
    merge(reference_list$metadata[, .(metadata_code, metadata_name)], by.x = "variable", by.y = "metadata_code")

  metadata_long <- metadata_long[, .(metadata_name, value)][, metadata_name := ifelse(grepl(x = metadata_name, pattern = "Atelier"), yes = "Atelier hors sol", no = metadata_name)]

  metadata_count <- metadata_long[, .(n = .N), by = .(metadata_name, value)]

  metadata_count <- data.table::rbindlist(list(metadata_count, possible_age, possible_elevage, possible_atelier))

  quant_data <- metadata_count[, c("Nom", "Modalite", "Nombre d'exploitations") := .(metadata_name, value, n)][, .(Nom, Modalite, "Nombre d'exploitations" = n)][order(Nom), ]

  # Write the disclaimer
  title <- data.frame(NA)
  names(title) <- "NB : Analyse reposant sur un \u00e9chantillon d'une base de donn\u00e9es non repr\u00e9sentative des exploitations agricoles fran\u00e7aises"
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 1, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )


  # Write the title
  title <- data.frame(NA)
  names(title) <- paste0("Caract\u00e9ristiques num\u00e9riques (N = ", nrow(metadata), ")")
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 3, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write data
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", num_data,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 4, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write the title
  title <- data.frame(NA)
  names(title) <- paste0("Caract\u00e9ristiques cat\u00e9gorielles (N = ", nrow(metadata), ")")
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 12, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write data
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", quant_data,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 13, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Col widths
  openxlsx::setColWidths(wb, "Donn\u00e9es structurelles", cols = c(2:15), widths = "auto")


  ## Dimensions

  ## Creating worksheet
  openxlsx::addWorksheet(wb, "Dimensions")

  ## Creating dataset
  df_dim <- unique(data.table::setDT(IDEAdata$data$dataset)[, .(farm_id, dimension_code, dimension_value = as.numeric(dimension_value))]) |>
    merge(unique(reference_list$indic_dim[, .(dimension_code, dimension)]), by = "dimension_code") |>
    transform(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique")))

  ## Individual stats
  stat_indiv <- df_dim[, dimension_code := NULL][, data.table::dcast(.SD, farm_id ~ dimension, value.var = "dimension_value")]

  data.table::setnames(stat_indiv, "farm_id", "Exploitation")

  ## Group stats
  group_stat <- df_dim[, .(
    "Minimum" = min(dimension_value),
    "1er quartile" = stats::quantile(dimension_value, 0.25, na.rm = TRUE),
    "Moyenne" = round(mean(dimension_value, na.rm = TRUE)),
    "Mediane" = stats::median(dimension_value, na.rm = TRUE),
    "3e quartile" = stats::quantile(dimension_value, 0.75, na.rm = TRUE),
    "Maximum" = max(dimension_value)
  ), by = dimension][, data.table::melt(.SD, id.vars = "dimension")][, data.table::dcast(.SD, variable ~ dimension, value.var = "value")]

  data.table::setnames(group_stat, "variable", "Statistique")

  # Write the title
  title <- data.frame(NA)
  names(title) <- paste0("Resultats individuels du groupe (N = ", nrow(stat_indiv), ")")
  openxlsx::writeData(wb, "Dimensions", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 2, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write individual stats
  openxlsx::writeData(wb, "Dimensions", stat_indiv,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 3, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write title for groups
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", nrow(stat_indiv), ")")
  openxlsx::writeData(wb, "Dimensions", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = nrow(stat_indiv) + 5, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  # Write group stats
  openxlsx::writeData(wb, "Dimensions", group_stat,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Add dimension boxplot
  openxlsx::insertImage(wb, "Dimensions", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_dimensions.png"), nrow(stat_indiv) + 15, startCol = "A", width = 14.87, height = 12.92, units = "cm")

  ## Add dimension histogram
  openxlsx::insertImage(wb, "Dimensions", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Histogramme_dimensions.png"), startRow = 3, startCol = "F", width = 8.5, height = 5.05, units = "in")

  ## Format color according to dimensions
  openxlsx::conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Agro\u00e9cologique", style = AEStyle)
  openxlsx::conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-", style = STStyle)
  openxlsx::conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique", style = ECStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Dimensions", cols = c(1:3), widths = "auto")
  openxlsx::setColWidths(wb, "Dimensions", cols = 4, widths = 12)


  # Components -------------------------------------------------------------

  ## Create worksheet
  openxlsx::addWorksheet(wb, "Composantes")

  ## Create dataset
  df_compo <- unique(data.table::setDT(IDEAdata$data$dataset)[, .(farm_id, dimension_code, component_code, component_value = as.numeric(component_value))]) |>
    merge(unique(reference_list$indic_dim[, .(dimension_code, dimension, component_code, component, component_max)]), by = c("component_code", "dimension_code")) |>
    transform(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique")))

  ## Individual stats
  stat_indiv <- df_compo[, .(farm_id, component, component_value)][, component := ifelse(component == "Assurer des conditions favorables \u00e0 la production \u00e0 moyen et long terme",
    yes = "Assurer des conditions favorables \u00e0 la production\n \u00e0 moyen et long terme", no = component
  )][, component := ifelse(component == "Bouclage de flux \nde mati\u00e8res et d'\u00e9nergie \npar une recherche d'autonomie",
    yes = "Bouclage de flux de mati\u00e8res et d'\u00e9nergie \npar une recherche d'autonomie", no = component
  )][, component := factor(component, levels = unique(component))][, data.table::dcast(.SD, farm_id ~ component, value.var = "component_value")]

  ## Group stats
  group_stat1 <- df_compo[, .(
    "Minimum" = min(component_value),
    "1er quartile" = quantile(component_value, 0.25),
    "Mediane" = quantile(component_value, 0.5),
    "Moyenne" = round(mean(component_value)),
    "3e quartile" = quantile(component_value, 0.75),
    "Maximum" = max(component_value),
    "Maximum theorique" = unique(component_max)
  ), by = component_code][, data.table::melt(.SD, id.vars = "component_code")] |>
    merge(unique(reference_list$indic_dim[, component_code, component]), by = "component_code") |>
    transform(component = factor(component, levels = unique(component)))

  group_stat <- group_stat1[, component_code := NULL][, data.table::dcast(.SD, variable ~ component, value.var = "value")]

  data.table::setnames(group_stat, "variable", "Statistique")

  # Write individual title
  title <- data.frame(NA)
  names(title) <- paste0("Resultats individuels du groupe (N = ", nrow(stat_indiv), ")")
  openxlsx::writeData(wb, "Composantes", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 2, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )


  # Write individual stats header
  openxlsx::writeData(wb, "Composantes", stat_indiv[, 1],
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 3, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write individual stats dim A
  openxlsx::writeData(wb, "Composantes", stat_indiv[, 2:6],
    colNames = TRUE, rowNames = FALSE, startCol = "B",
    startRow = 3, borders = "all", headerStyle = header_ae, borderStyle = "medium"
  )

  # Write individual stats dim B
  openxlsx::writeData(wb, "Composantes", stat_indiv[, 7:10],
    colNames = TRUE, rowNames = FALSE, startCol = "G",
    startRow = 3, borders = "all", headerStyle = header_st, borderStyle = "medium"
  )

  # Write individual stats dim C
  openxlsx::writeData(wb, "Composantes", stat_indiv[, 11:14],
    colNames = TRUE, rowNames = FALSE, startCol = "K",
    startRow = 3, borders = "all", headerStyle = header_ec, borderStyle = "medium"
  )

  # Write group title
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", nrow(stat_indiv), ")")
  openxlsx::writeData(wb, "Composantes", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = nrow(stat_indiv) + 5, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  # Write group stats header
  openxlsx::writeData(wb, "Composantes", group_stat[, 1],
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write group stats dim A
  openxlsx::writeData(wb, "Composantes", group_stat[, 2:6],
    colNames = TRUE, rowNames = FALSE, startCol = "B",
    startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = header_ae, borderStyle = "medium"
  )

  # Write group stats dim B
  openxlsx::writeData(wb, "Composantes", group_stat[, 7:10],
    colNames = TRUE, rowNames = FALSE, startCol = "G",
    startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = header_st, borderStyle = "medium"
  )

  # Write group stats dim C
  openxlsx::writeData(wb, "Composantes", group_stat[, 11:14],
    colNames = TRUE, rowNames = FALSE, startCol = "K",
    startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = header_ec, borderStyle = "medium"
  )

  ## Add component boxplot
  openxlsx::insertImage(wb, "Composantes", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_composantes.png"), startRow = nrow(stat_indiv) + 8 + nrow(group_stat), startCol = "B", width = 20.44, height = 16.17, units = "cm")

  ## Col widths
  openxlsx::setColWidths(wb, "Composantes", cols = c(1:15), widths = 40)


  # Indicators -------------------------------------------------------------

  ## Add worksheet
  openxlsx::addWorksheet(wb, "Indicateurs")

  ## Create dataset
  df_indic <- merge(
    unique(IDEAdata$data$dataset[, .(farm_id, dimension_code, indic, scaled_value)]),
    reference_list$indic_dim,
    by.x = c("indic", "dimension_code"), by.y = c("indic_code", "dimension_code")
  )[, dimension := factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique"))][, indic_number := as.numeric(regmatches(indic, regexpr("[[:digit:]]+", indic)))][order(dimension, indic_number), ][, full_name := factor(indic_name, levels = unique(indic_name))]

  ## Individual stats
  stat_indiv <- df_indic[, .(dimension, farm_id, indic, indic_number, full_name, scaled_value)][, data.table::dcast(.SD, dimension + indic + indic_number + full_name ~ farm_id, value.var = "scaled_value")][order(dimension, indic_number)][, indic_number := NULL]

  data.table::setnames(stat_indiv, c("dimension", "indic", "full_name"), c("Dimension", "Code", "Indicateur"))

  ## Group stats

  group_stat <- df_indic[, .(
    "Minimum" = min(scaled_value),
    "1er quartile" = quantile(scaled_value, 0.25),
    "Mediane" = quantile(scaled_value, 0.5),
    "Moyenne" = round(mean(scaled_value)),
    "3e quartile" = quantile(scaled_value, 0.75),
    "Maximum" = max(scaled_value),
    "Maximum theorique" = unique(max_indic)
  ), by = c("dimension", "indic", "full_name")]

  data.table::setnames(group_stat, c("dimension", "indic", "full_name"), c("Dimension", "Code", "Indicateur"))

  ## Individual title
  title <- data.frame(NA)
  names(title) <- paste0("Resultats individuels du groupe (N = ", ncol(stat_indiv) - 2, ")")
  openxlsx::writeData(wb, "Indicateurs", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 2, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write individual stats
  openxlsx::writeData(wb, "Indicateurs", stat_indiv,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 3, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write group stats title
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", ncol(stat_indiv) - 2, ")")
  openxlsx::writeData(wb, "Indicateurs", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = nrow(stat_indiv) + 5, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write group stats
  openxlsx::writeData(wb, "Indicateurs", group_stat,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to dimension
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Agro\u00e9cologique", style = AEStyle)
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-", style = STStyle)
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique", style = ECStyle)
  openxlsx::setColWidths(wb, "Indicateurs", cols = c(1:10), widths = "auto")

  ## Add boxplots for each dimension
  openxlsx::insertImage(wb, "Indicateurs", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_indicateurs_agroecologiques.png"), startRow = nrow(stat_indiv) + 8 + nrow(group_stat), startCol = "A", width = 21.06, height = 22.12, units = "cm")
  openxlsx::insertImage(wb, "Indicateurs", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_indicateurs_socio_territoriaux.png"), startRow = nrow(stat_indiv) + 8 + nrow(group_stat), startCol = "C", width = 18.94, height = 22.29, units = "cm")
  openxlsx::insertImage(wb, "Indicateurs", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_indicateurs_economiques.png"), startRow = 160, startCol = "B", width = 24.08, height = 22.65, units = "cm")


  # Properties --------------------------------------------------------------

  # Custom function to attribute a code to each node
  replace_indic <- function(indic) {
    list_indic <- reference_list$indic_dim$indic_code

    res <- data.table::fcase(
      indic %in% list_indic, indic,
      indic == "Diversit\u00e9 de l'organisation spatiale et temporelle", "R1",
      indic == "Limiter l'exposition aux al\u00e9as", "R2",
      indic == "Diversit\u00e9 des activit\u00e9s", "R3",
      indic == "En favorisant la diversit\u00e9", "R4",
      indic == "De l'outil de production", "R5",
      indic == "En d\u00e9veloppant l'inertie et les capacit\u00e9s tampon", "R6",
      indic == "R\u00e9duire la sensibilit\u00e9", "R7",
      indic == "par l'insertion dans les r\u00e9seaux", "R8",
      indic == "Augmenter la capacit\u00e9 d'adaptation", "R9",
      indic == "Robustesse", "R10",
      indic == "Naturelles", "CP1",
      indic == "Travail", "CP2",
      indic == "Comp\u00e9tences et \u00e9quipements", "CP3",
      indic == "Sociales et humaines", "CP4",
      indic == "Pr\u00e9server ou cr\u00e9er des ressources pour l'acte de production", "CP5",
      indic == "D\u00e9velopper la capacit\u00e9 alimentaire", "CP6",
      indic == "Capacit\u00e9 \u00e0 produire dans le temps des biens et services remun\u00e9r\u00e9s", "CP7",
      indic == "Capacit\u00e9 de remboursement", "CP8",
      indic == "Capacit\u00e9 \u00e0 d\u00e9gager un revenu dans le temps", "CP9",
      indic == "Capacit\u00e9 productive et reproductive de biens et de services", "CP10",
      indic == "Libert\u00e9 de d\u00e9cision organisationnelle", "AU1",
      indic == "Libert\u00e9 de d\u00e9cision dans les relations commerciales", "AU2",
      indic == "Disposer d'une libert\u00e9 de d\u00e9cision dans ses choix de gouvernance et de production", "AU3",
      indic == "Disposer d'une autonomie financi\u00e8re", "AU4",
      indic == "Autonomie dans le processus productif", "AU5",
      indic == "Autonomie", "AU6",
      indic == "Partage et transparence des activit\u00e9s productives", "RG1",
      indic == "Ouverture et relation au monde non agricole", "RG2",
      indic == "S\u00e9curit\u00e9 alimentaire", "RG3",
      indic == "Implications et engagements sociaux", "RG4",
      indic == "Ressources naturelles", "RG5",
      indic == "Ressources \u00e9nerg\u00e9tiques et manufactur\u00e9es", "RG6",
      indic == "Partager \u00e9quitablement les ressources", "RG7",
      indic == "Conditions de travail de la main d'oeuvre ", "RG8",
      indic == "Conditions de travail de la main d'oeuvre", "RG8",
      indic == "Conditions de vie et de travail", "RG9",
      indic == "Bien \u00eatre de la vie animale", "RG10",
      indic == "Contribuer \u00e0 la qualit\u00e9 de vie sur l'exploitation", "RG11",
      indic == "R\u00e9duire les \u00e9missions", "RG12",
      indic == "R\u00e9duire l'usage des produits polluants", "RG13",
      indic == "R\u00e9duire ses impacts sur la sant\u00e9 et les \u00e9cosyst\u00e8mes", "RG14",
      indic == "Responsabilit\u00e9 globale", "RG15",
      indic == "Valoriser la qualit\u00e9 territoriale", "AN1",
      indic == "Contribuer \u00e0 des d\u00e9marches d'\u00e9conomie circulaire", "AN2",
      indic == "Par le travail et l'emploi", "AN3",
      indic == "S'inscrire dans des d\u00e9marches de territoire", "AN4",
      indic == "Ancrage territorial", "AN5"
    )


    return(res)
  }

  ## Terminal property

  ## Add worksheet
  openxlsx::addWorksheet(wb, "Synth\u00e8se Proprietes")

  # List of indicators
  list_indic <- reference_list$indic_dim$indic_code




  ## Crating dataset
  df <- merge(
    IDEAdata$data$nodes$Global[, data.table::melt(.SD, id.vars = "farm_id")][, .(farm_id, indic = variable, resultat = value)],
    reference_list$properties_nodes,
    by.x = "indic", by.y = "node_code"
  )[, resultat := factor(resultat, levels = c("tr\u00e8s favorable", "favorable", "interm\u00e9diaire", "d\u00e9favorable", "tr\u00e8s d\u00e9favorable", "NC"))][, node_name := ifelse(node_name == "Capacit\u00e9 productive et reproductive de biens et de services", yes = "Capacit\u00e9 productive et \n reproductive de biens et de \n services", no = node_name)][level == "propriete", .(Exploitation = farm_id, node_name, resultat)][, data.table::dcast(.SD, Exploitation ~ node_name, value.var = "resultat")]

  ## Empty row between data and counter
  empty <- df[1, ]
  empty[1, ] <- NA

  ## Creating counters
  to_add <- df[, data.table::melt(.SD, id.vars = "Exploitation")][, .(n = .N), by = c("variable", "value")][, data.table::dcast(.SD, value ~ variable, value.var = "n", fill = 0)][, value := paste0("Nombre de ", value, " :")]

  data.table::setnames(to_add, "value", "Exploitation")

  ## Write both data and counters
  openxlsx::writeData(wb, "Synth\u00e8se Proprietes", rbind(df, empty, to_add),
    startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 2:150, rows = 3:300, type = "contains", rule = "NC", style = NCStyle)

  ## Add properties frequency
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Fr\u00e9quence_propri\u00e9t\u00e9s.png"), startRow = 2, startCol = "H", width = 27.4, height = 12.7, units = "cm")

  ## Col widths
  openxlsx::setColWidths(wb, "Synth\u00e8se Proprietes", cols = 1:6, widths = "auto")
  openxlsx::setColWidths(wb, "Synth\u00e8se Proprietes", cols = 7, widths = 2)

  # Detail properties -------------------------------------------------------

  ## Create worksheet
  openxlsx::addWorksheet(wb, "D\u00e9tail Proprietes")

  ## Initialise the number of rows (depending on the number of farms)
  nrows_tab <- nrow(IDEAdata$data$nodes$Robustesse) + 10

  ## For each property, will write data + counters at the appropriate location
  for (i in names(IDEAdata$data$nodes)[-6]) {
    ## Which iteration are we
    counter <- which(i == names(IDEAdata$data$nodes))

    ## Starting row number
    no_rows <- counter * nrows_tab - (nrows_tab - 1)

    # We need to add + 3 for the first iteration
    if (counter > 1) {
      no_rows <- no_rows + 3
    }

    ## Creating data
    df <- IDEAdata$data$nodes[[i]]

    if ("index" %in% names(IDEAdata$data$nodes[[i]])) {
      df[, index := NULL]
    }

    data.table::setnames(df, "farm_id", "Exploitation")

    # Relocating
    column_order <- names(df)[-length(names(df))]
    data.table::setcolorder(df, c("Exploitation", column_order))


    branches <- merge(
      df[, data.table::melt(.SD, id.vars = "Exploitation")],
      reference_list$properties_nodes,
      by.x = "variable",
      by.y = "node_code"
    )[, .(Exploitation, name = node_name, value)][, data.table::dcast(.SD, Exploitation ~ name, value.var = "value")]

    indics1 <- merge(
      df[, data.table::melt(.SD, id.vars = "Exploitation")][, .(Exploitation, name = variable, value)],
      reference_list$indic_prop,
      by.x = "name",
      by.y = "indic_code"
    )[, .(Exploitation, name = indic_name, value)]


    indics <- unique(indics1)[, data.table::dcast(.SD, Exploitation ~ name, value.var = "value")]

    df <- merge(branches, indics, by = "Exploitation")

    ## Empty row between data an counter
    empty <- df[1, ]
    empty[1, ] <- NA



    ## Creating counter
    to_add <- df[, data.table::melt(.SD, id.vars = "Exploitation")][, .(n = .N), by = c("variable", "value")][, data.table::dcast(.SD, value ~ variable, value.var = "n", fill = 0)][, value := paste0("Nombre de ", value, " :")]

    data.table::setnames(to_add, "value", "Exploitation")

    ## Writing title
    title <- data.frame(NA, NA)
    names(title) <- c(paste0("Propriete : ", i), paste0("(N = ", nrow(df), ")"))
    openxlsx::writeData(wb, "D\u00e9tail Proprietes", title,
      colNames = TRUE, rowNames = FALSE, startCol = "A",
      startRow = no_rows, borders = "all", borderStyle = "medium", headerStyle = bold.style
    )


    ## Writing both data and counter
    openxlsx::writeData(wb, "D\u00e9tail Proprietes", rbind(df, empty, to_add),
      startCol = "A",
      startRow = no_rows + 1, borders = "all", borderStyle = "medium", headerStyle = hs1
    )
  }

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "de NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "D\u00e9tail Proprietes", cols = 1:150, widths = "auto")

  ## Save workbook
  openxlsx::saveWorkbook(wb, file.path(output_dir, output_file), overwrite = TRUE)
}

## Function to produce Excel group reports (anonymous, for reference only)
excel_group_report_reference <- function(IDEAdata, output_dir, outdir, output_file, dpi) {
  # Check if openxlsx installed
  rlang::check_installed("openxlsx", reason = "to make excel reports`")

  # Number of farms
  n_farm <- nrow(IDEAdata$data$metadata)

  # Creating plots
  write_idea(IDEAdata, output_directory = outdir, type = "local", dpi = dpi, quiet = TRUE)

  # Creating workbook
  wb <- openxlsx::createWorkbook()

  ## Setting styles
  hs1 <- openxlsx::createStyle(
    fgFill = "#C0504D", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "white"
  )
  hs2 <- openxlsx::createStyle(
    halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight"
  )

  bold.style <- openxlsx::createStyle(textDecoration = "Bold")

  # Dimensions
  header_ae <- openxlsx::createStyle(
    fgFill = "#2e9c15", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "#000000", wrapText = TRUE
  )

  header_st <- openxlsx::createStyle(
    fgFill = "#5077FE", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "#000000", wrapText = TRUE
  )

  header_ec <- openxlsx::createStyle(
    fgFill = "#FE962B", halign = "CENTER", textDecoration = "Bold",
    border = "TopBottomLeftRight", fontColour = "#000000", wrapText = TRUE
  )

  AEStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#2e9c15", wrapText = TRUE)
  STStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#5077FE", wrapText = TRUE)
  ECStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FE962B", wrapText = TRUE)
  standard <- openxlsx::createStyle(fontColour = "#000000", halign = "CENTER", borderStyle = "medium", border = "TopBottomLeftRight")

  # Properties
  FStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#33FF00")
  TFStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#008B00")
  IStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FCD400")
  DStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF6347")
  TDStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#CD0000")
  NCStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#cecece")



  # Beggining the xlsx sequence ---------------------------------------------

  ## Metadata

  ## Creating worksheet
  openxlsx::addWorksheet(wb, "Donn\u00e9es structurelles")

  metadata <- IDEAdata$data$metadata

  data.table::setDT(metadata)

  metadata <- metadata[, .(MTD_02 = round(MTD_02, 1), MTD_15 = round(MTD_15, 1), MTD_03 = round(MTD_03, 1), MTD_08 = round(MTD_08, 0), MTD_09 = round(MTD_09, 0), MTD_10 = round(MTD_10, 0))]

  metadata <- data.table::melt(metadata, id.vars = NULL, measure.vars = names(metadata)) |>
    merge(reference_list$metadata, by.x = "variable", by.y = "metadata_code")

  metadata <- metadata[, .(Variable = metadata_name, value = value)][, .(value = as.numeric(value)), by = Variable]

  num_data <- metadata[, .(
    "Percentile 5" = stats::quantile(value, 0.05, na.rm = TRUE),
    "Quartile 1" = stats::quantile(value, 0.25, na.rm = TRUE),
    "Moyenne" = round(mean(value, na.rm = TRUE)),
    "Mediane" = stats::median(value, na.rm = TRUE),
    "Quartile 3" = stats::quantile(value, 0.75, na.rm = TRUE),
    "Percentile 95" = stats::quantile(value, 0.95, na.rm = TRUE)
  ), by = Variable] |>
    transform(Variable = c("SAU (ha)", "UTH", "Capital d'exploitation (\u20ac)", "EBE (\u20ac)", "R\u00e9sultat courant (\u20ac)", "Part des PP dans la SAU (%)"))

  metadata <- IDEAdata$data$metadata

  unique_age <- unique(metadata$MTD_05)
  unique_atelier <- data.frame(MTD = unique(metadata$MTD_12)) |>
    transform(MTD = ifelse(MTD == "1", yes = "oui", no = "non")) |>
    unlist(use.names = FALSE)
  unique_elevage <- data.table::data.table(MTD = metadata$MTD_14)[, MTD := data.table::fcase(
    MTD == 0, "Pas d'\u00e9levage",
    MTD == 1, "Monogastrique",
    TRUE, "Herbivore"
  )][, .(MTD)][, unique(MTD)]

  possible_age <- data.table::data.table(
    metadata_name = "Tranche d'\u00e2ge du chef d'exploitation",
    value = c("-25", "26-35", "36-45", "46-55", "56-65", "65+"),
    n = 0
  ) |>
    subset(!value %in% unique_age)

  possible_atelier <- data.table::data.table(
    metadata_name = "Atelier hors sol",
    value = c("oui", "non"),
    n = 0
  ) |>
    subset(!value %in% unique_atelier)

  possible_elevage <- data.table::data.table(
    metadata_name = "Type d'\u00e9levage",
    value = c("Pas d'\u00e9levage", "Monogastrique", "Herbivore"),
    n = 0
  ) |>
    subset(!value %in% unique_elevage)


  metadata_selected <- data.table::as.data.table(metadata)[, .(MTD_05, MTD_06, MTD_11, MTD_12, MTD_13, MTD_14)]

  metadata_selected[, MTD_12 := ifelse(MTD_12 == "1", yes = "oui", no = "non")]

  metadata_selected[, MTD_14 := data.table::fcase(
    MTD_14 == 0, "Pas d'\u00e9levage",
    MTD_14 == 1, "Monogastrique",
    TRUE, "Herbivore"
  )]

  reference_list$metadata <- data.table::as.data.table(reference_list$metadata)

  metadata_long <- data.table::melt(metadata_selected, measure.vars = names(metadata_selected)) |>
    merge(reference_list$metadata[, .(metadata_code, metadata_name)], by.x = "variable", by.y = "metadata_code")

  metadata_long <- metadata_long[, .(metadata_name, value)][, metadata_name := ifelse(grepl(x = metadata_name, pattern = "Atelier"), yes = "Atelier hors sol", no = metadata_name)]

  metadata_count <- metadata_long[, .(n = .N), by = .(metadata_name, value)]

  metadata_count <- data.table::rbindlist(list(metadata_count, possible_age, possible_elevage, possible_atelier))

  quant_data <- metadata_count[, c("Nom", "Modalite", "Nombre d'exploitations") := .(metadata_name, value, n)][, .(Nom, Modalite, "Nombre d'exploitations" = n)][order(Nom), ]

  # Write the disclaimer
  title <- data.frame(NA)
  names(title) <- "NB : Analyse reposant sur un \u00e9chantillon d'une base de donn\u00e9es non repr\u00e9sentative des exploitations agricoles fran\u00e7aises"
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 1, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )


  # Write the title
  title <- data.frame(NA)
  names(title) <- paste0("Caract\u00e9ristiques num\u00e9riques (N = ", nrow(metadata), ")")
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 3, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write data
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", num_data,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 4, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write the title
  title <- data.frame(NA)
  names(title) <- paste0("Caract\u00e9ristiques cat\u00e9gorielles (N = ", nrow(metadata), ")")
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 12, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write data
  openxlsx::writeData(wb, "Donn\u00e9es structurelles", quant_data,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 13, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Col widths
  openxlsx::setColWidths(wb, "Donn\u00e9es structurelles", cols = c(2:15), widths = "auto")

  ## Dimensions

  ## Creating worksheet
  openxlsx::addWorksheet(wb, "Dimensions")

  ## Creating dataset
  df_dim <- unique(data.table::setDT(IDEAdata$data$dataset)[, .(farm_id, dimension_code, dimension_value = as.numeric(dimension_value))]) |>
    merge(unique(reference_list$indic_dim[, .(dimension_code, dimension)]), by = "dimension_code") |>
    transform(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique")))

  ## Group stats
  group_stat <- df_dim[, .(
    "Minimum" = min(dimension_value),
    "1er quartile" = stats::quantile(dimension_value, 0.25, na.rm = TRUE),
    "Moyenne" = round(mean(dimension_value, na.rm = TRUE)),
    "Mediane" = stats::median(dimension_value, na.rm = TRUE),
    "3e quartile" = stats::quantile(dimension_value, 0.75, na.rm = TRUE),
    "Maximum" = max(dimension_value)
  ), by = dimension][, data.table::melt(.SD, id.vars = "dimension")][, data.table::dcast(.SD, variable ~ dimension, value.var = "value")]

  data.table::setnames(group_stat, "variable", "Statistique")


  # Write title for groups
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", n_farm, ")")
  openxlsx::writeData(wb, "Dimensions", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 1, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  # Write group stats
  openxlsx::writeData(wb, "Dimensions", group_stat,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Add dimension boxplot
  openxlsx::insertImage(wb, "Dimensions", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_dimensions.png"), nrow(group_stat) + 10, startCol = "A", width = 14.87, height = 12.92, units = "cm")

  ## Format color according to dimensions
  openxlsx::conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Agro\u00e9cologique", style = AEStyle)
  openxlsx::conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-", style = STStyle)
  openxlsx::conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique", style = ECStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "Dimensions", cols = c(1:3), widths = "auto")
  openxlsx::setColWidths(wb, "Dimensions", cols = 4, widths = 12)



  # Components -------------------------------------------------------------

  ## Create worksheet
  openxlsx::addWorksheet(wb, "Composantes")

  ## Create dataset
  df_compo <- unique(data.table::setDT(IDEAdata$data$dataset)[, .(farm_id, dimension_code, component_code, component_value = as.numeric(component_value))]) |>
    merge(unique(reference_list$indic_dim[, .(dimension_code, dimension, component_code, component, component_max)]), by = c("component_code", "dimension_code")) |>
    transform(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique")))

  ## Group stats
  group_stat1 <- df_compo[, .(
    "Minimum" = min(component_value),
    "1er quartile" = quantile(component_value, 0.25),
    "Mediane" = quantile(component_value, 0.5),
    "Moyenne" = round(mean(component_value)),
    "3e quartile" = quantile(component_value, 0.75),
    "Maximum" = max(component_value),
    "Maximum theorique" = unique(component_max)
  ), by = component_code][, data.table::melt(.SD, id.vars = "component_code")] |>
    merge(unique(reference_list$indic_dim[, component_code, component]), by = "component_code") |>
    transform(component = factor(component, levels = unique(component)))

  group_stat <- group_stat1[, component_code := NULL][, data.table::dcast(.SD, variable ~ component, value.var = "value")]

  data.table::setnames(group_stat, "variable", "Statistique")

  # Write group title
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", n_farm, ")")
  openxlsx::writeData(wb, "Composantes", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 1, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  # Write group stats header
  openxlsx::writeData(wb, "Composantes", group_stat[, 1],
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  # Write group stats dim A
  openxlsx::writeData(wb, "Composantes", group_stat[, 2:6],
    colNames = TRUE, rowNames = FALSE, startCol = "B",
    startRow = 2, borders = "all", headerStyle = header_ae, borderStyle = "medium"
  )

  # Write group stats dim B
  openxlsx::writeData(wb, "Composantes", group_stat[, 7:10],
    colNames = TRUE, rowNames = FALSE, startCol = "G",
    startRow = 2, borders = "all", headerStyle = header_st, borderStyle = "medium"
  )

  # Write group stats dim C
  openxlsx::writeData(wb, "Composantes", group_stat[, 11:14],
    colNames = TRUE, rowNames = FALSE, startCol = "K",
    startRow = 3, borders = "all", headerStyle = header_ec, borderStyle = "medium"
  )

  ## Add component boxplot
  openxlsx::insertImage(wb, "Composantes", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_composantes.png"), startRow = 8 + nrow(group_stat), startCol = "B", width = 20.44, height = 16.17, units = "cm")

  ## Col widths
  openxlsx::setColWidths(wb, "Composantes", cols = c(1:15), widths = 40)



  # Indicators -------------------------------------------------------------

  ## Add worksheet
  openxlsx::addWorksheet(wb, "Indicateurs")

  ## Create dataset
  df_indic <- merge(
    unique(IDEAdata$data$dataset[, .(farm_id, dimension_code, indic, scaled_value)]),
    reference_list$indic_dim,
    by.x = c("indic", "dimension_code"), by.y = c("indic_code", "dimension_code")
  )[, dimension := factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique"))][, indic_number := as.numeric(regmatches(indic, regexpr("[[:digit:]]+", indic)))][order(dimension, indic_number), ][, full_name := factor(indic_name, levels = unique(indic_name))]

  ## Group stats
  group_stat <- df_indic[, .(
    "Minimum" = min(scaled_value),
    "1er quartile" = quantile(scaled_value, 0.25),
    "Mediane" = quantile(scaled_value, 0.5),
    "Moyenne" = round(mean(scaled_value)),
    "3e quartile" = quantile(scaled_value, 0.75),
    "Maximum" = max(scaled_value),
    "Maximum theorique" = unique(max_indic)
  ), by = c("dimension", "indic", "full_name")]

  data.table::setnames(group_stat, c("dimension", "indic", "full_name"), c("Dimension", "Code", "Indicateur"))



  # Write group stats title
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", n_farm, ")")
  openxlsx::writeData(wb, "Indicateurs", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 1, borders = "all", borderStyle = "medium", headerStyle = bold.style
  )

  # Write group stats
  openxlsx::writeData(wb, "Indicateurs", group_stat,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to dimension
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Agro\u00e9cologique", style = AEStyle)
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-", style = STStyle)
  openxlsx::conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique", style = ECStyle)
  openxlsx::setColWidths(wb, "Indicateurs", cols = c(1:10), widths = "auto")

  ## Add boxplots for each dimension
  openxlsx::insertImage(wb, "Indicateurs", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_indicateurs_agroecologiques.png"), startRow = 8 + nrow(group_stat), startCol = "A", width = 21.06, height = 22.12, units = "cm")
  openxlsx::insertImage(wb, "Indicateurs", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_indicateurs_socio_territoriaux.png"), startRow = 8 + nrow(group_stat), startCol = "C", width = 18.94, height = 22.29, units = "cm")
  openxlsx::insertImage(wb, "Indicateurs", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Distribution_indicateurs_economiques.png"), startRow = 105, startCol = "B", width = 24.08, height = 22.65, units = "cm")


  # Properties --------------------------------------------------------------

  # Custom function to attribute a code to each node
  replace_indic <- function(indic) {
    list_indic <- reference_list$indic_dim$indic_code

    res <- data.table::fcase(
      indic %in% list_indic, indic,
      indic == "Diversit\u00e9 de l'organisation spatiale et temporelle", "R1",
      indic == "Limiter l'exposition aux al\u00e9as", "R2",
      indic == "Diversit\u00e9 des activit\u00e9s", "R3",
      indic == "En favorisant la diversit\u00e9", "R4",
      indic == "De l'outil de production", "R5",
      indic == "En d\u00e9veloppant l'inertie et les capacit\u00e9s tampon", "R6",
      indic == "R\u00e9duire la sensibilit\u00e9", "R7",
      indic == "par l'insertion dans les r\u00e9seaux", "R8",
      indic == "Augmenter la capacit\u00e9 d'adaptation", "R9",
      indic == "Robustesse", "R10",
      indic == "Naturelles", "CP1",
      indic == "Travail", "CP2",
      indic == "Comp\u00e9tences et \u00e9quipements", "CP3",
      indic == "Sociales et humaines", "CP4",
      indic == "Pr\u00e9server ou cr\u00e9er des ressources pour l'acte de production", "CP5",
      indic == "D\u00e9velopper la capacit\u00e9 alimentaire", "CP6",
      indic == "Capacit\u00e9 \u00e0 produire dans le temps des biens et services remun\u00e9r\u00e9s", "CP7",
      indic == "Capacit\u00e9 de remboursement", "CP8",
      indic == "Capacit\u00e9 \u00e0 d\u00e9gager un revenu dans le temps", "CP9",
      indic == "Capacit\u00e9 productive et reproductive de biens et de services", "CP10",
      indic == "Libert\u00e9 de d\u00e9cision organisationnelle", "AU1",
      indic == "Libert\u00e9 de d\u00e9cision dans les relations commerciales", "AU2",
      indic == "Disposer d'une libert\u00e9 de d\u00e9cision dans ses choix de gouvernance et de production", "AU3",
      indic == "Disposer d'une autonomie financi\u00e8re", "AU4",
      indic == "Autonomie dans le processus productif", "AU5",
      indic == "Autonomie", "AU6",
      indic == "Partage et transparence des activit\u00e9s productives", "RG1",
      indic == "Ouverture et relation au monde non agricole", "RG2",
      indic == "S\u00e9curit\u00e9 alimentaire", "RG3",
      indic == "Implications et engagements sociaux", "RG4",
      indic == "Ressources naturelles", "RG5",
      indic == "Ressources \u00e9nerg\u00e9tiques et manufactur\u00e9es", "RG6",
      indic == "Partager \u00e9quitablement les ressources", "RG7",
      indic == "Conditions de travail de la main d'oeuvre ", "RG8",
      indic == "Conditions de travail de la main d'oeuvre", "RG8",
      indic == "Conditions de vie et de travail", "RG9",
      indic == "Bien \u00eatre de la vie animale", "RG10",
      indic == "Contribuer \u00e0 la qualit\u00e9 de vie sur l'exploitation", "RG11",
      indic == "R\u00e9duire les \u00e9missions", "RG12",
      indic == "R\u00e9duire l'usage des produits polluants", "RG13",
      indic == "R\u00e9duire ses impacts sur la sant\u00e9 et les \u00e9cosyst\u00e8mes", "RG14",
      indic == "Responsabilit\u00e9 globale", "RG15",
      indic == "Valoriser la qualit\u00e9 territoriale", "AN1",
      indic == "Contribuer \u00e0 des d\u00e9marches d'\u00e9conomie circulaire", "AN2",
      indic == "Par le travail et l'emploi", "AN3",
      indic == "S'inscrire dans des d\u00e9marches de territoire", "AN4",
      indic == "Ancrage territorial", "AN5"
    )


    return(res)
  }

  ## Terminal property

  ## Add worksheet
  openxlsx::addWorksheet(wb, "Synth\u00e8se Proprietes")

  # List of indicators
  list_indic <- reference_list$indic_dim$indic_code

  ## Crating dataset
  df <- merge(
    IDEAdata$data$nodes$Global[, data.table::melt(.SD, id.vars = "farm_id")][, .(farm_id, indic = variable, resultat = value)],
    reference_list$properties_nodes,
    by.x = "indic", by.y = "node_code"
  )[, resultat := factor(resultat, levels = c("tr\u00e8s favorable", "favorable", "interm\u00e9diaire", "d\u00e9favorable", "tr\u00e8s d\u00e9favorable", "NC"))][, node_name := ifelse(node_name == "Capacit\u00e9 productive et reproductive de biens et de services", yes = "Capacit\u00e9 productive et \n reproductive de biens et de \n services", no = node_name)][level == "propriete", .(Exploitation = farm_id, node_name, resultat)][, data.table::dcast(.SD, Exploitation ~ node_name, value.var = "resultat")]


  ## Empty row between data and counter
  empty <- df[1, ]
  empty[1, ] <- NA


  ## Creating counters
  to_add <- df[, data.table::melt(.SD, id.vars = "Exploitation")][, .(n = .N), by = c("variable", "value")][, data.table::dcast(.SD, value ~ variable, value.var = "n", fill = 0)][, value := paste0("Nombre de ", value, " :")]


  # Write title for groups
  title <- data.frame(NA)
  names(title) <- paste0("Synth\u00e8se du groupe (N = ", n_farm, ")")
  openxlsx::writeData(wb, "Synth\u00e8se Proprietes", title,
    colNames = TRUE, rowNames = FALSE, startCol = "A",
    startRow = 1, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  ## Write both data and counters
  openxlsx::writeData(wb, "Synth\u00e8se Proprietes", to_add,
    startCol = "A",
    startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 1:150, rows = 3:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "Synth\u00e8se Proprietes", cols = 2:150, rows = 3:300, type = "contains", rule = "NC", style = NCStyle)

  ## Add properties frequency
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Fr\u00e9quence_propri\u00e9t\u00e9s.png"), startRow = 2, startCol = "H", width = 27.4, height = 12.7, units = "cm")

  ## Col widths
  openxlsx::setColWidths(wb, "Synth\u00e8se Proprietes", cols = 1:6, widths = "auto")
  openxlsx::setColWidths(wb, "Synth\u00e8se Proprietes", cols = 7, widths = 2)



  # Detail properties -------------------------------------------------------

  ## Create worksheet
  openxlsx::addWorksheet(wb, "D\u00e9tail Proprietes")

  ## Initialise the number of rows (depending on the number of farms)
  nrows_tab <- nrow(IDEAdata$data$nodes$Robustesse) + 8

  ## For each property, will write data + counters at the appropriate location
  for (i in names(IDEAdata$data$nodes)[-6]) {
    ## Which iteration are we
    counter <- which(i == names(IDEAdata$data$nodes))

    ## Starting row number
    no_rows <- counter * nrows_tab - (nrows_tab - 1)

    # We need to add + 3 for the first iteration
    if (counter > 1) {
      no_rows <- no_rows + 3
    }

    ## Creating data
    df <- IDEAdata$data$nodes[[i]]

    if ("index" %in% names(IDEAdata$data$nodes[[i]])) {
      df[, index := NULL]
    }

    data.table::setnames(df, "farm_id", "Exploitation")

    # Relocating
    column_order <- names(df)[-length(names(df))]
    data.table::setcolorder(df, c("Exploitation", column_order))


    branches <- merge(
      df[, data.table::melt(.SD, id.vars = "Exploitation")],
      reference_list$properties_nodes,
      by.x = "variable",
      by.y = "node_code"
    )[, .(Exploitation, name = node_name, value)][, data.table::dcast(.SD, Exploitation ~ name, value.var = "value")]

    indics1 <- merge(
      df[, data.table::melt(.SD, id.vars = "Exploitation")][, .(Exploitation, name = variable, value)],
      reference_list$indic_prop,
      by.x = "name",
      by.y = "indic_code"
    )[, .(Exploitation, name = indic_name, value)]


    indics <- unique(indics1)[, data.table::dcast(.SD, Exploitation ~ name, value.var = "value")]

    df <- merge(branches, indics, by = "Exploitation")

    ## Empty row between data an counter
    empty <- df[1, ]
    empty[1, ] <- NA


    ## Creating counter
    to_add <- df[, data.table::melt(.SD, id.vars = "Exploitation")][, .(n = .N), by = c("variable", "value")][, data.table::dcast(.SD, value ~ variable, value.var = "n", fill = 0)][, value := paste0("Nombre de ", value, " :")]

    data.table::setnames(to_add, "value", "Exploitation")

    ## Writing title
    title <- data.frame(NA, NA)
    names(title) <- c(paste0("Propriete : ", i), paste0("(N = ", nrow(df), ")"))
    openxlsx::writeData(wb, "D\u00e9tail Proprietes", title,
      colNames = TRUE, rowNames = FALSE, startCol = "A",
      startRow = no_rows, borders = "all", borderStyle = "medium", headerStyle = bold.style
    )


    ## Writing both data and counter
    openxlsx::writeData(wb, "D\u00e9tail Proprietes", to_add,
      startCol = "A",
      startRow = no_rows + 1, borders = "all", borderStyle = "medium", headerStyle = hs1
    )
  }

  ## Format color according to evaluation
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable", style = FStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "d\u00e9favorable", style = DStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "interm\u00e9diaire", style = IStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s favorable", style = TFStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "tr\u00e8s d\u00e9favorable", style = TDStyle)
  openxlsx::conditionalFormatting(wb, "D\u00e9tail Proprietes", cols = 1:150, rows = 1:300, type = "contains", rule = "de NC", style = NCStyle)

  ## Col widths
  openxlsx::setColWidths(wb, "D\u00e9tail Proprietes", cols = 1:150, widths = "auto")

  ## Save workbook
  openxlsx::saveWorkbook(wb, file.path(output_dir, output_file), overwrite = TRUE)
}
