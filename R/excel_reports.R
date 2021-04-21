######################
#### EXCEL REPORTING #
######################


## Function to produce Excel single reports
excel_report <- function(IDEAdata, output_dir, outdir, output_file, prefix, dpi) {

  # Creating plots
  write_idea(IDEAdata, output_directory = outdir, type = "local", prefix = prefix, dpi = dpi, quiet = TRUE)

  # Creating workbook
  wb <- openxlsx::createWorkbook()

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
  FStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#1CDA53")
  TFStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#0D8A00")
  IStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFA300")
  DStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF6348")
  TDStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF0000")
  NCStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#cecece")

  ## Beginning the openxlsx sequence

  # Dimensions --------------------------------------------------------------

  ## Add worksheets
  openxlsx::addWorksheet(wb, "Dimensions", gridLines = FALSE, tabColour = "#cecece")
  openxlsx::addWorksheet(wb, "Composantes", gridLines = FALSE, tabColour = "#cecece")
  openxlsx::addWorksheet(wb, "Indicateurs", gridLines = FALSE, tabColour = "#cecece")

  ## Dimensions data
  df <- IDEAdata$data$dataset %>%
    dplyr::inner_join(reference_table, by = c("indic" = "indic_code")) %>%
    dplyr::select(Dimension = dimension, Score = dimension_value) %>%
    dplyr::distinct()

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
  df <- IDEAdata$data$dataset %>%
    dplyr::inner_join(reference_table, by = c("indic" = "indic_code")) %>%
    dplyr::select(Dimension = dimension, Composante = component, Score = component_value, `Max possible` = max_compo) %>%
    dplyr::distinct()

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
  df <- IDEAdata$data$dataset %>%
    dplyr::inner_join(reference_table, by = c("indic" = "indic_code")) %>%
    dplyr::select(Dimension = dimension, Composante = component, indicateur = indic, nom_indicateur = indic_name, Score = scaled_value, `Max possible` = max_indic) %>%
    dplyr::distinct() %>%
    tidyr::unite(Indicateur, c("indicateur", "nom_indicateur"), sep = " - ")

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
  props <- reference_table %>%
    dplyr::filter(level == "propriete") %>%
    dplyr::pull(indic_name)

  ## Create the dataset
  df <- IDEAdata$data$nodes$Global %>%
    tidyr::gather(key = indicateur, value = "Resultat") %>%
    dplyr::filter(indicateur %in% props) %>%
    dplyr::select("Propriete" = indicateur, "Resultat")

  # Extract results for coloring
  to_col <- df$"Resultat"
  names(to_col) <- df$"Propriete"


  ## Custom function with suited colors
  colorise <- function(res) {
    dplyr::case_when(
      res == "favorable" ~ "#1CDA53",
      res == "d\u00e9favorable" ~ "#FF6348",
      res == "tr\u00e8s favorable" ~ "#0D8A00",
      res == "tr\u00e8s d\u00e9favorable" ~ "#FF0000"
    )
  }

  ## Add sheets with conditional tab color
  openxlsx::addWorksheet(wb, "Robustesse", gridLines = FALSE, tabColour = colorise(to_col["Robustesse"]))
  openxlsx::addWorksheet(wb, "Ancrage Territorial", gridLines = FALSE, tabColour = colorise(to_col["Ancrage territorial"]))
  openxlsx::addWorksheet(wb, "Capacit\u00e9", gridLines = FALSE, tabColour = colorise(to_col["Capacit\u00e9 productive et reproductive de biens et de services"]))
  openxlsx::addWorksheet(wb, "Autonomie", gridLines = FALSE, tabColour = colorise(to_col["Autonomie"]))
  openxlsx::addWorksheet(wb, "Responsabilit\u00e9 globale", gridLines = FALSE, tabColour = colorise(to_col["Responsabilit\u00e9 globale"]))


  ## Global synthesis

  ## Write the data
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

  ## Add synthetic tree
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Arbre synth\u00e9tique.png"))
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = img, startRow = 2, startCol = "E", width = 22.67, height = 15.26, units = "cm")

  ## Add global tree
  img <- file.path(outdir, Sys.Date(), prefix, "Propri\u00e9t\u00e9s", "Arbres \u00e9clair\u00e9s", paste0(prefix, "_", "Arbre global.png"))
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = img, startRow = 35, startCol = "E", width = 22.92, height = 15.85, units = "cm")

  ## Robustesse

  ## Create the dataset
  df <- IDEAdata$data$nodes$Robustesse %>%
    tidyr::gather(key = indicateur, value = "Resultat") %>%
    dplyr::left_join(reference_table, by = c("indicateur" = "indic_code")) %>%
    dplyr::mutate(level = ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(full_name = ifelse(is.na(full_name), yes = indicateur, no = full_name)) %>%
    dplyr::select(indicateur, Indicateur = full_name, Niveau = level, "Resultat") %>%
    dplyr::left_join(IDEAdata$data$dataset, by = c("indicateur" = "indic")) %>%
    dplyr::select(Indicateur, Niveau, "Score_brut" = unscaled_value, "Resultat")

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
  openxlsx::insertImage(wb, "Robustesse", file = img, startRow = 2, startCol = "G", width = 18.86, height = 13.49, units = "cm")


  ## Ancrage

  ## Create the dataset
  df <- IDEAdata$data$nodes$Ancrage %>%
    tidyr::gather(key = indicateur, value = "Resultat") %>%
    dplyr::left_join(reference_table, by = c("indicateur" = "indic_code")) %>%
    dplyr::mutate(level = ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(full_name = ifelse(is.na(full_name), yes = indicateur, no = full_name)) %>%
    dplyr::select(indicateur, Indicateur = full_name, Niveau = level, "Resultat") %>%
    dplyr::left_join(IDEAdata$data$dataset, by = c("indicateur" = "indic")) %>%
    dplyr::select(Indicateur, Niveau, "Score_brut" = unscaled_value, "Resultat")


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
  openxlsx::insertImage(wb, "Ancrage Territorial", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")


  ## Capacit\u00e9 productive

  ## Create the dataset
  df <- IDEAdata$data$nodes$Capacite %>%
    tidyr::gather(key = indicateur, value = "Resultat") %>%
    dplyr::left_join(reference_table, by = c("indicateur" = "indic_code")) %>%
    dplyr::mutate(level = ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(full_name = ifelse(is.na(full_name), yes = indicateur, no = full_name)) %>%
    dplyr::select(indicateur, Indicateur = full_name, Niveau = level, "Resultat") %>%
    dplyr::left_join(IDEAdata$data$dataset, by = c("indicateur" = "indic")) %>%
    dplyr::select(Indicateur, Niveau, "Score_brut" = unscaled_value, "Resultat")


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
  openxlsx::insertImage(wb, "Capacit\u00e9", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")

  ## Autonomie

  ## Create the dataset
  df <- IDEAdata$data$nodes$Autonomie %>%
    tidyr::gather(key = indicateur, value = "Resultat") %>%
    dplyr::left_join(reference_table, by = c("indicateur" = "indic_code")) %>%
    dplyr::mutate(level = ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(full_name = ifelse(is.na(full_name), yes = indicateur, no = full_name)) %>%
    dplyr::select(indicateur, Indicateur = full_name, Niveau = level, "Resultat") %>%
    dplyr::left_join(IDEAdata$data$dataset, by = c("indicateur" = "indic")) %>%
    dplyr::select(Indicateur, Niveau, "Score_brut" = unscaled_value, "Resultat")


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
  openxlsx::insertImage(wb, "Autonomie", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")


  ## Responsabilit\u00e9 globale

  ## Create the dataset
  df <- IDEAdata$data$nodes$Responsabilite %>%
    tidyr::gather(key = indicateur, value = "Resultat") %>%
    dplyr::left_join(reference_table, by = c("indicateur" = "indic_code")) %>%
    dplyr::mutate(level = ifelse(is.na(max_indic), yes = "Noeud", no = "Indicateur")) %>%
    dplyr::arrange(level) %>%
    dplyr::mutate(full_name = ifelse(is.na(full_name), yes = indicateur, no = full_name)) %>%
    dplyr::select(indicateur, Indicateur = full_name, Niveau = level, "Resultat") %>%
    dplyr::left_join(IDEAdata$data$dataset, by = c("indicateur" = "indic")) %>%
    dplyr::select(Indicateur, Niveau, "Score_brut" = unscaled_value, "Resultat")


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
  openxlsx::insertImage(wb, "Responsabilit\u00e9 globale", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")


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
  FStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#1CDA53")
  TFStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#0D8A00")
  IStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFA300")
  DStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF6348")
  TDStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#FF0000")
  NCStyle <- openxlsx::createStyle(fontColour = "#000000", bgFill = "#cecece")



  # Beggining the xlsx sequence ---------------------------------------------


  ## Dimensions

  ## Creating worksheet
  openxlsx::addWorksheet(wb, "Dimensions")


  ## Creating dataset
  df_dim <- IDEAdata$data$dataset %>%
    dplyr::distinct(farm_id, dimension_code, dimension_value) %>%
    dplyr::inner_join(reference_table, by = "dimension_code") %>%
    dplyr::distinct(farm_id, dimension, dimension_value) %>%
    dplyr::mutate(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique")))


  ## Individual stats
  stat_indiv <- df_dim %>%
    tidyr::spread(key = dimension, value = dimension_value) %>%
    dplyr::rename("Exploitation" = "farm_id")

  ## Group stats
  group_stat <- suppressWarnings(
    df_dim %>%
      dplyr::group_by(dimension) %>%
      dplyr::summarise(
        "Minimum" = min(dimension_value),
        "1er quartile" = quantile(dimension_value, 0.25),
        "Mediane" = quantile(dimension_value, 0.5),
        "Moyenne" = mean(dimension_value),
        "3e quartile" = quantile(dimension_value, 0.75),
        "Maximum" = max(dimension_value)
      ) %>%
      tidyr::gather(key = Statistique, value = value, -dimension) %>%
      tidyr::spread(key = dimension, value = value) %>%
      dplyr::mutate(Statistique = factor(Statistique, levels = c("Minimum", "1er quartile", "Mediane", "Moyenne", "3e quartile", "Maximum"))) %>%
      dplyr::arrange(Statistique)
  )


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
  openxlsx::insertImage(wb, "Dimensions", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Histogramme_dimensions.png"), startRow = 3, startCol = "F", width = 20.97, height = 17.19, units = "cm")

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
  df_compo <- IDEAdata$data$dataset %>%
    dplyr::distinct(farm_id, dimension_code, component_code, component_value) %>%
    dplyr::inner_join(reference_table, by = c("component_code", "dimension_code")) %>%
    dplyr::mutate(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique")))

  ## Individual stats
  stat_indiv <- df_compo %>%
    dplyr::distinct(farm_id, component, component_value, component_code) %>%
    dplyr::mutate(component = ifelse(component == "Assurer des conditions favorables \u00e0 la production \u00e0 moyen et long terme",
                                     yes = "Assurer des conditions favorables \u00e0 la production\n \u00e0 moyen et long terme", no = component
    )) %>%
    dplyr::mutate(component = ifelse(component == "Bouclage de flux \nde mati\u00e8res et d'\u00e9nergie \npar une recherche d'autonomie",
                                     yes = "Bouclage de flux de mati\u00e8res et d'\u00e9nergie \npar une recherche d'autonomie", no = component
    )) %>%
    dplyr::arrange(component_code) %>%
    dplyr::mutate(component = factor(component, levels = unique(component))) %>%
    dplyr::select(-component_code) %>%
    tidyr::spread(key = component, value = component_value)

  ## Group stats
  group_stat <- suppressWarnings(
    df_compo %>%
      dplyr::group_by(component_code) %>%
      dplyr::summarise(
        "Minimum" = min(component_value),
        "1er quartile" = quantile(component_value, 0.25),
        "Mediane" = quantile(component_value, 0.5),
        "Moyenne" = mean(component_value),
        "3e quartile" = quantile(component_value, 0.75),
        "Maximum" = max(component_value),
        "Maximum theorique" = unique(max_compo)
      ) %>%
      tidyr::gather(key = "Statistique", value = value, -component_code) %>%
      dplyr::inner_join(reference_table %>% dplyr::distinct(component_code, component), by = "component_code") %>%
      dplyr::arrange(component_code) %>%
      dplyr::mutate(component = factor(component, levels = unique(component))) %>%
      dplyr::select(-component_code) %>%
      tidyr::spread(key = component, value = value) %>%
      dplyr::mutate(Statistique = factor(Statistique, levels = c("Minimum", "1er quartile", "Mediane", "Moyenne", "3e quartile", "Maximum", "Maximum theorique"))) %>%
      dplyr::arrange(Statistique)
  )

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
  df_indic <- IDEAdata$data$dataset %>%
    dplyr::distinct(farm_id, dimension_code, indic, scaled_value) %>%
    dplyr::inner_join(reference_table, by = c("indic" = "indic_code", "dimension_code")) %>%
    dplyr::mutate(dimension = factor(dimension, levels = c("Agro\u00e9cologique", "Socio-Territoriale", "Economique"))) %>%
    dplyr::arrange(dimension, indic_number) %>%
    dplyr::mutate(full_name = factor(full_name, levels = unique(full_name)))

  ## Individual stats
  stat_indiv <- df_indic %>%
    dplyr::select(dimension, farm_id, full_name, scaled_value) %>%
    tidyr::spread(key = farm_id, value = scaled_value) %>%
    dplyr::rename("Dimension" = "dimension", "Indicateur" = "full_name")

  ## Group stats
  group_stat <- df_indic %>%
    dplyr::group_by(dimension, full_name) %>%
    dplyr::summarise(
      "Minimum" = min(scaled_value),
      "1er quartile" = quantile(scaled_value, 0.25),
      "Mediane" = quantile(scaled_value, 0.5),
      "Moyenne" = mean(scaled_value),
      "3e quartile" = quantile(scaled_value, 0.75),
      "Maximum" = max(scaled_value),
      "Maximum theorique" = unique(max_indic)
    ) %>%
    dplyr::rename("Dimension" = "dimension", "Indicateur" = "full_name")


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
    list_indic <- reference_table %>%
      dplyr::filter(level == "indicateur") %>%
      dplyr::pull(indic_code)

    res <- dplyr::case_when(
      indic %in% list_indic ~ indic,
      indic == "Diversit\u00e9 de l'organisation spatiale et temporelle" ~ "R1",
      indic == "Limiter l'exposition aux al\u00e9as" ~ "R2",
      indic == "Diversit\u00e9 des activit\u00e9s" ~ "R3",
      indic == "En favorisant la diversit\u00e9" ~ "R4",
      indic == "De l'outil de production" ~ "R5",
      indic == "En d\u00e9veloppant l'inertie et les capacit\u00e9s tampon" ~ "R6",
      indic == "R\u00e9duire la sensibilit\u00e9" ~ "R7",
      indic == "par l'insertion dans les r\u00e9seaux" ~ "R8",
      indic == "Augmenter la capacit\u00e9 d'adaptation" ~ "R9",
      indic == "Robustesse" ~ "R10",

      indic == "Naturelles" ~ "CP1",
      indic == "Travail" ~ "CP2",
      indic == "Comp\u00e9tences et \u00e9quipements" ~ "CP3",
      indic == "Sociales et humaines" ~ "CP4",
      indic == "Pr\u00e9server ou cr\u00e9er des ressources pour l'acte de production" ~ "CP5",
      indic == "D\u00e9velopper la capacit\u00e9 alimentaire" ~ "CP6",
      indic == "Capacit\u00e9 \u00e0 produire dans le temps des biens et services remun\u00e9r\u00e9s" ~ "CP7",
      indic == "Capacit\u00e9 de remboursement" ~ "CP8",
      indic == "Capacit\u00e9 \u00e0 d\u00e9gager un revenu dans le temps" ~ "CP9",
      indic == "Capacit\u00e9 productive et reproductive de biens et de services" ~ "CP10",

      indic == "Libert\u00e9 de d\u00e9cision organisationnelle" ~ "AU1",
      indic == "Libert\u00e9 de d\u00e9cision dans les relations commerciales" ~ "AU2",
      indic == "Disposer d'une libert\u00e9 de d\u00e9cision dans ses choix de gouvernance et de production" ~ "AU3",
      indic == "Disposer d'une autonomie financi\u00e8re" ~ "AU4",
      indic == "Autonomie dans le processus productif" ~ "AU5",
      indic == "Autonomie" ~ "AU6",

      indic == "Partage et transparence des activit\u00e9s productives" ~ "RG1",
      indic == "Ouverture et relation au monde non agricole" ~ "RG2",
      indic == "S\u00e9curit\u00e9 alimentaire" ~ "RG3",
      indic == "Implications et engagements sociaux" ~ "RG4",
      indic == "Ressources naturelles" ~ "RG5",
      indic == "Ressources \u00e9nerg\u00e9tiques et manufactur\u00e9es" ~ "RG6",
      indic == "Partager \u00e9quitablement les ressources" ~ "RG7",
      indic == "Conditions de travail de la main d'oeuvre " ~ "RG8",
      indic == "Conditions de travail de la main d'oeuvre" ~ "RG8",
      indic == "Conditions de vie et de travail" ~ "RG9",
      indic == "Bien \u00eatre de la vie animale" ~ "RG10",
      indic == "Contribuer \u00e0 la qualit\u00e9 de vie sur l'exploitation" ~ "RG11",
      indic == "R\u00e9duire les \u00e9missions" ~ "RG12",
      indic == "R\u00e9duire l'usage des produits polluants" ~ "RG13",
      indic == "R\u00e9duire ses impacts sur la sant\u00e9 et les \u00e9cosyst\u00e8mes" ~ "RG14",
      indic == "Responsabilit\u00e9 globale" ~ "RG15",

      indic == "Valoriser la qualit\u00e9 territoriale" ~ "AN1",
      indic == "Contribuer \u00e0 des d\u00e9marches d'\u00e9conomie circulaire" ~ "AN2",
      indic == "Par le travail et l'emploi" ~ "AN3",
      indic == "S'inscrire dans des d\u00e9marches de territoire" ~ "AN4",
      indic == "Ancrage territorial" ~ "AN5"
    )


    return(res)
  }

  ## Terminal property

  ## Add worksheet
  openxlsx::addWorksheet(wb, "Synth\u00e8se Proprietes")

  # List of indicators
  list_indic <- reference_table %>%
    dplyr::filter(level == "indicateur") %>%
    dplyr::pull(indic_code)

  ## Crating dataset
  df <- IDEAdata$data$nodes$Global %>%
    tidyr::gather(key = indic, value = resultat, -farm_id) %>%
    dplyr::mutate(indic = replace_indic(indic)) %>%
    dplyr::inner_join(reference_table, by = c("indic" = "indic_code")) %>%
    dplyr::mutate(resultat = factor(resultat, levels = c("tr\u00e8s favorable", "favorable", "interm\u00e9diaire", "d\u00e9favorable", "tr\u00e8s d\u00e9favorable", "NC"))) %>%
    dplyr::mutate(indic_name = ifelse(indic_name == "Capacit\u00e9 productive et reproductive de biens et de services", yes = "Capacit\u00e9 productive et \n reproductive de biens et de \n services", no = indic_name)) %>%
    dplyr::arrange(dimension_code, indic_number) %>%
    dplyr::mutate(indic = factor(indic, levels = unique(indic))) %>%
    dplyr::mutate(level = dplyr::case_when(
      level == "indicateur" ~ "Indicateur",
      level == "propriete" ~ "Propriete"
    )) %>%
    dplyr::filter(level == "Propriete") %>%
    dplyr::select(Exploitation = farm_id, indic_name, resultat) %>%
    tidyr::spread(key = indic_name, value = resultat)


  ## Empty row between data and counter
  empty <- df %>% dplyr::slice(1)
  empty[1, ] <- NA


  ## Creating counters
  to_add <- df %>%
    tidyr::gather(key = prop, value = value, -Exploitation) %>%
    dplyr::group_by(prop) %>%
    dplyr::count(value) %>%
    tidyr::spread(key = prop, value = n) %>%
    dplyr::rename("Exploitation" = "value") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Exploitation = paste0("Nombre de ", Exploitation, " :")) %>%
    dplyr::ungroup() %>%
    dplyr::summarise_all(as.character) %>%
    dplyr::summarise_all(tidyr::replace_na, replace = 0)


  ## Write both data and counters
  openxlsx::writeData(wb, "Synth\u00e8se Proprietes", df %>% dplyr::bind_rows(empty, to_add),
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

  ## Add properties heatmap
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Matrice_propri\u00e9t\u00e9s.png"), startRow = 2, startCol = "H", width = 21.44, height = 14.06, units = "cm")

  ## Add properties frequency
  openxlsx::insertImage(wb, "Synth\u00e8se Proprietes", file = file.path(outdir, Sys.Date(), paste0("Groupe_", n_farm), "Graphiques", "Fr\u00e9quence_propri\u00e9t\u00e9s.png"), startRow = 31, startCol = "H", width = 25.4, height = 12.7, units = "cm")

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
    df <- IDEAdata$data$nodes[[i]] %>%
      dplyr::rename("Exploitation" = "farm_id") %>%
      dplyr::relocate("Exploitation")

    ## Empty row between data an counter
    empty <- df %>% dplyr::slice(1)
    empty[1, ] <- NA


    ## Creating counter
    to_add <- df %>%
      tidyr::gather(key = prop, value = value, -Exploitation) %>%
      dplyr::group_by(prop) %>%
      dplyr::count(value) %>%
      tidyr::spread(key = prop, value = n) %>%
      dplyr::rename("Exploitation" = "value") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Exploitation = paste0("Nombre de ", Exploitation, " :")) %>%
      dplyr::ungroup() %>%
      dplyr::summarise_all(as.character) %>%
      dplyr::summarise_all(tidyr::replace_na, replace = 0)

    ## Writing title
    title <- data.frame(NA, NA)
    names(title) <- c(paste0("Propriete : ", i), paste0("(N = ", nrow(df), ")"))
    openxlsx::writeData(wb, "D\u00e9tail Proprietes", title,
                        colNames = TRUE, rowNames = FALSE, startCol = "A",
                        startRow = no_rows, borders = "all", borderStyle = "medium", headerStyle = bold.style
    )


    ## Writing both data and counter
    openxlsx::writeData(wb, "D\u00e9tail Proprietes", df %>% bind_rows(empty, to_add),
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
