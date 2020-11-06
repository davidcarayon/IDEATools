#' Render an Excel spreadsheet of an IDEA diagnosis
#'
#' @param input a system path leading either to a single file or a directory. If the input is a single file, accepted formats are : .xls, .xlsx and .json.
#' @param output_dir the output directory name. Defaults to your working directory.
#' @param silent Should the algorithm be silent ?
#' @param append Should the results be appended to the original data ? This only works with the `.xlsx` extension
#'
#' @return An excel spreadsheet
#' @import rmarkdown
#' @import openxlsx
#' @import stringr
#' @import tidyr
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' MakeExcel(path)
MakeExcel <- function(input, output_dir = getwd(), silent = FALSE, append = TRUE) {

  Encoding(list_max_compo$composante) <- "UTF-8"

  extension <- tools::file_ext(input)

  if(!extension %in% c("xls","json","xlsx")) {
    stop("Invalid file extension. Please use .xls(x) or .json files")
  }


  if(extension == "xlsx") {

    file <- paste0(basename(tools::file_path_sans_ext(input)),"_augmenté",".xlsx")

  } else {

    file <- paste0(basename(tools::file_path_sans_ext(input)),"_résultats",".xlsx")

  }

  cli_h1("Reporting des données IDEA vers un fichier excel (Version 1.1)")

  if(!silent){
    cat_bullet("(1/4) Production des figures IDEA")
  }


  # Creating data
  IDEAdata <- importIDEA(input)
  dim <- dimensionsPlots(IDEAdata)
  trees <- MakeTrees(IDEAdata)
  radars <- radarPlots(IDEAdata)
  polar <- PolarComponent(IDEAdata)
  v <- str_replace_all(names(dim)[1], " ", "_")

  # Creating workbook
  # If the input is a xlsx file, we can import the input file
  # Otherwise we have to create a new workbook object

  if(extension == "xlsx") {

    wb <- loadWorkbook(input)

  } else {

    wb <- createWorkbook()

  }


  ## Setting styles
  hs1 <- createStyle(fgFill = "#C0504D", halign = "CENTER", textDecoration = "Bold",
                     border = "TopBottomLeftRight", fontColour = "white")
  AEStyle <- createStyle(fontColour = "#000000", bgFill = "#2e9c15")
  STStyle <- createStyle(fontColour = "#000000", bgFill = "#5077FE")
  ECStyle <- createStyle(fontColour = "#000000", bgFill = "#FE962B")
  standard <- createStyle(fontColour = "#000000", halign = "CENTER",
                          borderStyle = "medium", border = "TopBottomLeftRight")

  FStyle <- createStyle(fontColour = "#000000", bgFill = "#1CDA53")
  TFStyle <- createStyle(fontColour = "#000000", bgFill = "#0D8A00")
  IStyle <- createStyle(fontColour = "#000000", bgFill = "#FFA300")
  DStyle <- createStyle(fontColour = "#000000", bgFill = "#FF6348")
  TDStyle <- createStyle(fontColour = "#000000", bgFill = "#FF0000")
  NCStyle <- createStyle(fontColour = "#000000", bgFill = "#cecece")


  if(!silent){
    cat_bullet("(2/4) Ecriture des figures IDEA dans un répertoire temporaire")
  }


  ## Graph production
  exportIDEA(dim,"tmp")
  exportIDEA(trees,"tmp")
  exportIDEA(radars,"tmp")
  exportIDEA(polar,"tmp")

  if(!silent){
    cat_bullet("(3/4) Création et remplissage des nouveaux onglets...")
  }



  ## Beginning the openxlsx sequence

  # Dimensions --------------------------------------------------------------
  addWorksheet(wb, "Dimensions",gridLines = FALSE, tabColour = "#cecece")
  addWorksheet(wb, "Composantes",gridLines = FALSE, tabColour = "#cecece")
  addWorksheet(wb, "Indicateurs",gridLines = FALSE, tabColour = "#cecece")


  df <- IDEAdata$dataset %>% select(Dimension = dimension,Score = dimension_value) %>% distinct()

  writeData(wb, "Dimensions", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )


  setColWidths(wb, "Dimensions", cols = 4, widths = 2)
  setColWidths(wb, "Dimensions", cols = 1:3, widths = 26)

  img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Dimensions.png"))
  insertImage(wb,"Dimensions",file = img, startRow = 2, startCol = "E",width = 16.61, height = 10.21, units = "cm")


  ## Composantes

  df <- IDEAdata$dataset %>%
    inner_join(list_max_compo, by = "composante") %>% select(Dimension = dimension,Composante = composante,Score = composante_value, `Max possible` = max_compo) %>% distinct()

  writeData(wb, "Composantes", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Agroécologique",style = AEStyle)
  conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-",style = STStyle)
  conditionalFormatting(wb, "Composantes", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique",style = ECStyle)

  setColWidths(wb, "Composantes", cols = c(1,2,4,5), widths = "auto")
  setColWidths(wb, "Composantes", cols = 3, widths = 60)
  setColWidths(wb, "Composantes", cols = 6, widths = 2)

  img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Composantes.png"))

  insertImage(wb,"Composantes", file = img, startRow = 2, startCol = "G", width = 16.32, height = 12.52, units = "cm")

  img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Composantes_polaires.png"))

  insertImage(wb,"Composantes", file = img, startRow = 17, startCol = "B", width = 21.59, height = 21.59, units = "cm")

  ## Indicateurs

  df <- IDEAdata$dataset %>%
    inner_join(list_max, by = "indicateur") %>% select(Dimension = dimension,Composante = composante,indicateur,nom_indicateur,Score = value, `Max possible` = valeur_max) %>% distinct() %>% unite(Indicateur,c("indicateur","nom_indicateur"), sep = " - ")

  writeData(wb, "Indicateurs", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Agroécologique",style = AEStyle)
  conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Socio-",style = STStyle)
  conditionalFormatting(wb, "Indicateurs", cols = 1:2, rows = 1:300, type = "contains", rule = "Economique",style = ECStyle)


  setColWidths(wb, "Indicateurs", cols = 7, widths = 2)
  setColWidths(wb, "Indicateurs", cols = 4, widths = 75)

  # addStyle(wb, sheet = "Indicateurs", standard, rows = 3:56,
  #          cols = 1:6, gridExpand = TRUE)


  img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Indicateurs Agroécologiques.png"))
  insertImage(wb,"Indicateurs", file = img, startRow = 2, startCol = "H", width = 14.31, height = 16.07, units = "cm")

  img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Indicateurs Socio-Territoriaux.png"))
  insertImage(wb,"Indicateurs", file = img, startRow = 39, startCol = "H", width = 14.31, height = 18.08, units = "cm")

  img <- file.path("tmp",v,"Dimensions",paste0(v,"_","Indicateurs Economiques.png"))
  insertImage(wb,"Indicateurs", file = img, startRow = 57, startCol = "D", width = 14.31, height = 12.09, units = "cm")

  # Propriétés --------------------------------------------------------------

  addWorksheet(wb, "Synthèse Propriétés", gridLines = FALSE, tabColour = "yellow")


  props <- label_nodes %>% filter(level == "propriete") %>% pull(nom_indicateur)

  df <- IDEAdata$nodes$Global %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    full_join(IDEAdata$dataset, by= "indicateur") %>%
    rowwise() %>%
    filter(indicateur %in% props) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    select(Propriété=nom_indicateur, Résultat)

  FStyle <- createStyle(fontColour = "#000000", bgFill = "#1CDA53")
  TFStyle <- createStyle(fontColour = "#000000", bgFill = "#0D8A00")
  IStyle <- createStyle(fontColour = "#000000", bgFill = "#FFA300")
  DStyle <- createStyle(fontColour = "#000000", bgFill = "#FF6348")
  TDStyle <- createStyle(fontColour = "#000000", bgFill = "#FF0000")
  NCStyle <- createStyle(fontColour = "#000000", bgFill = "#cecece")

  to_col <- df$Résultat
  names(to_col) <- df$Propriété


  colorise = function(res){
    case_when(res == "favorable" ~ "#1CDA53",
              res == "défavorable" ~ "#FF6348",
              res == "très favorable" ~ "#0D8A00",
              res == "très défavorable" ~ "#FF0000")
  }


  ## La couleur pourrait ici être conditionnelle
  addWorksheet(wb, "Robustesse", gridLines = FALSE, tabColour = colorise(to_col["Robustesse"]))
  addWorksheet(wb, "Ancrage Territorial", gridLines = FALSE, tabColour = colorise(to_col["Ancrage territorial"]))
  addWorksheet(wb, "Capacité", gridLines = FALSE, tabColour = colorise(to_col["Capacité productive et reproductive de biens et de services"]))
  addWorksheet(wb, "Autonomie", gridLines = FALSE, tabColour = colorise(to_col["Autonomie"]))
  addWorksheet(wb, "Responsabilité globale", gridLines = FALSE, tabColour = colorise(to_col["Responsabilité globale"]))


  ## Synthèse globale

  props <- label_nodes %>% filter(level == "propriete") %>% pull(nom_indicateur)

  df <- IDEAdata$nodes$Global %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    full_join(IDEAdata$dataset, by= "indicateur") %>%
    rowwise() %>%
    filter(indicateur %in% props) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    select(Propriété=nom_indicateur, Résultat)

  writeData(wb, "Synthèse Propriétés", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Synthèse Propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Synthèse Propriétés", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)

  setColWidths(wb, "Synthèse Propriétés", cols = 4, widths = 2)
  setColWidths(wb, "Synthèse Propriétés", cols = 1:3, widths = "auto")


  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "zoom.png")
  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)

  insertImage(wb,"Synthèse Propriétés", file = img, startRow = 2, startCol = "E", width = 22.67, height = 15.26, units = "cm")

  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Global.png")
  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
  insertImage(wb,"Synthèse Propriétés", file = img, startRow = 35, startCol = "E", width = 22.92, height = 15.85, units = "cm")

  ## Robustesse

  df <- IDEAdata$nodes$Robustesse %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
    full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_robustesse), by= "indicateur") %>%
    rowwise() %>%
    mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    arrange(level) %>%
    select(Indicateur=nom_indicateur,Niveau = level, `Score déplafonné` = unscaled_value, Résultat)

  writeData(wb, "Robustesse", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Robustesse", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Robustesse", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)

  setColWidths(wb, "Robustesse", cols = 6, widths = 2)
  setColWidths(wb, "Robustesse", cols = 1:5, widths = "auto")


  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Robustesse.png")
  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)
  insertImage(wb,"Robustesse", file = img, startRow = 2, startCol = "G", width = 18.86, height = 13.49, units = "cm")


  ## Ancrage

  df <- IDEAdata$nodes$Ancrage %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
    full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_ancrage), by= "indicateur") %>%
    rowwise() %>%
    mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    arrange(level) %>%
    select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)

  writeData(wb, "Ancrage Territorial", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Ancrage Territorial", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Ancrage Territorial", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)

  setColWidths(wb, "Ancrage Territorial", cols = 6, widths = 2)
  setColWidths(wb, "Ancrage Territorial", cols = 1:5, widths = "auto")


  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Territorial.png")

  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)

  insertImage(wb,"Ancrage Territorial", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")


  ## Capacité productive

  df <- IDEAdata$nodes$Capacité %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
    full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_capacite), by= "indicateur") %>%
    rowwise() %>%
    mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    arrange(level) %>%
    select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)

  writeData(wb, "Capacité", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Capacité", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Capacité", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)

  setColWidths(wb, "Capacité", cols = 6, widths = 2)
  setColWidths(wb, "Capacité", cols = 1:5, widths = "auto")


  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "services.png")

  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)

  insertImage(wb,"Capacité", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")

  ## Autonomie

  df <- IDEAdata$nodes$Autonomie %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
    full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_autonomie), by= "indicateur") %>%
    rowwise() %>%
    mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    arrange(level) %>%
    select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)

  writeData(wb, "Autonomie", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Autonomie", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Autonomie", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)

  setColWidths(wb, "Autonomie", cols = 6, widths = 2)
  setColWidths(wb, "Autonomie", cols = 1:5, widths = "auto")


  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "Autonomie.png")

  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)

  insertImage(wb,"Autonomie", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")


  ## Responsabilité globale

  df <- IDEAdata$nodes$Responsabilité %>% gather(key = indicateur, value = Résultat,-id_exploit) %>% select(-id_exploit) %>%
    left_join(label_nodes %>% select(code_indicateur, level), by = c("indicateur"="code_indicateur")) %>%
    full_join(IDEAdata$dataset %>% filter(indicateur %in% indicateurs_proprietes$indicateurs_responsabilite), by= "indicateur") %>%
    rowwise() %>%
    mutate(level = ifelse(is.na(level), yes = "Noeud", no = "Indicateur")) %>%
    mutate(nom_indicateur = ifelse(is.na(nom_indicateur), yes = indicateur, paste(indicateur,nom_indicateur, sep = " - "))) %>%
    ungroup() %>%
    arrange(level) %>%
    select(Indicateur=nom_indicateur, Niveau = level,`Score déplafonné` = unscaled_value, Résultat)

  writeData(wb, "Responsabilité globale", df,
            colNames = TRUE, rowNames = TRUE, startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Responsabilité globale", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Responsabilité globale", cols = 4, rows = 1:300, type = "contains", rule = "NC",style = NCStyle)

  setColWidths(wb, "Responsabilité globale", cols = 6, widths = 2)
  setColWidths(wb, "Responsabilité globale", cols = 1:5, widths = "auto")


  end <- list.files(file.path("tmp",v,"Propriétés","Arbres_éclairés"), pattern = "globale.png")

  img <- file.path("tmp",v,"Propriétés","Arbres_éclairés",end)

  insertImage(wb,"Responsabilité globale", file = img, startRow = 2, startCol = "G", width = 18.97, height = 10.64, units = "cm")

  addWorksheet(wb, "Annexe", gridLines = FALSE)

  end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "Robustesse.png")
  img <- file.path("tmp",v,"Propriétés",end)
  insertImage(wb,"Annexe", file = img, startRow = 2, startCol = "B", width = 23.42, height = 11.07, units = "cm")

  end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "Territorial.png")
  img <- file.path("tmp",v,"Propriétés",end)
  insertImage(wb,"Annexe", file = img, startRow = 2, startCol = "M", width = 23.42, height = 11.07, units = "cm")

  end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "services.png")
  img <- file.path("tmp",v,"Propriétés",end)
  insertImage(wb,"Annexe", file = img, startRow = 26, startCol = "B", width = 23.42, height = 11.07, units = "cm")

  end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "Autonomie.png")
  img <- file.path("tmp",v,"Propriétés",end)
  insertImage(wb,"Annexe", file = img, startRow = 26, startCol = "M", width = 23.42, height = 11.07, units = "cm")

  end <- list.files(file.path("tmp",v,"Propriétés"), pattern = "globale.png")
  img <- file.path("tmp",v,"Propriétés",end)
  insertImage(wb,"Annexe", file = img, startRow = 49, startCol = "B", width = 23.42, height = 11.07, units = "cm")

  if(!silent){
    cat_bullet("(3.5/4) Données correctement transférées", bullet = "tick", bullet_col = "green")
  }


  if(!silent){
    cat_bullet("(4/4) Ecriture du fichier excel")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  saveWorkbook(wb, file.path(output_dir,file), overwrite = TRUE)

  unlink("tmp",recursive = TRUE)


  if(!silent){
    cat_bullet(paste0("Le document excel a été exporté à l'adresse '",file.path(output_dir,file)), bullet = "info", bullet_col = "green")
  }

  cli_h1("Fin de l'algorithme")

}
