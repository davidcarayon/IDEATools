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
#' @import ggplot2
#' @import cli
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("", package = "IDEATools")
#' MakeGroupExcel(path)
MakeGroupExcel <- function(input,output_dir = getwd(), file = "analyse_de_groupe" , silent = FALSE) {

  Encoding(list_max_compo$composante) <- "UTF-8"

  cli_h1("Reporting de groupe de données IDEA vers un fichier excel (Version 1.1)")

  if(!silent){
    cat_bullet("(1/5) Import des données...")
  }
  IDEAdata <- importIDEA(input)

  data <- IDEAdata$dataset


  if(!silent){
    cat_bullet("(2/5) Production des figures de groupe")
  }
  meta <- metaIDEA(IDEAdata)

  v <- str_replace_all(names(dim)[1], " ", "_")

  # Styles
  hs1 <- createStyle(fgFill = "#C0504D", halign = "CENTER", textDecoration = "Bold",
                     border = "TopBottomLeftRight", fontColour = "white")
  hs2 <- createStyle(halign = "CENTER", textDecoration = "Bold",
                     border = "TopBottomLeftRight")

  bold.style <- createStyle(textDecoration = "Bold")
  header_ae <- createStyle(fgFill = "#2e9c15", halign = "CENTER", textDecoration = "Bold",
                           border = "TopBottomLeftRight", fontColour = "#000000")

  header_st <- createStyle(fgFill = "#5077FE", halign = "CENTER", textDecoration = "Bold",
                           border = "TopBottomLeftRight", fontColour = "#000000")

  header_ec <- createStyle(fgFill = "#FE962B", halign = "CENTER", textDecoration = "Bold",
                           border = "TopBottomLeftRight", fontColour = "#000000")

  AEStyle <- createStyle(fontColour = "#000000", bgFill = "#2e9c15")
  STStyle <- createStyle(fontColour = "#000000", bgFill = "#5077FE")
  ECStyle <- createStyle(fontColour = "#000000", bgFill = "#FE962B")
  standard <- createStyle(fontColour = "#000000", halign = "CENTER", borderStyle = "medium", border = "TopBottomLeftRight")

  FStyle <- createStyle(fontColour = "#000000", bgFill = "#1CDA53")
  TFStyle <- createStyle(fontColour = "#000000", bgFill = "#0D8A00")
  IStyle <- createStyle(fontColour = "#000000", bgFill = "#FFA300")
  DStyle <- createStyle(fontColour = "#000000", bgFill = "#FF6348")
  TDStyle <- createStyle(fontColour = "#000000", bgFill = "#FF0000")
  NCStyle <- createStyle(fontColour = "#000000", bgFill = "#cecece")









  if(!silent){
    cat_bullet("(3/5) Ecriture des figures de groupe dans un répertoire temporaire")
  }

  # Production des graphes IDEA
  exportIDEA(meta,"tmp")

  if(!silent){
    cat_bullet("(4/5) Création et remplissage des nouveaux onglets...")
  }


  wb <- createWorkbook()

  ## Production des nouveaux graphes



  # Dimensions --------------------------------------------------------------

  df_dim <- IDEAdata$dataset %>%
    distinct(id_exploit,dimension,dimension_value)%>%
    mutate(dimension = factor(dimension, levels = c("Agroécologique","Socio-Territoriale","Economique")))

  # Dimensions (groupe)

  addWorksheet(wb,"Dimensions")


  stat_indiv <- df_dim %>% spread(key = dimension, value = dimension_value) %>%
    rename("Exploitation"="id_exploit")

  stat_groupe <- suppressWarnings(df_dim %>% group_by(dimension) %>%
    summarise(
      `Minimum`= min(dimension_value),
      `Premier quartile` = quantile(dimension_value,0.25),
      Médiane = quantile(dimension_value,0.5),
      Moyenne = mean(dimension_value),
      `Troisième quartile` = quantile(dimension_value,0.75),
      `Maximum` = max(dimension_value)) %>%
    gather(key = id_exploit, value = value,-dimension) %>%
    spread(key = dimension, value = value) %>%
    mutate(id_exploit = factor(id_exploit, levels = c("Minimum","Premier quartile","Médiane","Moyenne","Troisième quartile","Maximum"))) %>%
    arrange(id_exploit) %>%
    rename("Statistique"="id_exploit"))


  title <- data.frame(NA)
  names(title) <- paste0("Résultats individuels du groupe (N = ",nrow(stat_indiv),")")

  writeData(wb, "Dimensions",title ,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = 2, borders = "all", borderStyle = "medium",headerStyle = bold.style
  )

  writeData(wb, "Dimensions", stat_indiv,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = 3, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  title <- data.frame(NA)
  names(title) <- paste0("Synthèse du groupe (N = ",nrow(stat_indiv),")")

  writeData(wb, "Dimensions", title,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = nrow(stat_indiv) + 5, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  writeData(wb, "Dimensions", stat_groupe,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  insertImage(wb,"Dimensions", file = file.path("tmp","Distribution_dimensions.png"), nrow(stat_indiv) + 15, startCol = "A",width = 14.87, height = 12.92, units = "cm")

  insertImage(wb,"Dimensions", file = file.path("tmp","Hist_Dimensions.png"), startRow = 3, startCol = "F",width = 20.97, height = 17.19, units = "cm")


  conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Agroécologique",style = AEStyle)
  conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-",style = STStyle)
  conditionalFormatting(wb, "Dimensions", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique",style = ECStyle)

  setColWidths(wb, "Dimensions", cols = c(1:3), widths = "auto")
  setColWidths(wb, "Dimensions", cols = 4, widths = 12)



  # Composantes -------------------------------------------------------------
  addWorksheet(wb,"Composantes")


  df_compo <- IDEAdata$dataset %>%
    distinct(id_exploit,dimension,composante,composante_value)%>%
    inner_join(list_max_compo, by = "composante") %>%
    mutate(dimension = factor(dimension, levels = c("Agroécologique","Socio-Territoriale","Economique")))

  stat_indiv <- df_compo %>%
    select(id_exploit, composante, composante_value) %>%
    mutate(composante = ifelse(composante == "Assurer des conditions favorables à la production à moyen et long terme",
                               yes = "Assurer des conditions favorables à la production\n à moyen et long terme", no = composante
    )) %>%
    mutate(composante = ifelse(composante == "Bouclage de flux \nde matières et d'énergie \npar une recherche d'autonomie",
                               yes = "Bouclage de flux de matières et d'énergie \npar une recherche d'autonomie", no = composante
    )) %>%
    mutate(composante = factor(composante, levels = glob_levels)) %>%
    spread(key = composante, value = composante_value)


  stat_groupe <- suppressWarnings(df_compo %>% group_by(composante) %>%
    summarise(
      `Minimum`= min(composante_value),
      `Premier quartile` = quantile(composante_value,0.25),
      Médiane = quantile(composante_value,0.5),
      Moyenne = mean(composante_value),
      `Troisième quartile` = quantile(composante_value,0.75),
      `Maximum` = max(composante_value),
      `Maximum théorique` = unique(max_compo)) %>%
    gather(key = id_exploit, value = value,-composante) %>%
    mutate(composante = ifelse(composante == "Assurer des conditions favorables à la production à moyen et long terme",
                               yes = "Assurer des conditions favorables à la production\n à moyen et long terme", no = composante
    )) %>%
    mutate(composante = ifelse(composante == "Bouclage de flux \nde matières et d'énergie \npar une recherche d'autonomie",
                               yes = "Bouclage de flux de matières et d'énergie \npar une recherche d'autonomie", no = composante
    )) %>%
    mutate(composante = factor(composante, levels = glob_levels)) %>%
    spread(key = composante, value = value) %>%
    mutate(id_exploit = factor(id_exploit, levels = c("Minimum","Premier quartile","Médiane","Moyenne","Troisième quartile","Maximum","Maximum théorique"))) %>%
    arrange(id_exploit) %>%
    rename("Statistique"="id_exploit"))


  title <- data.frame(NA)
  names(title) <- paste0("Résultats individuels du groupe (N = ",nrow(stat_indiv),")")

  writeData(wb, "Composantes", title,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = 2, borders = "all", borderStyle = "medium",headerStyle = bold.style
  )


  writeData(wb, "Composantes", stat_indiv[,1],
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = 3, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_indiv[,2:6],
            colNames = TRUE, rowNames = FALSE, startCol = "B",
            startRow = 3, borders = "all", headerStyle = header_ae, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_indiv[,7:10],
            colNames = TRUE, rowNames = FALSE, startCol = "G",
            startRow = 3, borders = "all", headerStyle = header_st, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_indiv[,11:14],
            colNames = TRUE, rowNames = FALSE, startCol = "K",
            startRow = 3, borders = "all", headerStyle = header_ec, borderStyle = "medium"
  )

  title <- data.frame(NA)
  names(title) <- paste0("Synthèse du groupe (N = ",nrow(stat_indiv),")")

  writeData(wb, "Composantes", title,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = nrow(stat_indiv) + 5, borders = "all", headerStyle = bold.style, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_groupe[,1],
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_groupe[,2:6],
            colNames = TRUE, rowNames = FALSE, startCol = "B",
            startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = header_ae, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_groupe[,7:10],
            colNames = TRUE, rowNames = FALSE, startCol = "G",
            startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = header_st, borderStyle = "medium"
  )

  writeData(wb, "Composantes", stat_groupe[,11:14],
            colNames = TRUE, rowNames = FALSE, startCol = "K",
            startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = header_ec, borderStyle = "medium"
  )

  insertImage(wb,"Composantes", file = file.path("tmp","Distribution_composantes.png"), startRow = nrow(stat_indiv) + 8 + nrow(stat_groupe), startCol = "B",width = 20.44, height = 16.17, units = "cm")

  setColWidths(wb, "Composantes", cols = c(1:15), widths = "auto")

  # Indicateurs -------------------------------------------------------------

  addWorksheet(wb,"Indicateurs")

  df_indic <- IDEAdata$dataset %>%
    distinct(id_exploit,dimension,indicateur,value)%>%
    inner_join(list_max, by = "indicateur") %>%
    inner_join(label_nodes %>% select(code_indicateur,nom_complet),by = c("indicateur"="code_indicateur")) %>%
    mutate(dimension = factor(dimension, levels = c("Agroécologique","Socio-Territoriale","Economique"))) %>%
    mutate(indic_no = parse_number(nom_complet)) %>%
    arrange(dimension,indic_no) %>%
    mutate(nom_complet = factor(nom_complet, levels = unique(nom_complet)))

  stat_indiv <- df_indic %>% select(dimension,id_exploit, nom_complet,value) %>%
    spread(key = id_exploit, value = value) %>%
    rename("Dimension"="dimension","Indicateur"="nom_complet")


  stat_groupe <- df_indic %>%
    group_by(dimension, nom_complet) %>%
    summarise(`Minimum`= min(value),
              `Premier quartile` = quantile(value,0.25),
              Médiane = quantile(value,0.5),
              Moyenne = mean(value),
              `Troisième quartile` = quantile(value,0.75),
              `Maximum` = max(value),
              `Maximum théorique` = unique(valeur_max)) %>%
    rename("Dimension"="dimension","Indicateur"="nom_complet")


  title <- data.frame(NA)
  names(title) <- paste0("Résultats individuels du groupe (N = ",ncol(stat_indiv)-2,")")

  writeData(wb, "Indicateurs", title,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = 2, borders = "all", borderStyle = "medium",headerStyle = bold.style
  )


  writeData(wb, "Indicateurs", stat_indiv,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = 3, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  title <- data.frame(NA)
  names(title) <- paste0("Synthèse du groupe (N = ",ncol(stat_indiv)-2,")")

  writeData(wb, "Indicateurs", title,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = nrow(stat_indiv) + 5, borders = "all", borderStyle = "medium",headerStyle = bold.style
  )

  writeData(wb, "Indicateurs", stat_groupe,
            colNames = TRUE, rowNames = FALSE, startCol = "A",
            startRow = nrow(stat_indiv) + 6, borders = "all", headerStyle = hs1, borderStyle = "medium"
  )

  conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Agroécologique",style = AEStyle)
  conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Socio-",style = STStyle)
  conditionalFormatting(wb, "Indicateurs", cols = 1:5, rows = 1:300, type = "contains", rule = "Economique",style = ECStyle)
  setColWidths(wb, "Indicateurs", cols = c(1:10), widths = "auto")


  insertImage(wb,"Indicateurs", file = file.path("tmp","Distribution_indicateurs_agroecologiques.png"), startRow = nrow(stat_indiv) + 8 + nrow(stat_groupe), startCol = "A",width = 21.06, height = 22.12, units = "cm")
  insertImage(wb,"Indicateurs", file = file.path("tmp","Distribution_indicateurs_socio_territoriaux.png"), startRow = nrow(stat_indiv) + 8 + nrow(stat_groupe), startCol = "C",width = 18.94, height = 22.29, units = "cm")
  insertImage(wb,"Indicateurs", file = file.path("tmp","Distribution_indicateurs_economiques.png"), startRow = 160, startCol = "B",width = 24.08, height = 22.65, units = "cm")


  # Propriétés --------------------------------------------------------------

  ## Propriétés terminales
  addWorksheet(wb, "Synthèse propriétés")



  liste_indicateurs <- label_nodes %>% filter(level == "indicateur") %>% pull(code_indicateur)

  df <- IDEAdata$nodes$Global %>%
    gather(key = indicateur, value = resultat, -id_exploit) %>%
    mutate(indicateur = replace_indicateur(indicateur)) %>%
    inner_join(label_nodes, by = c("indicateur" = "code_indicateur")) %>%
    mutate(resultat = factor(resultat, levels = c("très favorable", "favorable", "intermédiaire", "défavorable", "très défavorable", "NC"))) %>%
    mutate(nom_indicateur = ifelse(nom_indicateur == "Capacité productive et reproductive de biens et de services", yes = "Capacité productive et \n reproductive de biens et de \n services", no = nom_indicateur)) %>%
    mutate(num_indic = parse_number(indicateur)) %>%
    arrange(dim, num_indic) %>%
    mutate(indicateur = factor(indicateur, levels = unique(indicateur))) %>%
    mutate(level = case_when(
      level == "indicateur" ~ "Indicateur",
      level == "propriete" ~ "Propriété"
    )) %>%
    filter(level == "Propriété") %>%
    select(Exploitation = id_exploit,nom_indicateur,resultat) %>%
    spread(key = nom_indicateur, value = resultat)


  empty <- df %>% slice(1)
  empty[1,] = NA



  to_add <- df %>%
    gather(key = param, value = value, -Exploitation) %>%
    group_by(param) %>%
    count(value) %>%
    spread(key = param, value = n) %>%
    rename("Exploitation" = "value") %>%
    rowwise() %>%
    mutate(Exploitation = paste0("Nombre de ",Exploitation," :")) %>%
    ungroup() %>%
    summarise_all(as.character) %>%
    summarise_all(replace_na,replace = 0)


  # Propriétés --------------------------------------------------------------


  writeData(wb, "Synthèse propriétés",df %>% bind_rows(empty,to_add),startCol = "A",
            startRow = 2, borders = "all", headerStyle = hs1, borderStyle = "medium")

  conditionalFormatting(wb, "Synthèse propriétés", cols = 1:150, rows = 3:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Synthèse propriétés", cols = 1:150, rows = 3:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Synthèse propriétés", cols = 1:150, rows = 3:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Synthèse propriétés", cols = 1:150, rows = 3:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Synthèse propriétés", cols = 1:150, rows = 3:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Synthèse propriétés", cols = 2:150, rows = 3:300, type = "contains", rule = "NC",style = NCStyle)

  insertImage(wb,"Synthèse propriétés", file = file.path("tmp","Matrice_Propriétés.png"), startRow = 2, startCol = "H",width = 21.44, height = 14.06, units = "cm")

  setColWidths(wb, "Synthèse propriétés", cols = 1:6, widths = "auto")
  setColWidths(wb, "Synthèse propriétés", cols = 7, widths = 2)


  addWorksheet(wb, "Détail propriétés")


  nrows_tab <- nrow(IDEAdata$nodes$Robustesse) + 10

  for (i in names(IDEAdata$nodes)[-6]){

    counter = which(i == names(IDEAdata$nodes))

    no_rows <- counter*nrows_tab - (nrows_tab-1)

    if(counter > 1){no_rows = no_rows +3}

    df <- IDEAdata$nodes[[i]]

    names(df)[1] <- "Exploitation"

    empty <- df %>% slice(1)
    empty[1,] = NA


    to_add <- df %>%
      gather(key = param, value = value, -Exploitation) %>%
      group_by(param) %>%
      count(value) %>%
      spread(key = param, value = n) %>%
      rename("Exploitation" = "value") %>%
      rowwise() %>%
      mutate(Exploitation = paste0("Nombre de ",Exploitation," :")) %>%
      ungroup() %>%
      summarise_all(as.character)%>%
      summarise_all(replace_na,replace = 0)

    title <- data.frame(NA,NA)
    names(title) <- c(paste0("Propriété : ",i),paste0("(N = ",nrow(df),")"))

    writeData(wb, "Détail propriétés", title,
              colNames = TRUE, rowNames = FALSE, startCol = "A",
              startRow = no_rows, borders = "all", borderStyle = "medium",headerStyle = bold.style
    )


    writeData(wb, "Détail propriétés",df %>% bind_rows(empty,to_add), startCol = "A",
              startRow = no_rows+1, borders = "all", borderStyle = "medium", headerStyle = hs1)



  }

  conditionalFormatting(wb, "Détail propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "favorable",style = FStyle)
  conditionalFormatting(wb, "Détail propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "défavorable",style = DStyle)
  conditionalFormatting(wb, "Détail propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "intermédiaire",style = IStyle)
  conditionalFormatting(wb, "Détail propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "très favorable",style = TFStyle)
  conditionalFormatting(wb, "Détail propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "très défavorable",style = TDStyle)
  conditionalFormatting(wb, "Détail propriétés", cols = 1:150, rows = 1:300, type = "contains", rule = "de NC",style = NCStyle)

  setColWidths(wb, "Détail propriétés", cols = 1:150, widths = "auto")


  if(!silent){
    cat_bullet("Données correctement transférées", bullet = "tick", bullet_col = "green")
  }


  if(!silent){
    cat(" (5/5) Ecriture du fichier excel...\n")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }


  out_file <- paste0(file,"_",Sys.Date(),".xlsx")

  saveWorkbook(wb, file.path(output_dir,out_file), overwrite = TRUE)

  unlink("tmp",recursive = TRUE)

  if(!silent){
    cat_bullet(paste0("Le document excel a été exporté à l'adresse '",file.path(output_dir,file)), bullet = "info", bullet_col = "green")
  }

}
