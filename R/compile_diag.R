#' Compilation de diagnostics IDEA4
#'
#' @param input_dir chemin vers le dossier avec des calculateurs
#' @param perfecto_path chemin vers la base perfecto
#'
#' @return exporte une base de donnees dans le dossier "BDD"
compile_diag <- function(input_dir, perfecto_path) {

  cli::cli_h1("Algorithme d'import de calculateurs dans la base de donn\u00E9es IDEA4")

  cli::cli_h2("D\u00E9tection de nouveaux calculateurs")

  # Localisation et chemin absolu des calculateurs
  calcs <- list.files(input_dir, recursive = TRUE, full.names = TRUE)

  cli::cli_alert_info(paste0("Nombre de calculateurs d\u00E9tect\u00E9s : ", length(calcs)," (",51+length(calcs)," avec Perfecto)"))

  # Import des nouvelles donn\u00E9es --------------------------------------------

  cli::cli_h2("Import des nouveaux calculateurs")

  # On initialise les tableaux de donn\u00E9es vides
  dim_df <- tibble::tibble()
  meta_df <- tibble::tibble()
  item_df <- tibble::tibble()

  # On lance la boucle d'import des calculateurs 1 a 1
  for(i in calcs) {

    number = which(i == calcs)

    filename = i
    codename = basename(tools::file_path_sans_ext(filename))

    ## Try the first pipeline
    test_version <- try(read_idea(filename), silent = TRUE)

    ## If the first pipeline fails, try the old_idea one
    if (any(class(test_version) == "try-error")) {

      # Import IDEATools
      options(warn=-1) # On \u00E9teint les warnings
      IDEA_data <- old_idea(filename)
      options(warn=0) # On les rallume

      # scores indicateurs
      dim_df <- dplyr::bind_rows(dim_df,
                          IDEA_data$dataset %>% dplyr::mutate(id_exploit = codename) %>%
                            dplyr::select(id_exploit, dplyr::everything())
      )


      cli::cli_alert_success(paste0("Calculateur #",number,"/",length(calcs),"(",round(number/length(calcs)*100,1),"%)",", ",filename,"  Import\u00E9 !"))

    } else {

      # Import IDEATools
      options(warn=-1) # On \u00E9teint les warnings
      IDEA_items <- read_idea(filename)
      options(warn=0) # On les rallume

      IDEA_items$items$value = as.numeric(IDEA_items$items$value)

      version_number <- as.numeric(stringr::str_remove_all(IDEA_items$metadata$MTD_00, "\\."))

      if (version_number < 430) {

        items <- IDEA_items$items

        item_wide <- items %>%
          tidyr::spread(key = item, value = value)

        # 429 to 430
        item_wide$A3_1 = max(item_wide$A3_1,-1)
        item_wide$B19_3 = min(item_wide$B19_3,3)
        item_wide$A13_1 = max(item_wide$A13_1,-2)
        item_wide$A14_2 = max(item_wide$A14_2,-1)
        item_wide$A15_4 = item_wide$A15_3
        item_wide$A15_3 = 0
        item_wide$B12_3 = item_wide$B12_2
        item_wide$B12_2 = 0
        item_wide$B15_1 = min(item_wide$B15_1,3)
        item_wide$B16_1 = min(item_wide$B16_1,3)
        item_wide$B16_4 = max(item_wide$B16_4,-3)
        item_wide$B19_3 = min(item_wide$B19_3,3)
        item_wide$B19_4 = min((item_wide$B19_4 + item_wide$B19_5),3)
        item_wide$B19_5 = item_wide$B19_6


        if(item_wide$B20_2 == 6) {
          item_wide$B20_1 = 6
          item_wide$B20_2 = 0
        }

        if (item_wide$B20_2 == 0 & item_wide$B20_1 == 4) {
          item_wide$B20_1 = 4
          item_wide$B20_2 = 0
        }

        if (item_wide$B20_2 == 0 & item_wide$B20_1 == 2) {
          item_wide$B20_1 = 0
          item_wide$B20_2 = 3
        }

        if (item_wide$B20_2 == 0 & item_wide$B20_1 == 0) {
          item_wide$B20_1 = 0
          item_wide$B20_2 = 0
        }

        if (item_wide$C9_1 == 4) {
          item_wide$C9_1 = 3
        }

        item_wide$B20_1 = min(item_wide$B20_1,6)
        item_wide$B20_2 = min(item_wide$B20_2,3)
        item_wide$C4_2 = min(item_wide$C4_2,6)
        item_wide$C9_1 = min(item_wide$C9_1,4)
        item_wide$C9_2 = min(item_wide$C9_2,4)

        # 430 to 432
        item_wide$A8_1 = min((item_wide$A8_1 + item_wide$A8_2),8)
        item_wide$A17_3 = 0
        item_wide$B1_1 = min((item_wide$B1_1+item_wide$B1_3),6)

        item_wide$A17_3 = 0
        item_wide$A17_3 = item_wide$A17_2
        item_wide$A17_2 = ifelse(item_wide$A17_1 == 3, yes = 1, no = 0)
        item_wide$A17_1 = min(item_wide$A17_1, 2)
        item_wide$A17_4 = 0

        old_b16 <- c("2" = item_wide$B16_2, "3" = item_wide$B16_3, "4"=item_wide$B16_4)
        item_wide$B16_2 = unname(old_b16["4"])
        item_wide$B16_3 = unname(old_b16["2"])
        item_wide$B16_4 = unname(old_b16["3"])

        item_wide$A1_1 = min(item_wide$A1_1,5)
        item_wide$B12_1 = min(item_wide$B12_1,3)

        items <- tidyr::gather(item_wide, key = item, value = value) %>%
          dplyr::filter(!item %in% c("A8_2","B1_3","B19_6"))

      }

      if (version_number == 430) {

        items <- IDEA_items$items

        item_wide <- items %>%
          tidyr::spread(key = item, value = value)

        item_wide$A8_1 = min((item_wide$A8_1 + item_wide$A8_2),8)
        item_wide$A17_3 = 0
        item_wide$B1_1 = min((item_wide$B1_1+item_wide$B1_3),6)

        item_wide$A17_3 = 0
        item_wide$A17_3 = item_wide$A17_2
        item_wide$A17_2 = ifelse(item_wide$A17_1 == 3, yes = 1, no = 0)
        item_wide$A17_1 = min(item_wide$A17_1, 2)
        item_wide$A17_4 = 0

        old_b16 <- c("2" = item_wide$B16_2, "3" = item_wide$B16_3, "4"=item_wide$B16_4)
        item_wide$B16_2 = unname(old_b16["4"])
        item_wide$B16_3 = unname(old_b16["2"])
        item_wide$B16_4 = unname(old_b16["3"])

        item_wide$A1_1 = min(item_wide$A1_1,5)
        item_wide$B12_1 = min(item_wide$B12_1,3)

        items <- tidyr::gather(item_wide, key = item, value = value) %>% dplyr::filter(!item %in% c("A8_2","B1_3"))

      }

      if (version_number == 431) {

        items <- IDEA_items$items

        item_wide <- items %>%
          tidyr::spread(key = item, value = value)

        item_wide$A17_3 = item_wide$A17_2
        item_wide$A17_2 = ifelse(item_wide$A17_1 == 3, yes = 1, no = 0)
        item_wide$A17_1 = min(item_wide$A17_1, 2)
        item_wide$A17_4 = 0
        item_wide$A19_2 = max(item_wide$A19_2,-1)

        old_b16 <- c("2" = item_wide$B16_2, "3" = item_wide$B16_3, "4"=item_wide$B16_4)
        item_wide$B16_2 = unname(old_b16["4"])
        item_wide$B16_3 = unname(old_b16["2"])
        item_wide$B16_4 = unname(old_b16["3"])


        item_wide$A1_1 = min(item_wide$A1_1,5)
        item_wide$B12_1 = min(item_wide$B12_1,3)

        items <- tidyr::gather(item_wide, key = item, value = value)

      }

      if (version_number == 432) {

        items <- IDEA_items$items

        item_wide <- items %>%
          tidyr::spread(key = item, value = value)

        item_wide$A1_1 = min(item_wide$A1_1,5)
        item_wide$B12_1 = min(item_wide$B12_1,3)

        items <- tidyr::gather(item_wide, key = item, value = value)

      }

      if (version_number > 432) {

        items <- IDEA_items$items

      }

      IDEA_items$items <- items

      IDEA_data <- compute_idea(IDEA_items)

      item_df <- dplyr::bind_rows(item_df,
                           items %>% dplyr::mutate(id_exploit = codename) %>%
                             dplyr::select(id_exploit, dplyr::everything()))


      dim_df <- dplyr::bind_rows(dim_df,
                          IDEA_data$dataset %>% dplyr::mutate(id_exploit = codename) %>%
                            dplyr::select(id_exploit, dplyr::everything())
      )


      # metadata
      meta_df <- dplyr::bind_rows(meta_df,
                           IDEA_data$metadata %>% dplyr::bind_rows() %>% dplyr::mutate(id_exploit = codename)%>%
                             dplyr::select(id_exploit, dplyr::everything())
      )


      cli::cli_alert_success(paste0("Calculateur #",number,"/",length(calcs),"(",round(number/length(calcs)*100,1),"%)",", ",filename,"  Import\u00E9 !"))

    }

  }

  # Import Perfecto ---------------------------------------------------------

  scores_perfecto <- readr::read_delim(perfecto_path,col_types = readr::cols(
    .default = readr::col_double(),
    id_exploit = readr::col_character()
  ), delim = ";")

  # Construction fichier excel ----------------------------------------------

  ref <- reference_list$indic_dim

  # Onglet 1 : metadata -----------------------------------------------------

  # On n'affiche ici que les m\u00E9tadonn\u00E9es ayant \u00E9t\u00E9 r\u00E9cup\u00E9r\u00E9es automatiquement sur des calculateurs
  # Où un champs MTD existe

  meta_df <- meta_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id_number = as.numeric(stringr::str_split(id_exploit,"-")[[1]][2])) %>%
    dplyr::mutate(orga = stringr::str_split(id_exploit,"-")[[1]][1]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(orga,id_number) %>%
    dplyr::select(-orga,-id_number)

  # Onglet 2 : Scores plafonn\u00E9s indicateurs composantes dimensions --------------------

  indics <- dim_df %>%
    unique() %>%
    dplyr::select(id_exploit, indic, scaled_value) %>%
    tidyr::pivot_wider(names_from = indic, values_from = scaled_value)

  compo <- dim_df %>%
    dplyr::select(id_exploit, component_code, component_value) %>%
    dplyr::inner_join(ref, by = "component_code") %>%
    dplyr::distinct(id_exploit,component,component_value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(component = paste0("Composante ", component)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = component, values_from = component_value)

  dimension <- dim_df %>%
    dplyr::select(id_exploit, dimension_code, dimension_value) %>%
    dplyr::inner_join(ref, by = "dimension_code") %>%
    dplyr::distinct(id_exploit,dimension,dimension_value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dimension = paste0("Dimension ", dimension)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = dimension, values_from = dimension_value)

  onglet_scores <- dplyr::inner_join(indics,compo, by = "id_exploit") %>%
    dplyr::inner_join(dimension, by = "id_exploit") %>%
    dplyr::bind_rows(scores_perfecto) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id_number = as.numeric(stringr::str_split(id_exploit,"-")[[1]][2])) %>%
    dplyr::mutate(orga = stringr::str_split(id_exploit,"-")[[1]][1]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(orga,id_number) %>%
    dplyr::select(-orga,-id_number)


  # Onglet 3 : Scores d\u00E9plafonn\u00E9s -------------------------------------------

  deplafs <- dim_df %>%
    unique() %>%
    dplyr::select(id_exploit, indic, unscaled_value) %>%
    tidyr::pivot_wider(names_from = indic, values_from = unscaled_value) %>%
    dplyr::bind_rows(scores_perfecto) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id_number = as.numeric(stringr::str_split(id_exploit,"-")[[1]][2])) %>%
    dplyr::mutate(orga = stringr::str_split(id_exploit,"-")[[1]][1]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(orga,id_number) %>%
    dplyr::select(-orga,-id_number) %>%
    dplyr::mutate_at(dplyr::vars(A1:C11),round,0)


  # Onglet 4 : Items --------------------------------------------------------

  if(nrow(item_df) == 0) {
    items = tibble::tibble()

  } else {

    items <- item_df %>%
      dplyr::mutate(item_no = readr::parse_number(item),
                    item_name = stringr::str_extract(item,"[a-zA-Z]+")) %>%
      dplyr::arrange(item_name,item_no) %>%
      dplyr::select(-item_name,-item_no) %>%
      tidyr::pivot_wider(names_from = item, values_from = value)%>%
      dplyr::rowwise() %>%
      dplyr::mutate(id_number = as.numeric(stringr::str_split(id_exploit,"-")[[1]][2])) %>%
      dplyr::mutate(orga = stringr::str_split(id_exploit,"-")[[1]][1]) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(orga,id_number) %>%
      dplyr::select(-orga,-id_number)

  }

  # S\u00E9quence openxlsx -------------------------------------------------
  wb <- openxlsx::createWorkbook()

  ## metadata

  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata",meta_df)

  ## Scores

  openxlsx::addWorksheet(wb, "scores_dim")
  openxlsx::writeData(wb, "scores_dim",onglet_scores)

  ## Scores deplaf

  openxlsx::addWorksheet(wb, "scores_deplaf")
  openxlsx::writeData(wb, "scores_deplaf", deplafs)

  ## items

  openxlsx::addWorksheet(wb, "items")
  openxlsx::writeData(wb, "items",items)

  # Export des donn\u00E9es avec incr\u00E9mentation du nombre d'exploitations et de la date
  n_final <- dplyr::n_distinct(onglet_scores$id_exploit)

  cli::cli_alert_success(paste0("Base correctement aliment\u00E9e. Nombre d'exploitations : ",n_final))

  if(!dir.exists("BDD")) {dir.create("BDD")}

  openxlsx::saveWorkbook(wb, file.path("BDD",paste0("BDD_N",n_final,"_MAJ_",Sys.Date(),".xlsx")), TRUE)

  cli::cli_alert_success(paste0("Base export\u00E9e sous le nom '",file.path("BDD",paste0("BDD_N",n_final,"_MAJ_",Sys.Date(),".xlsx","'"))))

  cli::cli_h1("Fin de l'algorithme d'alimentation de la base")



}


