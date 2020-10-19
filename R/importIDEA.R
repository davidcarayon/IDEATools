#' Import IDEA4 data
#'
#' @param input a system path leading either to a single file or a directory. If the input is a single file, accepted formats are : .xls, .xlsx and .json.
#' @param anonymous Boolean. Should the results be anonymised ?
#'
#' @return a named list, containing :
#' \describe{
#'   \item{input.type}{a string which can be "single" or "folder" depending on the input type.}
#'   \item{dataset}{a tibble dataframe with the extracted data}
#'   \item{nodes}{results of the property analysis}
#'   \item{metadata}{metadata extracted from the input}
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange case_when bind_cols mutate_all bind_rows rowwise mutate ungroup select group_by inner_join slice filter pull distinct
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map2_dbl pmap_chr map_dbl map_chr map
#' @importFrom readxl read_excel
#' @importFrom stringi stri_rand_strings
#' @importFrom stringr str_replace_all str_split
#' @importFrom tibble tibble
#' @importFrom tidyr gather nest unnest spread drop_na
#' @importFrom tools file_ext
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' IDEAdata <- importIDEA(path, anonymous = FALSE)
importIDEA <- function(input, anonymous = FALSE) {

  input <- normalizePath(input)

  # Analysis type -----------------------------------------------------------

  import_list <- list()

  import_list$input.type <- ifelse(dir.exists(input),
    yes = "folder",
    no = "single"
  )


  ## Define custom function that imports a single file
  importFromFile <- function(file) {

    filetype <- file_ext(file)

    res_list <- list()

    ## Aggregates items into indicators
    Item2Indic <- function(indicateur, df) {

      df <- df %>% arrange(item)

      items <- df$value %>% as.numeric()

      if (indicateur %in% c("A1", "A5", "A7", "A8", "A14", "A19", "B23", "B2")) {
        if (indicateur == "A1") {
          value <- ifelse(items[2] == 4, yes = 4, no = sum(items))
        }
        if (indicateur == "A5") {
          value <- ifelse(metadata$MTD_15 >= 0.75, yes = 5, no = sum(items))
        }
        if (indicateur == "A7") {
          value <- case_when(
            metadata$MTD_14 == 0 ~ 0,
            metadata$MTD_14 == 2 ~ round(0.7 * items[1] + 0.3 * items[2] + 1e-10),
            metadata$MTD_14 == 1 ~ as.numeric(items[2])
          )
        }
        if (indicateur == "A8") {
          value <- ifelse(metadata$MTD_15 >= 0.95, yes = 8, no = sum(items))
        }
        if (indicateur == "A14") {
          value <- case_when(
            metadata$MTD_16 == 0 ~ 4,
            metadata$MTD_14 == 0 ~ as.numeric(items[1]),
            metadata$MTD_14 != 0 & metadata$MTD_16 != 0 ~ min(as.numeric(items))
          )
        }
        if (indicateur == "A19") {
          value <- ifelse(metadata$MTD_14 == 0, yes = items[1], no = min(as.numeric(items)))
        }
        if (indicateur == "B2") {
          value <- ifelse(metadata$MTD_14 == 0, yes = items[2], no = items[1])
        }
        if (indicateur == "B23") {
          value <- ifelse(metadata$MTD_14 == 0, yes = items[2], no = round(mean(items) + 1e-10))
        }
      } else {
        value <- sum(items)
      }


      return(as.numeric(value))
    }


    if (filetype == "json") {

      ## Read the json file
      res <- fromJSON(file)

      ## Rajoute une exception avec la nouvelle version du calculateur qui exporte en pourcentage
      Version_3 <- str_split(res$metadonnees$MTD_00, "\\.")[[1]][3] %>% as.numeric()


      if(Version_3 >= 5) {
        res$metadonnees$MTD_15 = res$metadonnees$MTD_15 / 100
      }

      ## Extract metadata and wrap them in a 1-line dataframe
      metadata <- res$metadonnees %>%
        bind_cols() %>%
        mutate_all(as.character)

      # Si l'identifiant est vide ou 0, alors code aléatoire
      if (metadata$MTD_01 %in% c("0", NA)) {
        metadata$MTD_01 <- stri_rand_strings(1, 5, "[A-Z]")
      }


      ## Transformation du champs d'export
      if (metadata$MTD_14 == "0 - pas d'élevage") {
        metadata$MTD_14 <- "0"
      }

      if (metadata$MTD_14 == "2 - herbivore") {
        metadata$MTD_14 <- "2"
      }

      if (metadata$MTD_14 == "1 - monogastrique") {
        metadata$MTD_14 <- "1"
      }

      ## Extract the farm id
      id_exploit <- metadata$MTD_01

      # Full pipeline from data -------------------------------------------------
      dataset <- bind_rows(res$items) %>%
        gather(key = item, value = value) %>%
        rowwise() %>%
        mutate(item = str_replace_all(item, "(?<=[:upper:])0", "")) %>% # To convert A01 to A1
        # separating indicator dans items
        mutate(
          indicateur = str_split(item, "_")[[1]][2],
          item = str_split(item, "_")[[1]][3]
        ) %>%
        mutate(item = as.numeric(item)) %>%
        ungroup() %>%
        select(indicateur, item, value) %>%
        group_by(indicateur) %>%
        nest() %>%
        ## Calculating the indicator unscaled, then scaled value
        mutate(unscaled_value = map2_dbl(indicateur, data, Item2Indic)) %>%
        mutate(value = map2_dbl(indicateur, unscaled_value, ScaleIndicator)) %>%
        select(-data) %>%
        ungroup() %>%
        ## Joining to gather info on dimension/composante
        inner_join(categ %>% select(-nom_indicateur), by = "indicateur") %>%
        inner_join(label_nodes, by = c("indicateur" = "code_indicateur")) %>%
        mutate(id_exploit = id_exploit) %>%
        mutate(unscaled_value = round(unscaled_value, 0)) %>%
        ## Converting unscaled value to a qualitative DEXi category
        mutate(categorie_dexi = pmap_chr(list(TD, D, I, F, unscaled_value), Score2Dexi)) %>%
        select(id_exploit, dimension, composante, indicateur, nom_indicateur, unscaled_value, categorie_dexi, value) %>%
        ## Adding indicator values to calculate composantes
        group_by(composante) %>%
        mutate(composante_value = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(composante_value = map2_dbl(composante, composante_value, ScaleComposante)) %>%
        group_by(dimension) %>%
        nest() %>%
        mutate(dimension_value = map_dbl(data, Composante2Dimension)) %>%
        unnest(cols = c(data)) %>%
        ungroup() %>%
        select(id_exploit, dimension, composante, indicateur, nom_indicateur, unscaled_value, categorie_dexi, value, composante_value, dimension_value)

      results_dexi <- dataset %>%
        select(id_exploit, dimension, indicateur, nom_indicateur, unscaled_value, categorie_dexi)

      ## Compile metadata and dataset in the output list
      res_list$metadata <- metadata
      res_list$dataset <- dataset




    } else if (filetype %in% c("xls", "xlsx")) {
      Version_no <- suppressMessages(read_excel(file, sheet = "Notice") %>% clean_names() %>% select(x11)) %>%
        as.data.frame() %>%
        `[`(3, 1)
      Version <- str_split(Version_no, "\\.")[[1]][2] %>% as.numeric()

      ## Check the version number
      if (!is.na(Version) & Version >= 2) {

        ## Mimic the .json input
        BDD <- suppressMessages(read_excel(file, sheet = "Renvoi BDD", skip = 3) %>% clean_names())
        metadata <- BDD %>%
          slice(1:17) %>%
          select(code, valeur) %>%
          spread(key = code, value = valeur) %>%
          mutate_all(as.character)

        metadata$MTD_14 <- suppressMessages(read_excel(file, sheet = "Saisie et Calculateur", skip = 3)) %>%
          clean_names() %>%
          filter(id_exploitation == "Présence et type d'élevage :") %>%
          pull(2) %>%
          as.character()

        if (metadata$MTD_14 == "0 - pas d'élevage" | metadata$MTD_14 == "pas d'élevage") {
          metadata$MTD_14 <- "0"
        }

        if (metadata$MTD_14 == "2 - herbivore" | metadata$MTD_14 == "herbivore") {
          metadata$MTD_14 <- "2"
        }

        if (metadata$MTD_14 == "1 - monogastrique" | metadata$MTD_14 == "monogastrique") {
          metadata$MTD_14 <- "1"
        }


        ## Si la case id_exploit est vide ou 0, alors code aléatoire.
        if (metadata$MTD_01 %in% c("0", NA)) {
          metadata$MTD_01 <- stri_rand_strings(1, 5, "[A-Z]")
        }

        ## Rajoute une exception avec la nouvelle version du calculateur qui exporte en pourcentage
        Version_3 <- str_split(Version_no, "\\.")[[1]][3] %>% as.numeric()

        if(Version_3 >= 5) {
          metadata$MTD_15 = as.character(as.numeric(metadata$MTD_15) / 100)
        }

        items <- BDD %>%
          slice(22:nrow(BDD)) %>%
          select(code, a_exporter) %>%
          spread(key = code, value = a_exporter)

        ## Extract the farm id
        id_exploit <- as.character(metadata$MTD_01)

        # Full pipeline from data -------------------------------------------------
        dataset <- items %>%
          gather(key = item, value = value) %>%
          rowwise() %>%
          mutate(item = str_replace_all(item, "(?<=[:upper:])0", "")) %>% # To convert A01 to A1
          # separating indicator dans items
          mutate(
            indicateur = str_split(item, "_")[[1]][2],
            item = str_split(item, "_")[[1]][3]
          ) %>%
          mutate(item = as.numeric(item)) %>%
          ungroup() %>%
          select(indicateur, item, value) %>%
          group_by(indicateur) %>%
          nest() %>%
          ## Calculating the indicator unscaled, then scaled value
          mutate(unscaled_value = map2_dbl(indicateur, data, Item2Indic)) %>%
          mutate(value = map2_dbl(indicateur, unscaled_value, ScaleIndicator)) %>%
          select(-data) %>%
          ungroup() %>%
          ## Joining to gather info on dimension/composante
          inner_join(categ %>% select(-nom_indicateur), by = "indicateur") %>%
          inner_join(label_nodes, by = c("indicateur" = "code_indicateur")) %>%
          mutate(id_exploit = id_exploit) %>%
          mutate(unscaled_value = round(unscaled_value, 0)) %>%
          ## Converting unscaled value to a qualitative DEXi category
          mutate(categorie_dexi = pmap_chr(list(TD, D, I, F, unscaled_value), Score2Dexi)) %>%
          select(id_exploit, dimension, composante, indicateur, nom_indicateur, unscaled_value, categorie_dexi, value) %>%
          ## Adding indicator values to calculate composantes
          group_by(composante) %>%
          mutate(composante_value = sum(value, na.rm = TRUE)) %>%
          ungroup() %>%
          mutate(composante_value = map2_dbl(composante, composante_value, ScaleComposante)) %>%
          group_by(dimension) %>%
          nest() %>%
          mutate(dimension_value = map_dbl(data, Composante2Dimension)) %>%
          unnest(cols = c(data)) %>%
          ungroup() %>%
          select(id_exploit, dimension, composante, indicateur, nom_indicateur, unscaled_value, categorie_dexi, value, composante_value, dimension_value)

        results_dexi <- dataset %>%
          select(id_exploit, dimension, indicateur, nom_indicateur, unscaled_value, categorie_dexi)

        ## Compile metadata and dataset in the output list
        res_list$metadata <- metadata
        res_list$dataset <- dataset
      } else {

        ## In the other case, it's an old excel file,

        # Reading metadata sheet
        Saisie_et_calc <- suppressMessages(read_excel(file, sheet = "Saisie et Calculateur") %>% clean_names() %>% select(1:6)) %>% as.data.frame()

        ### Defining metadata
        metadata <- tibble(MTD_00 = NA)
        metadata$MTD_01 <- as.character(Saisie_et_calc[4, 2])
        metadata$MTD_02 <- Saisie_et_calc[12, 2]
        metadata$MTD_03 <- Saisie_et_calc[27, 6]
        metadata$MTD_04 <- Saisie_et_calc[27, 2]
        metadata$MTD_05 <- NA
        metadata$MTD_06 <- NA
        metadata$MTD_07 <- NA
        metadata$MTD_08 <- Saisie_et_calc %>%
          filter(i_donnees_generales_et_inventaires_de_lexploitation == "Capital hors foncier: actif net total - valeur des terres (dans immo. corporelles)") %>%
          pull(x6) %>%
          `[`(1)
        metadata$MTD_09 <- Saisie_et_calc %>%
          filter(i_donnees_generales_et_inventaires_de_lexploitation == "EBE retenu IDEA") %>%
          pull(x6) %>%
          `[`(1)
        metadata$MTD_10 <- NA
        metadata$MTD_11 <- as.character(Saisie_et_calc[6, 2])
        metadata$MTD_12 <- NA
        metadata$MTD_13 <- Saisie_et_calc[4, 6] %>%
          as.numeric() %>%
          as.Date(origin = "1900-01-01") %>%
          str_split("-") %>%
          unlist() %>%
          `[`(1) %>%
          as.numeric()
        metadata$MTD_14 <- Saisie_et_calc %>%
          filter(i_donnees_generales_et_inventaires_de_lexploitation == "Présence d'élevage :") %>%
          pull(x2) %>%
          `[`(1)
        metadata$MTD_15 <- NA
        metadata$MTD_16 <- NA

        if(metadata$MTD_14 == "oui") {
          metadata$MTD_14 <- 1
        }

        if(metadata$MTD_14 == "non") {
          metadata$MTD_14 <- 0
        }

        if (metadata$MTD_14 == "0 - pas d'élevage") {
          metadata$MTD_14 <- 0
        }
        if (metadata$MTD_14 == "2 - herbivore") {
          metadata$MTD_14 <- 2
        }
        if (metadata$MTD_14 == "1 - monogastrique") {
          metadata$MTD_14 <- 1
        }

        ### If anonymous is TRUE, or if no ID could be found, then replace the ID by a random 5-letter string
        if (anonymous == TRUE | is.na(metadata$MTD_01) == TRUE) {
          metadata$MTD_01 <- stri_rand_strings(1, 5, "[A-Z]")
        }

        if (metadata$MTD_01 == "0") {
          metadata$MTD_01 <- stri_rand_strings(1, 5, "[A-Z]")
        }

        metadata <- metadata %>% mutate_all(as.character)

        # Add to the result list
        res_list$metadata <- metadata

        ## Agroécologie

        ## Label
        AE_lab <- suppressMessages(read_excel(file, sheet = "Dimension agroécologique") %>% clean_names() %>% select(x2) %>% drop_na())

        ## Valeur
        AE_val <- suppressMessages(read_excel(file, sheet = "Dimension agroécologique") %>% clean_names() %>% select(x8) %>% drop_na() %>% slice(seq(2, 38, 2)))

        AE <- bind_cols(AE_lab, AE_val) %>%
          mutate(dimension = "Durabilité Agroécologique") %>%
          select(dimension, indicateur = x2, score_deplaf = x8)

        ## Socio-territorial

        ## Label
        ST_lab <- suppressMessages(read_excel(file, sheet = "Dimension socio-territoriale") %>% clean_names() %>% select(x2) %>% drop_na())
        ## Valeur
        ST_val <- suppressMessages(read_excel(file, sheet = "Dimension socio-territoriale") %>% clean_names() %>% select(x8) %>% drop_na() %>% slice(seq(2, 46, 2)))

        ST <- bind_cols(ST_lab, ST_val) %>%
          mutate(dimension = "Durabilité Socio-Territoriale") %>%
          select(dimension, indicateur = x2, score_deplaf = x8)

        ## Economique

        ## Label
        EC_lab <- suppressMessages(read_excel(file, sheet = "Dimension économique") %>% clean_names() %>% select(x2) %>% drop_na())
        ## Valeur
        EC_val <- suppressMessages(read_excel(file, sheet = "Dimension économique") %>% clean_names() %>% select(x7) %>% drop_na() %>% slice(seq(2, 22, 2)))

        EC <- bind_cols(EC_lab, EC_val) %>%
          mutate(dimension = "Durabilité Economique") %>%
          select(dimension, indicateur = x2, score_deplaf = x7)

        ## Aggregation
        results <- bind_rows(AE, ST, EC) %>% mutate(unscaled_value = as.numeric(score_deplaf))

        results_dexi <- categ %>%
          mutate(id_exploit = metadata$MTD_01) %>%
          inner_join(results, by = "indicateur") %>%
          mutate(categorie_dexi = pmap_chr(list(TD, D, I, F, unscaled_value), Score2Dexi)) %>%
          select(id_exploit, dimension, indicateur, nom_indicateur, unscaled_value, categorie_dexi)

        ## Adding dimension data

        res_list$dataset <- results_dexi %>%
          select(indicateur, unscaled_value, categorie_dexi) %>%
          inner_join(label_nodes, by = c("indicateur" = "code_indicateur")) %>%
          mutate(id_exploit = metadata$MTD_01) %>%
          mutate(value = map2_dbl(indicateur, unscaled_value, ScaleIndicator)) %>%
          select(id_exploit, dimension, composante, indicateur, nom_indicateur, unscaled_value, categorie_dexi, value) %>%
          ## Adding indicator values to calculate composantes
          group_by(composante) %>%
          mutate(composante_value = sum(value, na.rm = TRUE)) %>%
          ungroup() %>%
          mutate(composante_value = map2_dbl(composante, composante_value, ScaleComposante)) %>%
          group_by(dimension) %>%
          nest() %>%
          mutate(dimension_value = map_dbl(data, Composante2Dimension)) %>%
          unnest(cols = c(data)) %>%
          ungroup() %>%
          select(id_exploit, dimension, composante, indicateur, nom_indicateur, unscaled_value, categorie_dexi, value, composante_value, dimension_value)
      }
    } else {
      stop("Invalid file extension. Please use .xls(x) or .json files")
    }


    # Computing nodes ---------------------------------------------------------

    if (is.na(metadata$MTD_14)) {
      stop("La présence ou non d'élevage n'a pas été renseignée. Merci de remplir cette cellule.")
    }

    if (metadata$MTD_14 == "0" | metadata$MTD_14 == "0 - pas d'élevage") {
      results_dexi <- results_dexi %>%
        rowwise() %>%
        mutate(categorie_dexi = ifelse(indicateur == "A7", yes = "NC", no = categorie_dexi)) %>%
        ungroup()
      res_list$dataset <- res_list$dataset %>%
        rowwise() %>%
        mutate(categorie_dexi = ifelse(indicateur == "A7", yes = "NC", no = categorie_dexi)) %>%
        ungroup()
    }

    prop_data <- results_dexi

    # Robustesse --------------------------------------------------------------

    ## Node 1

    decision_rules <- decision_rules_total$node_1
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_1 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A1", "A3", "A4")) %>%
      ungroup()

    ## Node 2

    decision_rules <- decision_rules_total$node_2
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_2 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_1, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A14", "C5", "Diversité de l'organisation spatiale et temporelle")) %>%
      ungroup()

    ## Node 3

    decision_rules <- decision_rules_total$node_3
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_3 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("C4", "C7")) %>%
      ungroup()

    ## Node 4

    decision_rules <- decision_rules_total$node_4
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_4 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_3, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A2", "Diversité des activités")) %>%
      ungroup()


    ## Node 5

    decision_rules <- decision_rules_total$node_5
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_5 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("C8", "C9")) %>%
      ungroup()

    ## Node 6

    decision_rules <- decision_rules_total$node_6
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_6 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_5, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A15", "De l'outil de production")) %>%
      ungroup()

    ## Node 7

    decision_rules <- decision_rules_total$node_7
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_7 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_4, by = "id_exploit") %>%
      inner_join(node_6, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B22", "En favorisant la diversité", "En développant l'inertie et les capacités tampon")) %>%
      ungroup()

    ## Node 8

    decision_rules <- decision_rules_total$node_8
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_8 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B13", "B15")) %>%
      ungroup()

    ## Node 9

    decision_rules <- decision_rules_total$node_9
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_9 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_8, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B16", "B18", "par l'insertion dans les réseaux")) %>%
      ungroup()

    ## Node 10
    decision_rules <- decision_rules_total$node_10
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_10 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_9, by = "id_exploit") %>%
      inner_join(node_7, by = "id_exploit") %>%
      inner_join(node_2, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Augmenter la capacité d'adaptation", "Réduire la sensibilité", "Limiter l'exposition aux aléas")) %>%
      ungroup()


    # Capacité productive et reproductive de biens et services ----------------

    ## Node 11

    decision_rules <- decision_rules_total$node_11
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_11 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A12", "A13", "A5")) %>%
      ungroup()


    ## Node 12

    decision_rules <- decision_rules_total$node_12
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_12 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B14", "B15", "B16")) %>%
      ungroup()

    ## Node 13

    decision_rules <- decision_rules_total$node_13
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_13 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B13", "B18")) %>%
      ungroup()


    ## Node 14

    decision_rules <- decision_rules_total$node_14
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_14 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_12, by = "id_exploit") %>%
      inner_join(node_13, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Travail", "Compétences et équipements")) %>%
      ungroup()

    ## Node 15

    decision_rules <- decision_rules_total$node_15
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_15 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_11, by = "id_exploit") %>%
      inner_join(node_14, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Naturelles", "Sociales et humaines")) %>%
      ungroup()


    ## Node 16

    decision_rules <- decision_rules_total$node_16
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_16 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B1", "B3")) %>%
      ungroup()

    ## Node 17

    decision_rules <- decision_rules_total$node_17
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_17 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_16, by = "id_exploit") %>%
      inner_join(node_15, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Développer la capacité alimentaire", "Préserver ou créer des ressources pour l'acte de production")) %>%
      ungroup()

    ## Node 18

    decision_rules <- decision_rules_total$node_18
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_18 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("C2", "C3")) %>%
      ungroup()


    ## Node 19

    decision_rules <- decision_rules_total$node_19
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_19 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_18, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("C1", "C10", "Capacité de remboursement")) %>%
      ungroup()

    ## Node 20

    decision_rules <- decision_rules_total$node_20
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_20 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_17, by = "id_exploit") %>%
      inner_join(node_19, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Capacité à produire dans le temps des biens et services remunérés", "Capacité à dégager un revenu dans le temps")) %>%
      ungroup()

    # Autonomie ---------------------------------------------------------------

    ## Node 21

    decision_rules <- decision_rules_total$node_21
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_21 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B13", "B15", "B18")) %>%
      ungroup()

    ## Node 22

    decision_rules <- decision_rules_total$node_22
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_22 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B8", "C5")) %>%
      ungroup()

    ## Node 23

    decision_rules <- decision_rules_total$node_23
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_23 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_21, by = "id_exploit") %>%
      inner_join(node_22, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Liberté de décision organisationnelle", "Liberté de décision dans les relations commerciales")) %>%
      ungroup()

    ## Node 24

    decision_rules <- decision_rules_total$node_24
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_24 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("C3", "C6")) %>%
      ungroup()

    ## Node 25

    decision_rules <- decision_rules_total$node_25
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_25 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A6", "A7", "A8")) %>%
      ungroup()

    ## Node 26

    decision_rules <- decision_rules_total$node_26
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_26 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_23, by = "id_exploit") %>%
      inner_join(node_24, by = "id_exploit") %>%
      inner_join(node_25, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Disposer d'une liberté de décision dans ses choix de gouvernance et de production", "Disposer d'une autonomie financière", "Autonomie dans le processus productif")) %>%
      ungroup()

    # Responsabilité Globale --------------------------------------------------

    ## Node 27

    decision_rules <- decision_rules_total$node_27
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_27 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B20", "B5")) %>%
      ungroup()

    ## Node 28

    decision_rules <- decision_rules_total$node_28
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_28 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B11", "B19")) %>%
      ungroup()


    ## Node 29

    decision_rules <- decision_rules_total$node_29
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_29 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B1", "B2", "B4")) %>%
      ungroup()

    ## Node 30

    decision_rules <- decision_rules_total$node_30
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_30 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_27, by = "id_exploit") %>%
      inner_join(node_28, by = "id_exploit") %>%
      inner_join(node_29, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Partage et transparence des activités productives", "Ouverture et relation au monde non agricole", "Sécurité alimentaire")) %>%
      ungroup()


    ## Node 31

    decision_rules <- decision_rules_total$node_31
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_31 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A10", "A9")) %>%
      ungroup()

    ## Node 32

    decision_rules <- decision_rules_total$node_32
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_32 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A11", "C11")) %>%
      ungroup()

    ## Node 33

    decision_rules <- decision_rules_total$node_33
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_33 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_32, by = "id_exploit") %>%
      inner_join(node_31, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Ressources énergétiques et manufacturées", "Ressources naturelles")) %>%
      ungroup()

    ## Node 34

    decision_rules <- decision_rules_total$node_34
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_34 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B14", "B17")) %>%
      ungroup()

    ## Node 35

    decision_rules <- decision_rules_total$node_35
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_35 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B16", "B21")) %>%
      ungroup()

    ## Node 36

    decision_rules <- decision_rules_total$node_36
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_36 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A5", "B23")) %>%
      ungroup()

    ## Node 37

    decision_rules <- decision_rules_total$node_37
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_37 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_34, by = "id_exploit") %>%
      inner_join(node_35, by = "id_exploit") %>%
      inner_join(node_36, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Conditions de travail de la main d'oeuvre", "Conditions de vie et de travail", "Bien être de la vie animale")) %>%
      ungroup()

    ## Node 38

    decision_rules <- decision_rules_total$node_38
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_38 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A16", "A17", "A18")) %>%
      ungroup()

    ## Node 39

    decision_rules <- decision_rules_total$node_39
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_39 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("A19", "B12")) %>%
      ungroup()

    ## Node 40

    decision_rules <- decision_rules_total$node_40
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_40 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_38, by = "id_exploit") %>%
      inner_join(node_39, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Réduire les émissions", "Réduire l'usage des produits polluants")) %>%
      ungroup()

    ## Node 41

    decision_rules <- decision_rules_total$node_41
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_41 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_30, by = "id_exploit") %>%
      inner_join(node_33, by = "id_exploit") %>%
      inner_join(node_37, by = "id_exploit") %>%
      inner_join(node_40, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Implications et engagements sociaux", "Partager équitablement les ressources", "Contribuer à la qualité de vie sur l'exploitation", "Réduire ses impacts sur la santé et les écosystèmes")) %>%
      ungroup()

    # Ancrage territorial -----------------------------------------------------

    ## Node 42

    decision_rules <- decision_rules_total$node_42
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_42 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B10", "B3")) %>%
      ungroup()

    ## Node 43

    decision_rules <- decision_rules_total$node_43
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_43 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B7", "B8", "B9")) %>%
      ungroup()


    ## Node 44

    decision_rules <- decision_rules_total$node_44
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_44 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B14", "B15")) %>%
      ungroup()


    ## Node 45

    decision_rules <- decision_rules_total$node_45
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_45 <- prop_data %>%
      filter(indicateur %in% names(decision_rules)) %>%
      distinct(id_exploit, indicateur, categorie_dexi) %>%
      spread(key = indicateur, value = categorie_dexi) %>%
      inner_join(node_44, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("B19", "B6", "Par le travail et l'emploi")) %>%
      ungroup()

    ## Node 46

    decision_rules <- decision_rules_total$node_46
    names(decision_rules) <- map_chr(names(decision_rules), simplify_indicator_name)

    node_46 <- prop_data %>%
      distinct(id_exploit) %>%
      inner_join(node_42, by = "id_exploit") %>%
      inner_join(node_43, by = "id_exploit") %>%
      inner_join(node_45, by = "id_exploit") %>%
      group_by(id_exploit) %>%
      inner_join(decision_rules, by = c("Valoriser la qualité territoriale", "Contribuer à des démarches d'économie circulaire", "S'inscrire dans des démarches de territoire")) %>%
      ungroup()

    node_final <- node_10 %>%
      inner_join(node_20, by = c("id_exploit", "B16", "B18", "B13", "B15")) %>%
      inner_join(node_26, by = c("id_exploit", "B18", "B13", "B15", "C5", "C3")) %>%
      inner_join(node_41, by = c("id_exploit", "B16", "B1", "A5", "B14")) %>%
      inner_join(node_46, by = c("id_exploit", "B15", "B3", "B14", "B8", "B19"))





    end_nodes <- list("Robustesse" = node_10, "Capacité" = node_20, "Autonomie" = node_26, "Responsabilité" = node_41, "Ancrage" = node_46, Global = node_final)

    ## On supprime les noeuds intermédiaires
    rm(list = ls(pattern = "node_"))

    res_list$nodes <- end_nodes

    return(res_list)
  }


  ## If it's a single file input, it's simple
  if (import_list$input.type == "single") {
    imported_data <- importFromFile(input)
  }


  ## If it's a folder input, the function has to run on each file and the results have to be gathered in a single list.

  if (import_list$input.type == "folder") {
    list_paths <- paste0(input, "/", c(
      list.files(input, pattern = "\\.xls*"),
      list.files(input, pattern = "\\.json")
    ))

    folder_data <- map(list_paths, importFromFile)

    nodes_list <- list(
      Robustesse = map(folder_data, "nodes") %>% map("Robustesse") %>% bind_rows(),
      Capacité = map(folder_data, "nodes") %>% map("Capacité") %>% bind_rows(),
      Autonomie = map(folder_data, "nodes") %>% map("Autonomie") %>% bind_rows(),
      Responsabilité = map(folder_data, "nodes") %>% map("Responsabilité") %>% bind_rows(),
      Ancrage = map(folder_data, "nodes") %>% map("Ancrage") %>% bind_rows(),
      Global = map(folder_data, "nodes") %>% map("Global") %>% bind_rows()
    )


    imported_data <- list(
      nodes = nodes_list,
      dataset = map(folder_data, "dataset") %>% bind_rows(),
      metadata = map(folder_data, "metadata") %>% bind_rows()
    )
  }



  return(c(import_list, imported_data))
}
