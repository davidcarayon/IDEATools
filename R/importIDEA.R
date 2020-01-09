#' Import IDEA4 data
#'
#' @param input a system path leading either to a single file or a directory. If the input is a single file, accepted formats are : .xls, .xlsx and .json.
#' @param anonymous Boolean. Should the results be anonymised ? If yes, a correspondence table will be added to  the result list
#'
#' @return a named list, containing :
#'     analysis.type : a string which can be "single" or "multi" depending on the input type.
#'     dataset : a tibble dataframe with the extracted data
#'     nodes : results of the property analysis
#'     metadata : metadata extracted from the input
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example.xls", package = "IDEATools")
#' IDEAdata <- importIDEA(path, anonymous = FALSE)
importIDEA <- function(input, anonymous = FALSE){

input <- normalizePath(input)

# Analysis type -----------------------------------------------------------

  result_list <- list()

  result_list$analysis.type <- ifelse(stringr::str_detect(input,".xls") |
                                      stringr::str_detect(input,".xlsx"),
                          yes = "single",
                          no = "multi")




create_single_data <- function(input){

  res_list <- list()

##### DATASET

# Dataset proprietes ------------------------------------------------------

    metadata <- suppressMessages(readxl::read_excel(input, sheet = "Saisie et Calculateur") %>% janitor::clean_names() %>%  dplyr::select(1,2) %>% tidyr::drop_na())

    names(metadata) = c("title","value")

    res_list$metadata <- metadata

    nom = metadata %>% dplyr::filter(title == "NOM Prénom :") %>% dplyr::pull(value) %>% `[`(1)

    if(is.na(nom)){

      metadata <- bind_rows(
        tibble(title = "NOM Prénom :",value = paste0("EA_",stringi::stri_rand_strings(1, 5, '[A-Z]'))),
        metadata)

      nom = metadata %>% dplyr::filter(title == "NOM Prénom :") %>% dplyr::pull(value) %>% `[`(1)

        }




    elevage = metadata %>% dplyr::filter(title == "Présence d'élevage :") %>% dplyr::pull(value) %>% `[`(1)

    metadata$nom_exploit = nom

    metadata <- metadata %>% dplyr::select(nom_exploit,title,value)

    res_list$metadata <- metadata

    ## Agroécologie

    ## Label
    AE_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension agroécologique") %>% janitor::clean_names() %>% dplyr::select(x2) %>% tidyr::drop_na())

    ## Valeur
    AE_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension agroécologique") %>% janitor::clean_names() %>% dplyr::select(x8) %>% tidyr::drop_na() %>% dplyr::slice(seq(2, 38, 2)))

    AE <- dplyr::bind_cols(AE_lab, AE_val) %>%
      dplyr::mutate(dimension = "Durabilité Agroécologique") %>%
      dplyr::select(dimension, indicateur = x2, score_deplaf = x8)

    ## Socio-territorial

    ## Label
    ST_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension socio-territoriale") %>% janitor::clean_names() %>% dplyr::select(x2) %>% tidyr::drop_na())
    ## Valeur
    ST_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension socio-territoriale") %>% janitor::clean_names() %>% dplyr::select(x8) %>% tidyr::drop_na() %>% dplyr::slice(seq(2, 46, 2)))

    ST <- dplyr::bind_cols(ST_lab, ST_val) %>%
      dplyr::mutate(dimension = "Durabilité Socio-Territoriale") %>%
      dplyr::select(dimension, indicateur = x2, score_deplaf = x8)


    ## Economique

    ## Label
    EC_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension économique") %>% janitor::clean_names() %>% dplyr::select(x2) %>% tidyr::drop_na())
    ## Valeur
    EC_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension économique") %>% janitor::clean_names() %>% dplyr::select(x7) %>% tidyr::drop_na() %>% dplyr::slice(seq(2, 22, 2)))

    EC <- dplyr::bind_cols(EC_lab, EC_val) %>%
      dplyr::mutate(dimension = "Durabilité Economique") %>%
      dplyr::select(dimension, indicateur = x2, score_deplaf = x7)

    ## On aggrége
    results <- dplyr::bind_rows(AE, ST, EC) %>% dplyr::mutate(score_deplafonne = as.numeric(score_deplaf))

    ## Fonction de tranformation des valeurs numériques en catégories
    categ_dexi <- function(TD, D, I, F, score) {
      seuils <- c(TD, D, I, F) %>% stats::na.omit()

      if (length(seuils) == 4) {
        res <- dplyr::case_when(
          score < D ~ "très défavorable",
          score >= D & score < I ~ "défavorable",
          score >= I & score < F ~ "intermédiaire",
          score >= F ~ "favorable"
        )
      }

      if (length(seuils) == 3) {
        res <- dplyr::case_when(
          score < I ~ "défavorable",
          score >= I & score < F ~ "intermédiaire",
          score >= F ~ "favorable"
        )
      }

      if (length(seuils) == 2) {
        res <- dplyr::case_when(
          score < F ~ "intermédiaire",
          score >= F ~ "favorable"
        )
      }


      return(res)
    }

    results_dexi <- categ %>%
      dplyr::mutate(nom_exploit = nom) %>%
      dplyr::inner_join(results, by = "indicateur") %>%
      dplyr::mutate(categorie_dexi = purrr::pmap_chr(list(TD, D, I, F, score_deplafonne), categ_dexi)) %>%
      dplyr::select(nom_exploit, dimension, indicateur, nom_indicateur, score_deplafonne, categorie_dexi)


    if(elevage == "non") {
      results_dexi = results_dexi %>%
        dplyr::rowwise() %>%
        dplyr::mutate(categorie_dexi = ifelse(indicateur == "A7", yes = "NC", no = categorie_dexi)) %>%
        dplyr::ungroup()
    }


    ## Ajout des données de dimension

# Dataset dimensions ------------------------------------------------------

    file <- suppressMessages(readxl::read_excel(input, sheet = "Bilan durabilité")) %>% janitor::clean_names()

    table_dimensions <- file %>%
      dplyr::select(x2, x5) %>%
      tidyr::fill(x2) %>%
      tidyr::drop_na() %>%
      dplyr::slice(2:54) %>%
      dplyr::select(composante = x2, indicateur = x5) %>%
      dplyr::mutate(
        dimension = dplyr::case_when(
          stringr::str_detect(indicateur, "A") ~ "Durabilité Agroécologique",
          stringr::str_detect(indicateur, "B") ~ "Durabilité Socio-Territoriale",
          stringr::str_detect(indicateur, "C") ~ "Durabilité Economique"
        ),
        dimension = factor(dimension, levels = c("Durabilité Agroécologique", "Durabilité Socio-Territoriale", "Durabilité Economique"))
      )

    results_ind <- file %>%
      dplyr::select(x5, x6, x8) %>%
      tidyr::drop_na() %>%
      dplyr::mutate_at(dplyr::vars(x6, x8), as.numeric) %>%
      dplyr::select(indicateur = x5, valeur = x6, valeur_max = x8)

    max_compo <- file %>%
      dplyr::select(x2, x11) %>%
      tidyr::fill(x2) %>%
      tidyr::drop_na() %>%
      dplyr::slice(1:15) %>%
      dplyr::filter(x11 != "100") %>%
      dplyr::mutate(x11 = as.numeric(x11)) %>%
      dplyr::select(composante = x2, max_compo = x11)

    results_compo <- results_ind %>%
      dplyr::inner_join(table_dimensions, by = "indicateur") %>%
      dplyr::group_by(composante) %>%
      dplyr::summarise(valeur_compo = sum(valeur, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(max_compo, by = "composante")


    results_indic_compo <- dplyr::inner_join(results_ind, table_dimensions, by = "indicateur") %>%
      dplyr::inner_join(results_compo, by = "composante") %>%
      dplyr::select(dimension, composante, indicateur, valeur, valeur_max, valeur_compo, max_compo) %>%
      dplyr::mutate(valeur_compo = ifelse(valeur_compo > max_compo, yes = max_compo, no = valeur_compo)) %>%
      dplyr::mutate(
        score_ind = valeur / valeur_max * 100,
        score_compo = valeur_compo / max_compo * 100
      )


    results_dim <- results_indic_compo %>%
      dplyr::distinct(dimension, composante, valeur_compo) %>%
      dplyr::group_by(dimension) %>%
      dplyr::summarise(score_dim = sum(valeur_compo))

    results <- results_indic_compo %>%
      dplyr::inner_join(results_dim, by = "dimension") %>%
      dplyr::mutate(nom_exploit = nom) %>%
      dplyr::select(nom_exploit, dimension:score_dim) %>%
      dplyr::mutate(score_dim = ifelse(score_dim > 100, yes = 100, no = score_dim)) %>%
      dplyr::mutate(dimension = as.character(dimension))

    ## Aggrégation données

    res_list$dataset <- dplyr::full_join(results_dexi, results, by = c("nom_exploit","dimension","indicateur"))

    ####### Calcul des noeuds

# Calcul des noeuds -------------------------------------------------------


    prop_data <- results_dexi

    # Robustesse --------------------------------------------------------------

    ## Node 1

    decision_rules <- decision_rules_total$node_1
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_1 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A1", "A3", "A4")) %>%
      dplyr::ungroup()

    ## Node 2

    decision_rules <- decision_rules_total$node_2
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_2 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_1, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A14", "C5", "Diversité de l'organisation spatiale et temporelle")) %>%
      dplyr::ungroup()

    ## Node 3

    decision_rules <- decision_rules_total$node_3
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_3 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("C4", "C7")) %>%
      dplyr::ungroup()

    ## Node 4

    decision_rules <- decision_rules_total$node_4
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_4 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_3, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A2", "Diversité des activités")) %>%
      dplyr::ungroup()


    ## Node 5

    decision_rules <- decision_rules_total$node_5
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_5 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("C8", "C9")) %>%
      dplyr::ungroup()

    ## Node 6

    decision_rules <- decision_rules_total$node_6
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_6 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_5, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A15", "De l'outil de production")) %>%
      dplyr::ungroup()

    ## Node 7

    decision_rules <- decision_rules_total$node_7
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_7 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_4, by = "nom_exploit") %>%
      dplyr::inner_join(node_6, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B22", "En favorisant la diversité", "En développant l'inertie et les capacités tampon")) %>%
      dplyr::ungroup()

    ## Node 8

    decision_rules <- decision_rules_total$node_8
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_8 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B13", "B15")) %>%
      dplyr::ungroup()

    ## Node 9

    decision_rules <- decision_rules_total$node_9
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_9 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_8, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B16", "B18", "par l'insertion dans les réseaux")) %>%
      dplyr::ungroup()

    ## Node 10
    decision_rules <- decision_rules_total$node_10
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_10 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_9, by = "nom_exploit") %>%
      dplyr::inner_join(node_7, by = "nom_exploit") %>%
      dplyr::inner_join(node_2, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Augmenter la capacité d'adaptation", "Réduire la sensibilité", "Limiter l'exposition aux aléas")) %>%
      dplyr::ungroup()


    # Capacité productive et reproductive de biens et services ----------------

    ## Node 11

    decision_rules <- decision_rules_total$node_11
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_11 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A12", "A13", "A5")) %>%
      dplyr::ungroup()


    ## Node 12

    decision_rules <- decision_rules_total$node_12
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_12 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B14", "B15", "B16")) %>%
      dplyr::ungroup()

    ## Node 13

    decision_rules <- decision_rules_total$node_13
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_13 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B13", "B18")) %>%
      dplyr::ungroup()


    ## Node 14

    decision_rules <- decision_rules_total$node_14
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_14 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_12, by = "nom_exploit") %>%
      dplyr::inner_join(node_13, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Travail", "Compétences et équipements")) %>%
      dplyr::ungroup()

    ## Node 15

    decision_rules <- decision_rules_total$node_15
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_15 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_11, by = "nom_exploit") %>%
      dplyr::inner_join(node_14, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Naturelles", "Sociales et humaines")) %>%
      dplyr::ungroup()


    ## Node 16

    decision_rules <- decision_rules_total$node_16
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_16 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B1", "B3")) %>%
      dplyr::ungroup()

    ## Node 17

    decision_rules <- decision_rules_total$node_17
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_17 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_16, by = "nom_exploit") %>%
      dplyr::inner_join(node_15, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Développer la capacité alimentaire", "Préserver ou créer des ressources pour l'acte de production")) %>%
      dplyr::ungroup()

    ## Node 18

    decision_rules <- decision_rules_total$node_18
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_18 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("C2", "C3")) %>%
      dplyr::ungroup()


    ## Node 19

    decision_rules <- decision_rules_total$node_19
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_19 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_18, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("C1", "C10", "Capacité de remboursement")) %>%
      dplyr::ungroup()

    ## Node 20

    decision_rules <- decision_rules_total$node_20
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_20 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_17, by = "nom_exploit") %>%
      dplyr::inner_join(node_19, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Capacité à produire dans le temps des biens et services remunérés", "Capacité à dégager un revenu dans le temps")) %>%
      dplyr::ungroup()

    # Autonomie ---------------------------------------------------------------

    ## Node 21

    decision_rules <- decision_rules_total$node_21
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_21 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B13", "B15", "B18")) %>%
      dplyr::ungroup()

    ## Node 22

    decision_rules <- decision_rules_total$node_22
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_22 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B8", "C5")) %>%
      dplyr::ungroup()

    ## Node 23

    decision_rules <- decision_rules_total$node_23
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_23 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_21, by = "nom_exploit") %>%
      dplyr::inner_join(node_22, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Liberté de décision organisationnelle", "Liberté de décision dans les relations commerciales")) %>%
      dplyr::ungroup()

    ## Node 24

    decision_rules <- decision_rules_total$node_24
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_24 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("C3", "C6")) %>%
      dplyr::ungroup()

    ## Node 25

    decision_rules <- decision_rules_total$node_25
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_25 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A6", "A7", "A8")) %>%
      dplyr::ungroup()

    ## Node 26

    decision_rules <- decision_rules_total$node_26
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_26 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_23, by = "nom_exploit") %>%
      dplyr::inner_join(node_24, by = "nom_exploit") %>%
      dplyr::inner_join(node_25, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Disposer d'une liberté de décision dans ses choix de gouvernance et de production", "Disposer d'une autonomie financière", "Autonomie dans le processus productif")) %>%
      dplyr::ungroup()

    # Responsabilité Globale --------------------------------------------------

    ## Node 27

    decision_rules <- decision_rules_total$node_27
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_27 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B20", "B5")) %>%
      dplyr::ungroup()

    ## Node 28

    decision_rules <- decision_rules_total$node_28
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_28 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B11", "B19")) %>%
      dplyr::ungroup()


    ## Node 29

    decision_rules <- decision_rules_total$node_29
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_29 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B1", "B2", "B4")) %>%
      dplyr::ungroup()

    ## Node 30

    decision_rules <- decision_rules_total$node_30
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_30 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_27, by = "nom_exploit") %>%
      dplyr::inner_join(node_28, by = "nom_exploit") %>%
      dplyr::inner_join(node_29, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Partage et transparence des activités productives", "Ouverture et relation au monde non agricole", "Sécurité alimentaire")) %>%
      dplyr::ungroup()


    ## Node 31

    decision_rules <- decision_rules_total$node_31
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_31 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A10", "A9")) %>%
      dplyr::ungroup()

    ## Node 32

    decision_rules <- decision_rules_total$node_32
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_32 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A11", "C11")) %>%
      dplyr::ungroup()

    ## Node 33

    decision_rules <- decision_rules_total$node_33
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_33 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_32, by = "nom_exploit") %>%
      dplyr::inner_join(node_31, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Ressources énergétiques et manufacturées", "Ressources naturelles")) %>%
      dplyr::ungroup()

    ## Node 34

    decision_rules <- decision_rules_total$node_34
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_34 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B14", "B17")) %>%
      dplyr::ungroup()

    ## Node 35

    decision_rules <- decision_rules_total$node_35
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_35 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B16", "B21")) %>%
      dplyr::ungroup()

    ## Node 36

    decision_rules <- decision_rules_total$node_36
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_36 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A5", "B23")) %>%
      dplyr::ungroup()

    ## Node 37

    decision_rules <- decision_rules_total$node_37
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_37 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_34, by = "nom_exploit") %>%
      dplyr::inner_join(node_35, by = "nom_exploit") %>%
      dplyr::inner_join(node_36, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Conditions de travail de la main d'oeuvre", "Conditions de vie et de travail", "Bien être de la vie animale")) %>%
      dplyr::ungroup()

    ## Node 38

    decision_rules <- decision_rules_total$node_38
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_38 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A16", "A17", "A18")) %>%
      dplyr::ungroup()

    ## Node 39

    decision_rules <- decision_rules_total$node_39
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_39 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("A19", "B12")) %>%
      dplyr::ungroup()

    ## Node 40

    decision_rules <- decision_rules_total$node_40
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_40 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_38, by = "nom_exploit") %>%
      dplyr::inner_join(node_39, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Réduire les émissions", "Réduire l'usage des produits polluants")) %>%
      dplyr::ungroup()

    ## Node 41

    decision_rules <- decision_rules_total$node_41
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_41 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_30, by = "nom_exploit") %>%
      dplyr::inner_join(node_33, by = "nom_exploit") %>%
      dplyr::inner_join(node_37, by = "nom_exploit") %>%
      dplyr::inner_join(node_40, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Implications et engagements sociaux", "Partager équitablement les ressources", "Contribuer à la qualité de vie sur l'exploitation", "Réduire ses impacts sur la santé et les écosystèmes")) %>%
      dplyr::ungroup()

    # Ancrage territorial -----------------------------------------------------

    ## Node 42

    decision_rules <- decision_rules_total$node_42
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_42 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B10", "B3")) %>%
      dplyr::ungroup()

    ## Node 43

    decision_rules <- decision_rules_total$node_43
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_43 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B7", "B8", "B9")) %>%
      dplyr::ungroup()


    ## Node 44

    decision_rules <- decision_rules_total$node_44
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_44 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B14", "B15")) %>%
      dplyr::ungroup()


    ## Node 45

    decision_rules <- decision_rules_total$node_45
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_45 <- prop_data %>%
      dplyr::filter(indicateur %in% names(decision_rules)) %>%
      dplyr::distinct(nom_exploit, indicateur, categorie_dexi) %>%
      tidyr::spread(key = indicateur, value = categorie_dexi) %>%
      dplyr::inner_join(node_44, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("B19", "B6", "Par le travail et l'emploi")) %>%
      dplyr::ungroup()

    ## Node 46

    decision_rules <- decision_rules_total$node_46
    names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

    node_46 <- prop_data %>%
      dplyr::distinct(nom_exploit) %>%
      dplyr::inner_join(node_42, by = "nom_exploit") %>%
      dplyr::inner_join(node_43, by = "nom_exploit") %>%
      dplyr::inner_join(node_45, by = "nom_exploit") %>%
      dplyr::group_by(nom_exploit) %>%
      dplyr::inner_join(decision_rules, by = c("Valoriser la qualité territoriale", "Contribuer à des démarches d'économie circulaire", "S'inscrire dans des démarches de territoire")) %>%
      dplyr::ungroup()

    node_final <- node_10 %>%
      dplyr::inner_join(node_20, by = c("nom_exploit", "B16", "B18", "B13", "B15")) %>%
      dplyr::inner_join(node_26, by = c("nom_exploit", "B18", "B13", "B15", "C5", "C3")) %>%
      dplyr::inner_join(node_41, by = c("nom_exploit", "B16", "B1", "A5", "B14")) %>%
      dplyr::inner_join(node_46, by = c("nom_exploit", "B15", "B3", "B14", "B8", "B19"))

    end_nodes <- list("Robustesse" = node_10, "Capacité" = node_20, "Autonomie" = node_26, "Responsabilité" = node_41, "Ancrage" = node_46, Global = node_final)

    ## On supprime les noeuds intermédiaires
    rm(list = ls(pattern = "node_"))

  res_list$nodes <- end_nodes


  return(res_list)


  }

if (result_list$analysis.type == "single") {
single_data <- create_single_data(input)

res <- c(result_list,single_data)

}

if(result_list$analysis.type == "multi"){

  list_paths <- c(paste0(input,"/",list.files(input, pattern = "\\.xls$")),paste0(input,"/",list.files(input, pattern = "\\.xlsx$")))

  group_data <- purrr::map(list_paths,create_single_data)

  nodes_list <- list(
    Robustesse = purrr::map(group_data,"nodes") %>% purrr::map("Robustesse") %>% dplyr::bind_rows(),
    Capacité = purrr::map(group_data,"nodes") %>% purrr::map("Capacité") %>% dplyr::bind_rows(),
    Autonomie = purrr::map(group_data,"nodes") %>% purrr::map("Autonomie") %>% dplyr::bind_rows(),
    Responsabilité = purrr::map(group_data,"nodes") %>% purrr::map("Responsabilité") %>% dplyr::bind_rows(),
    Ancrage = purrr::map(group_data,"nodes") %>% purrr::map("Ancrage") %>% dplyr::bind_rows(),
    Global = purrr::map(group_data,"nodes") %>% purrr::map("Global") %>% dplyr::bind_rows()
  )


  grouped_datasets <- list(
    nodes = nodes_list,
    dataset = purrr::map(group_data,"dataset") %>% dplyr::bind_rows(),
    metadata = purrr::map(group_data,"metadata") %>% dplyr::bind_rows()
  )


  res <- c(result_list,grouped_datasets)

}


if(anonymous) {

  nf <- dplyr::n_distinct(res$dataset$nom_exploit)
  noms <- unique(res$dataset$nom_exploit)

  corresp_table <- tibble::tibble(nom_exploit = noms) %>%
  dplyr::mutate(code_exploit = paste0("EA",1:nf))

  res$dataset <- res$dataset %>% dplyr::inner_join(corresp_table, by ="nom_exploit") %>%
    dplyr::select(-nom_exploit) %>%
    dplyr::rename("nom_exploit"="code_exploit")

  res$nodes <- res$nodes %>% dplyr::inner_join(corresp_table, by ="nom_exploit") %>%
    dplyr::select(-nom_exploit) %>%
    dplyr::rename("nom_exploit"="code_exploit")

  res$metadata <- res$metadata %>% dplyr::inner_join(corresp_table, by ="nom_exploit") %>%
    dplyr::select(-nom_exploit) %>%
    dplyr::rename("nom_exploit"="code_exploit")

  res$corresp_table <- corresp_table

  return(res)

} else {return(res)}


  } # end of function

