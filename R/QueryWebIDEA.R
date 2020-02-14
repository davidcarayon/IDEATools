#' Query the WEBIDEA database
#'
#' @param id_organisme identifiant de l'organisme
#' @param id_dossier identifiant du dossier
#' @param anneee year
#'
#' @return a named list, containing :
#'     analysis.type : a string which can be "single" or "folder" depending on the input type.
#'     dataset : a tibble dataframe with the extracted data
#'     nodes : results of the property analysis
#'     metadata : metadata extracted from the input
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' IDEAdata <- importIDEA(path, anonymous = FALSE)
QueryWebIDEA <- function(user = "idea_v4_user_readonly", password = "idea_v4_user_readonly_password",id_organisme = 1,id_dossier = "654321",annee = 2009){

id_dossier = paste0("'",id_dossier,"'")

# Connexion database ------------------------------------------------------
BDD_IDEA <- DBI::dbConnect(RPostgres::Postgres(),dbname = "idea_v4_db",host="194.199.250.56", port=5432,user=user, password=password)


# Définition de la requête SQL --------------------------------------------
custom_query <- paste0("SELECT i.code, v.valeur FROM valeur_indicateur v JOIN indicateur i ON v.indicateur_id = i.id WHERE v.organisme_id = ",id_organisme," AND v.dossier = ",id_dossier," AND v.annee = ",annee," ORDER BY i.code")

## Requête
IDEAQuery <- DBI::dbGetQuery(BDD_IDEA,custom_query) %>% dplyr::tbl_df() %>%
  dplyr::filter(code %in% filtre_json) %>%
  dplyr::mutate(type_indicateur = ifelse(stringr::str_detect(code,"MTD_"), yes = "metadonnees", no = "items"))

# Disconnect --------------------------------------------------------------
DBI::dbDisconnect(BDD_IDEA)
rm(BDD_IDEA)

items <- subset(IDEAQuery, type_indicateur == "items")
metadonnees <- subset(IDEAQuery,type_indicateur == "metadonnees")

res <- list(metadonnees = split(metadonnees$valeur,metadonnees$code),
     items = split(items$valeur, items$code))


# A partir d'ici, code identique à import.R -------------------------------


res_list <- list()

## Agregates items into indicators
Item2Indic <- function(indicateur,df) {

  df <- df %>% dplyr::arrange(item)

  items <- df$value %>% as.numeric()

  if(indicateur %in% c("A1","A5","A7","A8","A14","A19","B23","B2")) {

    if(indicateur == "A1") {value = ifelse(items[2]==4, yes = 4, no = sum(items))}
    if(indicateur == "A5") {value = ifelse(metadata$MTD_15 >= 0.75, yes = 5, no = sum(items))}
    if(indicateur == "A7") {value = dplyr::case_when(metadata$MTD_14 == 0 ~ 0,
                                                     metadata$MTD_14 == 1 ~ round(0.7*items[1]+0.3*items[2]+1e-10),
                                                     metadata$MTD_14 == 2 ~ as.numeric(items[2]))}
    if(indicateur == "A8") {value = ifelse(metadata$MTD_15 >= 0.95, yes = 8, no = sum(items))}
    if(indicateur == "A14"){value = dplyr::case_when(metadata$MTD_16 == 0 ~ 4,
                                                     metadata$MTD_14 == 0 ~ as.numeric(items[1]),
                                                     metadata$MTD_14 != 0 & metadata$MTD_16 !=0 ~ min(as.numeric(items)))}
    if(indicateur == "A19") {value = ifelse(metadata$MTD_14 == 0, yes = items[1], no = min(as.numeric(items)))}
    if(indicateur == "B2") {value = ifelse(metadata$MTD_14 == 0, yes = items[2], no = items[1])}
    if(indicateur == "B23") {value = ifelse(metadata$MTD_14 == 0, yes = items[2], no = round(mean(items)+1e-10))}

  } else {

    value = sum(items)

  }


  return(as.numeric(value))


}


## Extract metadata and wrap them in a 1-line dataframe
metadata <- res$metadonnees %>% dplyr::bind_cols() %>% dplyr::mutate_all(as.character)

if(metadata$MTD_01 %in% c("0",NA)){metadata$MTD_01  <- stringi::stri_rand_strings(1, 5, '[A-Z]')}

## Extract the farm id
id_exploit <- metadata$MTD_01

# Full pipeline from data -------------------------------------------------
dataset <- dplyr::bind_rows(res$items) %>%
  tidyr::gather(key = item, value = value) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(item = stringr::str_replace_all(item,"(?<=[:upper:])0","")) %>% # To convert A01 to A1
  # separating indicator dans items
  dplyr::mutate(indicateur = stringr::str_split(item,"_")[[1]][2],
                item = stringr::str_split(item,"_")[[1]][3]) %>%
  dplyr::mutate(item = as.numeric(item)) %>%
  dplyr::ungroup() %>%
  dplyr::select(indicateur,item,value) %>%
  dplyr::group_by(indicateur) %>%
  tidyr::nest() %>%
  ## Calculating the indicator unscaled, then scaled value
  dplyr::mutate(unscaled_value = purrr::map2_dbl(indicateur,data,Item2Indic)) %>%
  dplyr::mutate(value = purrr::map2_dbl(indicateur,unscaled_value, ScaleIndicator)) %>%
  dplyr::select(-data) %>%
  dplyr::ungroup() %>%
  ## Joining to gather info on dimension/composante
  dplyr::inner_join(categ %>% dplyr::select(-nom_indicateur), by = "indicateur")%>%
  dplyr::inner_join(label_nodes, by = c("indicateur"="code_indicateur")) %>%
  dplyr::mutate(id_exploit = id_exploit) %>%
  dplyr::mutate(unscaled_value = round(unscaled_value,0)) %>%
  ## Converting unscaled value to a qualitative DEXi category
  dplyr::mutate(categorie_dexi = purrr::pmap_chr(list(TD, D, I, F, unscaled_value), Score2Dexi)) %>%
  dplyr::select(id_exploit, dimension, composante,indicateur, nom_indicateur, unscaled_value, categorie_dexi, value) %>%
  ## Adding indicator values to calculate composantes
  dplyr::group_by(composante) %>%
  dplyr::mutate(composante_value = sum(value, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(composante_value = purrr::map2_dbl(composante,composante_value,ScaleComposante)) %>%
  dplyr::group_by(dimension) %>%
  tidyr::nest() %>%
  dplyr::mutate(dimension_value = purrr::map_dbl(data,Composante2Dimension)) %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::ungroup() %>%
  dplyr::select(id_exploit,dimension,composante,indicateur,nom_indicateur,unscaled_value,categorie_dexi,value,composante_value,dimension_value)

results_dexi <- dataset %>%
  dplyr::select(id_exploit,dimension,indicateur,nom_indicateur,unscaled_value,categorie_dexi)

## Compile metadata and dataset in the output list
res_list$metadata <- metadata
res_list$dataset <- dataset

prop_data <- results_dexi

# Robustesse --------------------------------------------------------------

## Node 1

decision_rules <- decision_rules_total$node_1
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_1 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A1", "A3", "A4")) %>%
  dplyr::ungroup()

## Node 2

decision_rules <- decision_rules_total$node_2
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_2 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_1, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A14", "C5", "Diversité de l'organisation spatiale et temporelle")) %>%
  dplyr::ungroup()

## Node 3

decision_rules <- decision_rules_total$node_3
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_3 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("C4", "C7")) %>%
  dplyr::ungroup()

## Node 4

decision_rules <- decision_rules_total$node_4
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_4 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_3, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A2", "Diversité des activités")) %>%
  dplyr::ungroup()

## Node 5

decision_rules <- decision_rules_total$node_5
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_5 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("C8", "C9")) %>%
  dplyr::ungroup()

## Node 6

decision_rules <- decision_rules_total$node_6
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_6 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_5, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A15", "De l'outil de production")) %>%
  dplyr::ungroup()

## Node 7

decision_rules <- decision_rules_total$node_7
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_7 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_4, by = "id_exploit") %>%
  dplyr::inner_join(node_6, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B22", "En favorisant la diversité", "En développant l'inertie et les capacités tampon")) %>%
  dplyr::ungroup()

## Node 8

decision_rules <- decision_rules_total$node_8
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_8 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B13", "B15")) %>%
  dplyr::ungroup()

## Node 9

decision_rules <- decision_rules_total$node_9
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_9 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_8, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B16", "B18", "par l'insertion dans les réseaux")) %>%
  dplyr::ungroup()

## Node 10
decision_rules <- decision_rules_total$node_10
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_10 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_9, by = "id_exploit") %>%
  dplyr::inner_join(node_7, by = "id_exploit") %>%
  dplyr::inner_join(node_2, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Augmenter la capacité d'adaptation", "Réduire la sensibilité", "Limiter l'exposition aux aléas")) %>%
  dplyr::ungroup()


# Capacité productive et reproductive de biens et services ----------------

## Node 11

decision_rules <- decision_rules_total$node_11
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_11 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A12", "A13", "A5")) %>%
  dplyr::ungroup()


## Node 12

decision_rules <- decision_rules_total$node_12
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_12 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B14", "B15", "B16")) %>%
  dplyr::ungroup()

## Node 13

decision_rules <- decision_rules_total$node_13
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_13 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B13", "B18")) %>%
  dplyr::ungroup()


## Node 14

decision_rules <- decision_rules_total$node_14
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_14 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_12, by = "id_exploit") %>%
  dplyr::inner_join(node_13, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Travail", "Compétences et équipements")) %>%
  dplyr::ungroup()

## Node 15

decision_rules <- decision_rules_total$node_15
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_15 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_11, by = "id_exploit") %>%
  dplyr::inner_join(node_14, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Naturelles", "Sociales et humaines")) %>%
  dplyr::ungroup()


## Node 16

decision_rules <- decision_rules_total$node_16
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_16 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B1", "B3")) %>%
  dplyr::ungroup()

## Node 17

decision_rules <- decision_rules_total$node_17
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_17 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_16, by = "id_exploit") %>%
  dplyr::inner_join(node_15, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Développer la capacité alimentaire", "Préserver ou créer des ressources pour l'acte de production")) %>%
  dplyr::ungroup()

## Node 18

decision_rules <- decision_rules_total$node_18
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_18 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("C2", "C3")) %>%
  dplyr::ungroup()


## Node 19

decision_rules <- decision_rules_total$node_19
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_19 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_18, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("C1", "C10", "Capacité de remboursement")) %>%
  dplyr::ungroup()

## Node 20

decision_rules <- decision_rules_total$node_20
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_20 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_17, by = "id_exploit") %>%
  dplyr::inner_join(node_19, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Capacité à produire dans le temps des biens et services remunérés", "Capacité à dégager un revenu dans le temps")) %>%
  dplyr::ungroup()

# Autonomie ---------------------------------------------------------------

## Node 21

decision_rules <- decision_rules_total$node_21
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_21 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B13", "B15", "B18")) %>%
  dplyr::ungroup()

## Node 22

decision_rules <- decision_rules_total$node_22
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_22 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B8", "C5")) %>%
  dplyr::ungroup()

## Node 23

decision_rules <- decision_rules_total$node_23
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_23 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_21, by = "id_exploit") %>%
  dplyr::inner_join(node_22, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Liberté de décision organisationnelle", "Liberté de décision dans les relations commerciales")) %>%
  dplyr::ungroup()

## Node 24

decision_rules <- decision_rules_total$node_24
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_24 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("C3", "C6")) %>%
  dplyr::ungroup()

## Node 25

decision_rules <- decision_rules_total$node_25
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_25 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A6", "A7", "A8")) %>%
  dplyr::ungroup()

## Node 26

decision_rules <- decision_rules_total$node_26
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_26 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_23, by = "id_exploit") %>%
  dplyr::inner_join(node_24, by = "id_exploit") %>%
  dplyr::inner_join(node_25, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Disposer d'une liberté de décision dans ses choix de gouvernance et de production", "Disposer d'une autonomie financière", "Autonomie dans le processus productif")) %>%
  dplyr::ungroup()

# Responsabilité Globale --------------------------------------------------

## Node 27

decision_rules <- decision_rules_total$node_27
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_27 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B20", "B5")) %>%
  dplyr::ungroup()

## Node 28

decision_rules <- decision_rules_total$node_28
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_28 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B11", "B19")) %>%
  dplyr::ungroup()


## Node 29

decision_rules <- decision_rules_total$node_29
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_29 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B1", "B2", "B4")) %>%
  dplyr::ungroup()

## Node 30

decision_rules <- decision_rules_total$node_30
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_30 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_27, by = "id_exploit") %>%
  dplyr::inner_join(node_28, by = "id_exploit") %>%
  dplyr::inner_join(node_29, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Partage et transparence des activités productives", "Ouverture et relation au monde non agricole", "Sécurité alimentaire")) %>%
  dplyr::ungroup()


## Node 31

decision_rules <- decision_rules_total$node_31
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_31 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A10", "A9")) %>%
  dplyr::ungroup()

## Node 32

decision_rules <- decision_rules_total$node_32
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_32 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A11", "C11")) %>%
  dplyr::ungroup()

## Node 33

decision_rules <- decision_rules_total$node_33
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_33 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_32, by = "id_exploit") %>%
  dplyr::inner_join(node_31, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Ressources énergétiques et manufacturées", "Ressources naturelles")) %>%
  dplyr::ungroup()

## Node 34

decision_rules <- decision_rules_total$node_34
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_34 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B14", "B17")) %>%
  dplyr::ungroup()

## Node 35

decision_rules <- decision_rules_total$node_35
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_35 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B16", "B21")) %>%
  dplyr::ungroup()

## Node 36

decision_rules <- decision_rules_total$node_36
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_36 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A5", "B23")) %>%
  dplyr::ungroup()

## Node 37

decision_rules <- decision_rules_total$node_37
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_37 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_34, by = "id_exploit") %>%
  dplyr::inner_join(node_35, by = "id_exploit") %>%
  dplyr::inner_join(node_36, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Conditions de travail de la main d'oeuvre", "Conditions de vie et de travail", "Bien être de la vie animale")) %>%
  dplyr::ungroup()

## Node 38

decision_rules <- decision_rules_total$node_38
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_38 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A16", "A17", "A18")) %>%
  dplyr::ungroup()

## Node 39

decision_rules <- decision_rules_total$node_39
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_39 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("A19", "B12")) %>%
  dplyr::ungroup()

## Node 40

decision_rules <- decision_rules_total$node_40
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_40 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_38, by = "id_exploit") %>%
  dplyr::inner_join(node_39, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Réduire les émissions", "Réduire l'usage des produits polluants")) %>%
  dplyr::ungroup()

## Node 41

decision_rules <- decision_rules_total$node_41
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_41 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_30, by = "id_exploit") %>%
  dplyr::inner_join(node_33, by = "id_exploit") %>%
  dplyr::inner_join(node_37, by = "id_exploit") %>%
  dplyr::inner_join(node_40, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Implications et engagements sociaux", "Partager équitablement les ressources", "Contribuer à la qualité de vie sur l'exploitation", "Réduire ses impacts sur la santé et les écosystèmes")) %>%
  dplyr::ungroup()

# Ancrage territorial -----------------------------------------------------

## Node 42

decision_rules <- decision_rules_total$node_42
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_42 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B10", "B3")) %>%
  dplyr::ungroup()

## Node 43

decision_rules <- decision_rules_total$node_43
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_43 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B7", "B8", "B9")) %>%
  dplyr::ungroup()


## Node 44

decision_rules <- decision_rules_total$node_44
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_44 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B14", "B15")) %>%
  dplyr::ungroup()


## Node 45

decision_rules <- decision_rules_total$node_45
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_45 <- prop_data %>%
  dplyr::filter(indicateur %in% names(decision_rules)) %>%
  dplyr::distinct(id_exploit, indicateur, categorie_dexi) %>%
  tidyr::spread(key = indicateur, value = categorie_dexi) %>%
  dplyr::inner_join(node_44, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("B19", "B6", "Par le travail et l'emploi")) %>%
  dplyr::ungroup()

## Node 46

decision_rules <- decision_rules_total$node_46
names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

node_46 <- prop_data %>%
  dplyr::distinct(id_exploit) %>%
  dplyr::inner_join(node_42, by = "id_exploit") %>%
  dplyr::inner_join(node_43, by = "id_exploit") %>%
  dplyr::inner_join(node_45, by = "id_exploit") %>%
  dplyr::group_by(id_exploit) %>%
  dplyr::inner_join(decision_rules, by = c("Valoriser la qualité territoriale", "Contribuer à des démarches d'économie circulaire", "S'inscrire dans des démarches de territoire")) %>%
  dplyr::ungroup()

node_final <- node_10 %>%
  dplyr::inner_join(node_20, by = c("id_exploit", "B16", "B18", "B13", "B15")) %>%
  dplyr::inner_join(node_26, by = c("id_exploit", "B18", "B13", "B15", "C5", "C3")) %>%
  dplyr::inner_join(node_41, by = c("id_exploit", "B16", "B1", "A5", "B14")) %>%
  dplyr::inner_join(node_46, by = c("id_exploit", "B15", "B3", "B14", "B8", "B19"))

end_nodes <- list("Robustesse" = node_10, "Capacité" = node_20, "Autonomie" = node_26, "Responsabilité" = node_41, "Ancrage" = node_46, Global = node_final)

## On supprime les noeuds intermédiaires
rm(list = ls(pattern = "node_"))

res_list$nodes <- end_nodes
res_list$input.type <- "single"
return(res_list)

}
