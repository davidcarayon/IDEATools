#' Compute IDEA4 indicators and aggregation metrics
#'
#' Aggregates items from \code{read_idea()} to produce IDEA4 indicators, components, dimensions and properties.
#'
#' @param data an object of class \code{IDEA_items} produced with \code{read_idea()}
#'
#' @return An object of class "IDEA_data" with three attributes :
#'  \describe{
#'   \item{metadata}{a named list containing the 17 metadata entries about the farm}
#'   \item{dataset}{a tibble containing the score computed for the 53 indicators, 13 components and 3 dimensions}
#'   \item{nodes}{a list of tibbles, one per property plus a global one, which all describe the qualitative evaluation obtained for each leaf/node and for the final property.}
#' }
#'
#' @export
#'
#' @details This function is designed to compute IDEA scores for the dimensions and properties approaches. A copy of the decision rules used for the properties approach can be locally exported as an excel file with :
#'
#' \code{IDEATools::show_decision_rules()}
#'
#' Further information about decision rules can be found in this vignette :
#'
#' \code{vignette("decision_rules", package = "IDEATools")}
#'
#' @encoding UTF-8
#'
#' @importFrom dplyr arrange case_when filter pull distinct rowwise mutate ungroup select group_by inner_join bind_cols
#' @importFrom purrr map2_dbl map_dbl pmap_chr map_chr
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stats na.omit
#' @importFrom stringr str_split
#' @importFrom tidyr nest unnest spread
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("idea_example.json", package = "IDEATools")
#' my_data <- read_idea(path)
#' computed_data <- compute_idea(my_data)
compute_idea <- function(data) {

  ## Check if correct input
  if (!any(class(data) == "IDEA_items")) (stop("The input data is not of class 'IDEA_items'"))

  ## Checking the encoding of reference_table in case of a windows user.
  Encoding(reference_table$component) <- "UTF-8"

  ## Extracting metadata from input data
  metadata <- data$metadata

  # 5 Decision rules for indicators using custom functions --------------------------------------------------------

  ## Aggregates items into indicators
  Item2Indic <- function(indicateur, df) {
    df <- df %>% dplyr::arrange(item)

    items <- df$value %>% as.numeric()

    if (indicateur %in% c("A1", "A5", "A7", "A8", "A14", "A19", "B23", "B2")) {
      if (indicateur == "A1") {
        value <- ifelse(items[2] == 4, yes = 4, no = sum(items))
      }

      if (indicateur == "A5") {
        value <- ifelse(metadata$MTD_15 >= 0.75, yes = 5, no = sum(items))
      }

      if (indicateur == "A7") {
        value <- dplyr::case_when(
          metadata$MTD_14 == 0 ~ 0,
          metadata$MTD_14 == 2 ~ round(0.7 * items[1] + 0.3 * items[2] + 1e-10),
          metadata$MTD_14 == 1 ~ as.numeric(items[2])
        )
      }

      if (indicateur == "A8") {
        value <- ifelse(metadata$MTD_15 >= 0.95, yes = 8, no = sum(items))
      }

      if (indicateur == "A14") {
        value <- dplyr::case_when(
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

  ## Scale indicators
  ScaleIndicator <- function(indic, value) {
    max <- reference_table %>%
      dplyr::filter(indic_code == indic) %>%
      dplyr::pull(max_indic)

    # We add 1e-10 to make .5 rounded to above.
    value <- round(value + 1e-10)

    scaled_value <- ifelse(value > max, yes = max, no = value)

    if (scaled_value < 0) {
      scaled_value <- 0
    }

    return(scaled_value)
  }

  ## Re-scales the component value according to the max authorized value
  ScaleComponent <- function(compo, value) {
    max <- reference_table %>%
      dplyr::filter(component_code == compo) %>%
      dplyr::pull(max_compo) %>%
      unique()
    scaled_value <- ifelse(value > max, yes = max, no = value)
    if (scaled_value < 0) {
      scaled_value <- 0
    }
    return(scaled_value)
  }

  ## Calculates the dimension score based on components
  Component2Dimension <- function(df) {
    df %>%
      dplyr::distinct(component, component_value) %>%
      dplyr::pull(component_value) %>%
      sum(na.rm = TRUE)
  }

  ## Converts the unscaled indicator values to qualitative categories according to the DEXi model
  Score2Category <- function(TDEF, DEF, INT, FAV, score) {
    vals <- c(TDEF, DEF, INT, FAV) %>% stats::na.omit()

    if (length(vals) == 4) {
      res <- dplyr::case_when(
        score < DEF ~ "tr\u00e8s d\u00e9favorable",
        score >= DEF & score < INT ~ "d\u00e9favorable",
        score >= INT & score < FAV ~ "interm\u00e9diaire",
        score >= FAV ~ "favorable"
      )
    }

    if (length(vals) == 3) {
      res <- dplyr::case_when(
        score < INT ~ "d\u00e9favorable",
        score >= INT & score < FAV ~ "interm\u00e9diaire",
        score >= FAV ~ "favorable"
      )
    }

    if (length(vals) == 2) {
      res <- dplyr::case_when(
        score < FAV ~ "interm\u00e9diaire",
        score >= FAV ~ "favorable"
      )
    }

    return(res)
  }

  # Compute dimensions ------------------------------------------------------

  computed_dimensions <- data$items %>%
    dplyr::rowwise() %>%
    # separating indicator and item, could be done with dplyr::separate()
    dplyr::mutate(
      indic = stringr::str_split(item, "_")[[1]][1],
      item = stringr::str_split(item, "_")[[1]][2]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(indic, item, value) %>%
    # Computing indicators
    dplyr::group_by(indic) %>%
    # For each indicator, we nest the data and apply our custom functions
    tidyr::nest() %>%
    dplyr::mutate(unscaled_value = purrr::map2_dbl(indic, data, Item2Indic)) %>%
    dplyr::mutate(scaled_value = purrr::map2_dbl(indic, unscaled_value, ScaleIndicator)) %>%
    dplyr::mutate(unscaled_value = round(unscaled_value + 1e-10, 0)) %>% # We add 1e-10 to make .5 rounded to above.
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    # Computing components
    dplyr::inner_join(reference_table, by = c("indic" = "indic_code")) %>%
    dplyr::group_by(component_code) %>%
    dplyr::mutate(component_value = sum(scaled_value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(component_value = purrr::map2_dbl(component_code, component_value, ScaleComponent)) %>%
    # Computing dimensions
    dplyr::group_by(dimension) %>%
    tidyr::nest() %>%
    dplyr::mutate(dimension_value = purrr::map_dbl(data, Component2Dimension)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::ungroup() %>%
    dplyr::select(indic, unscaled_value, scaled_value, dimension_code, component_code, component_value, dimension_value)

  # Compute properties ------------------------------------------------------

  ## Score to category
  computed_categories <- computed_dimensions %>%
    dplyr::inner_join(decision_rules_total$categorisation, by = "indic") %>%
    dplyr::mutate(score_category = purrr::pmap_chr(list(TDEF, DEF, INT, FAV, unscaled_value), Score2Category)) %>%
    dplyr::select(indic, unscaled_value, scaled_value, score_category, dimension_code, component_code, component_value, dimension_value)

  ### Assigning A7 to "NC" if MTD_14 is 0
  if (metadata$MTD_14 == "0") {
    computed_categories <- computed_categories %>%
      dplyr::rowwise() %>%
      dplyr::mutate(score_category = ifelse(indic == "A7", yes = "NC", no = score_category)) %>%
      dplyr::ungroup()
  }

  # Computing nodes ---------------------------------------------------------

  # Custom function to simplify indicator names for joining
  simplify_indicator_name <- function(name) {
    list_indic <- reference_table %>%
      dplyr::filter(level == "indicateur") %>%
      dplyr::pull(indic_code)

    indic <- ifelse(stringr::str_split(name, " ")[[1]][1] %in% list_indic,
      yes = stringr::str_split(name, " ")[[1]][1],
      no = name
    )

    return(indic)
  }

  # Renaming computed_categories for the full pipeline
  prop_data <- computed_categories

  # Robustesse --------------------------------------------------------------

  ## Node 1

  decision_rules <- decision_rules_total$node_1
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_1 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A1", "A3", "A4"))

  ## Node 2

  decision_rules <- decision_rules_total$node_2
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_2 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_1) %>%
    dplyr::inner_join(decision_rules, by = c("A14", "C5", "Diversit\u00e9 de l'organisation spatiale et temporelle"))

  ## Node 3

  decision_rules <- decision_rules_total$node_3
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_3 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("C4", "C7"))

  ## Node 4

  decision_rules <- decision_rules_total$node_4
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_4 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_3) %>%
    dplyr::inner_join(decision_rules, by = c("A2", "Diversit\u00e9 des activit\u00e9s"))

  ## Node 5

  decision_rules <- decision_rules_total$node_5
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_5 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("C8", "C9"))

  ## Node 6

  decision_rules <- decision_rules_total$node_6
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_6 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_5) %>%
    dplyr::inner_join(decision_rules, by = c("A15", "De l'outil de production"))

  ## Node 7

  decision_rules <- decision_rules_total$node_7
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_7 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_4) %>%
    dplyr::bind_cols(node_6) %>%
    dplyr::inner_join(decision_rules, by = c("B22", "En favorisant la diversit\u00e9", "En d\u00e9veloppant l'inertie et les capacit\u00e9s tampon"))

  ## Node 8

  decision_rules <- decision_rules_total$node_8
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_8 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B13", "B15"))

  ## Node 9

  decision_rules <- decision_rules_total$node_9
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_9 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_8) %>%
    dplyr::inner_join(decision_rules, by = c("B16", "B18", "par l'insertion dans les r\u00e9seaux"))

  ## Node 10
  decision_rules <- decision_rules_total$node_10
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_10 <- node_9 %>%
    dplyr::bind_cols(node_7) %>%
    dplyr::bind_cols(node_2) %>%
    dplyr::inner_join(decision_rules, by = c("Augmenter la capacit\u00e9 d'adaptation", "R\u00e9duire la sensibilit\u00e9", "Limiter l'exposition aux al\u00e9as"))

  # Capacit\u00e9 productive et reproductive de biens et services ----------------

  ## Node 11

  decision_rules <- decision_rules_total$node_11
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_11 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A12", "A13", "A5"))


  ## Node 12

  decision_rules <- decision_rules_total$node_12
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_12 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B14", "B15", "B16"))

  ## Node 13

  decision_rules <- decision_rules_total$node_13
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_13 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B13", "B18"))

  ## Node 14

  decision_rules <- decision_rules_total$node_14
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_14 <- node_12 %>%
    dplyr::bind_cols(node_13) %>%
    dplyr::inner_join(decision_rules, by = c("Travail", "Comp\u00e9tences et \u00e9quipements"))

  ## Node 15

  decision_rules <- decision_rules_total$node_15
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_15 <- node_11 %>%
    dplyr::bind_cols(node_14) %>%
    dplyr::inner_join(decision_rules, by = c("Naturelles", "Sociales et humaines"))


  ## Node 16

  decision_rules <- decision_rules_total$node_16
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_16 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B1", "B3"))

  ## Node 17

  decision_rules <- decision_rules_total$node_17
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_17 <- node_16 %>%
    dplyr::bind_cols(node_15) %>%
    dplyr::inner_join(decision_rules, by = c("D\u00e9velopper la capacit\u00e9 alimentaire", "Pr\u00e9server ou cr\u00e9er des ressources pour l'acte de production"))

  ## Node 18

  decision_rules <- decision_rules_total$node_18
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_18 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("C2", "C3"))

  ## Node 19

  decision_rules <- decision_rules_total$node_19
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_19 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_18) %>%
    dplyr::inner_join(decision_rules, by = c("C1", "C10", "Capacit\u00e9 de remboursement"))

  ## Node 20

  decision_rules <- decision_rules_total$node_20
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_20 <- node_17 %>%
    dplyr::bind_cols(node_19) %>%
    dplyr::inner_join(decision_rules, by = c("Capacit\u00e9 \u00e0 produire dans le temps des biens et services remun\u00e9r\u00e9s", "Capacit\u00e9 \u00e0 d\u00e9gager un revenu dans le temps"))

  # Autonomie ---------------------------------------------------------------

  ## Node 21

  decision_rules <- decision_rules_total$node_21
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_21 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B13", "B15", "B18"))

  ## Node 22

  decision_rules <- decision_rules_total$node_22
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_22 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B8", "C5"))

  ## Node 23

  decision_rules <- decision_rules_total$node_23
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_23 <- node_21 %>%
    dplyr::bind_cols(node_22) %>%
    dplyr::inner_join(decision_rules, by = c("Libert\u00e9 de d\u00e9cision organisationnelle", "Libert\u00e9 de d\u00e9cision dans les relations commerciales"))

  ## Node 24

  decision_rules <- decision_rules_total$node_24
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_24 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("C3", "C6"))

  ## Node 25

  decision_rules <- decision_rules_total$node_25
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_25 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A6", "A7", "A8"))

  ## Node 26

  decision_rules <- decision_rules_total$node_26
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_26 <- node_23 %>%
    dplyr::bind_cols(node_24) %>%
    dplyr::bind_cols(node_25) %>%
    dplyr::inner_join(decision_rules, by = c("Disposer d'une libert\u00e9 de d\u00e9cision dans ses choix de gouvernance et de production", "Disposer d'une autonomie financi\u00e8re", "Autonomie dans le processus productif"))

  # Responsabilit\u00e9 Globale --------------------------------------------------

  ## Node 27

  decision_rules <- decision_rules_total$node_27
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_27 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B20", "B5"))

  ## Node 28

  decision_rules <- decision_rules_total$node_28
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_28 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B11", "B19"))


  ## Node 29

  decision_rules <- decision_rules_total$node_29
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_29 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B1", "B2", "B4"))

  ## Node 30

  decision_rules <- decision_rules_total$node_30
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_30 <- node_27 %>%
    dplyr::bind_cols(node_28) %>%
    dplyr::bind_cols(node_29) %>%
    dplyr::inner_join(decision_rules, by = c("Partage et transparence des activit\u00e9s productives", "Ouverture et relation au monde non agricole", "S\u00e9curit\u00e9 alimentaire"))


  ## Node 31

  decision_rules <- decision_rules_total$node_31
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_31 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A10", "A9"))


  ## Node 32

  decision_rules <- decision_rules_total$node_32
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_32 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A11", "C11"))

  ## Node 33

  decision_rules <- decision_rules_total$node_33
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_33 <- node_32 %>%
    dplyr::bind_cols(node_31) %>%
    dplyr::inner_join(decision_rules, by = c("Ressources \u00e9nerg\u00e9tiques et manufactur\u00e9es", "Ressources naturelles"))

  ## Node 34

  decision_rules <- decision_rules_total$node_34
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_34 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B14", "B17"))

  ## Node 35

  decision_rules <- decision_rules_total$node_35
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_35 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B16", "B21"))

  ## Node 36

  decision_rules <- decision_rules_total$node_36
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_36 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A5", "B23"))

  ## Node 37

  decision_rules <- decision_rules_total$node_37
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_37 <- node_34 %>%
    dplyr::bind_cols(node_35) %>%
    dplyr::bind_cols(node_36) %>%
    dplyr::inner_join(decision_rules, by = c("Conditions de travail de la main d'oeuvre", "Conditions de vie et de travail", "Bien \u00eatre de la vie animale"))

  ## Node 38

  decision_rules <- decision_rules_total$node_38
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_38 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A16", "A17", "A18"))

  ## Node 39

  decision_rules <- decision_rules_total$node_39
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_39 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("A19", "B12"))

  ## Node 40

  decision_rules <- decision_rules_total$node_40
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_40 <- node_38 %>%
    dplyr::bind_cols(node_39, by = "id_exploit") %>%
    dplyr::inner_join(decision_rules, by = c("R\u00e9duire les \u00e9missions", "R\u00e9duire l'usage des produits polluants"))

  ## Node 41

  decision_rules <- decision_rules_total$node_41
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_41 <- node_30 %>%
    dplyr::bind_cols(node_33) %>%
    dplyr::bind_cols(node_37) %>%
    dplyr::bind_cols(node_40) %>%
    dplyr::inner_join(decision_rules, by = c("Implications et engagements sociaux", "Partager \u00e9quitablement les ressources", "Contribuer \u00e0 la qualit\u00e9 de vie sur l'exploitation", "R\u00e9duire ses impacts sur la sant\u00e9 et les \u00e9cosyst\u00e8mes"))

  # Ancrage territorial -----------------------------------------------------

  ## Node 42

  decision_rules <- decision_rules_total$node_42
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_42 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B10", "B3"))

  ## Node 43

  decision_rules <- decision_rules_total$node_43
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_43 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B7", "B8", "B9"))


  ## Node 44

  decision_rules <- decision_rules_total$node_44
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_44 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::inner_join(decision_rules, by = c("B14", "B15"))


  ## Node 45

  decision_rules <- decision_rules_total$node_45
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_45 <- prop_data %>%
    dplyr::filter(indic %in% names(decision_rules)) %>%
    dplyr::distinct(indic, score_category) %>%
    tidyr::spread(key = indic, value = score_category) %>%
    dplyr::bind_cols(node_44) %>%
    dplyr::inner_join(decision_rules, by = c("B19", "B6", "Par le travail et l'emploi"))

  ## Node 46

  decision_rules <- decision_rules_total$node_46
  names(decision_rules) <- purrr::map_chr(names(decision_rules), simplify_indicator_name)

  node_46 <- node_42 %>%
    dplyr::bind_cols(node_43) %>%
    dplyr::bind_cols(node_45) %>%
    dplyr::inner_join(decision_rules, by = c("Valoriser la qualit\u00e9 territoriale", "Contribuer \u00e0 des d\u00e9marches d'\u00e9conomie circulaire", "S'inscrire dans des d\u00e9marches de territoire"))


  ## Final node
  node_final <- node_10 %>%
    dplyr::inner_join(node_20, by = c("B16", "B18", "B13", "B15")) %>%
    dplyr::inner_join(node_26, by = c("B18", "B13", "B15", "C5", "C3")) %>%
    dplyr::inner_join(node_41, by = c("B16", "B1", "A5", "B14")) %>%
    dplyr::inner_join(node_46, by = c("B15", "B3", "B14", "B8", "B19"))


  ## Final nodes list
  end_nodes <- list("Robustesse" = node_10, "Capacite" = node_20, "Autonomie" = node_26, "Responsabilite" = node_41, "Ancrage" = node_46, Global = node_final)

  ## Removing intermediate node objects
  rm(list = ls(pattern = "node_"))

  ## Generating output
  output <- list(
    metadata = metadata,
    dataset = computed_categories,
    nodes = end_nodes
  )

  ## Assigning the right class
  class(output) <- c(class(output), "IDEA_data")

  return(output)
}
