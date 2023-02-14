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
#' @import data.table
#' @importFrom stats na.omit
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_data/idea_example_1.json", package = "IDEATools")
#' my_data <- read_idea(path)
#' computed_data <- compute_idea(my_data)
compute_idea <- function(data) {
  ## Check if correct input
  if (!any(class(data) == "IDEA_items")) (stop("The input data is not of class 'IDEA_items'"))

  ## Checking the encoding of reference_table in case of a windows user.
  Encoding(reference_list$indic_dim$component) <- "UTF-8"

  ## Extracting metadata from input data
  metadata <- data$metadata
  version <- metadata$MTD_00
  version_number <- as.numeric(gsub("[[:punct:] ]+", "", version))


  # 5 Decision rules for indicators using custom functions --------------------------------------------------------

  ## Aggregates items into indicators
  Item2Indic <- function(indicateur, df) {
    df <- df[order(df$item), ]

    items <- df$value |> as.numeric()

    if (indicateur %in% c("A1", "A5", "A7", "A8", "A14", "A19", "B23", "B2")) {
      if (indicateur == "A1") {
        value <- ifelse(items[2] == 4, yes = 4, no = sum(items))
      }

      if (indicateur == "A5") {
        if (version_number < 430) {
          value <- ifelse(metadata$MTD_15 >= 0.75, yes = 5, no = sum(items))
        } else {
          value <- sum(items)
        }
      }

      if (indicateur == "A7") {
        value <- ifelse(metadata$MTD_14 == 0, 0,
          ifelse(metadata$MTD_14 == 2, round(0.7 * items[1] + 0.3 * items[2] + 1e-10),
            as.numeric(items[2])
          )
        )
      }

      if (indicateur == "A8") {
        if (version_number <= 430) {
          value <- ifelse(metadata$MTD_15 >= 0.95, yes = 8, no = sum(items))
        } else {
          value <- sum(items)
        }
      }

      if (indicateur == "A14") {
        value <- ifelse(metadata$MTD_16 == 0, 4,
          ifelse(metadata$MTD_14 == 0, as.numeric(items[1]),
            ifelse(metadata$MTD_14 != 0 & metadata$MTD_16 != 0, min(as.numeric(items)), NA)
          )
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
    max <- unique(reference_list$indic_dim[reference_list$indic_dim$indic_code == indic, "max_indic"]) |>
      unlist(use.names = FALSE)


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
    max <- unique(reference_list$indic_dim[reference_list$indic_dim$component_code == compo, "component_max"]) |>
      unlist(use.names = FALSE)

    scaled_value <- ifelse(value > max, yes = max, no = value)
    if (scaled_value < 0) {
      scaled_value <- 0
    }
    return(scaled_value)
  }

  ## Calculates the dimension score based on components
  Component2Dimension <- function(df) {
    df <- df |>
      subset(select = c("component", "component_value")) |>
      unique()
    df <- sum(df$component_value, na.rm = TRUE)
  }

  ## Converts the unscaled indicator values to qualitative categories according to the DEXi model
  Score2Category <- function(data) {
    score <- data$unscaled_value
    TDEF <- data$TDEF
    DEF <- data$DEF
    INT <- data$INT
    FAV <- data$FAV

    vals <- stats::na.omit(c(TDEF, DEF, INT, FAV))

    if (length(vals) == 4) {
      res <- cut(score, breaks = c(-Inf, DEF, INT, FAV, Inf), labels = c("tr\u00e8s d\u00e9favorable", "d\u00e9favorable", "interm\u00e9diaire", "favorable"), right = FALSE)
    }

    if (length(vals) == 3) {
      res <- cut(score, breaks = c(-Inf, INT, FAV, Inf), labels = c("d\u00e9favorable", "interm\u00e9diaire", "favorable"), right = FALSE)
    }

    if (length(vals) == 2) {
      res <- cut(score, breaks = c(-Inf, FAV, Inf), labels = c("interm\u00e9diaire", "favorable"), right = FALSE)
    }

    return(res)
  }

  # Compute dimensions ------------------------------------------------------

  # Convertir "data" en data.table
  data_dt <- data.table::as.data.table(data$items)

  data_dt[, c("indic", "item") := list(sub("\\_.*", "", item), sub(".*\\_", "", item))]

  data_dt_grouped_by_indic <- data_dt[, .(item, value), by = indic]

  data_dt_nested_by_indic <- data_dt_grouped_by_indic[, .(data = list(.SD)), by = indic]
  data_dt_nested_by_indic[, unscaled_value := mapply(indic, data, FUN = Item2Indic)]
  data_dt_nested_by_indic[, scaled_value := mapply(indic, unscaled_value, FUN = ScaleIndicator)]
  data_dt_nested_by_indic[, unscaled_value := round(unscaled_value + 1e-10, 0)]
  data_dt_nested_by_indic[, c("data") := NULL]

  data.table::setDT(reference_list[["indic_dim"]])
  decision_rules_total <- lapply(decision_rules_total, data.table::as.data.table)

  # Compute components
  data_dt_with_indic_dim <- data_dt_nested_by_indic[reference_list[["indic_dim"]], on = c("indic" = "indic_code")]
  data_dt_grouped_by_component_code <- data_dt_with_indic_dim[, .(indic, unscaled_value, scaled_value, dimension, component, dimension_code), by = component_code]
  data_dt_grouped_by_component_code[, component_value := sum(scaled_value, na.rm = TRUE), by = component_code]
  data_dt_grouped_by_component_code[, component_value := mapply(component_code, component_value, FUN = ScaleComponent)]

  # Compute dimensions
  data_dt_grouped_by_dimension <- data_dt_grouped_by_component_code[, .(data = list(.SD)), by = dimension]
  data_dt_grouped_by_dimension[, dimension_value := lapply(data, FUN = Component2Dimension)]
  data_dt_unnested <- data_dt_grouped_by_dimension[, c("data") := NULL]

  # Selectionner les colonnes finales
  computed_dimensions <- data_dt_grouped_by_component_code[data_dt_unnested, on = "dimension"][, .(indic, unscaled_value, scaled_value, dimension_code, component_code, component_value, dimension_value)]


  # Compute properties ------------------------------------------------------

  ## Score to category

  # Joindre avec "decision_rules_total$categorisation" sur la colonne "indic"
  computed_categories_dt <- computed_dimensions[decision_rules_total[["categorisation"]], on = "indic"]

  # Grouper par "indic"
  computed_categories_dt_grouped <- computed_categories_dt[, .(unscaled_value, scaled_value, dimension_code, component_code, component_value, dimension_value, TDEF, DEF, INT, FAV), by = indic]

  # Inclure les donnees et appliquer la fonction "Score2Category"
  computed_categories_dt_nested <- computed_categories_dt_grouped[, .(data = list(.SD)), by = indic]
  computed_categories_dt_nested[, score_category := lapply(data, FUN = Score2Category)]

  # De-nidifier les colonnes "data" et "score_category"
  computed_categories_dt_unnested <- computed_categories_dt_nested[, c("data") := NULL]

  # Selectionner les colonnes finales
  computed_categories_dt <- computed_categories_dt_unnested[computed_dimensions, on = "indic"][, .(indic, unscaled_value, scaled_value, score_category, dimension_code, component_code, component_value, dimension_value)]


  ### Assigning A7 to "NC" if MTD_14 is 0
  if (metadata$MTD_14 == "0") {
    computed_categories_dt[indic == "A7", score_category := "NC"]
  }

  # Computing nodes ---------------------------------------------------------

  # Renaming computed_categories for the full pipeline
  prop_data <- computed_categories_dt
  decision_rules_total <- lapply(decision_rules_total, data.table::as.data.table)

  # Robustesse --------------------------------------------------------------

  ## Node 1
  node_1 <- prop_data[indic %in% names(decision_rules_total$node_1)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_1, by = c("A1", "A3", "A4"))

  ## Node 2
  node_2 <- prop_data[indic %in% names(decision_rules_total$node_2)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_1, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_2, by = c("A14", "C5", "R1"))

  ## Node 3
  node_3 <- prop_data[indic %in% names(decision_rules_total$node_3)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_3, by = c("C4", "C7"))

  ## Node 4
  node_4 <- prop_data[indic %in% names(decision_rules_total$node_4)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_3, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_4, by = c("A2", "R3"))

  ## Node 5
  node_5 <- prop_data[indic %in% names(decision_rules_total$node_5)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_5, by = c("C8", "C9"))

  ## Node 6
  node_6 <- prop_data[indic %in% names(decision_rules_total$node_6)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_5, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_6, by = c("A15", "R5"))

  ## Node 7
  node_7 <- prop_data[indic %in% names(decision_rules_total$node_7)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_4, on = "index"][node_6, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_7, by = c("B22", "R4", "R6"))

  ## Node 8
  node_8 <- prop_data[indic %in% names(decision_rules_total$node_8)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_8, by = c("B13", "B15"))

  ## Node 9
  node_9 <- prop_data[indic %in% names(decision_rules_total$node_9)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_8, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_9, by = c("B16", "B18", "R8"))

  ## Node 10
  node_10 <- node_9[node_7, on = "index"][node_2, on = "index"] |>
    merge(decision_rules_total$node_10, by = c("R9", "R7", "R2"))

  # Capacit\u00e9 productive et reproductive de biens et services ----------------

  ## Node 11
  node_11 <- prop_data[indic %in% names(decision_rules_total$node_11)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_11, by = c("A12", "A13", "A5"))

  ## Node 12
  node_12 <- prop_data[indic %in% names(decision_rules_total$node_12)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_12, by = c("B14", "B15", "B16"))

  ## Node 13
  node_13 <- prop_data[indic %in% names(decision_rules_total$node_13)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_13, by = c("B13", "B18"))

  ## Node 14
  node_14 <- node_12[node_13, on = "index"] |>
    merge(decision_rules_total$node_14, by = c("CP2", "CP3"))

  ## Node 15
  node_15 <- node_11[node_14, on = "index"] |>
    merge(decision_rules_total$node_15, by = c("CP1", "CP4"))

  ## Node 16
  node_16 <- prop_data[indic %in% names(decision_rules_total$node_16)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_16, by = c("B1", "B3"))

  ## Node 17
  node_17 <- node_16[node_15, on = "index"] |>
    merge(decision_rules_total$node_17, by = c("CP6", "CP5"))

  ## Node 18
  node_18 <- prop_data[indic %in% names(decision_rules_total$node_18)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_18, by = c("C2", "C3"))

  ## Node 19
  node_19 <- prop_data[indic %in% names(decision_rules_total$node_19)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_18, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_19, by = c("C1", "C10", "CP8"))

  ## Node 20
  node_20 <- node_17[node_19, on = "index"] |>
    merge(decision_rules_total$node_20, by = c("CP7", "CP9"))

  # Autonomie ---------------------------------------------------------------

  ## Node 21
  node_21 <- prop_data[indic %in% names(decision_rules_total$node_21)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_21, by = c("B13", "B15", "B18"))

  ## Node 22
  node_22 <- prop_data[indic %in% names(decision_rules_total$node_22)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_22, by = c("B8", "C5"))

  ## Node 23
  node_23 <- node_21[node_22, on = "index"] |>
    merge(decision_rules_total$node_23, by = c("AU1", "AU2"))

  ## Node 24
  node_24 <- prop_data[indic %in% names(decision_rules_total$node_24)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_24, by = c("C3", "C6"))

  ## Node 25
  node_25 <- prop_data[indic %in% names(decision_rules_total$node_25)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_25, by = c("A6", "A7", "A8"))

  ## Node 26
  node_26 <- node_23[node_24, on = "index"][node_25, on = "index"] |>
    merge(decision_rules_total$node_26, by = c("AU3", "AU4", "AU5"))

  # Responsabilit\u00e9 Globale --------------------------------------------------

  ## Node 27
  node_27 <- prop_data[indic %in% names(decision_rules_total$node_27)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_27, by = c("B20", "B5"))

  ## Node 28
  node_28 <- prop_data[indic %in% names(decision_rules_total$node_28)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_28, by = c("B11", "B19"))

  ## Node 29
  node_29 <- prop_data[indic %in% names(decision_rules_total$node_29)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_29, by = c("B1", "B2", "B4"))

  ## Node 30
  node_30 <- node_27[node_28, on = "index"][node_29, on = "index"] |>
    merge(decision_rules_total$node_30, by = c("RG1", "RG2", "RG3"))

  ## Node 31
  node_31 <- prop_data[indic %in% names(decision_rules_total$node_31)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_31, by = c("A10", "A9"))

  ## Node 32
  node_32 <- prop_data[indic %in% names(decision_rules_total$node_32)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_32, by = c("A11", "C11"))

  ## Node 33
  node_33 <- node_32[node_31, on = "index"] |>
    merge(decision_rules_total$node_33, by = c("RG6", "RG5"))

  ## Node 34
  node_34 <- prop_data[indic %in% names(decision_rules_total$node_34)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_34, by = c("B14", "B17"))

  ## Node 35
  node_35 <- prop_data[indic %in% names(decision_rules_total$node_35)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_35, by = c("B16", "B21"))

  ## Node 36
  node_36 <- prop_data[indic %in% names(decision_rules_total$node_36)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_36, by = c("A5", "B23"))

  ## Node 37
  node_37 <- node_34[node_35, on = "index"][node_36, on = "index"] |>
    merge(decision_rules_total$node_37, by = c("RG8", "RG9", "RG10"))

  ## Node 38
  node_38 <- prop_data[indic %in% names(decision_rules_total$node_38)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_38, by = c("A16", "A17", "A18"))

  ## Node 39
  node_39 <- prop_data[indic %in% names(decision_rules_total$node_39)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_39, by = c("A19", "B12"))

  ## Node 40
  node_40 <- node_38[node_39, on = "index"] |>
    merge(decision_rules_total$node_40, by = c("RG12", "RG13"))

  ## Node 41
  node_41 <- node_30[node_33, on = "index"][node_37, on = "index"][node_40, on = "index"] |>
    merge(decision_rules_total$node_41, by = c("RG4", "RG7", "RG11", "RG14"))

  # Ancrage territorial -----------------------------------------------------

  ## Node 42
  node_42 <- prop_data[indic %in% names(decision_rules_total$node_42)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_42, by = c("B10", "B3"))

  ## Node 43
  node_43 <- prop_data[indic %in% names(decision_rules_total$node_43)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_43, by = c("B7", "B8", "B9"))

  ## Node 44
  node_44 <- prop_data[indic %in% names(decision_rules_total$node_44)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_44, by = c("B14", "B15"))

  ## Node 45
  node_45 <- prop_data[indic %in% names(decision_rules_total$node_45)][, .(score_category, index = 1), by = indic][, data.table::dcast(.SD, index ~ indic, value.var = "score_category")][node_44, on = "index"][, lapply(.SD, unlist), by = index][, lapply(.SD, as.character), by = index] |>
    merge(decision_rules_total$node_45, by = c("B19", "B6", "AN3"))

  ## Node 46
  node_46 <- node_42[node_43, on = "index"][node_45, on = "index"] |>
    merge(decision_rules_total$node_46, by = c("AN1", "AN2", "AN4"))

  ## Final node
  node_final <- node_10 |>
    merge(node_20[, c("index") := NULL], by = c("B16", "B18", "B13", "B15")) |>
    merge(node_26[, c("index") := NULL], by = c("B18", "B13", "B15", "C5", "C3")) |>
    merge(node_41[, c("index") := NULL], by = c("B16", "B1", "A5", "B14")) |>
    merge(node_46[, c("index") := NULL], by = c("B15", "B3", "B14", "B8", "B19"))


  ## Final nodes list
  end_nodes <- list("Robustesse" = tibble::tibble(node_10), "Capacite" = tibble::tibble(node_20), "Autonomie" = tibble::tibble(node_26), "Responsabilite" = tibble::tibble(node_41), "Ancrage" = tibble::tibble(node_46), Global = tibble::tibble(node_final))

  ## Removing intermediate node objects
  rm(list = ls(pattern = "node_"))

  ## Generating output
  output <- list(
    metadata = metadata,
    dataset = tibble::tibble(computed_categories_dt),
    nodes = end_nodes
  )

  ## Assigning the right class
  class(output) <- c(class(output), "IDEA_data")

  return(output)
}
