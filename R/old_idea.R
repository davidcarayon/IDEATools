#' Read "old" IDEA data files
#'
#' This function is an alternative to the read/compute pipeline for older versions of IDEA data excel files.
#'
#' @param input a system path to the file containing the IDEA data.
#' The file extension has to be xls or xlsx.
#'
#' @return An object of class "IDEA_data" with three attributes :
#'  \describe{
#'   \item{metadata}{a named list containing the 17 metadata entries about the farm}
#'   \item{dataset}{a tibble containing the score computed for the 53 indicators, 13 components and 3 dimensions}
#'   \item{nodes}{a list of tibbles, one per property plus a global one, which all describe the qualitative evaluation}
#' }
#' @export
#'
#' @details This function is designed to import data from "old" IDEA data files. It will most probably work for IDEA4 excel files which are later than 2019-01-01. This actually works because this function focuses on indicators directly computed in the excel file rather than items. The potential drawbacks being that no information about items are collected and that some metadata may be missed.
#'
#' Note : For the farm id metadata, the full First/Last name will be used if found.
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tools file_ext file_path_sans_ext
#' @import data.table
#' @importFrom stats na.omit
#'
#' @examples
#' library(IDEATools)
#'
#' ## Importing from an old IDEA file
#' input <- "path_to_your_old_file.xlsx"
#' if(file.exists(input)) {
#' computed_data <- old_idea(input)
#' }
old_idea <- function(input) {
  # Standardizing the input encoding
  Encoding(input) <- "UTF-8"

  # Is it a file or a directory ?
  if (dir.exists(input)) (stop("input is a directory. Please input valid IDEA4 excel or json file."))

  # Which extension is it ?
  filetype <- tools::file_ext(input)

  # Is it a correct extension ?

  ## No extension
  if (filetype == "") (stop("input has no extension. Please input valid IDEA4 excel"))

  ## json
  if (filetype == "json") (stop("input is a .json file. For json IDEA data files, please use `IDEATools::read_idea()`"))

  ## Other than json/excel
  if (!filetype %in% c("xls", "xlsx")) {
    (stop(paste0(
      "input has the extension '.",
      filetype, "'. Please input valid IDEA4 excel file."
    )))
  }

  # Is it something related to IDEA ? ---------------------------------------

  sheets <- readxl::excel_sheets(input)
  if (!any(sheets == "Notice")) (stop(paste0("The input file '", basename(input), "' has no worksheet named 'Notice'. Please input valid IDEA4 excel file.")))

  # Decision rules for indicators using custom functions --------------------------------------------------------

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

  # Reading metadata --------------------------------------------------------

  ## Extract the appropriate sheet
  saisie_et_calc <- suppressMessages(as.data.frame(readxl::read_excel(input, sheet = "Saisie et Calculateur")[, 1:6]))

  names(saisie_et_calc)[1] <- "donnees_exploit"

  ### Manually defining metadata (will potentially fail for some of them)
  metadata <- list(MTD_00 = NA)
  metadata$MTD_01 <- as.character(saisie_et_calc[4, 2])
  metadata$MTD_02 <- saisie_et_calc[12, 2]
  metadata$MTD_03 <- saisie_et_calc[27, 6]
  metadata$MTD_04 <- saisie_et_calc[27, 2]
  metadata$MTD_05 <- NA
  metadata$MTD_06 <- NA
  metadata$MTD_07 <- NA
  metadata$MTD_08 <- subset(saisie_et_calc, donnees_exploit == "Capital hors foncier: actif net total - valeur des terres (dans immo. corporelles)")[1, 6]
  metadata$MTD_09 <- subset(saisie_et_calc, donnees_exploit == "EBE retenu IDEA")[1, 6]
  metadata$MTD_10 <- NA
  metadata$MTD_11 <- as.character(saisie_et_calc[6, 2])
  metadata$MTD_12 <- NA
  metadata$MTD_13 <- saisie_et_calc[4, 6] |>
    as.numeric() |>
    as.Date(origin = "1900-01-01") |>
    sub(pattern = "\\-.*", replacement = "") |>
    as.numeric()
  metadata$MTD_14 <- subset(saisie_et_calc, donnees_exploit == "Pr\u00e9sence d'\u00e9levage :")[1, 2]
  metadata$MTD_15 <- NA
  metadata$MTD_16 <- NA


  ## Standardizing MTD_14
  if (metadata$MTD_14 == "oui") {
    metadata$MTD_14 <- 1
  }
  if (metadata$MTD_14 == "non") {
    metadata$MTD_14 <- 0
  }
  if (metadata$MTD_14 == "0 - pas d'\u00e9levage") {
    metadata$MTD_14 <- 0
  }
  if (metadata$MTD_14 == "2 - herbivore") {
    metadata$MTD_14 <- 2
  }
  if (metadata$MTD_14 == "1 - monogastrique") {
    metadata$MTD_14 <- 1
  }

  # If no MTD_01, we assign the file name
  if (metadata$MTD_01 %in% c("0", NA)) {
    file_name <- tools::file_path_sans_ext(basename(input))
    file_name_short <- substr(file_name, start = 1, stop = 10) # Limit to 10
    metadata$MTD_01 <- gsub(x = file_name_short, pattern = " ", replacement = "_")
  }

  # Making sure metadata is of right format and cleaned.
  metadata$MTD_00 <- as.character(metadata$MTD_00)
  metadata$MTD_01 <- as.character(metadata$MTD_01)
  metadata$MTD_02 <- round(as.numeric(metadata$MTD_02), 1)
  metadata$MTD_03 <- round(as.numeric(metadata$MTD_03), 1)
  metadata$MTD_04 <- suppressWarnings(round(as.numeric(metadata$MTD_04), 1))
  metadata$MTD_05 <- as.character(metadata$MTD_05)
  metadata$MTD_06 <- as.character(metadata$MTD_06)
  metadata$MTD_07 <- round(as.numeric(metadata$MTD_07), 1)
  metadata$MTD_08 <- round(as.numeric(metadata$MTD_08), 1)
  metadata$MTD_09 <- round(as.numeric(metadata$MTD_09), 1)
  metadata$MTD_10 <- round(as.numeric(metadata$MTD_10), 1)
  metadata$MTD_11 <- as.character(metadata$MTD_11)
  metadata$MTD_12 <- as.character(metadata$MTD_12)
  metadata$MTD_13 <- as.character(metadata$MTD_13)
  metadata$MTD_14 <- as.character(metadata$MTD_14)
  metadata$MTD_15 <- round(as.numeric(metadata$MTD_15), 1)
  metadata$MTD_16 <- as.character(metadata$MTD_16)

  # Reading indicators ------------------------------------------------------

  ## Agroecologie

  ## Label
  AE_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension agro\u00e9cologique")[, 2] |>
    na.omit()) |> unlist(use.names = FALSE)

  ## Value
  AE_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension agro\u00e9cologique"))
  AE_val <- AE_val[!is.na(AE_val[, 8]), ][seq(2, 38, 2), ][, 8] |> unlist(use.names = FALSE)

  ## Error if not complete
  if (length(AE_val) != 19) (stop("The algorithm expects 19 indicators in the 'Dimension agro\u00e9cologique' sheet"))

  AE <- data.frame(indic = AE_lab, unscaled_value = AE_val)

  ## Socio-territorial

  ## Label
  ST_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension socio-territoriale")[, 2] |>
    na.omit()) |> unlist(use.names = FALSE)

  ## Value
  ST_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension socio-territoriale"))
  ST_val <- ST_val[!is.na(ST_val[, 8]), ][seq(2, 46, 2), ][, 8] |> unlist(use.names = FALSE)

  ## Error if not complete
  if (length(ST_val) != 23) (stop("The algorithm expects 23 indicators in the 'Dimension socio-territoriale' sheet"))

  ST <- data.frame(indic = ST_lab, unscaled_value = ST_val)


  ## Economique




  ## Label
  EC_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension \u00e9conomique")[, 2] |>
    na.omit()) |> unlist(use.names = FALSE)

  ## Value
  EC_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension \u00e9conomique"))
  EC_val <- EC_val[!is.na(EC_val[, 7]), ][seq(2, 22, 2), ][, 7] |> unlist(use.names = FALSE)

  ## Error if not complete
  if (length(EC_val) != 11) (stop("The algorithm expects 11 indicators in the 'Dimension \u00e9conomique' sheet"))

  EC <- data.frame(indic = EC_lab, unscaled_value = EC_val)

  # Computing dimensions and properties -------------------------------------

  data.table::setDT(reference_list[["indic_dim"]])

  rbind_data <- rbind(AE, ST, EC) |> as.data.table()

  rbind_data[, unscaled_value := as.numeric(unscaled_value)]
  rbind_data[, scaled_value := mapply(indic, unscaled_value, FUN = ScaleIndicator)]
  rbind_data[, unscaled_value := round(unscaled_value + 1e-10, 0)]

  data_dt_with_indic_dim <- rbind_data[reference_list[["indic_dim"]], on = c("indic" = "indic_code")]
  data_dt_grouped_by_component_code <- data_dt_with_indic_dim[, .(indic, unscaled_value, scaled_value, dimension, component, dimension_code), by = component_code]
  data_dt_grouped_by_component_code[, component_value := sum(scaled_value, na.rm = TRUE), by = component_code]
  data_dt_grouped_by_component_code[, component_value := mapply(component_code, component_value, FUN = ScaleComponent)]

  data_dt_grouped_by_dimension <- data_dt_grouped_by_component_code[, .(data = list(.SD)), by = dimension]
  data_dt_grouped_by_dimension[, dimension_value := lapply(data, FUN = Component2Dimension)]
  data_dt_unnested <- data_dt_grouped_by_dimension[, c("data") := NULL]

  computed_dimensions <- data_dt_grouped_by_component_code[data_dt_unnested, on = "dimension"][, .(indic, unscaled_value, scaled_value, dimension_code, component_code, component_value, dimension_value)]

  data.table::setDT(decision_rules_total[["categorisation"]])

  computed_categories_dt <- computed_dimensions[decision_rules_total[["categorisation"]], on = "indic"]

  computed_categories_dt_grouped <- computed_categories_dt[, .(unscaled_value, scaled_value, dimension_code, component_code, component_value, dimension_value, TDEF, DEF, INT, FAV), by = indic]

  computed_categories_dt_nested <- computed_categories_dt_grouped[, .(data = list(.SD)), by = indic]
  computed_categories_dt_nested[, score_category := lapply(data, FUN = Score2Category)]

  computed_categories_dt_unnested <- computed_categories_dt_nested[, c("data") := NULL]

  computed_categories_dt <- computed_categories_dt_unnested[computed_dimensions, on = "indic"][, .(indic, unscaled_value, scaled_value, score_category, dimension_code, component_code, component_value, dimension_value)]

  ### Assigning A7 to "NC" if MTD_14 is 0
  if (metadata$MTD_14 == "0" | metadata$MTD_14 == 0) {
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
  end_nodes <- list("Robustesse" = node_10, "Capacite" = node_20, "Autonomie" = node_26, "Responsabilite" = node_41, "Ancrage" = node_46, Global = node_final)

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
