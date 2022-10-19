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
#' @examples
#' library(IDEATools)
#'
#' ## Importing from an old file
#' \dontrun{
#' input <- "path_to_your_old_file.xlsx"
#' computed_data <- old_idea(input)
#' }
#'
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
    max <- reference_list$indic_dim |>
      dplyr::filter(indic_code == indic) |>
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
    max <- reference_list$indic_dim |>
      dplyr::filter(component_code == compo) |>
      dplyr::pull(component_max) |>
      unique()
    scaled_value <- ifelse(value > max, yes = max, no = value)
    if (scaled_value < 0) {
      scaled_value <- 0
    }
    return(scaled_value)
  }

  ## Calculates the dimension score based on components
  Component2Dimension <- function(df) {
    df |>
      dplyr::distinct(component, component_value) |>
      dplyr::pull(component_value) |>
      sum(na.rm = TRUE)
  }

  ## Converts the unscaled indicator values to qualitative categories according to the DEXi model
  Score2Category <- function(data) {
    score <- data$unscaled_value
    TDEF <- data$TDEF
    DEF <- data$DEF
    INT <- data$INT
    FAV <- data$FAV

    vals <-  stats::na.omit(c(TDEF, DEF, INT, FAV))

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

  # Reading metadata --------------------------------------------------------

  ## Extract the appropriate sheet
  saisie_et_calc <- suppressMessages(readxl::read_excel(input, sheet = "Saisie et Calculateur") |>
    janitor::clean_names() |>
    dplyr::select(1:6) |>
    as.data.frame())

  ### Manually defining metadata (will potentially fail for some of them)
  metadata <- list(MTD_00 = NA)
  metadata$MTD_01 <- as.character(saisie_et_calc[4, 2])
  metadata$MTD_02 <- saisie_et_calc[12, 2]
  metadata$MTD_03 <- saisie_et_calc[27, 6]
  metadata$MTD_04 <- saisie_et_calc[27, 2]
  metadata$MTD_05 <- NA
  metadata$MTD_06 <- NA
  metadata$MTD_07 <- NA
  metadata$MTD_08 <- saisie_et_calc |>
    dplyr::filter(i_donnees_generales_et_inventaires_de_lexploitation == "Capital hors foncier: actif net total - valeur des terres (dans immo. corporelles)") |>
    dplyr::pull(x6) |>
    dplyr::first()
  metadata$MTD_09 <- saisie_et_calc |>
    dplyr::filter(i_donnees_generales_et_inventaires_de_lexploitation == "EBE retenu IDEA") |>
    dplyr::pull(x6) |>
    dplyr::first()
  metadata$MTD_10 <- NA
  metadata$MTD_11 <- as.character(saisie_et_calc[6, 2])
  metadata$MTD_12 <- NA
  metadata$MTD_13 <- saisie_et_calc[4, 6] |>
    as.numeric() |>
    as.Date(origin = "1900-01-01") |>
    stringr::str_split("-") |>
    unlist() |>
    dplyr::first() |>
    as.numeric()
  metadata$MTD_14 <- saisie_et_calc |>
    dplyr::filter(i_donnees_generales_et_inventaires_de_lexploitation == "Pr\u00e9sence d'\u00e9levage :") |>
    dplyr::pull(x2) |>
    dplyr::first()
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
    metadata$MTD_01 <- stringr::str_replace_all(file_name_short," ","_")
  }

  # Making sure metadata is of right format and cleaned.
  metadata$MTD_00 <- as.character(metadata$MTD_00)
  metadata$MTD_01 <- as.character(metadata$MTD_01)
  metadata$MTD_02 <- round(as.numeric(metadata$MTD_02), 1)
  metadata$MTD_03 <- round(as.numeric(metadata$MTD_03), 1)
  metadata$MTD_04 <- round(as.numeric(metadata$MTD_04), 1)
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
  AE_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension agro\u00e9cologique") |>
    janitor::clean_names() |>
    dplyr::select(x2) |>
    tidyr::drop_na())

  ## Value
  AE_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension agro\u00e9cologique") |>
    janitor::clean_names() |>
    dplyr::select(x8) |>
    tidyr::drop_na() |>
    dplyr::slice(seq(2, 38, 2)))

  ## Error if not complete
  if (nrow(AE_val) != 19) (stop("The algorithm expects 19 indicators in the 'Dimension agro\u00e9cologique' sheet"))

  AE <- dplyr::bind_cols(AE_lab, AE_val) |>
    dplyr::select(indic = x2, unscaled_value = x8)

  ## Socio-territorial

  ## Label
  ST_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension socio-territoriale") |>
    janitor::clean_names() |>
    dplyr::select(x2) |>
    tidyr::drop_na())

  ## Value
  ST_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension socio-territoriale") |>
    janitor::clean_names() |>
    dplyr::select(x8) |>
    tidyr::drop_na() |>
    dplyr::slice(seq(2, 46, 2)))

  ## Error if not complete
  if (nrow(ST_val) != 23) (stop("The algorithm expects 23 indicators in the 'Dimension socio-territoriale' sheet"))

  ST <- dplyr::bind_cols(ST_lab, ST_val) |>
    dplyr::select(indic = x2, unscaled_value = x8)


  ## Economique

  ## Label
  EC_lab <- suppressMessages(readxl::read_excel(input, sheet = "Dimension \u00e9conomique") |>
    janitor::clean_names() |>
    dplyr::select(x2) |>
    tidyr::drop_na())

  ## Value
  EC_val <- suppressMessages(readxl::read_excel(input, sheet = "Dimension \u00e9conomique") |>
    janitor::clean_names() |>
    dplyr::select(x7) |>
    tidyr::drop_na() |>
    dplyr::slice(seq(2, 22, 2)))

  ## Error if not complete
  if (nrow(EC_val) != 11) (stop("The algorithm expects 11 indicators in the 'Dimension \u00e9conomique' sheet"))

  EC <- dplyr::bind_cols(EC_lab, EC_val) |>
    dplyr::select(indic = x2, unscaled_value = x7)

  # Computing dimensions and properties -------------------------------------

  ## Aggregation of indicators
  computed_dimensions <- dplyr::bind_rows(AE, ST, EC) |>
    dplyr::mutate(unscaled_value = as.numeric(unscaled_value)) |>
    dplyr::inner_join(reference_list$indic_dim, by = c("indic" = "indic_code")) |>
    dplyr::mutate(scaled_value = mapply(indic, unscaled_value, FUN=ScaleIndicator)) |>
    ## Adding indicator values to calculate components
    dplyr::group_by(component_code) |>
    dplyr::mutate(component_value = sum(scaled_value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    ## Scaling components
    dplyr::mutate(component_value = mapply(component_code, component_value, FUN=ScaleComponent)) |>
    ## Adding components for dimension calculation
    dplyr::group_by(dimension) |>
    tidyr::nest() |>
    dplyr::mutate(dimension_value = lapply(data, FUN=Component2Dimension)) |>
    tidyr::unnest(cols = c(data,dimension_value)) |>
    dplyr::ungroup() |>
    dplyr::select(indic, unscaled_value, scaled_value, dimension_code, component_code, component_value, dimension_value)

  ## Score to category
  computed_categories <- computed_dimensions |>
    dplyr::inner_join(decision_rules_total$categorisation, by = "indic") |>
    dplyr::group_by(indic) |>
    tidyr::nest() |>
    dplyr::mutate(score_category = lapply(data, FUN=Score2Category)) |>
    tidyr::unnest(cols = c(data,score_category)) |>
    dplyr::ungroup() |>
    dplyr::select(indic, unscaled_value, scaled_value, score_category, dimension_code, component_code, component_value, dimension_value)

  ### Assigning A7 to "NC" if MTD_14 is 0
  if (metadata$MTD_14 == "0" | metadata$MTD_14 == 0) {
    computed_categories <- computed_categories |>
      dplyr::rowwise() |>
      dplyr::mutate(score_category = ifelse(indic == "A7", yes = "NC", no = score_category)) |>
      dplyr::ungroup()
  }

  # Computing nodes ---------------------------------------------------------

  # Custom function to simplify indicator names for joining
  simplify_indicator_name <- function(name) {
    list_indic <- reference_list$indic_dim |>
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

  node_1 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A1", "A3", "A4"))

  ## Node 2

  decision_rules <- decision_rules_total$node_2

  node_2 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_1) |>
    dplyr::inner_join(decision_rules, by = c("A14", "C5", "R1"))

  ## Node 3

  decision_rules <- decision_rules_total$node_3

  node_3 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("C4", "C7"))

  ## Node 4

  decision_rules <- decision_rules_total$node_4

  node_4 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_3) |>
    dplyr::inner_join(decision_rules, by = c("A2", "R3"))

  ## Node 5

  decision_rules <- decision_rules_total$node_5

  node_5 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("C8", "C9"))

  ## Node 6

  decision_rules <- decision_rules_total$node_6

  node_6 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_5) |>
    dplyr::inner_join(decision_rules, by = c("A15", "R5"))

  ## Node 7

  decision_rules <- decision_rules_total$node_7

  node_7 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_4) |>
    dplyr::bind_cols(node_6) |>
    dplyr::inner_join(decision_rules, by = c("B22", "R4", "R6"))

  ## Node 8

  decision_rules <- decision_rules_total$node_8

  node_8 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B13", "B15"))

  ## Node 9

  decision_rules <- decision_rules_total$node_9

  node_9 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_8) |>
    dplyr::inner_join(decision_rules, by = c("B16", "B18", "R8"))

  ## Node 10
  decision_rules <- decision_rules_total$node_10

  node_10 <- node_9 |>
    dplyr::bind_cols(node_7) |>
    dplyr::bind_cols(node_2) |>
    dplyr::inner_join(decision_rules, by = c("R9", "R7", "R2"))

  # Capacit\u00e9 productive et reproductive de biens et services ----------------

  ## Node 11

  decision_rules <- decision_rules_total$node_11

  node_11 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A12", "A13", "A5"))


  ## Node 12

  decision_rules <- decision_rules_total$node_12

  node_12 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B14", "B15", "B16"))

  ## Node 13

  decision_rules <- decision_rules_total$node_13

  node_13 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B13", "B18"))

  ## Node 14

  decision_rules <- decision_rules_total$node_14

  node_14 <- node_12 |>
    dplyr::bind_cols(node_13) |>
    dplyr::inner_join(decision_rules, by = c("CP2", "CP3"))

  ## Node 15

  decision_rules <- decision_rules_total$node_15

  node_15 <- node_11 |>
    dplyr::bind_cols(node_14) |>
    dplyr::inner_join(decision_rules, by = c("CP1", "CP4"))


  ## Node 16

  decision_rules <- decision_rules_total$node_16

  node_16 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B1", "B3"))

  ## Node 17

  decision_rules <- decision_rules_total$node_17

  node_17 <- node_16 |>
    dplyr::bind_cols(node_15) |>
    dplyr::inner_join(decision_rules, by = c("CP6", "CP5"))

  ## Node 18

  decision_rules <- decision_rules_total$node_18

  node_18 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("C2", "C3"))

  ## Node 19

  decision_rules <- decision_rules_total$node_19

  node_19 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_18) |>
    dplyr::inner_join(decision_rules, by = c("C1", "C10", "CP8"))

  ## Node 20

  decision_rules <- decision_rules_total$node_20

  node_20 <- node_17 |>
    dplyr::bind_cols(node_19) |>
    dplyr::inner_join(decision_rules, by = c("CP7", "CP9"))

  # Autonomie ---------------------------------------------------------------

  ## Node 21

  decision_rules <- decision_rules_total$node_21

  node_21 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B13", "B15", "B18"))

  ## Node 22

  decision_rules <- decision_rules_total$node_22

  node_22 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B8", "C5"))

  ## Node 23

  decision_rules <- decision_rules_total$node_23

  node_23 <- node_21 |>
    dplyr::bind_cols(node_22) |>
    dplyr::inner_join(decision_rules, by = c("AU1", "AU2"))

  ## Node 24

  decision_rules <- decision_rules_total$node_24

  node_24 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("C3", "C6"))

  ## Node 25

  decision_rules <- decision_rules_total$node_25

  node_25 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A6", "A7", "A8"))

  ## Node 26

  decision_rules <- decision_rules_total$node_26

  node_26 <- node_23 |>
    dplyr::bind_cols(node_24) |>
    dplyr::bind_cols(node_25) |>
    dplyr::inner_join(decision_rules, by = c("AU3", "AU4", "AU5"))

  # Responsabilit\u00e9 Globale --------------------------------------------------

  ## Node 27

  decision_rules <- decision_rules_total$node_27

  node_27 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B20", "B5"))

  ## Node 28

  decision_rules <- decision_rules_total$node_28

  node_28 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B11", "B19"))


  ## Node 29

  decision_rules <- decision_rules_total$node_29

  node_29 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B1", "B2", "B4"))

  ## Node 30

  decision_rules <- decision_rules_total$node_30

  node_30 <- node_27 |>
    dplyr::bind_cols(node_28) |>
    dplyr::bind_cols(node_29) |>
    dplyr::inner_join(decision_rules, by = c("RG1", "RG2", "RG3"))


  ## Node 31

  decision_rules <- decision_rules_total$node_31

  node_31 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A10", "A9"))


  ## Node 32

  decision_rules <- decision_rules_total$node_32

  node_32 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A11", "C11"))

  ## Node 33

  decision_rules <- decision_rules_total$node_33

  node_33 <- node_32 |>
    dplyr::bind_cols(node_31) |>
    dplyr::inner_join(decision_rules, by = c("RG6", "RG5"))

  ## Node 34

  decision_rules <- decision_rules_total$node_34

  node_34 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B14", "B17"))

  ## Node 35

  decision_rules <- decision_rules_total$node_35

  node_35 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B16", "B21"))

  ## Node 36

  decision_rules <- decision_rules_total$node_36

  node_36 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A5", "B23"))

  ## Node 37

  decision_rules <- decision_rules_total$node_37

  node_37 <- node_34 |>
    dplyr::bind_cols(node_35) |>
    dplyr::bind_cols(node_36) |>
    dplyr::inner_join(decision_rules,by = c("RG8", "RG9", "RG10"))

  ## Node 38

  decision_rules <- decision_rules_total$node_38

  node_38 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A16", "A17", "A18"))

  ## Node 39

  decision_rules <- decision_rules_total$node_39

  node_39 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("A19", "B12"))

  ## Node 40

  decision_rules <- decision_rules_total$node_40

  node_40 <- node_38 |>
    dplyr::bind_cols(node_39) |>
    dplyr::inner_join(decision_rules, by = c("RG12", "RG13"))

  ## Node 41

  decision_rules <- decision_rules_total$node_41

  node_41 <- node_30 |>
    dplyr::bind_cols(node_33) |>
    dplyr::bind_cols(node_37) |>
    dplyr::bind_cols(node_40) |>
    dplyr::inner_join(decision_rules, by = c("RG4", "RG7", "RG11", "RG14"))

  # Ancrage territorial -----------------------------------------------------

  ## Node 42

  decision_rules <- decision_rules_total$node_42

  node_42 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B10", "B3"))

  ## Node 43

  decision_rules <- decision_rules_total$node_43

  node_43 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B7", "B8", "B9"))


  ## Node 44

  decision_rules <- decision_rules_total$node_44

  node_44 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::inner_join(decision_rules, by = c("B14", "B15"))


  ## Node 45

  decision_rules <- decision_rules_total$node_45

  node_45 <- prop_data |>
    dplyr::filter(indic %in% names(decision_rules)) |>
    dplyr::distinct(indic, score_category) |>
    tidyr::spread(key = indic, value = score_category) |>
    dplyr::bind_cols(node_44) |>
    dplyr::inner_join(decision_rules, by = c("B19", "B6", "AN3"))

  ## Node 46

  decision_rules <- decision_rules_total$node_46

  node_46 <- node_42 |>
    dplyr::bind_cols(node_43) |>
    dplyr::bind_cols(node_45) |>
    dplyr::inner_join(decision_rules, by = c("AN1", "AN2", "AN4"))


  ## Final node
  node_final <- node_10 |>
    dplyr::inner_join(node_20, by = c("B16", "B18", "B13", "B15")) |>
    dplyr::inner_join(node_26, by = c("B18", "B13", "B15", "C5", "C3")) |>
    dplyr::inner_join(node_41, by = c("B16", "B1", "A5", "B14")) |>
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
