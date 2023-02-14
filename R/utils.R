#' Convert IDEA excel files to json, without output
#'
#' @param input the directory containing the xls/xlsx files to convert
#' @param output the directory where the json files will be created
#'
#' @return a list of json files exported to the output directory
#' @import data.table
#' @importFrom jsonlite toJSON
#' @importFrom readxl read_excel
jsonify2 <- function(input, output) {
  ## Collecte du numero de version
  version <- readxl::read_excel(input, sheet = "Notice", range = "K4") |> names()
  version_number <- as.numeric(gsub("[[:punct:] ]+", "", version))

  ## Import metadata
  metadata <- as.list(data.table::as.data.table(readxl::read_excel(input, sheet = "Renvoi BDD", range = "A4:E22"))[, c(1, 4)][, id := 1] |> na.omit() |> data.table::dcast(id ~ Code, value.var = "Valeur"))

  metadata$id <- NULL


  # Standardisation MTD_14
  if (metadata$MTD_14 == "0 - pas d'\u00e9levage" | metadata$MTD_14 == "pas d'\u00e9levage") {
    metadata$MTD_14 <- "0"
  }
  if (metadata$MTD_14 == "2 - herbivore" | metadata$MTD_14 == "herbivore") {
    metadata$MTD_14 <- "2"
  }
  if (metadata$MTD_14 == "1 - monogastrique" | metadata$MTD_14 == "monogastrique") {
    metadata$MTD_14 <- "1"
  }

  # If version < 4.2.5, then we have to convert MTD_15 to a percentage between 0 and 100
  if (version_number < 425) {
    metadata$MTD_15 <- as.numeric(metadata$MTD_15) * 100
  }

  # Only extracting number from the OTEX
  metadata$MTD_06 <- regmatches(as.character(metadata$MTD_06), regexpr("[[:digit:]]+", as.character(metadata$MTD_06)))

  # Only extracting number from the department
  metadata$MTD_11 <- regmatches(as.character(metadata$MTD_11), regexpr("[[:digit:]]+", as.character(metadata$MTD_11)))

  # Making sure metadata is of right format and cleaned.
  metadata$MTD_00 <- as.character(metadata$MTD_00)
  metadata$MTD_01 <- as.character(metadata$MTD_01)
  metadata$MTD_02 <- round(as.numeric(metadata$MTD_02), 1)
  metadata$MTD_03 <- round(as.numeric(metadata$MTD_03), 1)
  metadata$MTD_04 <- round(as.numeric(metadata$MTD_04), 1)
  metadata$MTD_05 <- as.character(metadata$MTD_05)
  metadata$MTD_06 <- as.character(metadata$MTD_06)
  metadata$MTD_07 <- round(as.numeric(metadata$MTD_07), 2)
  metadata$MTD_08 <- round(as.numeric(metadata$MTD_08), 1)
  metadata$MTD_09 <- round(as.numeric(metadata$MTD_09), 1)
  metadata$MTD_10 <- round(as.numeric(metadata$MTD_10), 1)
  metadata$MTD_11 <- as.character(metadata$MTD_11)
  metadata$MTD_12 <- as.numeric(metadata$MTD_12)
  metadata$MTD_13 <- as.numeric(metadata$MTD_13)
  metadata$MTD_14 <- as.numeric(metadata$MTD_14)
  metadata$MTD_15 <- round(as.numeric(metadata$MTD_15), 1)
  metadata$MTD_16 <- as.numeric(metadata$MTD_16)


  ## ITEMS

  if (version_number < 430) {
    items <- data.table::as.data.table(suppressMessages(readxl::read_excel(input, sheet = "Renvoi BDD", range = "A25:E143")))[, .(item = Code, value = `A Exporter`)][, item := gsub(x = item, pattern = "IDEA_", replacement = "")]

    items[, index := 1]

    items_wide <- items[, data.table::dcast(.SD, index ~ item, value.var = "value")]


    # 429 to 430
    item_wide$A03_1 <- max(item_wide$A03_1, -1)
    item_wide$B19_3 <- min(item_wide$B19_3, 3)
    item_wide$A13_1 <- max(item_wide$A13_1, -2)
    item_wide$A14_2 <- max(item_wide$A14_2, -1)
    item_wide$A15_4 <- item_wide$A15_3
    item_wide$A15_3 <- 0
    item_wide$B12_3 <- item_wide$B12_2
    item_wide$B12_2 <- 0
    item_wide$B15_1 <- min(item_wide$B15_1, 3)
    item_wide$B16_1 <- min(item_wide$B16_1, 3)
    item_wide$B16_4 <- max(item_wide$B16_4, -3)
    item_wide$B19_3 <- min(item_wide$B19_3, 3)
    item_wide$B19_4 <- min((item_wide$B19_4 + item_wide$B19_5), 3)
    item_wide$B19_5 <- item_wide$B19_6


    if (item_wide$B20_2 == 6) {
      item_wide$B20_1 <- 6
      item_wide$B20_2 <- 0
    }

    if (item_wide$B20_2 == 0 & item_wide$B20_1 == 4) {
      item_wide$B20_1 <- 4
      item_wide$B20_2 <- 0
    }

    if (item_wide$B20_2 == 0 & item_wide$B20_1 == 2) {
      item_wide$B20_1 <- 0
      item_wide$B20_2 <- 3
    }

    if (item_wide$B20_2 == 0 & item_wide$B20_1 == 0) {
      item_wide$B20_1 <- 0
      item_wide$B20_2 <- 0
    }

    if (item_wide$C09_1 == 4) {
      item_wide$C09_1 <- 3
    }

    item_wide$B20_1 <- min(item_wide$B20_1, 6)
    item_wide$B20_2 <- min(item_wide$B20_2, 3)
    item_wide$C04_2 <- min(item_wide$C04_2, 6)
    item_wide$C09_1 <- min(item_wide$C09_1, 4)
    item_wide$C09_2 <- min(item_wide$C09_2, 4)

    # 430 to 432
    item_wide$A08_1 <- min((item_wide$A08_1 + item_wide$A08_2), 8)
    item_wide$A17_3 <- 0
    item_wide$B01_1 <- min((item_wide$B01_1 + item_wide$B01_3), 6)

    item_wide$A17_3 <- 0
    item_wide$A17_3 <- item_wide$A17_2
    item_wide$A17_2 <- ifelse(item_wide$A17_1 == 3, yes = 1, no = 0)
    item_wide$A17_1 <- min(item_wide$A17_1, 2)
    item_wide$A17_4 <- 0

    old_b16 <- c("2" = item_wide$B16_2, "3" = item_wide$B16_3, "4" = item_wide$B16_4)
    item_wide$B16_2 <- unname(old_b16["4"])
    item_wide$B16_3 <- unname(old_b16["2"])
    item_wide$B16_4 <- unname(old_b16["3"])

    item_wide$A01_1 <- min(item_wide$A01_1, 5)
    item_wide$B12_1 <- min(item_wide$B12_1, 3)


    items <- item_wide[, data.table::melt(.SD, id.vars = "index")][!variable %in% c("A08_2", "B01_3", "B19_6"), ][, .(item = variable, value)]
  }

  if (version_number == 430) {
    items <- suppressMessages(readxl::read_excel(input, sheet = "Renvoi BDD", range = "A25:E144"))

    data.table::setDT(items)

    item_wide <- items[, .(item = Code, value = `A Exporter`, index = 1)][, item := gsub(x = item, pattern = "IDEA_", replacement = "")][, data.table::dcast(.SD, index ~ item, value.var = "value")]

    item_wide$A08_1 <- min((item_wide$A08_1 + item_wide$A08_2), 8)
    item_wide$A17_3 <- 0
    item_wide$B01_1 <- min((item_wide$B01_1 + item_wide$B01_3), 6)

    item_wide$A17_3 <- 0
    item_wide$A17_3 <- item_wide$A17_2
    item_wide$A17_2 <- ifelse(item_wide$A17_1 == 3, yes = 1, no = 0)
    item_wide$A17_1 <- min(item_wide$A17_1, 2)
    item_wide$A17_4 <- 0

    old_b16 <- c("2" = item_wide$B16_2, "3" = item_wide$B16_3, "4" = item_wide$B16_4)
    item_wide$B16_2 <- unname(old_b16["4"])
    item_wide$B16_3 <- unname(old_b16["2"])
    item_wide$B16_4 <- unname(old_b16["3"])

    item_wide$A01_1 <- min(item_wide$A01_1, 5)
    item_wide$B12_1 <- min(item_wide$B12_1, 3)

    items <- item_wide[, data.table::melt(.SD, id.vars = "index")][!variable %in% c("A08_2", "B01_3"), ][, .(item = variable, value)]
  }

  if (version_number == 431) {
    items <- suppressMessages(readxl::read_excel(input, sheet = "Renvoi BDD", range = "A25:E143"))

    data.table::setDT(items)

    item_wide <- items[, .(item = Code, value = `A Exporter`, index = 1)][, item := gsub(x = item, pattern = "IDEA_", replacement = "")][, data.table::dcast(.SD, index ~ item, value.var = "value")]

    item_wide$A17_3 <- item_wide$A17_2
    item_wide$A17_2 <- ifelse(item_wide$A17_1 == 3, yes = 1, no = 0)
    item_wide$A17_1 <- min(item_wide$A17_1, 2)
    item_wide$A17_4 <- 0
    item_wide$A19_2 <- max(item_wide$A19_2, -1)

    old_b16 <- c("2" = item_wide$B16_2, "3" = item_wide$B16_3, "4" = item_wide$B16_4)
    item_wide$B16_2 <- unname(old_b16["4"])
    item_wide$B16_3 <- unname(old_b16["2"])
    item_wide$B16_4 <- unname(old_b16["3"])


    item_wide$A01_1 <- min(item_wide$A01_1, 5)
    item_wide$B12_1 <- min(item_wide$B12_1, 3)

    items <- item_wide[, data.table::melt(.SD, id.vars = "index")][, .(item = variable, value)]
  }

  if (version_number == 432) {
    items <- suppressMessages(readxl::read_excel(input, sheet = "Renvoi BDD", range = "A25:E144"))

    data.table::setDT(items)

    item_wide <- items[, .(item = Code, value = `A Exporter`, index = 1)][, item := gsub(x = item, pattern = "IDEA_", replacement = "")][, data.table::dcast(.SD, index ~ item, value.var = "value")]

    item_wide$A01_1 <- min(item_wide$A01_1, 5)
    item_wide$B12_1 <- min(item_wide$B12_1, 3)

    items <- item_wide[, data.table::melt(.SD, id.vars = "index")][, .(item = variable, value)]
  }


  if (version_number > 432) {
    items <- suppressMessages(readxl::read_excel(input, sheet = "Renvoi BDD", range = "A25:E144"))

    data.table::setDT(items)

    items <- items[, .(item = Code, value = `A Exporter`, index = 1)][, item := gsub(x = item, pattern = "IDEA_", replacement = "")]
  }

  ### Formatage vers du JSON

  items_clean <- items[, item := paste0("IDEA_", item)]

  items_json <- as.list(items_clean$value)
  names(items_json) <- items_clean$item

  ## Rajout d'une MTD_17 si inexistante
  if (version_number < 433) {
    metadata$MTD_17 <- as.numeric(0)
  }

  output_data <- list(
    metadonnees = metadata,
    items = items_json
  )

  file <- jsonlite::toJSON(output_data, pretty = TRUE, auto_unbox = TRUE)

  ## Export du fichier
  write(file, output)
}
