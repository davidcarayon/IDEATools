#' Read IDEA4 items and metadata
#'
#' Reads and imports items and farm metadata from .xls, .xlsx or .json files containing IDEA4 data.
#'
#' @param input a system path to the file containing the IDEA data.
#' The file extension can either be \code{json} or \code{xls(x)}. For the latter, the version number must be >= 4.2.0
#'
#' @return An object of class \code{IDEA_items} with two attributes :
#'  \describe{
#'   \item{metadata}{a named list containing the 17 metadata entries about the farm}
#'   \item{items}{a tibble with the extracted 118 items found in the input}
#' }
#' @export
#'
#' @details This function is designed to import items and farm metadata from a single IDEA data file. Errors will be produced if the input file does not contain any "metadonnees" field in the case of a json file or any "Notice" sheet in the case of an excel file.
#'
#' The R code has been developed according to the newest versions of IDEA data collecting files (version numbers >= 4.2.0)  and will produce an error if the version number is lower than 4.2.0 or can't be found in the 'Notice$K4' cell of the input in the case of excel input. There are no limitations for json input files as they were introduced after version number 4.2.0.
#'
#' For some older versions (from about 2019-01-01), you can replace the " \code{read_idea() |>  compute_idea()} " pipeline by "\code{old_idea()}" which will focus on indicators rather than items.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tibble tibble
#' @importFrom tools file_ext
#' @importFrom stats na.omit reshape setNames
#' @examples
#' library(IDEATools)
#' path <- system.file("example_data/idea_example_1.json", package = "IDEATools")
#' my_data <- read_idea(path)
#' my_data
read_idea <- function(input) {
  # Standardizing the input encoding
  Encoding(input) <- "UTF-8"

  # Is it a file or a directory ?
  if (dir.exists(input)) (stop("input is a directory. Please input valid IDEA4 excel or json file."))

  # Which extension is it ?
  filetype <- tools::file_ext(input)

  # Is it a correct extension ?
  if (filetype == "") (stop("input has no extension. Please input valid IDEA4 excel or json file."))
  if (!filetype %in% c("xls", "xlsx", "json")) {
    (stop(paste0(
      "input has the extension '.",
      filetype, "'. Please input valid IDEA4 excel or json file."
    )))
  }


  # Is it something related to IDEA ? ---------------------------------------

  ## json files
  if (filetype == "json") {
    json_file <- jsonlite::fromJSON(input)
    if (!any(names(json_file) == "metadonnees")) (stop(paste0("The input file '", basename(input), "' has no field named 'metadonnees'. Please input valid IDEA4 json file.")))
  }

  ## Excel files
  if (filetype %in% c("xls", "xlsx")) {
    sheets <- readxl::excel_sheets(input)
    if (!any(sheets == "Notice")) (stop(paste0("The input file '", basename(input), "' has no worksheet named 'Notice'. Please input valid IDEA4 excel file.")))
  }


  # Which version is it ? ---------------------------------------------------

  ## json files
  if (filetype == "json") {
    version_number <- json_file$metadonnees$MTD_00
  }

  ## Excel files
  if (filetype %in% c("xls", "xlsx")) {
    version_number <- readxl::read_excel(input, sheet = "Notice", range = "K4") |> names()
    if (length(version_number) == 0) (version_number <- "old")
  }

  ## Overall version_number converted to numeric for "if" statements
  version_number <- ifelse(version_number == "old",
    yes = "Unknown",
    no = as.numeric(gsub("[[:punct:] ]+", "", version_number))
  )

  ## Error if not the appropriate version
  if (version_number == "Unknown" | version_number < 420) (stop(paste0("The input file '", basename(input), "' is too old for being processed by IDEATools: \n Version = ", version_number, " and needs >=420.\n Please use `IDEATools::old_idea()` for an alternative pathway.")))

  # Finding metadata -----------------------------------------------------------

  ## json files
  if (filetype == "json") {
    ## Extract metadata in a named list
    metadata <- json_file$metadonnees

    # If version > 4.2.5, then we have to convert MTD_15 to a ratio between 0 and 1 rather than a percentage
    if (version_number >= 425) {
      metadata$MTD_15 <- metadata$MTD_15 / 100
    }

    # If no MTD_01, we assign the file name
    if (metadata$MTD_01 %in% c("0", NA)) {
      file_name <- tools::file_path_sans_ext(basename(input))
      file_name_short <- substr(file_name, start = 1, stop = 10) # Limit to 10
      metadata$MTD_01 <- gsub(x = file_name_short, pattern = " ", replacement = "_")
    }

    # Standardizing the MTD_14 field
    if (metadata$MTD_14 == "0 - pas d'\u00e9levage") {
      metadata$MTD_14 <- "0"
    }
    if (metadata$MTD_14 == "2 - herbivore") {
      metadata$MTD_14 <- "2"
    }
    if (metadata$MTD_14 == "1 - monogastrique") {
      metadata$MTD_14 <- "1"
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

    ## Special case for newer versions
    if (version_number >= 433) {
      metadata$MTD_17 <- as.character(metadata$MTD_17)

      ## Error if not complete
      if (length(metadata) != 18) (stop(paste0("The metadonnees field in '", basename(input), "' has ", length(metadata), " entries but expects 18.")))
    } else {
      ## Error if not complete
      if (length(metadata) != 17) (stop(paste0("The metadonnees field in '", basename(input), "' has ", length(metadata), " entries but expects 17.")))
    }
  }

  ## excel files
  if (filetype %in% c("xls", "xlsx")) {
    ## Reading metadata from "Renvoi BDD"
    metadata <- readxl::read_excel(input, sheet = "Renvoi BDD", range = "A4:E22")[, c(1, 4)] |>
      subset(!is.na(Code)) |>
      transform(id = 1) |>
      stats::reshape(direction = "wide", idvar = "id", timevar = "Code", v.names = "Valeur") |>
      as.list()

    names(metadata) <- unlist(lapply(names(metadata), gsub, pattern = "Valeur\\.", replacement = ""))
    attr(metadata, "reshapeWide") <- NULL
    metadata[["id"]] <- NULL


    ## In the rare case where the version number wasn't transfered to the "Renvoi BDD" sheet, extract it in
    ## "Notice"
    if (metadata$MTD_00 == 0) {
      metadata$MTD_00 <- names(readxl::read_excel(input, sheet = "Notice", range = "K4"))
    }

    # Standardizing the MTD_14 field
    if (metadata$MTD_14 == "0 - pas d'\u00e9levage" | metadata$MTD_14 == "pas d'\u00e9levage") {
      metadata$MTD_14 <- "0"
    }
    if (metadata$MTD_14 == "2 - herbivore" | metadata$MTD_14 == "herbivore") {
      metadata$MTD_14 <- "2"
    }
    if (metadata$MTD_14 == "1 - monogastrique" | metadata$MTD_14 == "monogastrique") {
      metadata$MTD_14 <- "1"
    }

    # If version > 4.2.5, then we have to convert MTD_15 to a ratio between 0 and 1 rather than a percentage
    if (version_number >= 425) {
      metadata$MTD_15 <- as.numeric(metadata$MTD_15) / 100
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


    ## Special case for newer versions
    if (version_number >= 433) {
      metadata$MTD_17 <- as.character(metadata$MTD_17)

      ## Error if not complete
      if (length(metadata) != 18) (stop(paste0("The 18-rows dataframe for metadata in 'Renvoi BDD' can't be found in range A4:E22 for the file '", basename(input), "'.")))
    } else {
      ## Error if not complete
      if (length(metadata) != 17) (stop(paste0("The 17-rows dataframe for metadata in 'Renvoi BDD' can't be found in range A4:E21 for the file '", basename(input), "'.")))
    }
  }

  ## Final check : checking the presence of crucial metadata needed for indicator calculation.
  if (any(metadata[15:17] == "Iconnu(e)")) (stop(paste0("The algorithm could not find crucial metadata fields in file ", basename(input), "'.")))

  # Finding items -----------------------------------------------------------
  if (filetype == "json") {
    items <- as.data.frame(json_file$items) |>
      stats::reshape(direction = "long", varying = list(names(as.data.frame(json_file$items))), v.names = c("value"), timevar = "item") |>
      transform(item = names(json_file$items)) |>
      transform(item = gsub(x = item, pattern = "(?<=[[:upper:]])0", replacement = "", perl = TRUE)) |> # Convert A01 to A1
      transform(item = gsub(x = item, pattern = "IDEA_", replacement = "")) |>
      subset(!is.na(item)) |>
      subset(select = -id)

    ## Error if not complete
    if (sum(is.na(items$value)) > 0) {
      stop(
        paste0("The items field in '", basename(input), "' has missing values")
      )
    }
  }

  if (filetype %in% c("xls", "xlsx")) {
    ## Change area if version > 433
    start_row <- ifelse(version_number < 433, yes = 25, no = 26)
    end_row <- ifelse(version_number < 433, yes = 144, no = 145)
    range <- paste0("A", start_row, ":E", end_row)

    items <- suppressMessages(readxl::read_excel(input, sheet = "Renvoi BDD", range = range))[, c("Code", "A Exporter")] |>
      stats::setNames(c("item", "value")) |>
      transform(item = gsub(x = item, pattern = "(?<=[[:upper:]])0", replacement = "", perl = TRUE)) |>
      transform(item = gsub(x = item, pattern = "IDEA_", replacement = "")) |>
      subset(!is.na(item))

    ## Error if not complete
    if (sum(is.na(items$value)) > 0) {
      stop(
        paste0("The items field in '", basename(input), "' has missing values")
      )
    }
  }

  ## Building the output list
  output <- list(
    metadata = metadata,
    items = tibble::tibble(items)
  )

  class(output) <- c(class(output), "IDEA_items")

  return(output)
}
