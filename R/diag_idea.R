#' Run a complete IDEA4 diagnosis
#'
#' This is a wrapper function designed to compute a complete IDEA4 diagnosis with a single function. According to the user's input, functions from IDEATools will be sequentially called to produce the desired output.
#'
#' @param input a character vector with path names to a single file, multiple files or even a directory with IDEA data. File extensions can either be \code{json} or \code{xls(x)}
#' @param output_directory the output directory for the rendered reports and/or plots. Defaults to "IDEATools_output"
#' @param type the type of analysis to perform. Can be "single" for single farm-related results, "group" for group-related results, or even both c("single","report"), provided that the number of farms is at least 3.
#' @param export_type the type of output to produce. Can be either "report" to produce compiled reports and/or "local" to write raw plots. If NULL, the algorithm will not produce any plots on machine and will return a list with the IDEA results.
#' @param plot_choices the type of plots to be produced. Can be either "dimensions", "trees" or "radars" or a combination of these 3. Ignored if the export type is "report".
#' @param report_format a string indicating the output format if \code{type = "report"}. Can be a single format (e.g \code{"pdf"}) or multiple formats (e.g. \code{c("pdf","docx")}). Possible formats are "pdf","docx","odt","pptx","xlsx" and "html".
#' @param prefix a prefix which will be added to output files names. Typically, the name of the farm. Ignored if \code{length(input) > 1} or in the case of a group analysis : the \code{metadata$MTD_01} field will be used to identify each farm.
#' @param dpi ggplot output resolution.
#' @param quiet A command to remove console printing.
#'
#' @return Either reports and/or raw plots in \code{output_directory} or a named list with all the results.
#'
#' @details This function is designed to provide the user a single function to use for a full IDEA4 diagnosis.
#'
#' If the input is a single file, then a simple "\code{read_idea() %>% compute_idea() %>% plot_idea() %>% write_idea()}" or "\code{old_idea() %>% plot_idea() %>% write_idea()}" pipeline will be used. If export_type is NULL, then the output of \code{plot_idea()} will be returned.
#'
#' If the input is a list of files and/or a directory, and if type is "single", then the single analysis pipelines are iterated over each file. If export_type is NULL, then the multiple outputs of \code{plot_idea()} are gathered in an unique list and returned.
#'
#' If the input is a list of files and/or a directory, and if type is "group", then the "import" (\code{read_idea() %>% compute_idea()} or \code{old_idea()}) pipeline is iterated over each file and the results are gathered in an object of class "IDEA_group_data". This object introduced in the \code{plot_idea() %>% write_idea()} pipeline will trigger a new algorithm suited to group analysis. If export_type is NULL, then the output of \code{plot_idea()} will be returned.
#'
#' Note that group analysis requires a number of unique farms greater or equal to 3.
#'
#' @export
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("idea_example.json", package = "IDEATools")
#' \dontrun{
#'
#' # Find your temporary directory
#' tempdir <- tempdir()
#'
#' # Run a full individual diagnosis with local export, without radars
#' diag_idea(
#'   input = path,
#'   output_directory = tempdir,
#'   type = "single",
#'   export_type = "local",
#'   prefix = "Exploitation_A",
#'   dpi = 300,
#'   plot_choices = c("dimensions", "trees"),
#'   quiet = FALSE
#' )
#'
#' # Run a full individual diagnosis with report export as odt
#' diag_idea(
#'   input = path,
#'   output_directory = tempdir,
#'   type = "single",
#'   export_type = "report",
#'   prefix = "Exploitation_A",
#'   dpi = 300,
#'   report_format = "odt",
#'   quiet = FALSE
#' )
#' }
diag_idea <- function(input, output_directory, type = "single", export_type = c("report", "local"), plot_choices = c("dimensions", "trees", "radars"), report_format = "docx", prefix = NULL, dpi = 320, quiet = FALSE) {

  ## Estimating total duration
  global_start <- Sys.time()

  # Standardizing the input encoding
  Encoding(input) <- "UTF-8"

  # Is the input a file, a file list or a directory ?
  if (length(input) > 1) {
    input_type <- "list_files"
  } else if (dir.exists(input)) {
    input_type <- "directory"
  } else {
    input_type <- "file"
  }

  # Individual analysis -----------------------------------------------------
  if (any(type == "single") & input_type == "file") {

    ## Printing starting info
    if (!quiet) {
      cli::cli_h1("D\u00e9but du diagnostic individuel IDEA4")
      if (any(export_type == "local")) {
        cli::cat_bullet(paste0("Nombre de types de graphiques demand\u00e9s : ", length(plot_choices)), bullet = "info", bullet_col = "blue")
      }
      if (any(export_type == "report")) {
        cli::cat_bullet(paste0("Nombre de rapports demand\u00e9s : ", length(report_format)), bullet = "info", bullet_col = "blue")
      }
    }

    # Print state
    if (!quiet) {
      cli::cli_h2("Import du calculateur")
    }

    ## Try the first pipeline
    test_version <- try(read_idea(input), silent = TRUE)

    ## If the first pipeline fails, try the old_idea one
    if (any(class(test_version) == "try-error")) {
      if (!quiet) {
        cli::cat_bullet("Note : Erreur dans la lecture du calculateur (Utilisez la fonction `read_idea()` pour plus d'informations). Tentative de r\u00e9cup\u00e9ration des donn\u00e9es via un algorithme de secours.\n ", bullet = "warning", bullet_col = "orange", col = "orange")
      }

      ## Reading items (and estimating duration)
      start <- Sys.time()

      IDEA_data <- old_idea(input)

      end <- Sys.time()

      duration <- round(difftime(end, start, units = "secs"))

      if (!quiet) {
        cli::cat_bullet(paste0("Calculateur bien import\u00e9 et les 53 indicateurs ont bien \u00e9t\u00e9 calcul\u00e9s (", duration, "s)\n"), bullet = "tick", bullet_col = "green")
      }
    } else {

      ## Reading items (and estimating duration)
      start <- Sys.time()

      IDEA_items <- read_idea(input)
      end <- Sys.time()

      duration <- round(difftime(end, start, units = "secs"), 2)

      # Confirm success
      if (!quiet) {
        cli::cat_bullet(paste0("Calculateur bien import\u00e9 (", duration, "s)\n"), bullet = "tick", bullet_col = "green")
      }

      # Printing state
      if (!quiet) {
        cli::cli_h2("Calcul des indicateurs IDEA4")
      }

      ## computing scores (and estimating duration)
      start <- Sys.time()
      IDEA_data <- compute_idea(IDEA_items)
      end <- Sys.time()

      duration <- round(difftime(end, start, units = "secs"))

      # Confirm success
      if (!quiet) {
        cli::cat_bullet(paste0("Les 53 indicateurs ont bien \u00e9t\u00e9 calcul\u00e9s (", duration, "s)\n"), bullet = "tick", bullet_col = "green")
      }
    }

    # Printing state
    if (!quiet) {
      cli::cli_h2("Trac\u00e9 des graphiques")
    }

    ## Drawing plots (and estimating duration)
    start <- Sys.time()
    IDEA_plots <- plot_idea(IDEA_data, choices = plot_choices)
    end <- Sys.time()

    duration <- round(difftime(end, start, units = "secs"))

    ## Confirm success
    if (!quiet) {
      cli::cat_bullet(paste0("Les graphiques ont \u00e9t\u00e9 construits sans erreur (", duration, "s)\n"), bullet = "tick", bullet_col = "green")
    }

    ## If export is NULL, then return a list with the results, Otherwise run write_idea
    if (is.null(export_type)) {
      if (!quiet) {
        global_end <- Sys.time()
        global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)
        cli::cli_h1(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
      }

      return(IDEA_plots)
    } else {
      write_idea(IDEA_plots, output_directory,
        type = export_type, prefix = prefix,
        dpi = dpi, report_format = report_format, quiet = quiet
      )
    }
  }


  # Multi-individual analysis -----------------------------------------------
  if ((any(type == "single") & input_type == "list_files") | (any(type == "single") & input_type == "directory")) {

    ## Creating the iterating vector according to the input
    if (input_type == "list_files") {
      files_iter <- input
    }
    if (input_type == "directory") {
      files_iter <- list.files(input, full.names = TRUE)
    }


    ## Printing starting info
    if (!quiet) {
      cli::cli_h1("D\u00e9but du diagnostic IDEA4 multi-individuel")
      cli::cat_bullet(paste0("Nombre d'exploitations : ", length(files_iter)), bullet = "info", bullet_col = "blue")
      if (!any(export_type == "report")) {
        cli::cat_bullet(paste0("Nombre de types de graphiques demand\u00e9s : ", length(plot_choices)), bullet = "info", bullet_col = "blue")
      }
      if (any(export_type == "report")) {
        cli::cat_bullet(paste0("Nombre de formats de rapports demand\u00e9s : ", length(report_format)), bullet = "info", bullet_col = "blue")
      }
    }

    # Initialize empty return list for export = NULL
    return_list <- list()

    # Printing state
    if (!quiet) {
      cli::cli_h2("D\u00e9but du traitement s\u00e9quentiel des calculateurs")
    }

    cli::cat_bullet("Traitement du premier calculateur...\n", bullet = "info", bullet_col = "blue")

    ## Starting a for loop for each file
    for (i in files_iter) {

      ## Try the first pipeline
      test_version <- try(read_idea(i), silent = TRUE)

      ## Estimating duration
      start <- Sys.time()

      ## If the first pipeline fails, try the old_idea one
      if (any(class(test_version) == "try-error")) {
        if (!quiet) {
          cli::cat_bullet("Note : Erreur dans la lecture du calculateur (Utilisez la fonction `read_idea()` pour plus d'informations). Tentative de r\u00e9cup\u00e9ration des donn\u00e9es via un algorithme de secours.\n ", bullet = "warning", bullet_col = "orange", col = "orange")
        }

        # Old alternative
        IDEA_data <- old_idea(i)
      } else {

        ## Full computation pipeline
        IDEA_items <- read_idea(i)
        IDEA_data <- compute_idea(IDEA_items)
      }

      # Plotting
      IDEA_plots <- plot_idea(IDEA_data, choices = plot_choices)

      ## If export is NULL, then return a list with results, otherwise run write_idea
      if (is.null(export_type)) {
        return_list[[IDEA_data$metadata$MTD_01]] <- IDEA_plots
      } else {
        write_idea(IDEA_plots, output_directory,
          type = export_type,
          prefix = IDEA_data$metadata$MTD_01, dpi = dpi, report_format = report_format, quiet = TRUE
        )
      }

      ## Estimating duration
      end <- Sys.time()
      duration <- round(difftime(end, start, units = "mins"), 1)

      ## Confirm success
      if (!quiet) {
        cli::cat_bullet(paste0("Calculateur '", basename(i), "' trait\u00e9 (", duration, " min)\n"), bullet = "tick", bullet_col = "green")
      }
    }

    ## If export is NULL, return the result list created through the for loop.
    if (is.null(export_type)) {
      if (!quiet) {
        global_end <- Sys.time()
        global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)
        cli::cli_h1(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
      }

      return(return_list)

    }
  }


  # Group analysis ----------------------------------------------------------

  ## Error if the input is a single file
  if (any(type == "group") & input_type == "file") {
    stop("Group analysis is not possible with only one file.")
  }

  if ((any(type == "group") & input_type == "list_files") | (any(type == "group") & input_type == "directory")) {

    ## Creating the iterating vector according to the input
    if (input_type == "list_files") {
      files_iter <- input
    }
    if (input_type == "directory") {
      files_iter <- list.files(input, full.names = TRUE)
    }

    ## Checking the number of farms (must be >= 3)
    if (length(unique(files_iter)) < 3) {
      stop("Group analysis requires >= 3 unique farms")
    }

    ## Printing starting info
    if (!quiet) {
      cli::cli_h1("D\u00e9but du diagnostic IDEA4 de groupe")
      cli::cat_bullet(paste0("Nombre d'exploitations : ", length(files_iter)), bullet = "info", bullet_col = "blue")
      if (any(export_type == "report")) {
        cli::cat_bullet(paste0("Nombre de formats de rapports demand\u00e9s : ", length(report_format)), bullet = "info", bullet_col = "blue")
      }
    }

    # Printing state
    if (!quiet) {
      cli::cli_h2("D\u00e9but du traitement s\u00e9quentiel des calculateurs")
    }

    # Initialize empty group list
    group_list <- list(
      metadata = tibble::tibble(),
      dataset = tibble::tibble(),
      nodes = list()
    )

    ## Starting a for loop for each file with read > compute
    for (i in files_iter) {

      # Duration estimation
      start <- Sys.time()

      ## Trying first pipeline
      test_version <- try(read_idea(i), silent = TRUE)

      ## Reading items (and estimating duration)
      start <- Sys.time()

      ## If the first pipeline fails, try the old_idea one
      if (any(class(test_version) == "try-error")) {
        if (!quiet) {
          cli::cat_bullet("Note : Erreur dans la lecture du calculateur (Utilisez la fonction `read_idea()` pour plus d'informations). Tentative de r\u00e9cup\u00e9ration des donn\u00e9es via un algorithme de secours.\n ", bullet = "warning", bullet_col = "orange", col = "orange")
        }

        # Old alternative
        IDEA_data <- old_idea(i)
      } else {

        ## Full computation pipeline
        IDEA_items <- read_idea(i)
        IDEA_data <- compute_idea(IDEA_items)
      }

      ## Adding farm id
      IDEA_data$dataset$farm_id <- IDEA_data$metadata$MTD_01
      IDEA_data$nodes <- purrr::map(IDEA_data$nodes, ~ dplyr::mutate(., farm_id = IDEA_data$metadata$MTD_01))

      ## Creating the group list
      group_list$dataset <- dplyr::bind_rows(group_list$dataset, IDEA_data$dataset)
      group_list$metadata <- dplyr::bind_rows(group_list$metadata, IDEA_data$metadata)
      group_list$nodes[[IDEA_data$metadata$MTD_01]] <- IDEA_data$nodes

      ## Estimating duration
      end <- Sys.time()
      duration <- round(difftime(end, start, units = "secs"))

      ## Confirm success
      if (!quiet) {
        cli::cat_bullet(paste0("Calculateur '", basename(i), "' trait\u00e9 (", duration, "s)\n"), bullet = "tick", bullet_col = "green")
      }
    }

    ## printing state
    if (!quiet) {
      cli::cli_h2("Agr\u00e9gation des r\u00e9sultats")
    }

    # Estimating duration
    start <- Sys.time()


    # Aggregating results for properties
    group_list$nodes <- list(
      "Robustesse" = purrr::map(group_list$nodes, "Robustesse") %>% dplyr::bind_rows(),
      "Capacite" =  purrr::map(group_list$nodes, "Capacite") %>% dplyr::bind_rows(),
      "Autonomie" =  purrr::map(group_list$nodes, "Autonomie") %>% dplyr::bind_rows(),
      "Responsabilite" = purrr::map(group_list$nodes, "Responsabilite") %>% dplyr::bind_rows(),
      "Ancrage" =  purrr::map(group_list$nodes, "Ancrage") %>% dplyr::bind_rows(),
      "Global" =  purrr::map(group_list$nodes, "Global") %>% dplyr::bind_rows()
    )

    ## Applying a special class to the list for plot_idea to understand it's a group analysis
    class(group_list) <- c(class(group_list), "IDEA_group_data")

    ## Plotting
    IDEA_group_plots <- plot_idea(group_list)

    ## Estimating duration
    end <- Sys.time()
    duration <- round(difftime(end, start, units = "secs"))

    ## Confirm success
    if (!quiet) {
      cli::cat_bullet(paste0("R\u00e9sultats agr\u00e9g\u00e9s (", duration, "s)\n"), bullet = "tick", bullet_col = "green")
    }

    ## If export is NULL, then return a list with the results, Otherwise run write_idea
    if (is.null(export_type)) {
      if (!quiet) {
        global_end <- Sys.time()
        global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)
        cli::cli_h1(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
      }

      return(IDEA_group_plots)
    } else {
      write_idea(IDEA_group_plots, output_directory,
        type = export_type, prefix = prefix,
        dpi = dpi, report_format = report_format, quiet = quiet
      )
    }


    ## Global duration
    global_end <- Sys.time()
    global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)


    ## Final printing
    if (!quiet) {
      cli::cli_h1(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
    }
  }
}
