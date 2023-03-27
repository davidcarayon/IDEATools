#' Run a complete IDEA4 diagnosis
#'
#' This is a wrapper function designed to compute a complete IDEA4 diagnosis with a single function. According to the user's input, functions from IDEATools will be sequentially called to produce the desired output.
#'
#' @param input a character vector with path names to a single file, multiple files or even a directory with IDEA data. File extensions can either be \code{json} or \code{xls(x)}
#' @param output_directory the output directory for the rendered reports and/or plots. Defaults to "IDEATools_output"
#' @param type the type of analysis to perform. Can be "single" for single farm-related results, "group" for group-related results, "group_reference" for anonymous group-related results, or a combination of the latter, provided that the number of farms is at least 3. Note that "group_reference" is a special option designed to work with an online platform using this package (WEBIDEA)
#' @param export_type the type of output to produce. Can be either "report" to produce compiled reports and/or "local" to write raw plots. If NULL, the algorithm will not produce any plots on machine and will return a list with the IDEA results.
#' @param plot_choices the type of plots to be produced. Can be either "dimensions", "trees" or "radars" or a combination of these 3. Ignored if the export type is "report".
#' @param report_format a string indicating the output format if \code{type = "report"}. Can be a single format (e.g \code{"pdf"}) or multiple formats (e.g. \code{c("pdf","xlsx")}). Possible formats are "pdf", "docx", "pptx" and "xlsx"
#' @param prefix a prefix which will be added to output files names. Typically, the name of the farm. Ignored if \code{length(input) > 1} or in the case of a group analysis : the \code{metadata$MTD_01} field will be used to identify each farm.
#' @param dpi ggplot output resolution.
#' @param append In the case of a single excel report and if the input is an xlsx file, should the results be appended to the original file ?
#' @param quiet A command to remove console printing.
#'
#' @return Either reports and/or raw plots in \code{output_directory} or a named list with all the results.
#'
#' @details This function is designed to provide the user a single function to use for a full IDEA4 diagnosis.
#'
#' If the input is a single file, then a simple "\code{read_idea() |> compute_idea() |> plot_idea() |> write_idea()}" or "\code{old_idea() |> plot_idea() |> write_idea()}" pipeline will be used. If export_type is NULL, then the output of \code{plot_idea()} will be returned.
#'
#' If the input is a list of files and/or a directory, and if type is "single", then the single analysis pipelines are iterated over each file. If export_type is NULL, then the multiple outputs of \code{plot_idea()} are gathered in an unique list and returned.
#'
#' If the input is a list of files and/or a directory, and if type is "group*", then the "import" (\code{read_idea() |> compute_idea()} or \code{old_idea()}) pipeline is iterated over each file and the results are gathered in an object of class "IDEA_group_data". This object introduced in the \code{plot_idea() |> write_idea()} pipeline will trigger a new algorithm suited to group analysis. If export_type is NULL, then the output of \code{plot_idea()} will be returned.
#'
#' @export
#'
#' @import data.table
#' @importFrom tibble tibble
#'
#' @examples
#' library(IDEATools)
#' path <- system.file("example_data/idea_example_1.json", package = "IDEATools")
#' group_path <- system.file("example_data", package = "IDEATools")
#'
#' # Find your temporary directory (the output will be there)
#' tempdir <- tempdir()
#'
#' # Run a full individual diagnosis with no export, with only trees
#' my_diagnosis <- diag_idea(
#'   input = path,
#'   output_directory = tempdir,
#'   type = "single",
#'   export_type = NULL,
#'   prefix = "Farm_A",
#'   plot_choices = "trees",
#'   dpi = 20,  ## Can be much higher
#'   quiet = TRUE
#' )
diag_idea <- function(input, output_directory, type = "single", export_type = c("report", "local"), plot_choices = c("dimensions", "trees", "radars"), report_format = "pdf", prefix = NULL, dpi = 320, append = FALSE, quiet = FALSE) {
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
      message("D\u00e9but du diagnostic individuel IDEA4")
      if (any(export_type == "local")) {
        message(paste0("Nombre de types de graphiques demand\u00e9s : ", length(plot_choices)))
      }
      if (any(export_type == "report")) {
        message(paste0("Nombre de rapports demand\u00e9s : ", length(report_format)))
      }
    }

    # Print state
    if (!quiet) {
      message("Import du calculateur")
    }

    ## Try the first pipeline
    test_version <- try(read_idea(input), silent = FALSE)

    ## If the first pipeline fails, try the old_idea one
    if (any(class(test_version) == "try-error")) {
      if (!quiet) {
        message(paste0("Erreur dans l'ex\u00e9cution de la fonction `read_idea(", basename(input), ")`. Tentative de r\u00e9cup\u00e9ration des donn\u00e9es via un algorithme de secours (old_idea) si votre calculateur est juste trop ancien.\n "))
      }

      ## Reading items (and estimating duration)
      start <- Sys.time()

      IDEA_data <- old_idea(input)

      end <- Sys.time()

      duration <- round(difftime(end, start, units = "secs"))

      if (!quiet) {
        message(paste0("Calculateur bien import\u00e9 via `old_idea()` et les 53 indicateurs ont bien \u00e9t\u00e9 calcul\u00e9s (", duration, "s)\n"))
      }
    } else {
      ## Reading items (and estimating duration)
      start <- Sys.time()

      IDEA_items <- read_idea(input)
      end <- Sys.time()

      duration <- round(difftime(end, start, units = "secs"), 2)

      # Confirm success
      if (!quiet) {
        message(paste0("Calculateur bien import\u00e9 (", duration, "s)\n"))
      }

      # Printing state
      if (!quiet) {
        message("Calcul des indicateurs IDEA4")
      }

      ## computing scores (and estimating duration)
      start <- Sys.time()
      IDEA_data <- compute_idea(IDEA_items)
      end <- Sys.time()

      duration <- round(difftime(end, start, units = "secs"))

      # Confirm success
      if (!quiet) {
        message(paste0("Les 53 indicateurs ont bien \u00e9t\u00e9 calcul\u00e9s (", duration, "s)\n"))
      }
    }

    # Printing state
    if (!quiet) {
      message("Trac\u00e9 des graphiques")
    }

    ## Drawing plots (and estimating duration)
    start <- Sys.time()
    IDEA_plots <- plot_idea(IDEA_data, choices = plot_choices)
    end <- Sys.time()

    duration <- round(difftime(end, start, units = "secs"))

    ## Confirm success
    if (!quiet) {
      message(paste0("Les graphiques ont \u00e9t\u00e9 construits sans erreur (", duration, "s)\n"))
    }

    ## If export is NULL, then return a list with the results, Otherwise run write_idea
    if (is.null(export_type)) {
      if (!quiet) {
        global_end <- Sys.time()
        global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)
        message(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
      }

      return(IDEA_plots)
    } else {
      write_idea(IDEA_plots, output_directory,
        type = export_type, prefix = prefix,
        dpi = dpi, report_format = report_format, quiet = quiet, append = append,
        input_file_append = input
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
      message("D\u00e9but du diagnostic IDEA4 multi-individuel")
      message(paste0("Nombre d'exploitations : ", length(files_iter)))
      if (!any(export_type == "report")) {
        message(paste0("Nombre de types de graphiques demand\u00e9s : ", length(plot_choices)))
      }
      if (any(export_type == "report")) {
        message(paste0("Nombre de formats de rapports demand\u00e9s : ", length(report_format)))
      }
    }

    # Initialize empty return list for export = NULL
    return_list <- list()

    # Printing state
    if (!quiet) {
      message("D\u00e9but du traitement s\u00e9quentiel des calculateurs")
    }

    message("Traitement du premier calculateur...\n")

    ## Starting a for loop for each file
    for (i in files_iter) {
      ## Try the first pipeline
      test_version <- try(read_idea(i), silent = FALSE)

      ## Estimating duration
      start <- Sys.time()

      ## If the first pipeline fails, try the old_idea one
      if (any(class(test_version) == "try-error")) {
        if (!quiet) {
          message(paste0("Erreur dans l'ex\u00e9cution de la fonction `read_idea(", basename(i), ")`. Tentative de r\u00e9cup\u00e9ration des donn\u00e9es via un algorithme de secours (old_idea) si votre calculateur est juste trop ancien.\n "))
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
        message(paste0("Calculateur '", basename(i), "' trait\u00e9 (", duration, " min)\n"))
      }
    }

    ## If export is NULL, return the result list created through the for loop.
    if (is.null(export_type)) {
      if (!quiet) {
        global_end <- Sys.time()
        global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)
        message(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
      }

      return(return_list)
    }
  }


  # Group analysis ----------------------------------------------------------

  if ((any(type %in% c("group", "group_reference")) & input_type == "list_files") | (any(type %in% c("group", "group_reference")) & input_type == "directory")) {
    ## Creating the iterating vector according to the input
    if (input_type == "list_files") {
      files_iter <- input
    }
    if (input_type == "directory") {
      files_iter <- list.files(input, full.names = TRUE)
    }

    ## Printing starting info
    if (!quiet) {
      message("D\u00e9but du diagnostic IDEA4 de groupe")
      message(paste0("Nombre d'exploitations : ", length(files_iter)))
      if (any(export_type == "report")) {
        message(paste0("Nombre de formats de rapports demand\u00e9s : ", length(report_format)))
      }
    }

    # Printing state
    if (!quiet) {
      message("D\u00e9but du traitement s\u00e9quentiel des calculateurs")
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
          message(paste0("Erreur dans l'ex\u00e9cution de la fonction `read_idea(", basename(i), ")`. Tentative de r\u00e9cup\u00e9ration des donn\u00e9es via un algorithme de secours (old_idea) si votre calculateur est juste trop ancien.\n "))
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
      IDEA_data$nodes <- lapply(IDEA_data$nodes, function(x) transform(x, farm_id = IDEA_data$metadata$MTD_01))

      ## Creating the group list
      group_list$dataset <- rbind(group_list$dataset, IDEA_data$dataset)
      group_list$metadata <- rbind(group_list$metadata, IDEA_data$metadata)
      group_list$nodes[[IDEA_data$metadata$MTD_01]] <- IDEA_data$nodes

      ## Estimating duration
      end <- Sys.time()
      duration <- round(difftime(end, start, units = "secs"))

      ## Confirm success
      if (!quiet) {
        message(paste0("Calculateur '", basename(i), "' trait\u00e9 (", duration, "s)\n"))
      }
    }

    if (sum(duplicated(group_list$metadata$MTD_01)) > 1) {
      stop("farm ids are duplicated, please use a distict id for each farm")
    }

    ## printing state
    if (!quiet) {
      message("Agr\u00e9gation des r\u00e9sultats")
    }

    # Estimating duration
    start <- Sys.time()


    # Aggregating results for properties
    group_list$nodes <- list(
      "Robustesse" = lapply(group_list$nodes, FUN = function(x) x[[1]]) |> data.table::rbindlist(),
      "Capacite" = lapply(group_list$nodes, FUN = function(x) x[[2]]) |> data.table::rbindlist(),
      "Autonomie" = lapply(group_list$nodes, FUN = function(x) x[[3]]) |> data.table::rbindlist(),
      "Responsabilite" = lapply(group_list$nodes, FUN = function(x) x[[4]]) |> data.table::rbindlist(),
      "Ancrage" = lapply(group_list$nodes, FUN = function(x) x[[5]]) |> data.table::rbindlist(),
      "Global" = lapply(group_list$nodes, FUN = function(x) x[[6]]) |> data.table::rbindlist()
    )

    ## Applying a special class to the list for plot_idea to understand it's a group analysis
    class(group_list) <- c(class(group_list), "IDEA_group_data")

    group_list$nodes$Global[, index := NULL]

    ## Plotting
    IDEA_group_plots <- plot_idea(group_list)

    ## Special class if the output is "reference"
    if (type == "group_reference") {
      class(IDEA_group_plots) <- c(class(IDEA_group_plots), "IDEA_group_plots_ref")
    }


    ## Estimating duration
    end <- Sys.time()
    duration <- round(difftime(end, start, units = "secs"))

    ## Confirm success
    if (!quiet) {
      message(paste0("R\u00e9sultats agr\u00e9g\u00e9s (", duration, "s)\n"))
    }

    ## If export is NULL, then return a list with the results, Otherwise run write_idea
    if (is.null(export_type)) {
      if (!quiet) {
        global_end <- Sys.time()
        global_duration <- round(difftime(global_end, global_start, units = "mins"), 1)
        message(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
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
      message(paste0("Fin du diagnostic IDEA4 (", global_duration, " min)"))
    }
  }
}
