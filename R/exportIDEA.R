#' Export IDEA related plots to local folder
#'
#' @param IDEAres output of any IDEA plotting functions (either `Maketree()`, `dimensionPlots()` or `radarPlots()`)
#' @param outdir the output directory
#' @param svg Should an svg output be exported for trees ?
#'
#' @return Exports plots in png/pdf/svg format in the selected output directory
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when filter mutate rowwise ungroup n_distinct recode
#' @importFrom ggplot2 ggsave
#' @importFrom glue glue
#' @importFrom purrr map pwalk
#' @importFrom rsvg rsvg_png rsvg_pdf
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @export
#' @examples
#' library(IDEATools)
#' path <- system.file("example_json.json", package = "IDEATools")
#' IDEAdata <- importIDEA(path, anonymous = FALSE)
#' IDEAres <- dimensionsPlots(IDEAdata)
#' exportIDEA(IDEAres, outdir = "tmp")
exportIDEA <- function(IDEAres, outdir = paste0("RES_", Sys.Date()), svg = FALSE) {

  # On va toujours créer un dossier par exploitation
  # On stockera les graphes "meta" à la racine

  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }

  # Heuristic maps ----------------------------------------------------------
  if (IDEAres$plot.type == "tree") {
    convert_prop <- function(prop) {
      if (prop %in% c("Capacité", "Responsabilité", "Ancrage")) {
        newprop <- case_when(
          prop == "Capacité" ~ "Capacité productive et reproductive de biens et de services",
          prop == "Responsabilité" ~ "Responsabilité globale",
          prop == "Ancrage" ~ "Ancrage Territorial"
        )

        return(newprop)
      } else {
        return(prop)
      }
    }


    tab_res <- tibble(name = names(IDEAres), itemlist = IDEAres) %>%
      filter(!name %in% c("input.type", "plot.type")) %>%
      mutate(prop = map(itemlist, names)) %>%
      unnest(c(itemlist, prop)) %>%
      # mutate(prop = stri_trans_general(prop,"Latin-ASCII")) %>%
      mutate(name = str_replace_all(name, " ", "_")) %>%
      mutate(folder = file.path(outdir, name, "Propriétés", "Arbres_éclairés")) %>%
      rowwise() %>%
      mutate(prop_new = convert_prop(prop)) %>%
      ungroup() %>%
      mutate(path = file.path(outdir, name, "Propriétés", "Arbres_éclairés", glue("{name}_{prop_new}"))) %>%
      mutate(svg_path = glue("{path}.svg")) %>%
      mutate(
        png_path = glue("{path}.png"),
        pdf_path = glue("{path}.pdf")
      )

    export_heuristic_map <- function(prop, itemlist, folder, png_path, pdf_path, svg_path) {
      heuristic_res <- list(
        Robustesse = c(1072, 767),
        Capacité = c(1193, 652),
        Autonomie = c(1073, 601),
        Responsabilité = c(1063, 674),
        Ancrage = c(1072, 601),
        Global = c(1488, 1052),
        Global_zoom = c(829, 558)
      )


      dim <- heuristic_res[[prop]]

      if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
      }
      writeLines(itemlist, svg_path)
      rsvg_png(svg_path, png_path, width = dim[1], height = dim[2])
      rsvg_pdf(svg_path, pdf_path)

      if (svg == FALSE) {
        file.remove(svg_path)
      }
    }

    pwalk(.l = list(tab_res$prop, tab_res$itemlist, tab_res$folder, tab_res$png_path, tab_res$pdf_path, tab_res$svg_path), .f = export_heuristic_map)
  }

  # Dimension plots ---------------------------------------------------------
  if (IDEAres$plot.type == "dim") {
    dimension_res <- list(
      dimensions = c(9.11, 5.6),
      composantes = c(13.69, 10.5),
      `indic_Socio-Territoriale` = c(10.69, 13.5),
      indic_Economique = c(10.69, 9),
      indic_Agroécologique = c(10.69, 12)
    )

    n_exploit <- n_distinct(names(IDEAres)) - 2

    tab_res <- tibble(name = names(IDEAres), plot = IDEAres) %>%
      filter(!name %in% c("input.type", "plot.type")) %>%
      mutate(plotname = map(plot, names)) %>%
      unnest(c(plot, plotname)) %>%
      mutate(plotname = recode(plotname, "dimensions" = "Dimensions", "composantes" = "Composantes", "indic_Socio-Territoriale" = "Indicateurs Socio-Territoriaux", "indic_Economique" = "Indicateurs Economiques", "indic_Agroécologique" = "Indicateurs Agroécologiques")) %>%
      mutate(
        widths = rep(c(9.11, 13.69, 10.69, 10.69, 10.69), n_exploit),
        heights = rep(c(5.6, 10.5, 13.5, 9, 12), n_exploit)
      ) %>%
      mutate(name = str_replace_all(name, " ", "_")) %>%
      mutate(folder = file.path(outdir, name, "Dimensions")) %>%
      mutate(path = file.path(outdir, name, "Dimensions", glue("{name}_{plotname}"))) %>%
      mutate(png_path = glue("{path}.png"))

    export_dimplot <- function(plotname, plot, widths, heights, folder, png_path) {
      if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
      }

      print(plot) %>%
        ggsave(filename = png_path, dpi = "retina", width = widths, height = heights)
    }

    pwalk(.l = list(tab_res$plotname, tab_res$plot, tab_res$widths, tab_res$heights, tab_res$folder, tab_res$png_path), .f = export_dimplot)
  }

  # Radar plots ---------------------------------------------------------
  if (IDEAres$plot.type == "radar") {
    dimension_radar <- list(
      `Responsabilité globale` = c(16.1, 7.61),
      `Ancrage Territorial` = c(16.1, 7.61),
      Autonomie = c(16.1, 7.61),
      Robustesse = c(16.1, 5.61),
      `Capacité productive et reproductive \nde biens et de services` = c(16.1, 7.61)
    )

    n_exploit <- n_distinct(names(IDEAres)) - 2

    tab_res <- tibble(name = names(IDEAres), plot = IDEAres) %>%
      filter(!name %in% c("input.type", "plot.type")) %>%
      mutate(plotname = map(plot, names)) %>%
      unnest(c(plot, plotname)) %>%
      mutate(plotname = recode(plotname, "Capacité productive et reproductive \nde biens et de services" = "Capacité productive et reproductive de biens et de services")) %>%
      mutate(
        widths = rep(c(16.1, 16.1, 16.1, 16.1, 16.1), n_exploit),
        heights = rep(c(7.61, 7.61, 7.61, 7.61, 7.61), n_exploit)
      ) %>%
      mutate(name = str_replace_all(name, " ", "_")) %>%
      mutate(folder = file.path(outdir, name, "Propriétés")) %>%
      mutate(path = file.path(outdir, name, "Propriétés", glue("{name}_Radar_{plotname}"))) %>%
      mutate(png_path = glue("{path}.png"))

    export_radarplot <- function(plotname, plot, widths, heights, folder, png_path) {
      if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
      }

      print(plot) %>%
        ggsave(filename = png_path, dpi = "retina", width = widths, height = heights)
    }

    pwalk(.l = list(tab_res$plotname, tab_res$plot, tab_res$widths, tab_res$heights, tab_res$folder, tab_res$png_path), .f = export_radarplot)
  }

  # meta plots ---------------------------------------------------------
  if (IDEAres$plot.type == "meta") {
    n_exploit <- IDEAres$n_exploit

    dimension_res <- list(
      metaProp = c(10.4, 6.82),
      metaIndic = c(8.8, 10),
      metaDim = c(10.5, 8.61)
    )


    tab_res <- tibble(plotname = names(IDEAres), plot = IDEAres) %>%
      filter(!plotname %in% c("input.type", "plot.type", "n_exploit")) %>%
      mutate(plotname = recode(plotname, "metaProp" = "Matrice_Propriétés", "metaIndic" = "Matrice_Indicateurs", "metaDim" = "Hist_Dimensions")) %>%
      mutate(
        widths = c(10.4, 8.8, 10.5),
        heights = c(6.82, 10, 8.61)
      ) %>%
      mutate(plotname = str_replace_all(plotname, " ", "_")) %>%
      mutate(folder = file.path(outdir, plotname)) %>%
      mutate(path = file.path(outdir, plotname)) %>%
      mutate(png_path = glue("{path}.png"))

    export_metaplot <- function(plotname, plot, widths, heights, png_path) {
      print(plot) %>%
        ggsave(filename = png_path, dpi = "retina", width = widths, height = heights)
    }

    pwalk(.l = list(tab_res$plotname, tab_res$plot, tab_res$widths, tab_res$heights, tab_res$png_path), .f = export_metaplot)
  }
}
