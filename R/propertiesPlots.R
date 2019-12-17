#' Plot visual outputs linked to the "Property" approach for IDEA4
#'
#' @param IDEAdata output of the `importIDEA()` function
#'
#' @return a named list of plots
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' path <- system.file("example.xls", package = "RIDEATools")
#' dat <- importIDEA(path, anonymous = FALSE)
#' plots <- dimensionsPlots(dat, output_dir = NULL)
propertiesPlots <- function(IDEAdata){

  return_list <- list()

  nom <- unique(IDEAdata$dataset$nom_exploit)

  for (prop in names(IDEAdata$nodes)){

    liste_indicateurs_prop <- switch(prop,
                                     "Ancrage"=c("B10","B3","B9","B8","B7","B6","B15","B14","B19","AN4","AN2","AN1","AN5","AN3"),
                                     "Autonomie" = c("B13","B15","B18","B8","C5","C3","C6","A7","A8","A6","AU1","AU2","AU3","AU4","AU5","AU6"),
                                     "Robustesse" = c("A1","A3","A4","A14","C5","C4","C7","A2","C8","C9","A15","B22","B13","B15","B18","B16","R1","R2","R3","R4","R5","R6","R7","R8","R9","R10"),
                                     "Responsabilité" = c("B20","B5","B19","B11","B1","B2","B4","A10","A9","A11","C11","B17","B14","B16","B21","B23","A5","A16","A17","A18","A19","B12","RG1","RG2","RG3","RG5","RG6","RG8","RG9","RG10","RG12","RG13","RG4","RG7","RG11","RG14","RG15"),
                                     "Capacité" = c("A5","A12","A13","B14","B15","B16","B13","B18","B1","B3","C1","C2","C3","C10","CP2","CP3","CP8","CP1","CP4","CP5","CP6","CP7","CP9","CP10"),
                                     "Global" = c(
                                       "B13","B15","B18","B8","C5","C3","C6","A7","A8","A6","AU1","AU2","AU3","AU4","AU5","AU6",
                                       "A5","A12","A13","B14","B15","B16","B13","B18","B1","B3","C1","C2","C3","C10","CP2","CP3","CP8","CP1","CP4","CP5","CP6","CP7","CP9","CP10",
                                       "A1","A3","A4","A14","C5","C4","C7","A2","C8","C9","A15","B22","B13","B15","B18","B16","R1","R2","R3","R4","R5","R6","R7","R8","R9","R10",
                                       "B20","B5","B19","B11","B1","B2","B4","A10","A9","A11","C11","B17","B14","B16","B21","B23","A5","A16","A17","A18","A19","B12","RG1","RG2","RG3","RG5","RG6","RG8","RG9","RG10","RG12","RG13","RG4","RG7","RG11","RG14","RG15",
                                       "B10","B3","B9","B8","B7","B6","B15","B14","B19","AN4","AN2","AN1","AN5","AN3")
    )


    car <- canvas[[prop]]

    span_rect <- switch(prop,
                        "Ancrage" = 14,
                        "Global"= 117,
                        "Autonomie" = 16,
                        "Robustesse" = 26,
                        "Responsabilité" = 37,
                        "Capacité" = 24)

    ## Numéros des lignes où on a un rect qui s'ouvre
    rect_no <- which(stringr::str_detect(car, "    <rect") == TRUE)[1:span_rect]

    tab_rect <- tibble::tibble(rect_no) %>%
      dplyr::mutate(rect_end = rect_no+7) %>%
      dplyr::mutate(rect_id = purrr::map2_dbl(.x = rect_no, .y = rect_end,.f = find_pos,choice = "id")) %>%
      dplyr::mutate(rect_style = purrr::map2_dbl(.x = rect_no, .y = rect_end,.f = find_pos,choice = "style")) %>%
      dplyr::mutate(rect_number = readr::parse_number(car[rect_id]))%>%
      dplyr::mutate(indicateur = liste_indicateurs_prop)

    rect_style <- tab_rect$rect_style

    tab_to_color <- IDEAdata$nodes[[prop]] %>%
      tidyr::gather(key = indicateur, value = resultat,-nom_exploit) %>%
      dplyr::mutate(indicateur = replace_indicateur(indicateur)) %>%
      dplyr::inner_join(tab_rect, by = "indicateur") %>%
      dplyr::mutate(color = replace_col(resultat)) %>%
      dplyr::arrange(rect_number)

    for(i in which(tab_to_color$color == "lightgreen")) {
      car[rect_style[i]] <- stringr::str_replace(car[rect_style[i]],"fill:#ffffff","fill:#1CDA53")
    }

    for(i in which(tab_to_color$color == "green")) {
      car[rect_style[i]] <- stringr::str_replace(car[rect_style[i]],"fill:#ffffff","fill:#0D8A00")
    }

    for(i in which(tab_to_color$color == "tomato")) {
      car[rect_style[i]] <- stringr::str_replace(car[rect_style[i]],"fill:#ffffff","fill:#FF6348")
    }

    for(i in which(tab_to_color$color == "red")) {
      car[rect_style[i]] <- stringr::str_replace(car[rect_style[i]],"fill:#ffffff","fill:#FF0000")
    }

    for(i in which(tab_to_color$color == "orange")) {
      car[rect_style[i]] <- stringr::str_replace(car[rect_style[i]],"fill:#ffffff","fill:#FFA300")
    }

    for(i in which(tab_to_color$color == "grey")) {
      car[rect_style[i]] <- stringr::str_replace(car[rect_style[i]],"fill:#ffffff","fill:#A0A0A0")
    }

    num_title <- which(stringr::str_detect(car,"Exploitation anonyme")==TRUE)

    car[num_title] <- stringr::str_replace(car[num_title],"Exploitation anonyme",nom)

    return_list[[prop]] <- car

  }

  return(return_list)


}


