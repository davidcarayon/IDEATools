#' Simplify indicator name if separated by a space
#'
#' @param name The indicator name
#'
#' @return a string. The simplified indicator name.
#' @importFrom magrittr %>%
simplify_indicator_name <- function(name) {

  indic <- ifelse(stringr::str_split(name, " ")[[1]][1] %in% liste_indicateurs,
                  yes = stringr::str_split(name, " ")[[1]][1],
                  no = name)

  return(indic)

}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Atribute valid color to a property
#'
#' @param resultat the value affected to the property
#'
#' @return the correct R color value
#' @importFrom magrittr %>%
replace_col <- function(resultat) {
  res <- dplyr::case_when(
    resultat == "NC" ~ "grey",
    resultat == "favorable" ~ "lightgreen",
    resultat == "très favorable" ~ "green",
    resultat == "défavorable" ~ "tomato",
    resultat == "très défavorable" ~ "red",
    resultat == "intermédiaire" ~ "orange"
  )
  return(res)
}



# Defining custom functions -----------------------------------------------


## Re-scales indicators according to the max authorized value
ScaleIndicator <- function(indic,value) {
  max = list_max %>% dplyr::filter(indicateur == indic) %>% dplyr::pull(valeur_max)
  value =  round(value + 1e-10)
  scaled_value = ifelse(value > max, yes = max, no = value)
  if(scaled_value < 0){scaled_value = 0}
  return(scaled_value)
}

## Converts the unscaled indicator values to qualitative categories according to the DEXi model
Score2Dexi <- function(TD, D, I, F, score) {
  seuils <- c(TD, D, I, F) %>% stats::na.omit()

  if (length(seuils) == 4) {
    res <- dplyr::case_when(
      score < D ~ "très défavorable",
      score >= D & score < I ~ "défavorable",
      score >= I & score < F ~ "intermédiaire",
      score >= F ~ "favorable"
    )
  }

  if (length(seuils) == 3) {
    res <- dplyr::case_when(
      score < I ~ "défavorable",
      score >= I & score < F ~ "intermédiaire",
      score >= F ~ "favorable"
    )
  }

  if (length(seuils) == 2) {
    res <- dplyr::case_when(
      score < F ~ "intermédiaire",
      score >= F ~ "favorable"
    )
  }


  return(res)
}

## Re-scales the composante value according to the max authorized value
ScaleComposante <- function(compo,value) {
  Encoding(list_max_compo$composante) <- "UTF-8"
  max = list_max_compo %>% dplyr::filter(composante == compo) %>% dplyr::pull(max_compo)
  scaled_value = ifelse(value > max, yes = max, no = value)
  if(scaled_value < 0){scaled_value = 0}
  return(scaled_value)
}

## Calculates the dimension score based on the composantes
Composante2Dimension <- function(df) {

  df %>%
    dplyr::distinct(composante, composante_value) %>%
    dplyr::pull(composante_value) %>%
    sum(na.rm=TRUE)


}

#' Attribute a code to each node
#'
#' @param indicateur the name of the node
#'
#' @return a string. the new code for this node
#' @importFrom magrittr %>%
replace_indicateur <- function(indicateur) {
  res <- dplyr::case_when(
    indicateur %in% liste_indicateurs ~ indicateur,
    indicateur == "Diversité de l'organisation spatiale et temporelle" ~ "R1",
    indicateur == "Limiter l'exposition aux aléas" ~ "R2",
    indicateur == "Diversité des activités" ~ "R3",
    indicateur == "En favorisant la diversité" ~ "R4",
    indicateur == "De l'outil de production" ~ "R5",
    indicateur == "En développant l'inertie et les capacités tampon" ~ "R6",
    indicateur == "Réduire la sensibilité" ~ "R7",
    indicateur == "par l'insertion dans les réseaux" ~ "R8",
    indicateur == "Augmenter la capacité d'adaptation" ~ "R9",
    indicateur == "Robustesse" ~ "R10",

    indicateur == "Naturelles" ~ "CP1",
    indicateur == "Travail" ~ "CP2",
    indicateur == "Compétences et équipements" ~ "CP3",
    indicateur == "Sociales et humaines" ~ "CP4",
    indicateur == "Préserver ou créer des ressources pour l'acte de production" ~ "CP5",
    indicateur == "Développer la capacité alimentaire" ~ "CP6",
    indicateur == "Capacité à produire dans le temps des biens et services remunérés" ~ "CP7",
    indicateur == "Capacité de remboursement" ~ "CP8",
    indicateur == "Capacité à dégager un revenu dans le temps" ~ "CP9",
    indicateur == "Capacité productive et reproductive de biens et de services" ~ "CP10",

    indicateur == "Liberté de décision organisationnelle" ~ "AU1",
    indicateur == "Liberté de décision dans les relations commerciales" ~ "AU2",
    indicateur == "Disposer d'une liberté de décision dans ses choix de gouvernance et de production" ~ "AU3",
    indicateur == "Disposer d'une autonomie financière" ~ "AU4",
    indicateur == "Autonomie dans le processus productif" ~ "AU5",
    indicateur == "Autonomie" ~ "AU6",

    indicateur == "Partage et transparence des activités productives" ~ "RG1",
    indicateur == "Ouverture et relation au monde non agricole" ~ "RG2",
    indicateur == "Sécurité alimentaire" ~ "RG3",
    indicateur == "Implications et engagements sociaux" ~ "RG4",
    indicateur == "Ressources naturelles" ~ "RG5",
    indicateur == "Ressources énergétiques et manufacturées" ~ "RG6",
    indicateur == "Partager équitablement les ressources" ~ "RG7",
    indicateur == "Conditions de travail de la main d'oeuvre " ~ "RG8",
    indicateur == "Conditions de travail de la main d'oeuvre" ~ "RG8",
    indicateur == "Conditions de vie et de travail" ~ "RG9",
    indicateur == "Bien être de la vie animale" ~ "RG10",
    indicateur == "Contribuer à la qualité de vie sur l'exploitation" ~ "RG11",
    indicateur == "Réduire les émissions" ~ "RG12",
    indicateur == "Réduire l'usage des produits polluants" ~ "RG13",
    indicateur == "Réduire ses impacts sur la santé et les écosystèmes" ~ "RG14",
    indicateur == "Responsabilité globale" ~ "RG15",

    indicateur == "Valoriser la qualité territoriale" ~ "AN1",
    indicateur == "Contribuer à des démarches d'économie circulaire" ~ "AN2",
    indicateur == "Par le travail et l'emploi" ~ "AN3",
    indicateur == "S'inscrire dans des démarches de territoire" ~ "AN4",
    indicateur == "Ancrage territorial" ~ "AN5"
  )


  return(res)
}

#' Adds a line break for too long strings
#'
#' @param text the string to modify
#'
#' @return a the string with line breaks
#' @importFrom magrittr %>%
wrapit <- function(text) {
  wtext <- paste(strwrap(text, width = 75), collapse = " \n ")
  return(wtext)
}

#' Finds the position of the given rectangle in the svg string
#'
#' @param start the starting position of the rectangle
#' @param end the ending position of the rectangle
#' @param choice output type, can be "id" or "style"
#' @param car the svg string canvas
#'
#' @return the correct rectangle id or style position
#' @importFrom magrittr %>%
find_pos <- function(start,end,choice,car){

  selection = car[start:end]
  rect_id = start + which(stringr::str_detect(selection,"id=") == TRUE) - 1
  rect_style = start + which(stringr::str_detect(selection,"style=") == TRUE) - 1

  val <- switch(choice,
                "id"=rect_id,
                "style"=rect_style)

  return(val)


}


theme_tq_cust <- function(base_size = 15) {

  # Tidyquant colors
  blue  <- "#2c3e50"
  green <- "#18BC9C"
  white <- "#FFFFFF"
  grey  <- "grey80"

  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size) %+replace%

    ggplot2::theme(

      # Base Inherited Elements
      line               =  ggplot2::element_line(colour = blue, size = 0.5, linetype = 1,
                                                  lineend = "butt"),
      rect               =  ggplot2::element_rect(fill = white, colour = blue,
                                                  size = 0.5, linetype = 1),
      text               =  ggplot2::element_text(face = "plain",
                                                  colour = blue, size = base_size,
                                                  lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                                  margin = ggplot2::margin(), debug = FALSE),

      # Axes
      axis.line          = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(size = rel(0.8)),
      axis.ticks         = ggplot2::element_line(color = grey, size = rel(1/3)),
      axis.title         = ggplot2::element_text(size = rel(1.0), face = "bold"),

      # Panel
      panel.background   = ggplot2::element_rect(fill = white, color = NA),
      panel.border       = ggplot2::element_rect(fill = NA, size = rel(1/2), color = blue),
      panel.grid.major   = ggplot2::element_line(color = grey, size = rel(1/3)),
      panel.grid.minor   = ggplot2::element_line(color = grey, size = rel(1/3)),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.spacing      = ggplot2::unit(.75, "cm"),

      # Legend
      legend.key         = ggplot2::element_rect(fill = white, color = NA),
      legend.position    = "top",
      legend.title = element_text(face = "bold"),

      # Strip (Used with multiple panels)
      strip.background   = ggplot2::element_rect(fill = blue, color = blue),
      strip.text         = ggplot2::element_text(color = white, size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 5, b = 5)),

      # Plot
      plot.title         = ggplot2::element_text(size = rel(1.2), hjust = 0,
                                                 margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
      plot.subtitle      = ggplot2::element_text(size = rel(0.9), hjust = 0,
                                                 margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),

      # Complete theme
      complete = TRUE
    )
}


ae_levels <- c("Diversité fonctionnelle", "Bouclage de flux de matières et d'énergie \npar une recherche d'autonomie", "Sobriété dans l'utilisation des ressources", "Assurer des conditions favorables à la production\n à moyen et long terme", "Réduire les impacts sur la santé humaine et les écosystèmes")

st_levels <- c("Alimentation", "Développement local \net économie circulaire", "Emploi et qualité au travail", "Ethique et développement humain")

ec_levels <- c("Viabilité économique et financière", "Indépendance", "Transmissibilité", "Efficience globale")

glob_levels <- c(ae_levels, st_levels, ec_levels)
