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


## Légende associée aux métadonnées du fichier .json
MTD_legende <- tibble::tribble(
  ~code,                                   ~nom,                                                      ~etendue,
  "MTD_00",      "Version du calculateur utilisée",                                                    "Alpha 10",
  "MTD_01",             "Identifiant exploitation",                                                       "Num 6",
  "MTD_02",                                  "SAU",                                           "0 à 10 000 (2 dc)",
  "MTD_03",                                  "UTH",                                           "0 à 10 000 (2 dc)",
  "MTD_04",                                "UTH F",                                              "0 à 100 (2 dc)",
  "MTD_05", "Tranche d’âge du chef d’exploitation", "«-25»,«25-35»,«35-45», «45-55»,«55-65» et «65+»",
  "MTD_06",      "Typologie d'exploitation (OTEX)",                                       "Codes OTEX simplifiés",
  "MTD_07",      "Surface en herbe en % de la SAU",                                              "0 à 100 (2 dc)",
  "MTD_08",               "Capital d’exploitation",                                             "0 à 100000 000",
  "MTD_09",                                  "EBE",                                    "– 1000 000 à 10000 000",
  "MTD_10",                     "Résultat courant",                                    "– 1000 000 à 10000 000",
  "MTD_11",      "Zone géographique (département)",                                      "liste des départements",
  "MTD_12",         "Atelier hors sol: oui / non",                                                      "0 ou 1",
  "MTD_13",                      "Année d'enquête",                                                       "Num 4",
  "MTD_14",                       "Type d’élevage",       "0 - pas d’élevage / 1 – monogastrique / 2 - herbivore",
  "MTD_15",              "Part des PP dans la SAU",                                              "0 à 100 (2 dc)",
  "MTD_16", "Usage des produits phytos: oui /non",                                                      "0 ou 1"
)


