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


wrapit <- function(text) {
  wtext <- paste(strwrap(text, width = 75), collapse = " \n ")
  return(wtext)
}

find_pos <- function(start,end,choice){

  selection = car[start:end]
  rect_id = start + which(stringr::str_detect(selection,"id=") == TRUE) - 1
  rect_style = start + which(stringr::str_detect(selection,"style=") == TRUE) - 1

  val <- switch(choice,
                "id"=rect_id,
                "style"=rect_style)

  return(val)


}
