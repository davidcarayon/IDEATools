library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(IDEATools)
library(DT)

## Définition de fonctions perso et de styles CSS à inserer dans l'UI
replace_col <- function(resultat) {
  res <- dplyr::case_when(
    resultat == "NC" ~ "grey",
    resultat == "favorable" ~ "lime",
    resultat == "très favorable" ~ "green",
    resultat == "défavorable" ~ "red",
    resultat == "très défavorable" ~ "maroon",
    resultat == "intermédiaire" ~ "orange"
  )
  return(res)
}
options(shiny.maxRequestSize=30*1024^2)

## CSS pour le style des gauges
css <- HTML("
.html-widget.gauge svg {
  height: 300px;
  width: 500px;
}")

## CSS pour la couleur du sidebar
css2 <- HTML("
.text {
    fill: #FFFFFF
}")

## CSS pour la taille du lien de téléchargement
css3 <- HTML("
.shiny-download-link {width: 100%;}")


list_max <- tibble::tribble(
  ~indicateur, ~valeur_max,
  "A1",           5,
  "A2",           5,
  "A3",           5,
  "A4",           5,
  "A5",           5,
  "A6",           8,
  "A7",           8,
  "A8",           8,
  "A9",           8,
  "A10",           8,
  "A11",           8,
  "A12",           8,
  "A13",           8,
  "A14",           4,
  "A15",           4,
  "A16",           6,
  "A17",           6,
  "A18",           6,
  "A19",           6,
  "B1",           6,
  "B2",           6,
  "B3",           6,
  "B4",           6,
  "B5",           6,
  "B6",           5,
  "B7",           3,
  "B8",           5,
  "B9",           5,
  "B10",           3,
  "B11",           3,
  "B12",           3,
  "B13",           3,
  "B14",           6,
  "B15",           6,
  "B16",           6,
  "B17",           5,
  "B18",           5,
  "B19",           6,
  "B20",           6,
  "B21",           6,
  "B22",           6,
  "B23",           6,
  "C1",          20,
  "C2",          12,
  "C3",           6,
  "C4",          10,
  "C5",          10,
  "C6",           6,
  "C7",           4,
  "C8",          15,
  "C9",           8,
  "C10",          12,
  "C11",           8
)

list_max_compo <- tibble::tribble(
  ~composante, ~max_compo,
  "Diversité fonctionnelle",         20,
  "Bouclage de flux \nde matières et d'énergie \npar une recherche d'autonomie",         20,
  "Sobriété dans l'utilisation des ressources",         20,
  "Assurer des conditions favorables à la production à moyen et long terme",         20,
  "Réduire les impacts sur la santé humaine et les écosystèmes",         20,
  "Alimentation",         25,
  "Développement local \net économie circulaire",         25,
  "Emploi et qualité au travail",         25,
  "Ethique et développement humain",         25,
  "Viabilité économique et financière",         35,
  "Indépendance",         25,
  "Transmissibilité",         20,
  "Efficience globale",         20
)


indicateurs_proprietes <- list(c("B10", "B3", "B9", "B8", "B7", "B6", "B15", "B14", "B19"),
     c("B13", "B15", "B18", "B8", "C5", "C3", "C6", "A7", "A8", "A6"),
     c("A1", "A3", "A4", "A14", "C5", "C4", "C7", "A2", "C8", "C9", "A15", "B22", "B13", "B15", "B18", "B16"),
     c("B20", "B5", "B19", "B11", "B1", "B2", "B4", "A10", "A9", "A11", "C11", "B17", "B14", "B16", "B21", "B23", "A5", "A16", "A17", "A18", "A19", "B12"),
     c("A5", "A12", "A13", "B14", "B15", "B16", "B13", "B18", "B1", "B3", "C1", "C2", "C3", "C10"))


