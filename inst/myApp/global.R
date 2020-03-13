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
