## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
scales::show_col(c(
  "tres defavorable" = "#CD0000",
  "defavorable" = "#FF6347",
  "intermédiaire" = "#FCD400",
  "favorable" = "#33FF00",
  "tres favorable" = "#008B00"
), borders = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("davidcarayon/IDEATools")

## ----eval = FALSE-------------------------------------------------------------
#  install.packages(c("officedown","openxlsx"))

## ----out.width = "100%", echo = FALSE-----------------------------------------
knitr::include_graphics("img/heuristic_numbers.png")

## ----out.width = "100%", echo = FALSE, fig.cap = "Exemple de figures produites par IDEATools"----
knitr::include_graphics("img/cap_graphs.png")

## ---- echo = FALSE------------------------------------------------------------
scales::show_col(c("Agroécologique"= "#2e9c15", "Socio-Territoriale" = "#5077FE", "Economique" = "#FE962B"), borders = FALSE)

## ----echo = FALSE-------------------------------------------------------------
scales::show_col(c(
  "tres defavorable" = "#FF0000",
  "defavorable" = "#FF6348",
  "intermédiaire" = "#FFA300",
  "favorable" = "#1CDA53",
  "tres favorable" = "#0D8A00"
), borders = FALSE)

## ----out.width = "100%", echo = FALSE-----------------------------------------
knitr::include_graphics("img/pipeline_individuel.png")

## ----out.width = "100%", echo = FALSE-----------------------------------------
knitr::include_graphics("img/pipeline_multi_individuel.png")

## ----out.width = "100%", echo = FALSE-----------------------------------------
knitr::include_graphics("img/pipeline_groupe.png")

