library(shiny)
library(shinydashboard)
library(flexdashboard)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(IDEATools)
library(DT)

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


# Define UI for application
ui = dashboardPage(skin = "blue",

                   ## Le header
                   dashboardHeader(title="Calculateur IDEA4", titleWidth = 300),

                   ## Le sidebar, avec l'input, des item et subitem
                   dashboardSidebar(width = 350,h4(
                       fileInput("files", "Charger le calculateur (format excel)", accept = c(".xls",".xlsx")),
                       sidebarMenu(id="tabs",
                       menuItem("Synthèse & Export", tabName = "synthese", icon = icon("tachometer")),
                       menuItem("Détail des composantes",tabName = "compo",icon = icon("chart-bar")),
                       menuItem("Détail des indicateurs par dimension",tabName = "indic",icon = icon("list"),
                       menuSubItem("Economiques",tabName = "indic_eco",icon = icon("chart-bar")),
                       menuSubItem("Socio-Territoriaux",tabName = "indic_st",icon = icon("chart-bar")),
                       menuSubItem("Agroécologiques",tabName = "indic_ae",icon = icon("chart-bar"))),
                       menuItem("Détail des indicateurs par propriété",tabName = "prop_indic",icon = icon("list"),
                                menuSubItem("Robustesse",tabName = "radar_robustesse", icon = icon("chart-pie")),
                                menuSubItem("Autonomie",tabName = "radar_autonomie", icon = icon("chart-pie")),
                                menuSubItem("Capacité Productive",tabName = "radar_cp", icon = icon("chart-pie")),
                                menuSubItem("Responsabilité Globale",tabName = "radar_rg", icon = icon("chart-pie")),
                                menuSubItem("Ancrage Territorial",tabName = "radar_an", icon = icon("chart-pie"))),
                       menuItem("Cartes heuristiques", tabName = "tree", icon = icon("project-diagram"),
                                menuSubItem("Robustesse",tabName = "robustesse", icon = icon("sitemap")),
                                menuSubItem("Autonomie",tabName = "autonomie", icon = icon("sitemap")),
                                menuSubItem("Capacité Productive",tabName = "cp", icon = icon("sitemap")),
                                menuSubItem("Responsabilité Globale",tabName = "rg", icon = icon("sitemap")),
                                menuSubItem("Ancrage Territorial",tabName = "an", icon = icon("sitemap")),
                                menuSubItem("Global",tabName = "global", icon = icon("sitemap"))),
                       menuItem("Légende",tabName = "legende",icon = icon("book")))
                   )),


                   ## Corps du dashboard
                   dashboardBody(

                       ## Définition des tags de style
                       tags$style(
                           type = 'text/css',
                           '.bg-lime {background-color: #1CDA53!important; }'),
                       # tags$style(".skin-blue .sidebar a { color: #000000; }"),
                       tags$style(
                           type = 'text/css',
                           '.bg-green {background-color: #0D8A00!important; }'),
                       tags$style(
                           type = 'text/css',
                           '.bg-red {background-color: #FF6348!important; }'),
                       tags$style(
                           type = 'text/css',
                           '.bg-maroon {background-color: #FF0000!important; }'),
                       tags$head(tags$style(css)),
                       tags$head(tags$style(css2)),
                       tags$head(tags$style(css3)),



                       ### UI graphique

                       tabItems(

                       tabItem(tabName = "synthese",

                       # Titre
                       h1(tags$b(div(style="display:inline-block;width:100%;text-align: center;",uiOutput("exploit")))),

                       fluidRow(),br(),

                       # Note globale obtenue
                       box(
                           h1(div(style="display:inline-block;width:100%;text-align: center;",textOutput("info"))),
                           width = 12, height = "100px",color = "red",
                           footer = textOutput("dimfooter")),

                       br(),br(),fluidRow(),HTML("<br>"),HTML("<br>"),

                       # Emplacement des 3 gauges
                       box(flexdashboard::gaugeOutput("plt1"),width=4, title="Durabilité Agroécologique", height = "300px"),
                       box(flexdashboard::gaugeOutput("plt2"),width=4, title="Durabilité Socio-territoriale", height = "300px"),
                       box(flexdashboard::gaugeOutput("plt3"),width=4, title="Durabilité Economique", height = "300px"),

                       # Emplacement des 5 infobox de propriétés + un footer
                       box(title = "Qualification des 5 propriétés de la durabilité pour l'exploitation (cliquer sur une propriété pour obtenir sa carte heuristique détaillée) :",
                           infoBoxOutput("prop1", width = 2),
                           infoBoxOutput("prop2", width = 2),
                           infoBoxOutput("prop5", width = 2),
                           infoBoxOutput("prop4", width = 3),
                           infoBoxOutput("prop3", width = 3),
                           width = 12, height = 200,
                           footer = uiOutput("globalfooter")),

                       # Emplacement des 3 liens de téléchargement (rapport imprimable, éditable et le .zip) avec un espace vertical entre
                       fluidRow(),HTML("<br>"),
                       div(style="display:inline-block;width:100%;text-align: center;",uiOutput("dl")),
                       fluidRow(),HTML("<br>"),
                       div(style="display:inline-block;width:100%;text-align: center;",uiOutput("dl2"))

                      ),

                      # Ensemble de tab liés aux cartes heuristiques, avec un bouton de retour à la synthèse. Chaque subtab est encapsulé dans un div centré, lui-même dans une box
                      tabItem(tabName = "tree",fluidPage()),

                      tabItem(tabName = "robustesse",
                              box(
                                div(style="display:inline-block;width:100%;text-align: center;",
                                imageOutput("robust_tree")), width = 12, height = "820px")),

                      tabItem(tabName = "autonomie",
                              box(
                                div(style="display:inline-block;width:100%;text-align: center;",
                                imageOutput("auto_tree")), width = 12, height = "820px")),

                      tabItem(tabName = "cp",
                              box(
                                div(style="display:inline-block;width:100%;text-align: center;",
                                imageOutput("cp_tree")), width = 12, height = "820px")),

                      tabItem(tabName = "rg",
                              box(
                                div(style="display:inline-block;width:100%;text-align: center;",
                                imageOutput("rg_tree")), width = 12, height = "820px")),

                      tabItem(tabName = "an",
                              box(
                                div(style="display:inline-block;width:100%;text-align: center;",
                                imageOutput("an_tree")), width = 12, height = "830px")),

                      tabItem(tabName = "global",
                              box(
                                div(style="display:inline-block;width:100%;text-align: center;",
                                imageOutput("global_tree")), width = 12, height = "1000px")),


                      ## Composantes
                      tabItem(tabName = "compo",
                              box(plotOutput("composantes", height = "800px"), width = 12)),


                      ## Détail des indicateurs par dimension
                      tabItem(tabName = "indic",fluidPage()),

                      tabItem(tabName = "indic_eco",
                              h1("Indicateurs économiques"),
                              box(plotOutput("indic_eco", height = "800px"), width = 12)),
                      tabItem(tabName = "indic_st",
                              h1("Indicateurs Socio-Territoriaux"),
                              box(plotOutput("indic_st", height = "880px"), width = 12)),
                      tabItem(tabName = "indic_ae",
                              h1("Indicateurs Agroécologiques"),
                              box(plotOutput("indic_ae", height = "800px"), width = 12)),


                      ### Plots radar
                      tabItem(tabName = "prop_indic",fluidPage()),

                      tabItem(tabName = "radar_robustesse",
                              box(plotOutput("robust_radar",height = "841px", width = "1314"), width = 12, height = "850px")),
                      tabItem(tabName = "radar_autonomie",
                              box(plotOutput("auto_radar",height = "800px"), width = 12, height = "820px")),
                      tabItem(tabName = "radar_cp",
                              box(plotOutput("cp_radar",height = "800px"), width = 12, height = "820px")),
                      tabItem(tabName = "radar_rg",
                              box(plotOutput("rg_radar",height = "800px"), width = 12, height = "820px")),
                      tabItem(tabName = "radar_an",
                              box(plotOutput("an_radar",height = "800px"), width = 12, height = "820px")),



                      tabItem(tabName = "legende",
                              h1("Légende des indicateurs"),
                              DT::dataTableOutput("legende_out",height = "800px"))
                              )


                       )
                   )


# Define server logic
server = function(input, output, session) {

IDEAdata <- eventReactive(input$files, {

  IDEATools::importIDEA(input = input$files$datapath, anonymous = FALSE)

})

IDEAresdim <- eventReactive(input$files, {

  IDEAdata() %>% dimensionsPlots()

})

IDEAresrad <- eventReactive(input$files, {

  IDEAdata() %>% radarPlots()

})

outdir <- tempdir(getwd())

## Tracé des arbres
observeEvent(input$files, {
  withProgress(message = "Coloration des cartes heuristiques...", detail = "Merci de patienter quelques instants", value = 0.2, {

    IDEATools::MakeTrees(IDEAdata()) %>% IDEATools::exportIDEA(outdir = outdir)
    incProgress(0.8)})


  })

#### Gauges
    output$plt1 <- renderGauge({

        ae <- IDEAdata()$dataset %>% distinct(nom_exploit,dimension,score_dim) %>% filter(dimension == "Durabilité Agroécologique") %>% pull(score_dim) %>% round()
        gauge(ae, min = 0, max = 100, gaugeSectors(
            colors = "#2E9C15"))
    })
    output$plt2 <- renderGauge({

        st <- IDEAdata()$dataset %>% distinct(nom_exploit,dimension,score_dim) %>% filter(dimension == "Durabilité Socio-Territoriale") %>% pull(score_dim)%>% round()

        gauge(st, min = 0, max = 100, gaugeSectors(
            colors = "#469FF9"))

    })
    output$plt3 <- renderGauge({

        ec <- IDEAdata()$dataset %>% distinct(nom_exploit,dimension,score_dim) %>% filter(dimension == "Durabilité Economique") %>% pull(score_dim)%>% round()

        gauge(ec, min = 0, max = 100, gaugeSectors(
            colors = "#FE962B"))

    })


#### Infos textuelles
    output$info <- renderText({

        inFile <- input$files

        if (is.null(inFile))
            return(NULL)

        dim <- IDEAdata()

        val <- dim$dataset %>% arrange(score_dim) %>% slice(1) %>% pull(score_dim)

        paste0("IDEA = ", round(as.numeric(val),0), "/100")


    })
    output$exploit<- renderUI({

        inFile <- input$files

        if (is.null(inFile))
            return(

              h1(tags$b(div(style="display:inline-block;width:100%;text-align: left;","\U2190 Pour démarrer l'application, charger un calculateur au format .xls ou .xlsx")))
            )

        dim <- IDEAdata()

        val <- dim$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)

        paste0("Nom de l'exploitation : ", val)


    })
    output$globalfooter<- renderUI({

      inFile <- input$files

      if (is.null(inFile))
        return()

     actionButton("glob","Consulter la carte heuristique globale", icon = icon("sitemap"))


    })
    output$dimfooter<- renderText({

      inFile <- input$files

      if (is.null(inFile))
        return()

      "NB : La note retenue correspond à la valeur la plus faible obtenue parmi les 3 dimensions de la durabilité ci-dessous."


    })

#### Téléchargements de rapports etc.
    output$dl <- renderUI({

      inFile <- input$files

      if (is.null(inFile))
        return()

      downloadButton("report", "Générer le rapport imprimable (.pdf)", style = "width:100%;")


    })
    output$dl2 <- renderUI({

      inFile <- input$files

      if (is.null(inFile))
        return()

      downloadButton("zipfile", "Générer le pack de figures (.zip)", style = "width:100%;")


    })

#### Définition des propriétés
    output$prop1 <- renderInfoBox({

      # inFile <- input$files
      #
      #   if (is.null(inFile))
      #       return(infoBox(title = "Robustesse",
      #                      paste(" "), icon = icon("dumbbell"),
      #                      color = "black", width = 2))

        val <- IDEAdata()$nodes$Robustesse$Robustesse

        color <- replace_col(val)

        val <- str_to_title(val)

        box1 <- infoBox(title = "Robustesse",
                paste(val), icon = icon("sitemap"),
                color = color, width = 2, href = "#")

        box1$children[[1]]$attribs$class<-"action-button"
        box1$children[[1]]$attribs$id<-"button_box_01"

        box1


    })
    output$prop2 <- renderInfoBox({

        # inFile <- input$files
        #
        # if (is.null(inFile))
        #     return(infoBox(title = "Autonomie",
        #                    paste(" "), icon = icon("hands-helping"),
        #                    color = "black", width = 2))
        #
        val <- IDEAdata()$nodes$Autonomie$Autonomie

        color <- replace_col(val)

        val <- str_to_title(val)

        box2 <- infoBox(title = "Autonomie",
                paste(val), icon = icon("sitemap"),
                color = color, width = 2,href = "#")

        box2$children[[1]]$attribs$class<-"action-button"
        box2$children[[1]]$attribs$id<-"button_box_02"

        box2

    })
    output$prop3 <- renderInfoBox({

        # inFile <- input$files
        #
        # if (is.null(inFile))
        #     return(infoBox(title = "Capacité productive et reproductive de biens et de services",
        #                    paste(" "), icon = icon("hand-holding-usd"),
        #                    color = "black", width = 2,href = "#"))



        val <- IDEAdata()$nodes$Capacité$`Capacité productive et reproductive de biens et de services`

        color <- replace_col(val)

        val <- str_to_title(val)

        box3 <- infoBox(title = "Capacité productive et reproductive de biens et de services",
                value = paste(val), icon = icon("sitemap"),
                color = color, width = 2, href = "#")

        box3$children[[1]]$attribs$class<-"action-button"
        box3$children[[1]]$attribs$id<-"button_box_03"

        box3


    })
    output$prop4 <- renderInfoBox({

        # inFile <- input$files
        #
        # if (is.null(inFile))
        #     return(infoBox(title = "Reponsabilité Globale",
        #                    value = paste(" "), icon = icon("medal"),
        #                    color = "black", width = 2))

        val <- IDEAdata()$nodes$Responsabilité$`Responsabilité globale`

        color <- replace_col(val)

        val <- str_to_title(val)

        box4 <- infoBox(title = "Reponsabilité Globale",
                value = paste(val), icon = icon("sitemap"),
                color = color, width = 2,href = "#")

        box4$children[[1]]$attribs$class<-"action-button"
        box4$children[[1]]$attribs$id<-"button_box_04"

        box4


    })
    output$prop5 <- renderInfoBox({

        # inFile <- input$files
        #
        # if (is.null(inFile))
        #     return(infoBox(title = "Ancrage territorial",
        #                    paste(" "), icon = icon("map-marker"),
        #                    color = "black", width = 2))
        #
        val <- IDEAdata()$nodes$Ancrage$`Ancrage territorial`

        color <- replace_col(val)

        val <- str_to_title(val)

        box5 <- infoBox(title = "Ancrage territorial",
                paste(val), icon = icon("sitemap"),
                color = color, width = 2,href = "#")

        box5$children[[1]]$attribs$class<-"action-button"
        box5$children[[1]]$attribs$id<-"button_box_05"

        box5

    })


#### Affichage des arbres
    output$robust_tree <- renderImage({

        inFile <- input$files

        if (is.null(inFile))
            return(box())

        val <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)
        v <- stringr::str_replace_all(val, " ", "_")

        outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Robustesse.png")

        list(src = normalizePath(outfile),
             contentType = 'image/png',
             width = 1151,
             height = 761,
             alt = "Robustesse")
    })
    output$auto_tree <- renderImage({

      inFile <- input$files

      if (is.null(inFile))
        return(box())

      val <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)
      v <- stringr::str_replace_all(val, " ", "_")

      outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Autonomie.png")

      list(src = normalizePath(outfile),
           contentType = 'image/png',
           width = 1151,
           height = 761,
           alt = "Autonomie")
    })
    output$cp_tree <- renderImage({

      inFile <- input$files

      if (is.null(inFile))
        return(box())

      val <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)
      v <- stringr::str_replace_all(val, " ", "_")

      outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Capacité.png")


      list(src = normalizePath(outfile),
           contentType = 'image/png',
           width = 1281,
           height = 750,
           alt = "Capacité productive")
    })
    output$rg_tree <- renderImage({

      inFile <- input$files

      if (is.null(inFile))
        return(box())

      val <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)
      v <- stringr::str_replace_all(val, " ", "_")

      outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Responsabilité.png")

      list(src = normalizePath(outfile),
           contentType = 'image/png',
           width = 1151,
           height = 761,
           alt = "Responsabilité globale")
    })
    output$an_tree <- renderImage({

      inFile <- input$files

      if (is.null(inFile))
        return(box())

      val <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)
      v <- stringr::str_replace_all(val, " ", "_")

      outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Ancrage.png")


      list(src = normalizePath(outfile),
           contentType = 'image/png',
           width = 1151,
           height = 761,
           alt = "Ancrage territorial")
    })
    output$global_tree <- renderImage({

      inFile <- input$files

      if (is.null(inFile))
        return(box())

      val <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value)
      v <- stringr::str_replace_all(val, " ", "_")

      outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Global.png")

      list(src = normalizePath(outfile),
           contentType = 'image/png',
           width = 1300,
           height = 890,
           alt = "Global",
           class="center")
    })

#### Boutons clickables propriétés
    observeEvent(input$button_box_01, {
      newtab <- "robustesse"
      updateTabItems(session,inputId="tabs",selected = newtab)
    })
    observeEvent(input$button_box_02, {
      newtab <- "autonomie"
      updateTabItems(session,inputId="tabs",selected = newtab)
    })
    observeEvent(input$button_box_03, {
      newtab <- "cp"
      updateTabItems(session,inputId="tabs",selected = newtab)
    })
    observeEvent(input$button_box_04, {
      newtab <- "rg"
      updateTabItems(session,inputId="tabs",selected = newtab)
    })
    observeEvent(input$button_box_05, {
      newtab <- "an"
      updateTabItems(session,inputId="tabs",selected = newtab)
    })
    observeEvent(input$glob, {
      newtab <- "global"
      updateTabItems(session,inputId="tabs",selected = newtab)
    })

## Plots dimension
output$composantes <- renderPlot({

p <- IDEAresdim()[[1]]$composantes

print(p)


})
output$indic_eco <- renderPlot({

  p <- IDEAresdim()[[1]]$indic_Economique+ facet_wrap(~composante, ncol = 2, scales = "free_y")+ theme(panel.spacing = unit(1, "lines"))

  print(p)


})
output$indic_st <- renderPlot({

  p <- IDEAresdim()[[1]]$`indic_Socio-Territoriale` + facet_wrap(~composante, ncol = 2, scales = "free_y") + theme(panel.spacing = unit(1, "lines"))

  print(p)


})
output$indic_ae <- renderPlot({

  p <- IDEAresdim()[[1]]$indic_Agroécologique+ facet_wrap(~composante, ncol = 2, scales = "free_y")+ theme(panel.spacing = unit(1, "lines"))

  print(p)


})


## Radar propriétés
output$robust_radar <- renderPlot({
  p <- IDEAresrad()[[1]]$Robustesse
  print(p)
})
output$auto_radar <- renderPlot({
  p <- IDEAresrad()[[1]]$Autonomie
  print(p)
})
output$cp_radar <- renderPlot({
  p <- IDEAresrad()[[1]]$`Capacité productive et reproductive \nde biens et de services`
  print(p)
})
output$rg_radar <- renderPlot({
  p <- IDEAresrad()[[1]]$`Responsabilité globale`
  print(p)
})
output$an_radar <- renderPlot({
  p <- IDEAresrad()[[1]]$`Ancrage Territorial`
  print(p)
})

output$legende_out <- DT::renderDataTable({


  IDEATools::label_nodes %>% select(`Code Indicateur`=code_indicateur, `Nom Indicateur`=nom_indicateur, Niveau = level, Dimension = dimension, Composante = composante, dim) %>%
    mutate(no_code = parse_number(`Code Indicateur`)) %>%
    mutate(Niveau = factor(Niveau, levels = c("indicateur","noeud","branche","propriete"))) %>%
    arrange(Niveau,dim,no_code) %>%
    mutate(Dimension = as.character(Dimension)) %>%
    select(-dim, -no_code) %>%
    mutate(Composante = str_remove_all(Composante,"\\n")) %>%
    mutate(Dimension =ifelse(is.na(Dimension),yes = " ",no = Dimension)) %>%
    filter(Niveau == "indicateur") %>%
    select(-Niveau) -> df

  DT::datatable(df,options = list(pageLength = 20))



})


#### REPORT
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"

        filename = "rapport_individuel.pdf",

        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).

          withProgress(message = "Rendu du rapport en cours........", detail = "Merci de patienter quelques instants", value = 0.2,{

            inFile <- input$files

            tempReport <- file.path(tempdir(), "rapport_individuel.Rmd")
            template <- system.file("myApp/rapport_individuel.Rmd", package = "IDEATools")
            file.copy(template, tempReport, overwrite = TRUE)
            # setwd(tempdir())

            # Set up parameters to pass to Rmd document
            params <- list(data = IDEAdata(),
                           outdir = outdir,
                           anon = FALSE)

            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            incProgress(0.8)
          })

        }
    )



    # output$zipfile <- downloadHandler(
    #   filename = "Pack_figures",
    #
    #
    # )

    output$zipfile <- downloadHandler(
      # For PDF output, change this to "report.pdf"

      filename = function() {
        v <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value) %>% stringr::str_replace_all(" ", "_")
        paste0("Figures_",v, ".zip")
      },

      content = function(fname) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).

        withProgress(message = "Production des figures en cours........", detail = "Merci de patienter quelques instants", value = 0.2,{

          v <- IDEAdata()$metadata %>% filter(title == "NOM Prénom :") %>% pull(value) %>% stringr::str_replace_all(" ", "_")
          IDEAresdim() %>% IDEATools::exportIDEA(outdir = outdir)
          IDEAresrad() %>% IDEATools::exportIDEA(outdir = outdir)
          IDEATools::MakeTrees(IDEAdata()) %>% IDEATools::exportIDEA(outdir = outdir)

          setwd(outdir)
          fs <- file.path(v,list.files(file.path(outdir,v), recursive=TRUE))


          zip(zipfile = fname, files = fs)

        incProgress(0.8)
        })

      }
      ,contentType = "application/zip"
      )



      }


# Run the application
shinyApp(ui = ui, server = server)
