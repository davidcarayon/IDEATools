# Définition des de la logique serveur de l'outil IDEATools.
# Auteur : David CARAYON (INRAE)
# Licence : GPL-3

## Chargement des packages et données
source("global.R")

# Define server logic
server = function(input, output, session) {

  ## Définition du répertoire temporaire utilisé pour exporter les images/rapports
  outdir <- tempdir()

  ## Import des données de l'input utilisateur
  IDEAdata <- eventReactive(input$files, {

    IDEATools::importIDEA(input = input$files$datapath, anonymous = FALSE)

  })

  ## Production des plots dimension
  IDEAresdim <- eventReactive(input$files, {

    IDEAdata() %>% dimensionsPlots()

  })

  ## Production des diagrammes radar
  IDEAresrad <- eventReactive(input$files, {

    IDEAdata() %>% radarPlots()

  })

  ## Tracé des arbres et export dans le répertoire temporaire avec barre de progression (au format SVG pour affichage web)
  observeEvent(input$files, {
    withProgress(message = "Coloration des arbres éclairés...", detail = "Merci de patienter quelques instants", value = 0.2, {
      IDEATools::MakeTrees(IDEAdata()) %>% IDEATools::exportIDEA(outdir = outdir, svg = TRUE)
      incProgress(0.8)})
  })

  #### Définition des 3 gauges de dimension
  output$plt1 <- renderGauge({

    ae <- IDEAdata()$dataset %>% distinct(id_exploit,dimension,dimension_value) %>% filter(dimension == "Agroécologique") %>% pull(dimension_value) %>% round()
    gauge(ae, min = 0, max = 100, gaugeSectors(
      colors = "#2E9C15"))
  })
  output$plt2 <- renderGauge({

    st <- IDEAdata()$dataset %>% distinct(id_exploit,dimension,dimension_value) %>% filter(dimension == "Socio-Territoriale") %>% pull(dimension_value)%>% round()

    gauge(st, min = 0, max = 100, gaugeSectors(
      colors = "#469FF9"))

  })
  output$plt3 <- renderGauge({

    ec <- IDEAdata()$dataset %>% distinct(id_exploit,dimension,dimension_value) %>% filter(dimension == "Economique") %>% pull(dimension_value)%>% round()

    gauge(ec, min = 0, max = 100, gaugeSectors(
      colors = "#FE962B"))

  })


  #### Infos textuelles dynamiques

  ## Note finale IDEA
  output$info <- renderText({

    inFile <- input$files

    if (is.null(inFile))
      return(NULL)

    dim <- IDEAdata()

    val <- dim$dataset %>% arrange(dimension_value) %>% slice(1) %>% pull(dimension_value)

    paste0("IDEA = ", round(as.numeric(val),0), "/100")


  })

  ## Footer expliquant le choix de la valeur limitante pour le score de performance globale
  output$dimfooter<- renderText({

    inFile <- input$files

    if (is.null(inFile))
      return()

    "NB : La note retenue correspond à la valeur la plus faible obtenue parmi les 3 dimensions de la durabilité ci-dessous."


  })

  ## Nom de l'exploitation
  output$exploit<- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return(

        h1(tags$b(div(style="display:inline-block;width:100%;text-align: left;","\U2190 Pour démarrer l'application, charger un calculateur au format .xls(x) ou .json")))
      )

    dim <- IDEAdata()

    val <- dim$metadata$MTD_01

    paste0("Nom de l'exploitation : ", val)


  })

  ## Titre des box des indicateurs de chaque dimension
  output$title_ae <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return(
        "Durabilité Agroécologique"
      )

    actionLink("detail_ae","Durabilité Agroécologique", style = "font-color: #000000; width: 100%", icon = icon("search"))


  })
  output$title_st <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return(
        "Durabilité Socio-territoriale"
      )

    actionLink("detail_st","Durabilité Socio-territoriale", style = "font-color: #000000; width: 100%", icon = icon("search"))


  })
  output$title_eco <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return(
        "Durabilité Economique"
      )

    actionLink("detail_eco","Durabilité Economique", style = "font-color: #000000; width: 100%", icon = icon("search"))


  })

  ## Footer de la synthèse des propriétés qui renvoie à l'arbre synthétique
  output$globalfooter<- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("glob","Arbre de synthèse", icon = icon("sitemap"), style = "font-size:120%; width:35%; padding:8px;")


  })

  ## Définition des 15 boutons de retour à la page d'accueil (Une page HTML n'accepte pas qu'un objet avec le même id soit dupliqué...)
  output$return_0 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_0","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_1 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_1","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_2 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_2","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_3 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_3","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_4 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_4","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_5 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_5","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_6 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_6","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_7 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_7","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_8 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_8","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_9 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_9","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_10 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_10","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_11 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_11","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_12 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_12","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_13 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_13","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_14 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_14","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })
  output$return_15 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    actionLink("return_15","Retour à la synthèse", icon = icon("backward"), style = "font-size:120%; width:35%; padding:8px;")


  })


  ## Définition des progress bar d'indicateurs, regroupés par dimension
  output$plots_c1 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("A",1:5)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "success",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)


  })
  output$plots_c2 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("A",6:8)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "success",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c3 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("A",9:11)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "success",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c4 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("A",12:15)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "success",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c5 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("A",16:19)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "success",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })

  output$plots_c6 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("B",1:5)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "primary",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c7 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("B",6:13)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "primary",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c8 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("B",14:18)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "primary",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c9 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("B",19:23)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "primary",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })

  output$plots_c10 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("C",1:3)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "warning",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c11 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("C",4:7)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "warning",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c12 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("C",8:9)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "warning",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })
  output$plots_c13 <- renderUI({

    d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% dplyr::inner_join(list_max, by = "indicateur")

    plot_output_list <- list()

    for (i in paste0("C",10:11)){

      val <- d %>% filter(indicateur == i) %>% pull(value)
      maxval <- d %>% filter(indicateur == i) %>% pull(valeur_max)
      lab <- d %>% filter(indicateur == i) %>% mutate(nom_complet = paste0(indicateur," - ",nom_indicateur)) %>% pull(nom_complet)

      plot_output_list[[i]] <- progressBar(id = i, value = val, total = maxval,title = lab, status = "warning",display_pct = TRUE)

    }


    do.call(tagList, plot_output_list)



  })

  ## Création des titres de chaque box de composante de manière dynamique
  ## (le titre contient le score obtenu pour la composante)
  observeEvent(input$files,{
    output$title_c1 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(1)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c2 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(2)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c3 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(3)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c4 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(4)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c5 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Agroécologique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(5)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })

    output$title_c6 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(1)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c7 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(2)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c8 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(3)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c9 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Socio-Territoriale") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(4)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })

    output$title_c10 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(1)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c11 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(2)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c12 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(3)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
    output$title_c13 <- renderText({
      d <- IDEAdata()$dataset %>% dplyr::filter(dimension == "Economique") %>% distinct(composante, composante_value) %>% inner_join(list_max_compo, by = "composante") %>% slice(4)
      paste0(d$composante," (",d$composante_value,"/",d$max_compo,")")

    })
  })


  #### Téléchargements de rapports (dl) et de zip (dl2)
  output$dl <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    downloadButton("report", "Générer le rapport imprimable (.pdf)", style = "font-size:120%; width:100%; padding:8px")


  })
  output$dl2 <- renderUI({

    inFile <- input$files

    if (is.null(inFile))
      return()

    downloadButton("zipfile", "Générer le pack de figures (.zip)", style = "font-size:120%; width:100%; padding:8px")


  })

  #### Définition des infobox propriétés
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


  #### Affichage des arbres (Insertion d'une image format svg)
  output$robust_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Robustesse.svg"))

    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1072,
         height = 767,
         alt = "Robustesse")
  })
  output$auto_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Autonomie.svg"))

    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1073,
         height = 601,
         alt = "Autonomie")
  })
  output$cp_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Capacité productive et reproductive de biens et de services.svg"))


    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1193,
         height = 652,
         alt = "Capacité productive")
  })
  output$rg_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Responsabilité globale.svg"))

    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1063,
         height = 674,
         alt = "Responsabilité globale")
  })
  output$an_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Ancrage Territorial.svg"))


    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1072,
         height = 601,
         alt = "Ancrage territorial")
  })
  output$global_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Global.svg"))

    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1158,
         height = 841,
         alt = "Global",
         class="center")
  })
  output$global_zoom_tree <- renderImage({

    inFile <- input$files

    if (is.null(inFile))
      return(box())

    val <- IDEAdata()$metadata$MTD_01
    v <- stringr::str_replace_all(val, " ", "_")

    outfile <- file.path(outdir,v,"Propriétés","Arbres_éclairés",paste0(v,"_","Global_zoom.svg"))

    list(src = normalizePath(outfile),
         contentType = 'image/svg+xml',
         width = 1244,
         height = 837,
         alt = "Global zoom",
         class="center")
  })

  #### Définition des changements de tab au click

  ## Renvoie vers les arbres éclairés
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
    newtab <- "global_zoom"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })

  ## Renvoie vers les indicateurs
  observeEvent(input$detail_ae, {
    newtab <- "indic_ae"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$detail_st, {
    newtab <- "indic_st"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$detail_eco, {
    newtab <- "indic_eco"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })

  ## Renvoie vers la page d'accueil
  observeEvent(input$return_0, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_1, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_2, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_3, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_4, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_5, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_6, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_7, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_8, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_9, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_10, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_11, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_12, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_13, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_14, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })
  observeEvent(input$return_15, {
    newtab <- "synthese"
    updateTabItems(session,inputId="tabs",selected = newtab)
  })



  ## Plots dimension (ceux des indicateurs ne sont actuellement plus utilisés côté UI, remplacés par des progress bar)
  output$composantes <- renderPlot({

    p <- IDEAresdim()[[1]]$composantes + theme(legend.text = element_text(family = "Roboto"))

    print(p)


  })
  output$indic_eco <- renderPlot({

    p <- IDEAresdim()[[1]]$indic_Economique
    print(p)


  })
  output$indic_st <- renderPlot({

    p <- IDEAresdim()[[1]]$`indic_Socio-Territoriale`

    print(p)


  })
  output$indic_ae <- renderPlot({

    p <- IDEAresdim()[[1]]$indic_Agroécologique

    print(p)

  })


  ## Radars propriétés
  output$robust_radar <- renderPlot({
    p <- IDEAresrad()[[1]]$Robustesse  + theme(legend.text = element_text(family = "Roboto"))
    print(p)
  })
  output$auto_radar <- renderPlot({
    p <- IDEAresrad()[[1]]$Autonomie  + theme(legend.text = element_text(family = "Roboto"))
    print(p)
  })
  output$cp_radar <- renderPlot({
    p <- IDEAresrad()[[1]]$`Capacité productive et reproductive \nde biens et de services`  + theme(legend.text = element_text(family = "Roboto"))
    print(p)
  })
  output$rg_radar <- renderPlot({
    p <- IDEAresrad()[[1]]$`Responsabilité globale`  + theme(legend.text = element_text(family = "Roboto"))
    print(p)
  })
  output$an_radar <- renderPlot({
    p <- IDEAresrad()[[1]]$`Ancrage Territorial`  + theme(legend.text = element_text(family = "Roboto"))
    print(p)
  })

  ## Table de légende de fin
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


  # Rapport automatisé ------------------------------------------------------

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"

    filename = function() {
      v <- IDEAdata()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
      paste0("Rapport_IDEA4_",v, ".pdf")
    },

    content = function(file) {


      withProgress(message = "Rendu du rapport en cours........", detail = "Merci de patienter quelques instants", value = 0.2,{

        inFile <- input$files

        tempReport <- file.path(tempdir(), "rapport_individuel.Rmd")
        template <- system.file("IDEAToolsApp/rapport_individuel.Rmd", package = "IDEATools")
        file.copy(template, tempReport, overwrite = TRUE)

        # Définition des paramètres pour le rendu
        params <- list(data = IDEAdata(),
                       outdir = outdir,
                       anon = FALSE)

        # Rendu du document dans un sous-environnement isolé
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        incProgress(0.8)
      })

    }
  )


  # Téléchargement .zip -----------------------------------------------------
  output$zipfile <- downloadHandler(

    filename = function() {
      v <- IDEAdata()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
      paste0("Figures_",v, ".zip")
    },

    content = function(fname) {

      withProgress(message = "Production des figures en cours........", detail = "Merci de patienter quelques instants", value = 0.2,{


        ## Production des figures nécessaires à l'archive
        v <- IDEAdata()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
        IDEAresdim() %>% IDEATools::exportIDEA(outdir = outdir)
        IDEAresrad() %>% IDEATools::exportIDEA(outdir = outdir)
        IDEATools::MakeTrees(IDEAdata()) %>% IDEATools::exportIDEA(outdir = outdir)


        ## Définition du chemin des fichiers à archiver
        setwd(outdir)
        fs <- file.path(v,list.files(file.path(outdir,v), recursive=TRUE))

        ## Ces trois fichiers sont générés en plus de leur version "correctement nommée" pour la production du rapport,
        ## qui est codé en LaTeX et a besoin de chemin de fichiers sans accents ni espaces...
        ## Il n'ont pas à être dans l'archive zip
        fs <- setdiff(fs,fs[stringr::str_detect(fs,"Ancrage.pdf")])
        fs <- setdiff(fs,fs[stringr::str_detect(fs,"CAP.pdf")])
        fs <- setdiff(fs,fs[stringr::str_detect(fs,"RESP.pdf")])

        # Export du zip
        zip(zipfile = fname, files = fs)

        incProgress(0.8)
      })

    }
    ,contentType = "application/zip"
  )



}
