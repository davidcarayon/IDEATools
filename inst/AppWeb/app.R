library(shiny)
library(shinydashboard)
library(IDEATools)

ui <- dashboardPage(skin = "red",
    dashboardHeader(title="Production de cartes heuristiques", titleWidth = 350),
    dashboardSidebar(width = 350,
                     numericInput("organisme","Saisir l'identifiant de l'organisme", value = 1),
                     actionButton("bdd", "Connexion à la base de données", width = "90%"),
                     uiOutput("selection_dossier"),
                     uiOutput("selection_annee"),
                     uiOutput("connexion"),
                     br(),
                     p("Contact : David CARAYON (INRAE)", align = "center"),
                     p("david.carayon@inrae.fr", align = "center")),
    dashboardBody(
        box(
        div(style="display:inline-block;width:100%;text-align: center;",imageOutput("global_zoom_tree")),height = "900px",width = 12), uiOutput("downloadFile")
       )
)

server <- function(input, output) {

    outdir <- tempdir(getwd())


liste_possibles <- eventReactive(input$bdd, {

        BDD_IDEA <- DBI::dbConnect(RPostgres::Postgres(),dbname = "idea_v4_db",host="194.199.250.56", port=5432,user = "idea_v4_user_readonly", password = "idea_v4_user_readonly_password")

        id_organisme = input$organisme

        # Définition de la requête SQL --------------------------------------------
        custom_query <- paste0("SELECT DISTINCT dossier,annee FROM valeur_indicateur v WHERE v.organisme_id = ",id_organisme)

        ## Requête
        IDEAQuery <- DBI::dbGetQuery(BDD_IDEA,custom_query)

        # Disconnect --------------------------------------------------------------
        DBI::dbDisconnect(BDD_IDEA)
        rm(BDD_IDEA)

        dplyr::tbl_df(IDEAQuery)

    })



observeEvent(input$bdd,{

    output$selection_dossier <- renderUI({
        choices <- liste_possibles()$dossier
        selectInput("input_dossier", "Choisir le numéro de dossier", choices = choices)
    })


    output$selection_annee <- renderUI({
        choices <- subset(liste_possibles(),dossier == input$input_dossier)$annee
        selectInput("input_annee", "Choisir l'année", choices = choices)
    })

})

output$connexion <- renderUI({

    if(is.null(input$input_dossier)){return(NULL)}

    actionButton("connect","Rechercher", width = "90%", style = "font-size:150%")

})

databdd <- eventReactive(input$connect,{

        d <- QueryWebIDEA(id_organisme = input$organisme,id_dossier = input$input_dossier,annee = input$input_annee)

        if(nrow(d$dataset) == 0) {return(NULL)}

        return(d)

    })

observeEvent(input$connect, {
        if(is.null(databdd())) {stop("Le dossier n'existe pas")}
        withProgress(message = "Coloration des cartes heuristiques...", detail = "Merci de patienter quelques instants", value = 0.2, {

            IDEATools::MakeTrees(databdd()) %>% IDEATools::exportIDEA(outdir = outdir)
            incProgress(0.8)})


        output$downloadFile <- renderUI({

            inFile <- input$connect

            if (is.null(inFile))
                return(box())

            downloadButton("zipfile", "Télécharger le détail des cartes heuristiques", style = "width:100%; color: #000000; background-color: #FFA500; border-color: #000000; padding:8px; font-size:150%")


        })


    })

output$global_zoom_tree <- renderImage({

        val <- databdd()$metadata$MTD_01
        file_id <- stringr::str_replace_all(val, " ", "_")

        outfile <- file.path(outdir,file_id,"Propriétés","Cartes_heuristiques","Global_zoom.png")

        list(src = normalizePath(outfile),
             contentType = 'image/png',
             width = 1300,
             height = 890,
             alt = "Global zoom",
             class="center")
    })











# Files -------------------------------------------------------------------

output$zipfile <- downloadHandler(
        # For PDF output, change this to "report.pdf"

        filename = function() {
            file_id <- databdd()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
            paste0("Cartes_heuristiques_",input$input_dossier,"_",input$input_annee, ".zip")
        },

        content = function(fname) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).

            withProgress(message = "Production des cartes heuristiques en cours........", detail = "Merci de patienter quelques instants", value = 0.2,{

                file_id <- databdd()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
                IDEATools::MakeTrees(databdd()) %>% IDEATools::exportIDEA(outdir = outdir)

                setwd(outdir)
                fs <- file.path(file_id,list.files(file.path(outdir,file_id), recursive=TRUE))


                zip(zipfile = fname, files = fs)

                incProgress(0.8)
            })

        }
        ,contentType = "application/zip"
    )




}

shinyApp(ui, server)
