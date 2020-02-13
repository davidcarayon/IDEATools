library(shiny)
library(shinydashboard)
library(IDEATools)

ui <- dashboardPage(skin = "yellow",
    dashboardHeader(title="Production de cartes heuristiques", titleWidth = 350),
    dashboardSidebar(width = 350,
                     passwordInput("password", "Saisir le mot de passe :"),
                     numericInput("organisme","Saisir l'identifiant de l'organisme", value = 1),
                     textInput("dossier", "Saisir le numéro de dossier", value = "654321"),
                     numericInput("annee","Saisir l'année", value = 2009),
                     actionButton("connect","Rechercher", width = "90%")),
    dashboardBody(
        box(
        div(style="display:inline-block;width:100%;text-align: center;",imageOutput("global_zoom_tree")),height = "900px",width = 12), uiOutput("dl")



        )
)

server <- function(input, output) {

    outdir <- tempdir(getwd())


    databdd <- eventReactive(input$connect,{
        d <- QueryWebIDEA(password = input$password, id_organisme = input$organisme,id_dossier = input$dossier,annee = input$annee)

        if(nrow(d$dataset) == 0) {return(NULL)}

        return(d)

    })


    observeEvent(input$connect, {
        if(is.null(databdd())) {stop("Le dossier n'existe pas")}
        withProgress(message = "Coloration des cartes heuristiques...", detail = "Merci de patienter quelques instants", value = 0.2, {

            IDEATools::MakeTrees(databdd()) %>% IDEATools::exportIDEA(outdir = outdir)
            incProgress(0.8)})

    })





    output$global_zoom_tree <- renderImage({

        inFile <- input$connect

        if (is.null(inFile))
            return(box())

        val <- databdd()$metadata$MTD_01
        v <- stringr::str_replace_all(val, " ", "_")

        outfile <- file.path(outdir,v,"Propriétés","Cartes_heuristiques","Global_zoom.png")

        list(src = normalizePath(outfile),
             contentType = 'image/png',
             width = 1300,
             height = 890,
             alt = "Global zoom",
             class="center")
    })




output$dl <- renderUI({

    inFile <- input$connect

    if (is.null(inFile))
        return(box())

    downloadButton("zipfile", "Télécharger le détail des cartes heuristiques", style = "width:100%;")


})







    output$zipfile <- downloadHandler(
        # For PDF output, change this to "report.pdf"

        filename = function() {
            v <- databdd()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
            paste0("Cartes_heuristiques_",v, ".zip")
        },

        content = function(fname) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).

            withProgress(message = "Production des cartes heuristiques en cours........", detail = "Merci de patienter quelques instants", value = 0.2,{

                v <- databdd()$metadata$MTD_01 %>%  stringr::str_replace_all(" ", "_")
                IDEATools::MakeTrees(databdd()) %>% IDEATools::exportIDEA(outdir = outdir)

                setwd(outdir)
                fs <- file.path(v,list.files(file.path(outdir,v), recursive=TRUE))


                zip(zipfile = fname, files = fs)

                incProgress(0.8)
            })

        }
        ,contentType = "application/zip"
    )




}

shinyApp(ui, server)
