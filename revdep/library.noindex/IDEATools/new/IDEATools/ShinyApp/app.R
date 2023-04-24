library(shiny)
library(IDEATools)

# Define UI for application that draws a histogram
ui <-   fluidPage(title = "Interface d'utilisation simplifiée IDEATools",

                     # Sidebar with a slider input for number of bins
                     tabsetPanel(
                       tabPanel("Analyse individuelle",
                                br(),
                                fileInput("solo_input","Chargez votre fichier", width = "30%", multiple = FALSE),
                                br(),
                                uiOutput("choices"),
                                uiOutput("fileinput"),
                       ),
                       tabPanel("Analyse de groupe",
                                br(),
                                fileInput("dir_input","Sélectionnez vos fichiers", multiple = TRUE, width = "30%"),
                                br(),
                                uiOutput("choices_dir"),
                                uiOutput("filesinput")
                       ),
                       tabPanel("Jsonify",
                                p("Dans le cas où la macro de conversion ne fonctionne pas sur votre calculateur excel, vous pouvez insérer ici votre calculateur au format XLS ou XLSX pour obtenir un fichier .JSON en retour, pouvant être utilisé sur cette plateforme ou d'autres outils IDEA en ligne."),
                                p("Note : L'algorithme de conversion JSON embarqué dans cette application va convertir les items et métadonnées issus du calculateur afin de correspondre au format le plus à jour de JSON (ajout/suppressions d'items, changement de format des métadonnées, etc.)"),
                                h5("Important : Le calculateur excel doit être intégralement rempli, c'est à dire que les 53 indicateurs doivent avoir une note et les métadonnées obligatoires en début de questionnaire doivent être remplies"),
                                fileInput("json_input","Sélectionnez votre fichier",multiple = FALSE, width = "30%"),
                                br(),
                                uiOutput("json_output")
                       )
                     )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  options(shiny.maxRequestSize=30*1024^2)

  observeEvent(input$solo_input, {

    output$choices <- renderUI({
      selectInput("format","Format d'export", choices = c("PDF"="pdf", "Excel"="xlsx","Word"="docx","Powerpoint"="pptx","Figures brutes"="zip"), selected = "pdf", width = "30%")
    })


    output$fileinput <- renderUI({
      downloadButton("diag_solo", label = "Lancer le diagnostic", icon = icon("play"))
    })


  })


  observeEvent(input$dir_input, {

    output$choices_dir <- renderUI({
      selectInput("format_dir","Format d'export", choices = c("PDF"="pdf", "Excel"="xlsx","Word"="docx","Powerpoint"="pptx","Figures brutes"="zip"), selected = "pdf", width = "30%")
    })


    output$filesinput <- renderUI({
      downloadButton("diag_dir", label = "Lancer le diagnostic", icon = icon("play"))
    })


  })

  observeEvent(input$json_input, {

    output$json_output <- renderUI({
      downloadButton("json_download", label = "Lancer la conversion", icon = icon("play"))
    })


  })


  # Download solo -----------------------------------------------------------


  output$diag_solo <- downloadHandler(

    filename = function() {
      format = input$format

      if(format == "zip") {
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$solo_input$name)), start = 1, stop = 11)
        paste0("Figures_", file_name_short, ".",format)
      } else {
        file_name_short <- substr(basename(tools::file_path_sans_ext(input$solo_input$name)), start = 1, stop = 11)
        paste0("Rapport_individuel_", file_name_short, ".",format)

      }


    },

    content = function(file) {
      format = input$format
      withProgress(value = 0,message = 'Production du diagnostic en cours...',{

        shiny::incProgress(1/10)

        file_name_short <- substr(basename(tools::file_path_sans_ext(input$solo_input$name)), start = 1, stop = 11)

        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir, mode = "0777"))

        shiny::incProgress(2/10)

        if(format == "zip") {

          outdir <- file.path(tempdir(), "Figures")

          diag_idea(input$solo_input$datapath,
                    output_directory = outdir, prefix = file_name_short,
                    export_type = "local", type = "single", quiet = TRUE
          )

          shiny::incProgress(8/10)

          ## Définition du chemin des fichiers à archiver
          oldwd <- getwd()
          setwd(file.path(outdir, Sys.Date()))

          fs <- list.files(file_name_short, recursive = TRUE, full.names = TRUE)

          # Export du zip
          zip::zip(zipfile = file, files = fs)
          setwd(oldwd)


        } else {

          diag_idea(input$solo_input$datapath,
                    output_directory = knitting_dir, prefix = file_name_short,
                    export_type = "report", type = "single", quiet = TRUE, report_format = format
          )

          shiny::incProgress(8/10)

          file.copy(file.path(knitting_dir, Sys.Date(), file_name_short, paste0("Rapport_individuel_", file_name_short, ".",format)), file)

        }
      })
    }
  )


  # Download dir ------------------------------------------------------------


  output$diag_dir <- downloadHandler(

    filename = function() {

      format = input$format_dir

      if(format == "zip") {
        paste0("Figures_groupe_", length(input$dir_input$datapath), ".",format)
      } else {
        paste0("Rapport_groupe_", length(input$dir_input$datapath), ".",format)
      }


    },

    content = function(file) {
      format = input$format_dir
      withProgress(value = 0,message = 'Production du diagnostic en cours...',{

        shiny::incProgress(1/10)

        # Defining a knitting dir in tempdir in case the user doesn't have all permissions in working dir
        knitting_dir <- file.path(tempdir(), "IDEATools_reports")
        if (!dir.exists(knitting_dir)) (dir.create(knitting_dir, mode = "0777"))

        shiny::incProgress(2/10)

        if(format == "zip") {

          outdir <- file.path(tempdir(), "Figures")

          diag_idea(dirname(input$dir_input$datapath[[1]]),
                    output_directory = outdir, prefix = file_name_short,
                    export_type = "local", type = "group", quiet = TRUE
          )

          shiny::incProgress(8/10)

          ## Définition du chemin des fichiers à archiver
          current_dir <- getwd()
          setwd(file.path(outdir, Sys.Date()))

          fs <- list.files(paste0("Groupe_",length(input$dir_input$datapath)), recursive = TRUE, full.names = TRUE)

          # Export du zip
          zip::zip(zipfile = file, files = fs)
          setwd(current_dir)


        } else {

          diag_idea(dirname(input$dir_input$datapath[[1]]),
                    output_directory = knitting_dir, prefix = file_name_short,
                    export_type = "report", type = "group", quiet = TRUE, report_format = format
          )

          shiny::incProgress(8/10)

          file.copy(file.path(knitting_dir, Sys.Date(), paste0("Groupe_",length(input$dir_input$datapath)), paste0("Rapport_groupe_", length(input$dir_input$datapath), ".",format)), file)

        }
      })
    }
  )


  # Download JSON -----------------------------------------------------------

  output$json_download <- downloadHandler(

    filename = function() {
      file_name_short <- substr(basename(tools::file_path_sans_ext(input$json_input$name)), start = 1, stop = 11)
      paste0(file_name_short,".json")
    },

    content = function(file) {
      IDEATools:::jsonify2(input = input$json_input$datapath,output = file)
    }
  )




}

# Run the application
shinyApp(ui = ui, server = server)
