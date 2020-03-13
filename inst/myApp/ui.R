source("global.R")

# Define UI for application
ui = dashboardPage(skin = "green",

                   ## Le header
                   dashboardHeader(title="IDEATools", titleWidth = 300),

                   ## Le sidebar, avec l'input, des item et subitem
                   dashboardSidebar(width = 350,h4(
                     fileInput("files", "Charger le calculateur (format excel)", accept = c(".xls",".xlsx",".json")),
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
                                 menuItem("Arbres éclairés", tabName = "tree", icon = icon("project-diagram"),
                                          menuSubItem("Synthèse",tabName = "global_zoom", icon = icon("sitemap")),
                                          menuSubItem("Robustesse",tabName = "robustesse", icon = icon("sitemap")),
                                          menuSubItem("Autonomie",tabName = "autonomie", icon = icon("sitemap")),
                                          menuSubItem("Capacité Productive",tabName = "cp", icon = icon("sitemap")),
                                          menuSubItem("Responsabilité Globale",tabName = "rg", icon = icon("sitemap")),
                                          menuSubItem("Ancrage Territorial",tabName = "an", icon = icon("sitemap")),
                                          menuSubItem("Global",tabName = "global", icon = icon("sitemap"))),
                                 menuItem("Légende",tabName = "legende",icon = icon("book")))
                   )),


                   ## Corps du Dashboard
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
                     tags$head(tags$style(".progress-bar{background-color:#2666cc2666cc;}")),
                     tags$head(tags$style(HTML('.logo {
                              background-color: #2666cc !important;
                              }
                              .navbar {
                              background-color: #2666cc !important;
                              }
                              '))),



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
                               box(flexdashboard::gaugeOutput("plt1"),width=4, title= uiOutput("title_ae"), height = "300px"),
                               box(flexdashboard::gaugeOutput("plt2"),width=4, title= uiOutput("title_st"), height = "300px"),
                               box(flexdashboard::gaugeOutput("plt3"),width=4, title= uiOutput("title_eco"), height = "300px"),

                               # Emplacement des 5 infobox de propriétés + un footer
                               box(title = "Qualification des 5 propriétés de la durabilité pour l'exploitation (cliquer sur une propriété pour obtenir son arbre détaillé) :",
                                   infoBoxOutput("prop1", width = 2),
                                   infoBoxOutput("prop2", width = 2),
                                   infoBoxOutput("prop5", width = 2),
                                   infoBoxOutput("prop4", width = 3),
                                   infoBoxOutput("prop3", width = 3),
                                   width = 12, height = 200,
                                   footer = uiOutput("globalfooter", inline = TRUE)),

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
                                     imageOutput("auto_tree")), width = 12, height = "720px")),

                       tabItem(tabName = "cp",
                               box(
                                 div(style="display:inline-block;width:100%;text-align: center;",
                                     imageOutput("cp_tree")), width = 12, height = "720px")),

                       tabItem(tabName = "rg",
                               box(
                                 div(style="display:inline-block;width:100%;text-align: center;",
                                     imageOutput("rg_tree")), width = 12, height = "720px")),

                       tabItem(tabName = "an",
                               box(
                                 div(style="display:inline-block;width:100%;text-align: center;",
                                     imageOutput("an_tree")), width = 12, height = "730px")),

                       tabItem(tabName = "global",
                               box(
                                 div(style="display:inline-block;width:100%;text-align: center;",
                                     imageOutput("global_tree")), width = 12, height = "900px")),

                       tabItem(tabName = "global_zoom",
                               box(
                                 div(style="display:inline-block;width:100%;text-align: center;",
                                     imageOutput("global_zoom_tree")), width = 12, height = "900px")),


                       ## Composantes
                       tabItem(tabName = "compo",
                               box(plotOutput("composantes", height = "800px"), width = 12)),


                       ## Détail des indicateurs par dimension
                       tabItem(tabName = "indic",fluidPage()),

                       tabItem(tabName = "indic_eco",
                               h1("Indicateurs Economiques"), h4(uiOutput("return_1")),
                               br(),
                               box(title = textOutput("title_c10"),
                                   uiOutput("plots_c10"), width = 6),
                               box(title = textOutput("title_c11"),
                                   uiOutput("plots_c11"), width = 6),
                               box(title = textOutput("title_c12"),
                                   uiOutput("plots_c12"), width = 6),
                               box(title = textOutput("title_c13"),
                                   uiOutput("plots_c13"), width = 6)),
                       tabItem(tabName = "indic_st",
                               h1("Indicateurs Socio-Territoriaux"), h4(uiOutput("return_2")),
                               br(),
                               box(title = textOutput("title_c6"),
                                   uiOutput("plots_c6"), width = 6),
                               box(title = textOutput("title_c7"),
                                   uiOutput("plots_c7"), width = 6),
                               box(title = textOutput("title_c8"),
                                   uiOutput("plots_c8"), width = 6),
                               box(title = textOutput("title_c9"),
                                   uiOutput("plots_c9"), width = 6)),
                       tabItem(tabName = "indic_ae",
                               h1("Indicateurs Agroécologiques"), h4(uiOutput("return_3")),
                               br(),
                               box(title = textOutput("title_c1"),
                                 uiOutput("plots_c1"), width = 6),
                               box(title = textOutput("title_c2"),
                                   uiOutput("plots_c2"), width = 6),
                               box(title = textOutput("title_c3"),
                                   uiOutput("plots_c3"), width = 6),
                               box(title = textOutput("title_c4"),
                                   uiOutput("plots_c4"), width = 6),
                               box(title = textOutput("title_c5"),
                                   uiOutput("plots_c5"), width = 6)
                       ),


                       ### Détail des indicateurs par propriété
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


                       ### Table générale des indicateurs
                       tabItem(tabName = "legende",
                               h1("Légende des indicateurs"),
                               DT::dataTableOutput("legende_out",height = "800px"))
                     )


                   )
)



