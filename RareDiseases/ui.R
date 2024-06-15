################################################################################
#                              USER INTERFACE
################################################################################




# Packages used in the app

library(shiny)
library(shinybrowser)
library(shinyWidgets) 
library(shinyjs)
library(ggplot2)
library(dplyr)
library(highcharter)
library(plotly)
library(heatmaply)

# User interface definition

tagList(
      
      # Adjustment to device size
      
      tags$head(tags$script('var dimension = [0, 0];
                  	$(document).on("shiny:connected", function(e) {
                  		dimension[0] = window.innerWidth;
                  		dimension[1] = window.innerHeight;
                  		Shiny.onInputChange("dimension", dimension);
                  	});
                  	$(window).resize(function(e) {
                  		dimension[0] = window.innerWidth;
                  		dimension[1] = window.innerHeight;
                  		Shiny.onInputChange("dimension", dimension);
                  	});
                  '),
                
      # Creates page with a top level navigation bar and several tabs panels
                     
      navbarPage(title =  div(tags$a(tags$img(src="lazo.svg", height=40)), "Rare diseases explorer", style = "position: relative; top: -8px;"),
                   theme = "style/style.css",
                   fluid = TRUE, 
                   collapsible = TRUE,
                 
                   # ----------------------------------
                   # tab panel 1 - Home
                 
                   tabPanel("Home",
                            # Initial text
                            fluidRow(column(width=1), column(width=10, h5("Rare diseases, often referred to as orphan diseases, are medical conditions that affect a small percentage of the population. The European Union (EU) defines a rare disease as one that affects fewer than 1 in 2,000 individuals. Despite their individual rarity, collectively, rare diseases affect millions of people worldwide. These conditions can vary widely in their symptoms, severity, and prognosis.")),column(width=1)),
                            # Graphs
                            h3("OVERVIEW"),
                            fluidRow(column(width=6, 
                                            fluidRow(h4("INHERITANCE"),highchartOutput("hc_BarcharHerencia", height = "35vh")),
                                            fluidRow(h4("AGE ONSET"),highchartOutput("hc_BarcharEdadAparicion", height = "35vh"))),
                                     column(width = 6,h4("SYMPTOMS"), highchartOutput("hc_PackedBubble", height = "70vh")))
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Diseases
                 
                   tabPanel("Diseases",
                            # Disease name and search box
                            fluidRow(style='height:10vh', 
                                     column(width = 7, textOutput("enfermedadSeleccionadaOut")),
                                     column(width = 5,
                                           shiny::selectizeInput(
                                                 inputId = "buscadorEnfermedad",
                                                 label = "Search a disease:",
                                                 multiple = FALSE,
                                                 choices = NULL,
                                                 options = list(
                                                       create = FALSE,
                                                       placeholder = "Ej.: Alexander disease",
                                                       maxItems = '1',
                                                       onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                       onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                 ),
                                                 selected = "Alexander disease")
                                     )),
                            # Disease text description
                            fluidRow(column(width=1), column(width=10,textOutput("enfermedadSeleccionadaTextoOut")),column(width=1)),
                            # Graphs
                            fluidRow(h3("OVERVIEW"),
                                     # Symptoms
                                     column(width = 5, fluidRow(h4("SYMPTOMS")), highchartOutput("treemapOut", height = "60vh"),
                                            tags$p("Legend: 5 (mandatory - 100%), 4 (very frequent - 99-80%), 3 (frequent - 79-30%), 2 (occasional - 29-5%) and 1 (very rare - 4-1%)."),
                                     ),
                                     column(width = 7,
                                          # Age onset
                                          fluidRow(style='height:25vh',fluidRow(h4("AGE ONSET")),
                                                   column(width=1,fluidRow(
                                                uiOutput("edadAparicionAntenatalImgOut")),
                                                      fluidRow(h5("Antenatal"))),
                                                column(width=1,fluidRow(
                                                uiOutput("edadAparicionNeonatalImgOut")),
                                                      fluidRow(h5("Neonatal"))),
                                                column(width=1,fluidRow(
                                                      uiOutput("edadAparicionInfanciaImgOut")),
                                                      fluidRow(h5("Infancy"))),
                                                column(width=1,fluidRow(
                                                      uiOutput("edadAparicionNinezImgOut")),
                                                      fluidRow(h5("Childhood"))),
                                                column(width=1,fluidRow(
                                                      uiOutput("edadAparicionAdolescenciaImgOut")),
                                                      fluidRow(h5("Adolescence"))),
                                                column(width=1,fluidRow(
                                                      uiOutput("edadAparicionAdultoImgOut")),
                                                      fluidRow(h5("Adulthood"))),
                                                column(width=1,fluidRow(
                                                      uiOutput("edadAparicionVejezImgOut")),
                                                      fluidRow(h5("Elderly")))
                                                   ),
                                            fluidRow(style='height:20vh',
                                                     # Logo
                                                     column(width = 3, uiOutput("logoImg")),
                                                     # Genes
                                                     column(width = 9,fluidRow(h4("RELATED GENES")), fluidRow(textOutput("enfermedadGenesRelacionados")))
                                                     ),
                                            # Inheritance
                                            fluidRow(style='height:25vh',fluidRow(h4("INHERITANCE")),column(width=1,fluidRow(
                                                        uiOutput("herenciaAutDomImgOut")),
                                                        fluidRow(h5("Autosomal dominant"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaAutRecImgOut")),
                                                        fluidRow(h5("Autosomal recessive"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaMultigenicaImgOut")),
                                                        fluidRow(h5("Multigenic"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaXDomImgOut")),
                                                        fluidRow(h5("X-linked dominant"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaXRecImgOut")),
                                                        fluidRow(h5("X-linked recessive"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaYImgOut")),
                                                        fluidRow(h5("Y-linked"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaMitocondrialImgOut")),
                                                        fluidRow(h5("Mitochondrial"))),
                                                  column(width=1,fluidRow(
                                                        uiOutput("herenciaSemidominanteImgOut")),
                                                        fluidRow(h5("Semidominance")))
                                            ),
                                     
                                     )
                        )
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Analysis
                   tabPanel("Analysis",
                            sidebarLayout(
                                  sidebarPanel(width = 3,
                                               # Search by disease
                                               h3("DISEASE:"),
                                               shiny::selectizeInput(
                                                     inputId = "buscadorEnfermedad2",
                                                     label = "Search a disease:",
                                                     multiple = FALSE,
                                                     choices = NULL,
                                                     options = list(
                                                           create = FALSE,
                                                           placeholder = "Ej.: Alexander disease",
                                                           maxItems = '1',
                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                           onType = I("function (str) {if (str === \"\") {this.close();}}")
                                                     ),
                                                     selected = "Alexander disease"),
                                               numericInput("nEnfermedades", "Number of similar diseases:", 10, max = 200),
                                               tags$head(
                                                     tags$style(HTML("
                                                      .center-text {
                                                        text-align: center;
                                                      }
                                                    "))
                                               ),
                                               tags$p("Search diseases that share symptoms with the selected disease"),
                                               actionButton("searchDiseases2","Search"),
                                               # Search by characteristics
                                               h4("OR",  class = "center-text"),
                                               h3("LIST:"),
                                               pickerInput("edadAparicion", "Age onset:", 
                                                           choices = c("All", "Antenatal", "Neonatal", "Infancy", "Childhood", "Adolescence", "Adulthood", "Elderly"), 
                                                           multiple=TRUE,
                                                           selected = c("All")), 
                                               pickerInput("herencia", "Inheritance:",
                                                           choices = c("All", "Autosomal dominant","Autosomal recessive","X linked dominant","X linked recessive","Y linked" ,"Oligogenic", "Multigenic multifactorial","Mitochondrial inheritance","Semi dominant"),
                                                           multiple=TRUE,
                                                           selected = "All"),
                                               pickerInput("genes", "Genes:",
                                                           choices = c("All", unique(datosGenes$Gene)),
                                                           multiple=TRUE,
                                                           selected = "All"),
                                               pickerInput("GruposSintomas", "Groups of symptoms:",
                                                           choices = c("All", listaSintomasGrupos),
                                                           multiple=TRUE,
                                                           selected = "All"),
                                               pickerInput("sintomas", "Symptoms:",
                                                           choices = c("All", listaSintomasTodos),
                                                           multiple=TRUE,
                                                           selected = "All"),
                                               tags$p("Filter diseases by characteristics or symptoms"),
                                               actionButton("searchDiseases","Search")
                                  ),
                                  mainPanel(width = 9,
                                          tags$p("Legend: 5 (mandatory - 100%), 4 (very frequent - 99-80%), 3 (frequent - 79-30%), 2 (occasional - 29-5%) and 1 (very rare - 4-1%)."),
                                          plotlyOutput("interactiveHeatmap", height='1000px'))
                                  
                            )
                   ),
                   
                   # ----------------------------------
                   # tab panel 4 - About the web
                   tabPanel("About",
                            tags$div( id= "About_panel",
                            style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 90vh;",
                             h5("This webside contains data for research and educational use only. Users seeking information about a personal medical or genetic condition are urged to consult with a qualified physician for diagnosis and for answers to personal questions."),
                             h5("This webside uses data from Orphanet, an online rare disease and orphan drug data base. © INSERM 1999. Available on http://www.orpha.net. Accessed November 2023."),
                             h5("Orphadata: Free access data from Orphanet. © INSERM 1999. Available on https://www.orphadata.com."),
                             h5("This software and its derivatives may not be used for commercial purposes without explicit written permission from the author. Commercial use includes, but is not limited to, selling, sublicensing, or using the software as part of a service for which a fee is charged."),
                             h5(),
                             h5("Elisa Díaz de la Guardia Bolívar. Software Engeneering bachelor's degree final thesis. Universidad Internacional de La Rioja."),
                             ),
                            h5(),
                            tags$style('div#About_panel {background-image: url(lazo2.svg); background-size: 40vh ;background-position: center; background-repeat:no-repeat; background-image-opacity:0.6;}')

                   ) 
)))
