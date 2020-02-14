#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(markdown)
library(gapminder)
library(colourpicker)
#library(devtools)
library(lubridate)
library(rsconnect)
library(forecast)
library(ggplot2)
library(ggfortify)
library(ggplot2)
library(highcharter)
library(zoo)
library(tseries)
#library(data.world)
library(rtweet)
library(quantmod)
library(RMySQL)
library(shiny)
library(RJSONIO)
library(Rbitcoin)
library(RJSONIO)
library(crypto)
library(shiny)
library(dplyr)
library(purrr)
library(tidyverse)
library(rlang)
library(stringr)
library(DT)
library(r2d3)
library(shinyWidgets)
library(shinydashboardPlus)
library(shiny.i18n)
library(shinythemes)
library(nycflights13)
library(quantmod)
#library(shinythemes)
library(rnoaa)
#library(data.world)
library(rtweet)
library(shiny)
#library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(caret)
library(broom)
library(lattice)
library(tidyr)
library(purrr)
library(Lahman)
#library(devtools)
#library(de)
#install_github("nik01010/dashboardthemes")
# Define UI for application that draws a histogram
theme <- c("flat","f538", "chalk", "darkunica", "db","economist","elementary","ffx", "ggplot2","google","gridlight","handrawn","monokai","sandsignika",
           "smpl","sparklike", "superheroes","tufte","tufte2")
dataset <- c("trainingset", "testingset","forecast","rsqr", "fitparameter", "predicted","newdata")
ui <- dashboardPage(
    dashboardHeader(color = "blue",title = "Randstad Analytics", inverted = TRUE),
    # dashboardHeader(
    #     title = "Example of a long title that needs more space",
    #     titleWidth = 450
    # ),
    # navbarPage(
    #     tabPanel("Main"),
    #     tabPanel("Statistiks"),
    #     tabPanel("Plot"),
    #     tabPanel("Machine Learning")
    # ),
    dashboardSidebar(
        size = "thin", color = "teal",
        sidebarMenu(
            menuItem(tabName = "extra", "Zeitreihen Prognose", icon = icon("car")),
            menuItem(tabName = "main", "Regression", icon = icon("chart-line")),
            menuItem(tabName = "Data", "Klassifikation Algorithmen", icon = icon("bar")),
            #menuItem(tabName = "Chart", "Chart", icon = icon("chart-line")),
           # menuItem(tabName = "ML", "Mal", icon = icon("car")),
            h4("Modell aussuchen"),
            prettyRadioButtons(inputId = "choice",  label = "Select a model",
                               choices = c("One", "Two", "Three", "Four","Five","Six","Seven","stl","stll"),
                               outline = TRUE, selected="One",
                               plain = TRUE, icon = icon("thumbs-up")),
           checkboxInput('all', 'Alle'),
           checkboxInput("fb", "Prophet", TRUE),
            h4("Periode"),
            uiOutput("freq"),
            h4("Startsdatum"),
            dateInput("date", label= "Format = yyyy-mm-dd", value = "2011-01-01"),
            h4("Vorhersagen"),
            sliderInput("h", "Tage,Woche,Monat oder  Jahr im Voraus:", 
                        min=1,step= 2, max=1000, value=2)
            
        )
        
    ),
    dashboardBody(
        tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                HTML('<meta name="viewport",content="width=1024">')
            ),
        # shinyDashboardThemes(
        #     theme = "blue_gradient"
        # ),
        tabItems(
            selected = 1,
            tabItem(
                tabName = "main",
                fluidRow(
                    
                    box(width = 8,
                        title = "Die Wichtigste Faktoren werden mit AI ermittelt sowie Modell Auswahl",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               #plotOutput("boxplot1")
                               uiOutput("barplot"),
                               uiOutput("RegressionR"),
                               uiOutput("Regressionslider")
                               
                        )
                    ),
                    box(width = 8,
                        title = "Korrelationsdiagramm sowie Parameter Auswahl",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               #plotlyOutput("dotplot1")
                               uiOutput("corr"),
                               uiOutput("radiobutton")
                        )
                    ),
                    
                    box(width = 5,
                        title = "Die Abhaengigspalte auswaehlen equivalent zu X",
                        color = "yellow", ribbon = TRUE, title_side = "top right",
                        column(width = 4,
                               #plotlyOutput("dotplot1")
                               valueBoxOutput("customerdata")
                        )
                    ),
                    box(width = 5,
                        title = "Her die Periode auswaehlen",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 4,
                               #plotlyOutput("dotplot1")
                               uiOutput("regfreq")
                        )
                    ),
                    box(width = 5,
                        title = "Die Zielvariable auswaehlen equivalent zu Y(X)",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 4,
                               #plotlyOutput("dotplot1")
                               uiOutput("Regressiondrop")
                        )
                    ),
                    # valueBoxOutput("customerdata"),
                    # #                      column(
                    # #                      width = 3,
                    #                      #uiOutput("RegressionR"),
                    #                      #uiOutput("Regressionslider"),
                    #                      uiOutput("Regressiondrop"),
                    #                           uiOutput("regfreq"),
                    # helpText("Is there any correlation in my data ?"),
                    #                       # uiOutput("corr"),
                    #                        helpText("Is there any correlation in my data ? See the correlation plot!1 indicates positve and highly correlated
                    #                                 while -1 negative correlation and 0 no correlation at all."),
                    # dataTableOutput("rand"),
                    # uiOutput("fitplot"),
                    
                    box(width = 16,
                        title = "Die Wichtigste Faktoren werden mit AI ermittelt sowie Modell Auswahl",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               dataTableOutput("rand")
                               
                        )
                    ),
                    box(width = 16,
                        title = "Regression  mit Vergleich zwischen vorhergesagte und eigentliche Werte.",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               uiOutput("fitplot")
                        )
                    ),
                    #uiOutput("barplot"),
                   # uiOutput("radiobutton"),
                    uiOutput("tidy"),
                                         #uiOutput("aug"),
                                           uiOutput("compare"),
                                           uiOutput("explan"),
                   # uiOutput("radiobutton"),
                    #                      uiOutput("tidy"),
                    #                      uiOutput("aug"),
                    #                      uiOutput("barplot")
                    #                      #uiOutput("radiobutton"),
                    uiOutput("glance")
                  

                )
            ),
            tabItem(
                tabName = "extra",
                fluidRow(
                    box(width = 4,
                        title = "Daten hochladen",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 4,
                               h3("Csv Datei hochladen"),
                                                 fileInput("file1", "Choose CSV File",
                                                           multiple = TRUE,
                                                           accept = c("text/csv",
                                                                      "text/comma-separated-values,text/plain",
                                                                      ".csv", ".zip",".xlsx")),
                               #br(),
                               #colourInput("colc", "Select  a colour for forecasted value", "red"),
                              # br(),
                               
                               radioButtons("disp", " ",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head"),
                              valueBoxOutput("perfomsaccu")

# h3("Choose quote"),
# br(),
#                                radioButtons("quote", "", 
#                                             choices = c(None = "",
#                                                         "Double Quote" = '"',
#                                                         "Single Quote" = "'"),
#                                             selected = '"')
                               
                        )
                    ),
                    box(width = 4,
                        title = "Header und Trennezeichen zu Auswahl",
                        color = "red", ribbon = TRUE, title_side = "top right",
                       
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        br(),
                        checkboxInput("stringsAsFactors", "stringsAsFactors", F),
                        br(),
                        radioButtons("sep", " ",

                                                                    choices = c(Comma = ",",
                                                                                Semicolon = ";",
                                                                                Tab = "\t", Space=""),
                                                                    selected = ","),
                        uiOutput("selecty")
                       # valueBoxOutput("perfomsaccu")
                        # br(),
                        #                                
                        #                                radioButtons("disp", " ",
                        #                                             choices = c(Head = "head",
                        #                                                         All = "all"),
                        #                                             selected = "head"),
                    
                       # colourInput("colc", "Select  a colour for forecasted value", "red")
                        # column(width = 8,
                        #        dataTableOutput("carstable")
                        # )
                    ),
box(width = 4,
    title = "Diagrammart zu Auswahl",
    color = "red", ribbon = TRUE, title_side = "top right",
    
    # Input: Checkbox if file has header ----
    checkboxInput('jitter', 'Jitter'),
                          checkboxInput('smooth', 'Smooth'),
                          checkboxInput('errorY', 'YErrorbars'),
                          checkboxInput('errorX', 'XErrorbars'),
                         colourInput("colploty", "Select colour", "red"),
                          selectInput(
                           inputId = "themes",
                           label = "select a theme :",
                           choices = theme,
                           selected = 'darkunica',
                           selectize = FALSE
                         )
                         # uiOutput("selecty")
                         # plotlyOutput('plot')
),
box(width = 4,
    title = "Diagrammart zu Auswahl",
    color = "red", ribbon = TRUE, title_side = "top right",
    
    # Input: Checkbox if file has header ----
    checkboxInput('boxplot', 'boxplot'),
                          checkboxInput('violin', 'violin'),
                          checkboxInput('line', 'line'),
                          #uiOutput("freq"),
                          uiOutput("selectivefile"),
                          colourInput("col", "Select background-colour", "purple"),
                          #valueBoxOutput("perfomsaccu"),
    colourInput("colc", "Select  a colour for forecasted value", "red")
),  #br(),
#  helpText("Hier wird ein Diagramm nach Wahl dargestellt."),                    #dataTableOutput("carstable"),
# br(),
#                       plotlyOutput('plot'),
# br(),
box(width = 16,
    title = "Diagrammsdarstellung",
    color = "red", ribbon = TRUE, title_side = "top right",
    column(width = 8,
           helpText("Hier wird ein Diagramm nach Wahl dargestellt."),                    #dataTableOutput("carstable"),
           br(),
           plotlyOutput('plot')
    )
),
box(width = 16,
    title = "Zeitreiheprognose Diagramm",
    color = "red", ribbon = TRUE, title_side = "top right",
    column(width = 8,
           br(),
           helpText("Hier Kommt Zeitreihe Prognose dargestellt."),                    #dataTableOutput("carstable"),
           br(),
           highchartOutput("custom"),
           dataTableOutput("modellet"),
           dataTableOutput("prophet")
    )
),
box(width = 16,
    title = "Darstellung der hochgeladene Daten als Tabelle.",
    color = "red", ribbon = TRUE, title_side = "top right",
    column(width = 8,
           #helpText("Display the data here in a table format."),
           br(),
           dataTableOutput("filedata")
    )
),
box(width = 16,
    title = "Zusammenfassung der Daten mit Max, Min, Mittelwert und usw..",
    color = "red", ribbon = TRUE, title_side = "top right",
    column(width = 8,
           #helpText("Render  a   summary. "),
           br(),
           dataTableOutput("summer")
    )
),
            dataTableOutput("numeric")
            
                )
                
             
                
                
            ),
            tabItem(
                tabName = "Data",
                
                fluidRow(
                    box(width = 16,
                        title = "Daten zu analysieren.",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               dataTableOutput("carstable2")
                               
                        )
                    ),
                    box(width = 5,
                        title = "Daten hochladen",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               fileInput("filec", "Choose CSV File",
                                         multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv", ".zip",".xlsx")),
                               br(),
                               checkboxInput("header1", "Header", TRUE)
                        )
                    ),
                    box(width = 5,
                        title = "Trennzeichen auswaehlen",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               radioButtons("sep1", " ",
                                            
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t", Space=""),
                                            selected = ",")
                               
                               # br(),
                               # uiOutput("confusion")
                              
                               
                        )
                    ),
                    box(width = 5,
                        title = "Zielvariablen zu Auswahl",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               uiOutput("Algc"),
                               uiOutput("confusion"),
                               dataTableOutput("Testing")
                        )
                    ),
                    box(width = 8,
                        title = "Modell,Dimensionen, Kennzahl-Spalte zu Auswahl",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               uiOutput("tablesum"),
                               br(),
                               uiOutput("tableaccu"),
                               br(),
                               uiOutput("numerici"),
                               br(),
                               uiOutput("dimension"),
                               radioButtons("traintest", "Modell testen mit:",
                                            
                                            choices = c("Training", "Testing"),
                                            selected = "Training")
                        )
                    ),
                    box(width = 8,
                        title = "Vorhersage mit AI",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               uiOutput("datat")
                        )
                    ),
                   
                   box(width = 16,
                       title = "KPIS nach AI Analyse sowie die Genauigkeit",
                       color = "red", ribbon = TRUE, title_side = "top right",
                       column(width = 8,
                              uiOutput("randomh"),
                              textOutput("accuracyclass")
                       )
                   ),
                   box(width = 8,
                       title = "Abhaengigt der Ziel Variablen von:",
                       color = "red", ribbon = TRUE, title_side = "top right",
                       column(width = 8,
                              uiOutput("factorn")
                       )
                   ),
                   box(width = 8,
                       title = "Abhaengigt der Ziel Variablen von:",
                       color = "red", ribbon = TRUE, title_side = "top right",
                       column(width = 8,
                              uiOutput("factor")
                       )
                   ),
                   
                   
                   box(width = 8,
                       title = "Datensets zu herunterladen",
                       color = "red", ribbon = TRUE, title_side = "top right",
                       column(width = 8,
                              selectInput("dataset", "Choose a dataset:",
                                          choices = c(dataset))
                       )
                   ),
                   box(width = 8,
                       title = "Herunterladen",
                       color = "red", ribbon = TRUE, title_side = "top right",
                       column(width = 8,
                              downloadBttn(
                                  outputId = "downloadData",
                                  style = "bordered",
                                  color = "primary"
                              )
                       ))
                   # ),
                   # # helpText("After analysis you can choose which dataset to download. No data will be saved during analysis.!" ),
                   #  selectInput("dataset", "Choose a dataset:",
                   #              choices = c(dataset)),
                   #  downloadBttn(
                   #      outputId = "downloadData",
                   #      style = "bordered",
                   #      color = "primary"
                   #  )
                    
                )
                
                
                
                
            )#,
            # tabItem(
            #     tabName = "Chart",
            #     fluidRow(
            #         dataTableOutput("carstable3")
            #     )
            #     
            #     
            # ),
            # tabItem(
            #     tabName = "ML",
            #     fluidRow(
            #         dataTableOutput("carstable4")
            #     )
            #     
            #     
            # )
            
        )
     ), theme = "cerulean"  #, theme = "cerulean"
)