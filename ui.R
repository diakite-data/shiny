library(shiny)
library(shinydashboard)
library(highcharter)
library(shinycssloaders)
library(shinyTime)


df2G <- read_csv("./data_app/df2G.csv") %>% mutate(event = `EVENT TIME`) %>% mutate(event2 = ymd_hms(event))


shinyUI(
  dashboardPage(skin = "black",
                dashboardHeader(title = "Analyse Incident"),
                dashboardSidebar(sidebarUserPanel("Orange",
                                                  subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                                                  image = "im.png"),
                            
                                 selectInput("techno","Sélectionner une technologie",c("2G","3G","TDD","FDD"), multiple = FALSE),
                                 selectInput("vision","Sélectionner le champ de vision",c("Durée d'indisponibiité","Nur"), multiple = FALSE),
                                 sidebarMenu(
                                   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeColor = "green"),
                                   hr(),
                                   menuItem(
                                     "Quotidien",
                                     tabName = "quotidien",
                                     icon = icon("calendar"),
                                     # Default start and end is the current date in the client's time zone
                                     dateRangeInput("daterange2", "Date range:",
                                                    start  = min(df2G$event2),
                                                    end    = max(df2G$event2),
                                                    min    = min(df2G$event2),
                                                    max    = max(df2G$event2)
                                                    
                                                    ),
                                     # Set to current time
                                     timeInput("time2", "Time:", value = Sys.time())
                                   ),
                                   hr(),
            
                                   menuItem("Mensuel", tabName = "mensuel", icon = icon("calendar", lib = "glyphicon"),
                                            sliderInput(
                                              inputId = "yearInput",
                                              label = "Year",
                                              min = 2010,
                                              max = 2020,
                                              value = 2010:2020
                                            ),
                                            selectInput("mois","Sélectionner le(s) mois",
                                                        c("Janvier","Fevrier","Mars","Avril", "Mai", 'Juin', 'Juillet', 'Aout', 'Septembre', 'Octobre', 'Novembre', 'Decembre'),
                                                        multiple = TRUE
                                                        )
                                            ),
                                   hr(),
                                   menuItem("Hebdomadaire", tabName = "hebdo", icon = icon("calendar", lib = "glyphicon"),
                                            sliderInput(
                                              inputId = "weekInput",
                                              label = "Week",
                                              min = 1,
                                              max = 52,
                                              value = 1:52
                                                  )
                                            ),
                                   hr(),
                                   hr()
                                   
                                 )
                ),
                dashboardBody(
                  tabBox(
                    title = "First tabBox",
                    width = 12,
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", 
                    tabPanel("Cause", 
                             # A static infoBox
                             #infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                             
                             infoBoxOutput("radio",width = 4)  %>% withSpinner(color="#0dc5c1") ,
                             infoBoxOutput("FIBER", width = 4),
                             infoBoxOutput("POWER", width = 4),
                             infoBoxOutput("ENV", width = 4),
                             infoBoxOutput("TRANS", width = 4),
                             br(),
                             h2(tableOutput("view"))
                             
                             
                             #infoBoxOutput("nurTDD", width = 3)
                             # Dynamic infoBoxes
                             #infoBoxOutput("progressBox"),
                             #infoBoxOutput("approvalBox")
                    ),
                    tabPanel("Zone",   leafletOutput("mymap")  %>% withSpinner(color="#0dc5c1")  ,
                             plotlyOutput("barplotzone") ),
                    tabPanel("Site", "Tab content 3"),
                    tabPanel("Responsibility", "Tab content 4"),
                    tabPanel("Visualisation des données", "Tab content 4")
                  ) 
                )
                
  )
)