library(shiny)
shinyUI( dashboardPage(
  skin = "black",
  
  dashboardHeader( title = "The Air Info",
                  titleWidth = 300),
  
  
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("university")),
      menuItem("Airports", tabName = "Airports", icon = icon("map-marker") ),
      menuItem("Airlines", tabName ="Airlines",icon =  icon("plane" ))
    )
    ), #dashboardSidebar
  
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Welcome",
              fluidRow(
             column(12,  tags$img(height = 300, width = 1100, src = "the air info.png"))
              ),
             headerPanel("Welcome!")
             ),
      ## Second tab content
      tabItem(tabName = "Airports",
              fluidRow(
                       tabsetPanel(
                         tabPanel("All Airports", 
                                  fluidPage(
                                    fluidRow(#####
                                    column(12,  ""), titlePanel("    "),titlePanel("    ")),#FluidRow space
                                    
                                    fluidRow(offset = 5,
                                             column(8, leafletOutput("airport"), offset = 2 )
                                             ),#fluidFlow Map 
                                   fluidRow(
                                  
                                     column(4, selectizeInput('Country', label = 'Country', choices = sort(airport$country), selected = airport$country)),
                                      column(4,uiOutput("City")),
                                     column(4, uiOutput("Name"))
                                   ), #fluidRow selecting the dataset 
                                   fluidRow(column(10,offset = 1, dataTableOutput("airTable")))
                                  )#FluidPage
                                  
                                  ), #tabPanel

                         tabPanel("Airport Network",
                                fluidPage(
                                  fluidRow(
                                    column(12,  ""), titlePanel("    "),titlePanel("    ")
                                  ),
                                  fluidRow(
                                    column(8, plotOutput("airportnet", height = 500, width = 1000), offset = 1 )
                                  ), # Airport dots plot
                                  fluidRow(
                                    column(4, selectizeInput('Aiport', label = 'Airport', choices = sort(routesair$sourceAirport) ))
                                    ), # select the airport for network displaying
                                  h3("The network map for the airport"),
                                  
                                  fluidRow(
                                    column(8, plotOutput("airportnetwork"), offset = 2)
                                  )# plot the network of the chosen airport
                                )
                              ), #airport network panel
                         tabPanel("Statistics", 
                                 fluidPage(  
                                   fluidRow(
                                    column(12,  ""), titlePanel("    "),titlePanel("  Dynamic Map  ")),
                                  fluidRow(
                                    column(8, plotOutput("heatmap", height = 500, width = 1000), offset = 1))
                                  )
                                  
                                  ) #StatisticsPanel
                      
                        ) #tabsetPanel
                       
                     ) # flowRow
                       
                       
             ), #tabItem
           
      
      # Third tab content
      tabItem(tabName = "Airlines",
              tabsetPanel( tabPanel("Predictive Model"
                                    
                                    ), #pred model panel
                          
                            tabPanel(" Airline Network",
                                    fluidPage(
                                      fluidRow(
                                        column(4, selectInput('Airline', label = 'Airline', choices = sort(flights$airline), selected = "American Airlines" ))
                                    ),
                                    h3("The network map for the airline"),
                                    
                                    fluidRow(
                                      column(8, plotOutput("airnetwork"), offset = 2)
                                    )
                                    )),#airline panel network
                           
                           
                           tabPanel("Statistics",
                                    fluidPage(
                                      fluidRow(
                                        column(4, selectizeInput('Year', label = 'Year', choices = sort(airlines$Year), selected = 2001 ))
                                        ),
                                      h3("Annual Flight and Passenger Records"),
                                      fluidRow(
                                        column(6, plotOutput("totalflights")),
                                        column(6, plotOutput("totalpass"))
                                      ),
                                      fluidRow(
                                        column(6, plotOutput("totalflights2")),
                                        column(6, plotOutput("totalpass2"))
                                      ),
                                      fluidRow(
                                        column(6, plotOutput("totalflights3")),
                                        column(6, plotOutput("totalpass3"))
                                      )
                                      
                                      )
                                    
                                    ) #stat panel
                           
                           
                           )#tabset panel
              
              
      )
    ) #tabItems
    )# DashBoard body
  )#Dashboard Page
) #SHiny UI
