
ui <- 
  dashboardPage(skin = "green", 
                dashboardHeader(title = "Pheonix Energy"),
                dashboardSidebar(
                  sidebarMenu( id = "tabs",
                               menuItem(tabName = "london","London DataSet", icon = icon("home"),
                                        menuSubItem("Home", tabName = "THome", icon = icon("home"),selected = TRUE),
                                        menuSubItem("Data", tabName = "Tdata", icon = icon("download")) 
                                        ),
                               menuItem("Predictive Modeling", tabName = "Tmodeling", icon = icon("chart-line"))
                  )
                ),
                dashboardBody(
                  tabItems(
                    #home tab content
                    tabItem(tabName = "THome",
                            fluidRow(
                              infoBox("Profit - 2020", 10800 , icon = icon("credit-card"), color = "blue"),
                              infoBox("CO2 Target Met",value= "80% ", color = "green", icon = icon("leaf", lib = "glyphicon")), #CO2 emissions
                              infoBox("Other Progress", value = "25 %", icon = icon("thumbs-up", lib = "glyphicon"), color = "purple"), 
                              infoBox("Smart Meters Installed- 2020", 5400 , icon = icon("star", lib = "glyphicon"), color = "blue"),
                              infoBox("Power Failure Count", value = "25 %", icon = icon("list"), color = "green"), #Power Failure Count
                              infoBox("Other Progress", value = "25 %", icon = icon("thumbs-up"), color = "purple")
                            ),
                            fluidRow(
                              box(title= "Power Failure Count",
                                  plotlyOutput("HomePageCO2")),
                              box(title= "Smart meter installation",
                                  plotlyOutput("HomePageMeters"))
                            )
                            
                    ),
                    
                    
                    # Second tab content
                    tabItem(tabName = "Tdata",
                            
                            
                            verticalLayout(
                              tabBox(title = "Data", width = "500px", height = "600px",
                              
                              tabPanel(title = "DataSet",
                              
                                       box(
                                title = "Case Analyses Details", status = "primary", height ="595",
                                width = "15", solidHeader = T, collapsible = TRUE,
                                   column(width = 12,
                                          #DT::dataTableOutput
                                          DT::dataTableOutput("Tcontents"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                          )
                                    ) #box end
                              ),
                              
                              tabPanel(title = "Visualization",
                                # plot the data
                                 plotOutput("TDataPlot", click = "plot_click"),
                                 plotOutput("TSeasonal")
                              ),
                              # tabPanel( title = "Aggregation",
                              #   box( width = "500px", 
                              #        sidebarPanel(
                              #          radioButtons("trend", "Aggregate on:",
                              #                  c("Day Type" = "Dtype",
                              #                    "Day of the Week" = "WeekDay",
                              #                    "Month" = "month",
                              #                    "Year"= "year",
                              #                    "Temprature" = "temp"))
                              #           ),
                              #     mainPanel(
                              #    plotOutput("TTrend")
                              #         )
                              #   )
                              # ),
                              tabPanel(
                                title = "Decomposition", 
                                plotOutput("Tdecomposition")
                              )
                              ) 
                          ) # vertical layout end 
                            
                    ),
                    
                
                    
                  
                    tabItem(tabName = "Tmodeling",
                            
                            fluidRow(
                              tabBox(title = "SARIMAX", width = "500px", height = "1000px",
                                     
                                     tabPanel(
                                       title = "Forecast",
                                       # fluidRow(
                                       #   valueBoxOutput("Tmape", width = 6),
                                       #   valueBoxOutput("Trmse", width = 6)
                                       #  ),
                                       h1("") ,
                                       
                                       
                                       
                                       box( title = "Overview",
                                            solidHeader = TRUE,
                                            status = "primary",
                                            collapsible = TRUE,
                                            width = "500px",
                                       plotOutput("Tmodelplot")
                                       ),
                                       box(title = "Comparitive Values: Actual Vs Prediction",
                                           solidHeader = TRUE,
                                           status = "primary",
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           width = "500px",
                                           plotOutput("Tmodel24Plot")
                                       ),
                                       h1(""),
                                       
                                       box(title = "24 hr Prediction (Interactive)",
                                           solidHeader = TRUE,
                                           width = "500px",
                                           status = "primary",
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           plotlyOutput("model24Interactive")
                                       )
                                       
                                      
                                       
                                      
                                       
                                       ),
                                     
                                     tabPanel(
                                       title = "Statistics and analysis",
                                       verticalLayout(
                                         
                                         
                                       box(
                                         title = "Error Values", status = "warning", 
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         #collapsed = TRUE,
                                         width = "500px",
                                          tableOutput("Tforecaststats")
                                          ),
                                        h1("") , 
                                      box(title = "Residue Analysis", width = "500px",
                                          status = "primary",
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      # h5( "ACF"),
                                      #  plotOutput("Tacf"),
                                       h5("Residue Values"),
                                       plotOutput("Tresidue") 
                                     )
                                       )
                                    
                                     ) #tabPanel
                              
                                    )
                              
                            
                            )
                    ) #Tmodeling tab
                  )
                )
  )

