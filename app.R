#london


library(shiny)
library(shinydashboard)
#install.packages("entropy")
library("entropy")
#install.packages("FSelector")
#library(FSelector)
#install.packages("shinythemes")
library("shinythemes")
library(ggfortify)
library(sqldf)
library(plotly)
library(forecast)


smarthh <- read.csv("RHHFulFinal.csv")

#Converting string DateTime to proper format
smarthh$DateTime <- as.POSIXct(smarthh$time, '%Y-%m-%d %H:%M:%S', tz='GMT')

#Remove Unwanted columns for xreg parameter, although date should not be removed
smarthhsubset <- subset(smarthh, select = -c( X , time, energy))

#Convert dataframe to matrix, as Xreg can only accept matrix or vector  format
xregsmart = data.matrix(smarthhsubset)

#dataSet for decomposed graph
tsdata <- ts(smarthh$energy, frequency=48)
#tsdata1 <- ts(smarthh$energy, frequency=48, start=c(2012,11), end=c(2018,10)) 

smarthhtest <- read.csv("RHHFulFinaltest.csv")
smarthhtestfilter <- smarthhtest[0:94,] #[0:47,]
smarthhtestfilterfinal <- subset(smarthhtest, select = -c( X , time, energy))
xregsmarttest = data.matrix(smarthhtestfilterfinal)
xregsmarttest <- xregsmarttest[0:94,]
loadsmartXmodel <- readRDS("smartX_model.rda") 

#Use the model to forecast reads for next day (24 reads per day (half hourly))
forecastsmarthh <- forecast(loadsmartXmodel,h=94, xreg = xregsmarttest)  #smartX_model

#storing the result as dataframe
df_fc <- as.data.frame(forecastsmarthh)

#adding the predicted demand values to new column of test data for accuracy
smarthhtestfilter$Pred_Demand <- df_fc$'Point Forecast'

smarthhtestfilter$DateTime <- as.POSIXct(smarthhtestfilter$time, '%Y-%m-%d %H:%M:%S', tz='GMT')

acc <- accuracy(forecastsmarthh,smarthhtestfilter$energy)
acc <- cbind(acc, c("Train Set","Test Set"))
colnames(acc) <-c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Data Set")




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
                    
                    # Third tab content
                    # tabItem(tabName = "Tanalytics",
                    #        
                    #         # verticalLayout(
                    #         #   tabBox(title = "Descriptive Analytics", width = "500px", height = "600px",
                    #         #          
                    #         #   tabPanel(
                    #         #     title = "Information Gain", status = "primary", solidHeader = T, 
                    #         #        plotOutput("Tinfoplot")
                    #         #     ),
                    #           tabPanel(
                    #             title = "Decomposition", 
                    #             plotOutput("Tdecomposition")
                    #             )
                    #         
                    #         
                    #         # )
                    #         # 
                    #         # )
                    #         
                    # ),
                    
                    
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



server <- function(input, output, session) { 
  
  
  
  smarthh <- read.csv("RHHFulFinal.csv")
  
  #Converting string DateTime to proper format
  smarthh$DateTime <- as.POSIXct(smarthh$time, '%Y-%m-%d %H:%M:%S', tz='GMT')
  
  #Remove Unwanted columns for xreg parameter, although date should not be removed
  smarthhsubset <- subset(smarthh, select = -c( X , time, energy))
  
  #Convert dataframe to matrix, as Xreg can only accept matrix or vector  format
  xregsmart = data.matrix(smarthhsubset)
  
  #dataSet for decomposed graph
  tsdata <- ts(smarthh$energy, frequency=48)
  #tsdata1 <- ts(smarthh$energy, frequency=48, start=c(2012,11), end=c(2018,10)) 
  
  smarthhtest <- read.csv("RHHFulFinaltest.csv")
  smarthhtestfilter <- smarthhtest[0:94,] #[0:47,]
  smarthhtestfilterfinal <- subset(smarthhtest, select = -c( X , time, energy))
  xregsmarttest = data.matrix(smarthhtestfilterfinal)
  xregsmarttest <- xregsmarttest[0:94,]
  loadsmartXmodel <- readRDS("smartX_model.rda") 
  
  #Use the model to forecast reads for next day (24 reads per day (half hourly))
  forecastsmarthh <- forecast(loadsmartXmodel,h=94, xreg = xregsmarttest)  #smartX_model
  
  #storing the result as dataframe
  df_fc <- as.data.frame(forecastsmarthh)
  
  #adding the predicted demand values to new column of test data for accuracy
  smarthhtestfilter$Pred_Demand <- df_fc$'Point Forecast'
  
  smarthhtestfilter$DateTime <- as.POSIXct(smarthhtestfilter$time, '%Y-%m-%d %H:%M:%S', tz='GMT')
  
  acc <- accuracy(forecastsmarthh,smarthhtestfilter$energy)
  acc <- cbind(acc, c("Train Set","Test Set"))
  colnames(acc) <-c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Data Set")
  
  codata <- read.csv("XregTest.csv")
  
  
  #Plotting the Demand with the period of time
  output$TDataPlot <- renderPlot({
    ggplot(data = smarthh, aes(x = X, y = energy))+ #if we need it aginst time we need to use 'x = DateTime'
      geom_line(color = "#00AFBB", size = 0.5) +
      ggtitle('Electricity Consumption - London, Jan 2015 - Oct 2018')
  })
  
  
  customerdatacolumn <- function(){
    customerdf = read.csv("RHHFulFinal.csv") #read.csv(customerdata()$datapath)
    customer = as.data.frame(customerdf)
    customerdatalist <- list(colnames(customerdf))
    print(colnames(customerdf))
    #return(customerdf)
  }
  
  entropyCalc <- function(){
    
    smart <- read.csv("RHHFulFinal.csv") #read.csv(customerdata()$datapath)
    print(c("Class of smartdf : ",class(smart)))
    inputTargetValue <- input$picktarget 
    print(c("TargetValue : ",inputTargetValue))
    smarttarget <- table(smart[inputTargetValue])
    print(c("Smart Target",summary(smarttarget)))
    entropydata <- entropy(smarttarget, unit = "log2") 
    print(c("Entropy of Dara : ",entropydata))
    return(entropydata)
    
  }
  
  #data table in Data tab
  output$Tcontents <- DT::renderDataTable({
    read.csv("RHHFulFinal.csv") 
  })
  
  
  output$Tdecomposition <- renderPlot({
    plot(decompose(tsdata))
  })
  
  
  output$TSeasonal <- renderPlot({
    ggseasonplot(x=tsdata, year.labels=FALSE, continuous=TRUE) + ggtitle('Hourly average')
  })
  
  #plotting based on radio button
  #has to be edited
  
  # dat <- smarthh
  # dat$Weekday <- weekdays(as.Date(dat$Date)) #weekday
  # dat$Date <- as.POSIXct(dat$Date, format="%d-%m-%Y")  #, tz = "Pacific/Auckland")
  # Date_Split = strsplit(as.character(dat$Date), "-")
  # Year = sapply(Date_Split, "[", 1)
  # Month = sapply(Date_Split, "[", 2)
  # Day = sapply(Date_Split, "[", 3)
  # dat_new = cbind(dat[,c(1,2,3,4,5,6)], Day, Month, Year)
  # weekday.df <- aggregate(dat_new$Demand,by=list(dat_new$Weekday),FUN=mean,na.rm=TRUE)
  # month.df <- aggregate(dat_new$Demand,by=list(dat_new$Month),FUN=mean,na.rm=TRUE)
  # year.df <- aggregate(dat_new$Demand,by=list(dat_new$Year),FUN=mean,na.rm=TRUE)
  # date.df <- aggregate(dat_new$Demand,by=list(dat_new$Day),FUN=mean,na.rm=TRUE)
  # dayT.tf <- aggregate(dat_new$Demand,by=list(dat_new$DayType),FUN=mean,na.rm=TRUE)
  # Temp.tf <- aggregate(dat_new$Demand,by=list(dat_new$Temp),FUN=mean,na.rm=TRUE)
  # colnames(weekday.df) <- c("WeekDay","Demand")
  # colnames(month.df) <- c("Months","Demand")
  # colnames(year.df) <- c("Year","Demand")
  # colnames(date.df) <- c("Date","Demand")
  # colnames(dayT.tf) <- c("DayType","Demand")
  # colnames(Temp.tf) <- c("Temp","Demand")
  # 
  # output$TTrend <- renderPlot({
  #   switch(input$trend,
  #           "Dtype" = barplot(dayT.tf$Demand,
  #                             main = "Aggregate Demand per DayType in London",
  #                             names = dayT.tf$DayType,
  #                             names.arg = c("DB1", "DB2", "DB3", "DB4", "HalfDay", "NewTear", "S"),
  #                             col = "darkred") ,
  #           "WeekDay" = barplot(weekday.df$Demand,
  #                               main = "Aggregate Demand per WeekDay",
  #                               xlab = "WeekDay",
  #                               ylab = "Demand",
  #                               names = weekday.df$WeekDay,
  #                               names.arg = c("Fri","Mon","Sat","Sun","Thu","Tue","Wed"),
  #                               col = "darkred"),
  #           "month" = barplot(month.df$Demand,
  #                             main = "Aggregate Demand per Month",
  #                             xlab = "Month",
  #                             ylab = "Demand",
  #                             names = month.df$Months,
  #                             names.arg = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
  #                             col = "darkred"),
  #           "year" = barplot(year.df$Demand,
  #                            main = "Aggregate Demand per Year",
  #                            xlab = "Year",
  #                            ylab = "Demand",
  #                            #names = year.df$Year,
  #                            #names.arg = c("2015","2016","2017","2018","2019"),
  #                            col = "darkred"),
  #           "temp" = ggplot(Temp.tf, aes(x=Temp, Demand)) +
  #            geom_line(color="#69b3a2", size=1, alpha=0.9) + ggtitle('London Power - Demand Vs Temperature')+
  #            scale_x_continuous(name="Temperature", limits=c(-20, 40))
  #   )
  # })
  
  
  
  
  
  
  #data tab end
  
  observeEvent(input$uploaddata, {
    #session$sendCustomMessage(type = 'testmessage',
    #                         message = 'Thank you for clicking')
    #customerdatacolumn()
    customerdf = read.csv("RHHFulFinal.csv") #read.csv(customerdata()$datapath)
    #updateSelectInput( session, "picktarget",choices = c('Dynamic','Dia'))
    updateSelectInput( session, "picktarget",choices = colnames(customerdf))
    updateSelectInput( session, "dateindex",choices = colnames(customerdf))
    updateTabItems(session, "tabs","analytics")
  })
  
  
  # eventReactive(input$historicdatafile, {
  #   customerdf = read.csv(customerdata()$datapath)
  #   updateSelectInput(session, "picktarget", data = customerdf)
  # })
  output$targetvariabletb <- renderPrint({ input$picktarget })
  
  output$dateIndextb <- renderPrint({ input$dateindex })
  
  observeEvent(input$analysisbutton, {
    print("I am here")
    #entropyCalc()
    output$entropydisplay <- renderText({ 
      #entropyCalc()
      c("Calculated Entropy is : ",entropyCalc())
      # "Entropy : "
    })
    
    
    #IG in data analytics tab
    #This needs to be looked into
    
    output$Tinfoplot <- renderPlot({
      smartinfo <- read.csv("RHHFulFinal.csv")    #read.csv(customerdata()$datapath)
      inputTargetValueinfo <- smartinfo$Demand   #input$picktarget 
      #smartentropy <- entropyCalc()
      #smartinfo <- subset(smartinfo, select = -c("inputTargetValueinfo"))
      
      smartinfodisp <- smartinfo[ , -which(names(smartinfo) %in% c(inputTargetValueinfo))]
      print(as.formula(paste(input$picktarget," ~ ", "." )))
      
      infogain <- information.gain(as.formula(paste(input$picktarget," ~ ", "." )), smartinfo, unit = "log2")
      print(colnames(smartinfodisp))
      barplot(unlist(infogain),names.arg=colnames(smartinfodisp),
              las=2, col = "lightblue", main = "FEATURE INFORMATION GAIN")
    })
    #names.arg=colnames(smartinfo)
  })
  
  #stats tab
  
  output$Tforecaststats <- renderTable(summary(loadsmartXmodel)) 
  
  
  #stats tab end
  
  
  #prediction tab
  
  
  #plotting the predicted demand
  
  output$Tmodelplot <-renderPlot({
    plot(forecastsmarthh)
  }) 
  
  output$Tmodel24Plot <- renderPlot({
    
    ggplot() + 
      geom_line(data= smarthhtestfilter,aes(x=DateTime, y = energy, color = "darkred")) + 
      geom_line(data= smarthhtestfilter,aes(x=DateTime, y = Pred_Demand, color="steelblue"), linetype="twodash") + 
      scale_color_discrete(name = "Demand Values", labels = c("Original Value", "Predicted Demand")) +
      ggtitle('London Power - Demand Vs Prediction')
    
    
  })
  
  output$model24Interactive <- renderPlotly({
    plot_ly(x = smarthhtestfilter$DateTime, y = smarthhtestfilter$Pred_Demand, mode = 'lines')
  })
  
  
  
  output$HomePageCO2 <- renderPlotly({
    plot_ly(x = codata$DateTime, y = codata$CO2) 
  })
  
  
  output$HomePageMeters <- renderPlotly({
    plot_ly(x = codata$DateTime, y = codata$Smart.meter.instalation) %>% 
      add_markers(alpha = 0.8, name = "alpha")
  })
  # rmse <- function(actual,pred){
  #   rmse<- sqrt(mean((actual - pred) ** 2)) 
  #   return(rmse)
  # }
  # 
  # mape <- function(actual,pred){
  #   mape <- mean(abs((actual - pred)/actual))*100
  #   return (mape)
  # }
  
  #trunc <- function(x, prec = 0) base::trunc(x * 10^prec) / 10^prec;
  
  TR <- acc[2,3] #RMSE    #rmse(forecastsmarthh$mean, smarthhtestfilter$energy)
  
  
  TM <- acc[2,5] #MAPE    #mape(forecastsmarthh$mean, smarthhtestfilter$energy)
  
  
  output$Trmse <- renderValueBox({
    valueBox(
      TR, "RMSE - Root Mean Squared Error",   #round(TR, digits = 4) , "RMSE - Root Mean Squared Error",   
      color = "green"
    )
  })
  
  output$Tmape <- renderValueBox({
    valueBox(
      TM , "MAPE - Mean Absolute Percentage Error", #round(acc[2,5],digits = 4),  #trunc(TM, prec= 4), "MAPE - Mean Absolute Percentage Error", 
      color = "green"
    )
  })
  
  # output$Tacf <- renderPlot({
  #   ggAcf(forecastsmarthh$residuals) 
  # })
  
  output$Tresidue <- renderPlot({
    checkresiduals(loadsmartXmodel,plot = TRUE)
  })
  
  
  #prediction tab end
  
  
  
  
  
} #server end

shinyApp(ui, server)
