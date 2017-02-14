

source("helpers.R")

library(shiny)

library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel("Extract Data from World Development Indicators"),
  
  sidebarLayout(
  
  sidebarPanel(
      
      
      selectInput(inputId = "dataType",label = "Step 1: Select Data Type", choices = c('Time Series','Cross Section','Panel')),
      
      
      conditionalPanel(
        
      condition = "input.dataType == 'Time Series'" , 
      
      selectInput(inputId = "ts_countrycode",
                  label = "Step 2: Select Country",
                  choices = countryNames$`Country Code`),
      
      textOutput("ts_countryname"),
      
      textOutput("ts_numindicators"),
      
      hr(),
      
      #uiOutput("ts_selectindicators"),
      
      selectInput(inputId = "ts_indicators",label = "Step 3: Select Indicators",
                  choices = NULL,selected = NULL, multiple = TRUE),
      
      textOutput("ts_indicatornames"),
      
      hr(),
      
      #uiOutput("ts_selectperiod")
      
      sliderInput("ts_period","Step 4: Select Period",min = 1960,max = 2015,
                  value = c(1960,2015),step = 1,sep = NULL)
      
      ),
      
      conditionalPanel(
        
        condition = "input.dataType == 'Cross Section'" , 
        
        selectInput(inputId = "cs_countrycodes",
                    label = "Step 2: Select Countries",
                    choices = countryNames$`Country Code` ,selected = NULL, multiple = TRUE),
        
        textOutput("cs_countrynames"),
        
        hr(),
        
        # uiOutput("cs_selectyear"),
        
        selectInput(inputId = "cs_year",label = "Step 3: Select Year",
                    choices = NULL, selected = NULL),
        
        textOutput("cs_numindicators"),
        
        hr(),
        
        # uiOutput("cs_selectindicators"),
        
        selectInput(inputId = "cs_indicators",label = "Step 4: Select Indicators",
                    choices = NULL, multiple = TRUE),
        
        
        textOutput("cs_indicatornames")
    
     ), 
      
      hr(),
      downloadButton("download","Download Data")
      
   ),
  
  mainPanel(
    
    # Time Series
    
    conditionalPanel(
    
    condition = "input.dataType == 'Time Series'" , 
    
    tableOutput("tsresults")
      
    ),
    
    # Cross Section
    
    conditionalPanel(
      
      condition = "input.dataType == 'Cross Section'" , 
      
      tableOutput("csresults")
      
    )
    
  )
  
  )
    
)
  

server = function(input,output,session){
  
 
 ind_ts <- reactive({
   
   req(input$ts_countrycode)
   
   which(countryNames$`Country Code` == input$ts_countrycode)
   
 })
  
 output$ts_countryname <- renderText({
  
  paste("Selected Country:",countryNames$`Country Name`[ind_ts()]) 
   
 })
 

 output$ts_numindicators <- renderText({
   
   paste("Number of Available Indicators:", countryNames$numIndicators[ind_ts()])
 })
 
 
 
 ts_red <- reactive({
  
  req(input$ts_countrycode)  
   
  filter(wdi, `Country Code` %in% input$ts_countrycode)
     
 })
 
 ts_gr <- reactive({
   
   wdiByIndicator(ts_red())
   
 })
 
 #output$ts_selectindicators <- renderUI({
  
  #selectInput(inputId = "ts_indicators",label = "Step 3: Select Indicators",
              #choices = unique(ts_gr()$`Indicator Code`), multiple = TRUE)
  
 #})
 
 observe({
   
   prev_ts <- input$ts_indicators
   
   chs_ts <- unique(ts_gr()$`Indicator Code`) 
   
   sld_ts <- intersect(prev_ts,chs_ts)
   
   input$ts_countrycode
   
   updateSelectInput(session, "ts_indicators",choices = chs_ts,
                     selected = sld_ts) 
   
   
 })
   
 output$ts_indicatornames <- renderText({
   
   chosen <- indicatorNames$`Indicator Code`%in% input$ts_indicators
   names <- indicatorNames$`Indicator Name`[chosen]
   names <- paste(names,collapse = "; ")
   
   paste("Selected Indicators:",names )
   
 })
 
 
 ts_gr_red <- reactive({
   
   req(input$ts_indicators)
   
   filter(ts_gr(), `Indicator Code` %in% input$ts_indicators)
   
 })
 
 #output$ts_selectperiod <- renderUI({
   
   #sliderInput("ts_period","Step 4: Select Period",min(ts_gr()$Year),max(ts_gr()$Year),
               #c(min(ts_gr_red()$Year),max(ts_gr_red()$Year)),1,sep = NULL)
   
 #})
 
 observe({
   
   
   
   min_ts <- max(min(ts_gr_red()$Year),input$ts_period[1])
   
   max_ts <- min(max(ts_gr_red()$Year),input$ts_period[2])
   
   
   input$ts_indicators
   
   updateSliderInput(session, "ts_period",value = c(min_ts,max_ts))
                     
   
   
 })
 
 ts_filtered <- reactive({
   
   req(input$ts_indicators,input$ts_period)
   
   ts <- ts_red() %>%
     filter(
        `Indicator Code` %in% input$ts_indicators,
         Year >= input$ts_period[1],
         Year <= input$ts_period[2]
     )%>%
     spread(key = `Indicator Code`,value = Value)%>%
     select(-`Country Code`)
   
   colnames(ts)[2:ncol(ts)] <- paste(colnames(ts)[2:ncol(ts)],input$ts_countrycode,sep = "_")
   ts  
     
   
  }) 
 
 
 
 
 output$tsresults <- renderTable({
   
   ts_filtered()
   
 })
   
# Cross Section
 
 ind_cs <- reactive({
   
   req(input$cs_countrycodes)
   
   countryNames$`Country Code` %in% input$cs_countrycodes
   
 })
 
 output$cs_countrynames <- renderText({
   
   cnames <- paste(countryNames$`Country Name`[ind_cs()],collapse = "; ")
   
   paste("Selected Country:",cnames ) 
   
 })
 
 cs_red <- reactive({
   
   req(input$cs_countrycodes)  
   
   filter(wdi, `Country Code` %in% input$cs_countrycodes)
   
 })
 
 cs_gr <- reactive({
   
   req(input$cs_countrycodes)
   
   l <- length(input$cs_countrycodes)
   gr <- wdiByIndicator(cs_red())%>%
     filter(numCountries == l)
   
   gr
 
 })
 
 #output$cs_selectyear <- renderUI({
   
   #selectInput(inputId = "cs_year",label = "Step 3: Select Year",
              # choices = sort(unique(cs_gr()$Year),decreasing = TRUE))
   
# })
 
 observe({
   
   prev_cs <- input$cs_year
   
   chs_cs <- sort(unique(cs_gr()$Year),decreasing = TRUE)
   
   sld_cs <- ifelse(prev_cs %in% chs_cs,prev_cs,chs_cs[1])
   
   input$cs_countrycodes
   
   updateSelectInput(session, "cs_year",choices = chs_cs,
                     selected = sld_cs) 
   
   
 })


 
 cs_gr_red <- reactive({
   
   req(input$cs_year)
   
   filter(cs_gr(), Year == input$cs_year)
   
 })
 
 output$cs_numindicators <- renderText({
   
  paste("Number of Available Indicators: ", nrow(cs_gr_red())) 
   
 })
 
 #output$cs_selectindicators <- renderUI({
   
   #selectInput(inputId = "cs_indicators",label = "Step 4: Select Indicators",
               #choices = cs_gr_red()$`Indicator Code`,multiple = TRUE)
   
 #})
 
 observe({
   
   prev_cs2 <- input$cs_indicators
   
   chs_cs2 <- cs_gr_red()$`Indicator Code`
   
   sld_cs2 <- intersect(prev_cs2,chs_cs2)
   
   input$cs_year
   
   updateSelectInput(session, "cs_indicators",choices = chs_cs2,
                     selected = sld_cs2) 
   
   
 })
 
 output$cs_indicatornames <- renderText({
   
   chosen <- indicatorNames$`Indicator Code`%in% input$cs_indicators
   names <- indicatorNames$`Indicator Name`[chosen]
   names <- paste(names,collapse = "; ")
   
   paste("Selected Indicators:",names)
   
 })
 
 cs_filtered <- reactive({
   
   req(input$cs_indicators,input$cs_year)
   
   cs <- cs_red() %>%
     filter(
       Year == input$cs_year,
       `Indicator Code` %in% input$cs_indicators
     )%>%
     spread(key = `Indicator Code`,value = Value)%>%
     select(-Year)
   
   colnames(cs)[2:ncol(cs)] <- paste(colnames(cs)[2:ncol(cs)],input$cs_year,sep = "_")
   cs  
     
  
 })
 
 
 output$csresults <- renderTable({
   
   cs_filtered()
   
 })
 

 
 
output$download <- downloadHandler(
  
  filename = function() {
    
    sprintf("wdi_%s.csv",humanTime())
    
  },
  
  content = function(file){
    
    filtered_data <- function(datatype) {switch(datatype,"Time Series" = ts_filtered(),"Cross Section" = cs_filtered())}
    
    write_csv(filtered_data(input$dataType),file)
    
  }
  
)



   
}

shinyApp(ui = ui,server = server)