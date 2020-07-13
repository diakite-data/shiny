library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(highcharter)
library(readr)
library('tidyverse')
library(lubridate)
library(ggmap)
library(ggplot2)
library(maptools)
library(maps)
library(leaflet)
library(plotly)
options(shiny.maxRequestSize=1000*1024^2)




df2G <- read_csv("./data_app/df2G.csv") %>% mutate(event = `EVENT TIME`) %>% mutate(event_time2 = ymd_hms(event)) %>% 
                                                  mutate(Year=year(event_time2),
                                                  Month=month(event_time2),
                                                  Day=day(event_time2),
                                                  Hour=hour(event_time2),
                                                  Min=minute(event_time2),
                                                  Sec=second(event_time2),
                                                  Week=week(event_time2)
                                                  
  )


ci <- read_csv("./data_app/ci.csv") %>% mutate(Departement = toupper(city))
df <-  df2G %>% group_by(Departement) %>% summarise(Duree = sum(DURATION)) %>% left_join(ci, by = "Departement") 



DURATION = "DURATION"
NUR = "NUR"


shinyServer(function(input,output){
  
  datasetInput2 <- reactive({  
    
    df <-  df2G %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
    group_by(Departement) %>% summarise(Duree = sum(DURATION)) %>% left_join(ci, by = "Departement") 
  
    })
  
  
  datasetInput <- reactive({
    switch(input$techno,
           "2G" = df2G,
           "3G" = ,
           "TDD" = ,
           "FDD" = )
  })

  
  VisonInput <- reactive({
    switch(input$vision,
           "Durée d'indisponibiité" = DURATION,
           "Nur" = NUR
            )
  })
  
  
  dateRangeInput<-reactive({
    
    data =  datasetInput() 
      
      
  })
  
  
    
  output$radio <- renderValueBox({
    
    dataset = datasetInput()
    vision = VisonInput()
    
   tmp = dataset %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
     group_by(CAUSE) %>% filter(CAUSE =='RADIO') %>% summarise(nb = sum(DURATION)) %>% select(nb) %>% pull()
    
    valueBox(
      "Radio",
      tmp,  color = "green"
    )
  })
  
  
  output$FIBER <- renderValueBox({
    
    dataset = datasetInput()
    
    tmp = dataset %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
      group_by(CAUSE) %>% filter(CAUSE =='FIBER') %>% summarise(nb =sum(DURATION)) %>% select(nb) %>% pull()
    
    valueBox(
      "Fiber",
      tmp,  color = "red"
    )
  })
  
  
  output$POWER <- renderValueBox({
    
    dataset = datasetInput()
    
    tmp = dataset %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
      group_by(CAUSE) %>% filter(CAUSE =='POWER') %>% summarise(nb =sum(DURATION)) %>% select(nb) %>% pull()
    
    valueBox(
      "Power",
      tmp,  color = "yellow"
    )
  })
  
  
  
  output$ENV <- renderValueBox({
    
    dataset = datasetInput()
    
    tmp = dataset %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
      group_by(CAUSE) %>% filter(CAUSE =='ENV') %>% summarise(nb =sum(DURATION)) %>% select(nb) %>% pull()
    
    valueBox(
      "Env",
      tmp,  color = "blue"
    )
  })
  
  
  
  
  output$TRANS <- renderValueBox({
    
    dataset = datasetInput()
    
    tmp = dataset %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
      group_by(CAUSE) %>% filter(CAUSE =='TRANS') %>% summarise(nb =sum(DURATION)) %>% select(nb) %>% pull()
    
    valueBox(
      "Trans",
      tmp
    )
  })
  
  br()
  
  output$view <- renderTable({
    
    dataset = datasetInput()
    
    tmp = dataset %>% filter(event_time2 >= input$daterange2[1], event_time2 <= input$daterange2[2] ) %>% 
      group_by(CAUSE) %>% summarise(Duree = sum(DURATION))
    
    head(tmp)
  })
  
  
  
  output$mymap <- renderLeaflet({
    
    df = datasetInput2()
    df  %>% 
      leaflet() %>% addTiles() %>%
      addCircleMarkers(~lng, ~lat, popup = ~paste(Departement,Duree,sep = "--") , radius = ~(Duree*100)/max(Duree))
    
  })
  
  
  output$barplotzone <-renderPlotly({
    df = datasetInput2()
    tmp = df %>% slice(1:7) %>% mutate(Departement = reorder(Departement,-Duree)) %>% 
      ggplot(aes(Departement, Duree)) + geom_bar(stat="identity", fill = "#f16e00") +  coord_flip()
   
  ggplotly(tmp)
  
  })
  

  
})
