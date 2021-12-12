# library(dplyr)
# library(tidyr)
# library(plotly)
# library(DT)
# library(lubridate)
# library(ggplot2)
# library(leaflet.extras)
# library(ggpubr)
# library("sp")
# library("rgdal")
# library("KernSmooth")
# library(tm)

library(shinythemes)

library(leaflet)
library(shiny)
library(tidyverse)

# data1 = read_csv('NYPD_Arrests_Data__Historic_.csv',show_col_types = FALSE)
# data2 = read_csv('NYPD_Arrest_Data__Year_to_Date_.csv',show_col_types = FALSE)
# colnames(data2)[ncol(data2)] = colnames(data1)[ncol(data1)]
# data = rbind(data1,data2)
# data$ARREST_DATE = as.Date(data$ARREST_DATE, "%m/%d/%Y")
# data = data %>% filter(ARREST_DATE >= as.Date('01/01/2012', "%m/%d/%Y"))
# data = data %>% drop_na()
# 
# # change borough name
# data$ARREST_BORO = case_when(
#     data$ARREST_BORO=='B'~"BRONX",
#     data$ARREST_BORO=='K'~"BROOKLYN",
#     data$ARREST_BORO=='Q'~"QUEENS",
#     data$ARREST_BORO=='S'~"STATEN ISLAND",
#     data$ARREST_BORO=='M'~"MANHATTAN",
# )
# 
# # change date to month
# #data$ARREST_DATE = as.Date(format(data$ARREST_DATE,format='%Y-%m'))
# 
# # change format
# data$ARREST_BORO = as.factor(data$ARREST_BORO)
# data$AGE_GROUP = as.factor(data$AGE_GROUP)
# data$PERP_SEX = as.factor(data$PERP_SEX)
# data$PERP_RACE = as.factor(data$PERP_RACE)
# data$OFNS_DESC= as.factor(data$OFNS_DESC)
# 
# # select useful data
# data = data %>% select(ARREST_DATE,ARREST_BORO,Longitude,Latitude,PERP_SEX,AGE_GROUP,OFNS_DESC,PERP_RACE)

data=read_csv('data.csv',show_col_types = FALSE)
data$ARREST_BORO = as.factor(data$ARREST_BORO)
data$AGE_GROUP = as.factor(data$AGE_GROUP)
data$PERP_SEX = as.factor(data$PERP_SEX)
data$PERP_RACE = as.factor(data$PERP_RACE)
data$OFNS_DESC= as.factor(data$OFNS_DESC)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("NYC Crime Map"),
                sidebarLayout(
                  sidebarPanel(
                    conditionalPanel(condition="input.tabselected==1", width="400px",
                                     uiOutput("dateSelect"),
                                     uiOutput("boroSelect"),
                                     uiOutput("ageSelect"),
                                     uiOutput("raceSelect"),
                                     uiOutput("crimetypeSelect"),
                                     uiOutput("genderSelect"),
                    ),
                  ),
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Map", value = 1,  leafletOutput("Map",height = 625)),
                                id = "tabselected")
                    
                  )
                )
)


#Server Function
server <- function(input, output) {
  
  output$dateSelect <- renderUI({
    dateInput(inputId = "startDate", 
              startview = "year",
              label = "Select Date",
              value = "2012-01-01",
              max = '2021-09-30',
              min = '2012-01-01'
    )
  })
  
  output$boroSelect <- renderUI({
    selectInput(inputId = "boro",
                label = "Select Borough",
                choices = c("ALL",levels(data$ARREST_BORO) ),
                selected = "ALL")
  })
  
  output$ageSelect <- renderUI({
    selectInput(inputId = "age",
                label = "Select Age",
                choices = c('ALL',levels(data$AGE_GROUP)),
                selected = "ALL")
  })
  
  output$raceSelect <- renderUI({
    selectInput(inputId = "race",
                label = "Select Race",
                choices = c('ALL',levels(data$PERP_RACE)),
                selected = "ALL")
  })
  
  output$crimetypeSelect <- renderUI({
    selectInput(inputId = "crimetype",
                label = "Select Crime Type",
                choices = c('ALL',levels(data$OFNS_DESC)),
                selected = "ALL")
  })
  
  output$genderSelect <- renderUI({
    selectInput(inputId = "gender",
                label = "Select Gender",
                choices = c('ALL',levels(data$PERP_SEX)),
                selected = "ALL")
  })
  
  
  #Reactive expressions for Map Tab
  points <- reactive({
    req(input$boro, input$startDate,input$age)
    if (input$boro == "ALL"){
      filtered_data <- data %>% filter(ARREST_DATE == input$startDate) 
    }
    else {
      filtered_data <- data %>% filter(ARREST_DATE == input$startDate) %>% filter(ARREST_BORO == input$boro) 
    }
    
    if (input$age == "ALL"){
      filtered_data <- filtered_data
    }
    else {
      filtered_data <- filtered_data %>% filter(AGE_GROUP== input$age) 
    }
    
    if (input$race == "ALL"){
      filtered_data <- filtered_data
    }
    else {
      filtered_data <- filtered_data %>% filter(PERP_RACE== input$race) 
    }
    
    if (input$crimetype == "ALL"){
      filtered_data <- filtered_data
    }
    else {
      filtered_data <- filtered_data %>% filter(OFNS_DESC== input$crimetype) 
    }
    
    if (input$gender == "ALL"){
      filtered_data <- filtered_data
    }
    else {
      filtered_data <- filtered_data %>% filter(PERP_SEX== input$gender) 
    }
    
  })
  
  
  output$Map <- renderLeaflet({
    req(points())
    
    leaflet(points()) %>% setView(lat = 40.7128, lng = -74.0060, zoom = 11) %>%
      addTiles() %>% 
      addCircleMarkers(data = points(),  
                       lng = ~Longitude, 
                       lat = ~Latitude, 
                       radius = 10, 
                       color = 'red',
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       popup = ~paste("<strong>arrest date: </strong>",ARREST_DATE,
                                      "<br /><strong>borough: </strong>", ARREST_BORO,
                                      "<br /><strong>age: </strong>", AGE_GROUP,
                                      "<br /><strong>race: </strong>", PERP_RACE,
                                      "<br /><strong>crime type: </strong>",OFNS_DESC,
                                      "<br /><strong>gender: </strong>",PERP_SEX
                       )) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)