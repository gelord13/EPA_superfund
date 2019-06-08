#For the creation of a map of EPA superfund sites and teh creation of buffers
#around those sites

library(tidyverse)
library(plyr)
library(dplyr)

library(RPostgreSQL)
library(postGIStools)
library(rgdal)
library(rgeos)
library(sp)
library(tmap)
library(leaflet)
library(raster)
library(sf)
library(RQGIS)
library(shiny)
library(spData)


#import and set CRS
superfund_shape <- readOGR(".", "superfund_npl")

superfund_shape2 <- st_as_sf(superfund_shape)

superfund_shape3 <- st_transform(superfund_shape2, 4326)

superfund_illinois <- superfund_shape3%>%
  filter(STATE == "IL")

il_buffer_test <- st_buffer(superfund_illinois, dist = 0.1)
il_buffer_1km <- st_buffer(superfund_illinois, dist = c(0.00904363554, 0.00898448379))
il_buffer_5km <- st_buffer(superfund_illinois, dist = c(0.045218177, 0.04492241898))


# Define UI for application that filters map points based on year and minimum population
ui <- fluidPage(
  
  # Application title
  titlePanel("EPA Superfund Sites in Illinois"),
  
  # Sidebar with a slider input for year, numeric input for population 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("hrs",
                  "HRS of Illinois sites",
                  min = 0,
                  max = 100,
                  step = 1,
                  sep = "",
                  value = 0)
      
    ),
    
    # Show the map and table
    mainPanel(
      leafletOutput("map2"),
      dataTableOutput("table2")
    )
  )
)

# Define server logic required to draw a map and table
server <- function(input, output) {
  
  
  
  output$map2 <- renderLeaflet({
    
    illinois_sf <- superfund_illinois%>%
      filter(HRS >= input$hrs)
    
    il_buff_test <- il_buffer_test%>%
      filter(HRS >= input$hrs)
    
    il_buff_1km <- il_buffer_1km %>%
      filter(HRS >= input$hrs)
    
    il_buff_5km <- il_buffer_5km %>%
      filter(HRS >= input$hrs)
    
    
    leaflet(data = illinois_sf) %>%
      addTiles() %>%
      addMarkers(lng = illinois_sf$LONGD, lat = illinois_sf$LATD, 
                 popup = illinois_sf$NAME) %>%
      addPolygons(data = il_buff_test, group = "Test")
    addPolygons(data = il_buff_1km, group = "1 Kilometer") %>%
      addPolygons(data = il_buff_5km, group = "5 Kilometers") %>%
      addLayersControl( 
        baseGroups = c("Test", "1 Kilometer", "5 Kilometers"), 
        options = layersControlOptions(collapsed = FALSE))%>%
      setView(lng = -89.3985, lat = 40.6331, zoom = 5)
  })
  
  
  output$table2 <- renderDataTable({
    
    illinois_sf <- superfund_illinois%>%
      filter(HRS >= input$hrs)
    
    illinois_sf
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)