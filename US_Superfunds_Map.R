#For the creation of a map of all superfund sites in the US 

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

#import and set CRS of the data
superfund_shape <- readOGR(".", "superfund_npl")
data(us_states)

superfund_shape2 <- st_as_sf(superfund_shape)
us_states2 <- st_as_sf(us_states)

superfund_shape3 <- st_transform(superfund_shape2, 4326)
us_states3 <- st_transform(us_states2, 4326)

# Define UI for application that filters map points based on year and minimum population
ui <- fluidPage(
  
  # Application title
  titlePanel("EPA Superfund Sites"),
  
  # Sidebar with a slider input for year, numeric input for population 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("hrs",
                  "HRS by State",
                  min = 0,
                  max = 100,
                  step = 1,
                  sep = "",
                  value = 0),
      
      selectInput("state", label = h3("State"),
                  choices = list("Albama" = "AL",
                                 "Alaska" = "AK",
                                 "Arizona" = "AZ",
                                 "Arkansas" = "AR",
                                 "California" = "CA",
                                 "Colorado" = "CO",
                                 "Conneticut" = "CT",
                                 "Delaware" = "DE",
                                 "Florida" = "FL",
                                 "Georgia" = "GA",
                                 "Hawaii" = "HI",
                                 "Idaho" = "ID",
                                 "Illinois" = "IL",
                                 "Indiana" = "IN",
                                 "Iowa" = "IA",
                                 "Kansas" = "KS",
                                 "Kentucky" = "KY",
                                 "Louisiana" = "LA",
                                 "Maine" = "ME",
                                 "Maryland" = "MD",
                                 "Massachusetts" = "MA",
                                 "Michigan" = "MI",
                                 "Minnesota" = "MN",
                                 "Mississippi" = "MS",
                                 "Missouri" = "MO",
                                 "Montana" = "MT",
                                 "Nebraska" = "NE",
                                 "Nevada" = "NV",
                                 "New Hampshire" = "NH",
                                 "New Jersey" = "NJ",
                                 "New Mexico" = "NM",
                                 "New York" = "NY",
                                 "North Carolina" = "NC",
                                 "North Dakota" = "ND",
                                 "Ohio" = "OH",
                                 "Oklahoma" = "OK",
                                 "Oregon" = "OR",
                                 "Pennsylvania" = "PA",
                                 "Rhode Island" = "RI",
                                 "South Carolina" = "SC",
                                 "South Dakota" = "SD",
                                 "Tennessee" = "TN",
                                 "Texas" = "TX",
                                 "Utah" = "UT",
                                 "Vermont" = "VT",
                                 "Virginia" = "VA",
                                 "Washington" = "WA",
                                 "West Virginia" = "WV",
                                 "Wisconsin" = "WI",
                                 "Wyoming" = "WY"))
      
      
    ),
    
    # Show the map and table
    mainPanel(
      leafletOutput("map1"),
      dataTableOutput("table1")
    )
  )
)

# Define server logic required to draw a map and table
server <- function(input, output) {
  
  
  
  output$map1 <- renderLeaflet({
    
    superfund_sites<- superfund_shape3%>%
      filter(HRS > input$hrs,
             STATE == input$state)
    
    leaflet(data = superfund_sites) %>%
      addTiles() %>%
      addMarkers(lng = superfund_sites$LONGD, lat = superfund_sites$LATD, 
                 popup = superfund_sites$NAME) %>% 
      addPolygons(data = us_states3) %>%
      setView(lng = -99, lat = 45, zoom = 3)
  })
  
  
  output$table1 <- renderDataTable({
    
    superfund_sites<- superfund_shape3%>%
      filter(HRS > input$hrs,
             STATE == input$state)
    
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)