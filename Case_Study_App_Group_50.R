#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if(!require("install.load")){
    install.packages("install.load")
}
library(install.load)

install_load("shiny", "leaflet", "htmltools", "highcharter","ggplot2", "maps","dplyr","tidyverse","rvest","raster","sf","rgeos","plotly","jpeg","png","RColorBrewer")

#load saved dataframe from Case_Study_Group_50.Rmd
load(file="final_data_Group_50.Rda")

#test: First run with 1000  
test <- final_data_Group_50%>%
  head(1000)


# Define UI for application that draws a histogram
ui <- fluidPage(
    #change font https://shiny.rstudio.com/articles/css.html
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Anton&display=swap');
        
        h1 {
          font-family: 'Lobster', cursive;
          font-weight: 500;
          line-height: 1.1;
          color: #ff0000;
        }
  
      "))
    ),
    # Application title
    headerPanel("Placeholder"),
    # Radius Input
    selectInput("value", "Selected Radius in m:", c(seq(25000, 700000, by=25000)), selected=50000, multiple=TRUE),
    #test for map
    leafletOutput("map"),
    #display renderPlot
    plotOutput("barplot"),
    #display logo
    uiOutput("Logo")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #image from URL https://stackoverflow.com/questions/45709189/renderimage-from-url-and-clickable
    output$Logo <- renderUI({
        # Return a list containining the filename
        imgur1 <- "https://www.qw.tu-berlin.de/fileadmin/_processed_/8/8d/csm_QW_ohne_Text_print_a4670877cd.jpg"
        tags$img(src = imgur1, width=80, height=65)    
    })
    # Make a reactve Radius
      distanz <- reactive({
        test%>%
          filter(test$dist <= max(as.integer(input$value)))
        # if(!is.null(input$value)){
        #   test <-test%>%
        #     filter(test$dist*1000 <= input$value[1])
        # }
        # test
      })
     
    
    
    #map output 
    output$map <- renderLeaflet({
      #golden marker for hamburg
      #sources: https://github.com/pointhi/leaflet-color-markers; https://rstudio.github.io/leaflet/markers.html
      hamburg_marker <- makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-gold.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 25/2, iconAnchorY = 41
      )
      #Funktion zum hinzufügen von Kreisen
      circleAdder <- function(x){
        my_map <- addCircles(my_map,lng=9.993682, lat=53.551085,radius = x, fillOpacity = 0.08, color="#C1A32D",  fillColor = "blue")
      }
      
      circleLoop <- function(data_input){
        for (i in 1:length(data_input)){
          circleAdder(data_input[i])
        }
      }

      #draw the map and add markers
      # See: https://stackoverflow.com/questions/40861908/shiny-r-implement-slider-input
      distanz <- distanz()
        my_map <- leaflet(distanz)%>%
        addTiles()%>%
        # https://rstudio.github.io/leaflet/markers.html  
        addMarkers(lng=~Laengengrad, 
                   lat=~Breitengrad,group="Cluster Marker", 
                   clusterOptions = markerClusterOptions(),
                   label=~paste("ID_Fahrzeug: ",
                                as.character(ID_Fahrzeug),
                                "\n",
                                "Produktionsdatum: ",
                                (Produktionsdatum),
                                "\n",
                                "Dist",
                                (dist))
                   )%>%
        addMarkers(lng=9.993682, lat=53.551085, icon=hamburg_marker)
        #Adds Radius/ Circle arround Hamburg to the map
        #addCircles(lng=9.993682, lat=53.551085,radius = as.integer(input$value[1]), fillOpacity = 0.08, color="#C1A32D",  fillColor = "blue")
        rad <- as.integer(input$value)
        rad_frame <- data.frame(rad)
        #Color platte for coloring circles with different radius
        pal <- colorFactor("Dark2", rad_frame$rad)
        my_map <- addCircles(map=my_map, data=rad_frame, lng=9.993682, lat=53.551085,radius = ~rad, fillOpacity = 0.08, color = ~pal(rad_frame$rad), fillColor = "transparent")
        
    })
    
    # Get Radi for Barplot
    #barplot output
    output$barplot <- renderPlot({
      ggplot(distanz())+
        geom_histogram(aes(Bundesland), stat="count")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)