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

install_load("shiny", "leaflet", "htmltools", "highcharter","ggplot2", "maps","dplyr","tidyverse","rvest","raster","sf","rgeos","plotly","jpeg","png")

#load saved dataframe from Case_Study_Group_50.Rmd
load(file="final_data_Group_50.Rda")

#test: First run with 1000  
test <- final_data_Group_50%>%
  head(10000)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Placeholder"),
    # Radius Input
    sliderInput("value", "Selected Radius in km:", min = 25, max= 700,value = 50,step=25),
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
        test
        if(!is.null(input$value)){
          test <-test%>%
            filter(test$dist <= input$value[1]*1000)
        }
        test
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
      
      #draw the map and add markers
      # See: https://stackoverflow.com/questions/40861908/shiny-r-implement-slider-input
      test <- distanz()
        leaflet(test)%>%
        addTiles()%>%
        addMarkers(lng=~Laengengrad, 
                   lat=~Breitengrad,group="Cluster Marker", 
                   clusterOptions = markerClusterOptions(),
                   label=~paste("ID_Fahrzeug: ",
                                as.character(ID_Fahrzeug),
                                "\n",
                                "Produktionsdatum: ",
                                (Produktionsdatum))
                   )%>%
        addMarkers(lng=9.993682, lat=53.551085, icon=hamburg_marker)%>%
        addCircles(lng=9.993682, lat=53.551085,radius = input$value[1]*1000)
        
    })
    
    #barplot output
    output$barplot <- renderPlot({
      ggplot(test)+
        geom_histogram(aes(Bundesland), stat="count")
    })
    # Connect slider bar with
}

# Run the application 
shinyApp(ui = ui, server = server)