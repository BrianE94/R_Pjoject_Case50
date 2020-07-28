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

#load saved dataframe 
load(file="final_data_Group_50.Rda")

#test 
test <- final_data_Group_50%>%
  head(1000)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Placeholder"),
    #test for map
    leafletOutput("map"),
    #display renderPlot
    plotOutput("barplot"),
    #display logo
    uiOutput("Logo")

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
    # sidebarPanel(
    #    sliderInput("bins",
    #               "Number of bins:",
    #               min = 1,
    #               max = 50,
    #              value = 30)
    #),
    
    # Show a plot of the generated distribution
    #mainPanel(
    
    #)
    #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #image from URL https://stackoverflow.com/questions/45709189/renderimage-from-url-and-clickable
    output$Logo <- renderUI({
        # Return a list containining the filename
        imgur1 <- "https://www.qw.tu-berlin.de/fileadmin/_processed_/8/8d/csm_QW_ohne_Text_print_a4670877cd.jpg"
        tags$img(src = imgur1, width=80, height=65)    
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
        addCircles(lng=9.993682, lat=53.551085,radius = 10000)
    })
    #barplot output
    output$barplot <- renderPlot({
      ggplot(test)+
        geom_histogram(aes(Bundesland), stat="count")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)