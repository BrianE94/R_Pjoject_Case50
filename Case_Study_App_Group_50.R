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

install_load("shiny", "leaflet", "htmltools", "highcharter","ggplot2", "maps","dplyr","tidyverse","rvest","raster","sf","rgeos","plotly")

#test 
test <- final_data_Group_50%>%
  head(50)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #test for map
    leafletOutput("map"),
    # Application title
    titlePanel("Placeholder"),
    
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
    output$map <- renderLeaflet({
        leaflet(test)%>%
        addTiles()%>%
        addMarkers(lng=~Laengengrad, lat=~Breitengrad)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)