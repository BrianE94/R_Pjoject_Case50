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

install_load("shiny", "leaflet", "htmltools", "highcharter","ggplot2", "maps","dplyr","tidyverse","rvest","raster","sf","rgeos","plotly","jpeg","png","RColorBrewer","DT")

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
    #Creating a sidebarLayout 
    sidebarLayout(
      #Creating Sidebar
      sidebarPanel(
      # Radius Input
        #selectInput("value", "Select Radius in km:", c(seq(25000, 700000, by=25000)), selected=50000, multiple=TRUE, width = 150), 
        #multiple choice number of radius
        selectInput("number", "select number of radius to compare", c(1:6), width= 200),
        actionButton("reset_input", "Reset inputs"),
        conditionalPanel(
          condition = "input.number == 1",
          sliderInput("n_1","Select Radius", min=25, max = 700, step = 25, value= 0)
        ), 
        conditionalPanel(
          condition = "input.number == 2",
          sliderInput("n_1_2","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2","Select Radius", min=0, max = 700, step = 25, value= 0)
        ),
        conditionalPanel(
          condition = "input.number == 3",
          sliderInput("n_1_3","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_3","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_3","Select Radius", min=0, max = 700, step = 25, value= 0)
        ),
        conditionalPanel(
          condition = "input.number == 4",
          sliderInput("n_1_4","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_4","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_3_4","Select Radius", min=0, max = 700, step = 25, value= 50),
          sliderInput("n_4","Select Radius", min=0, max = 700, step = 25, value= 0)
        ),
        conditionalPanel(
          condition = "input.number == 5",
          sliderInput("n_1_5","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_5","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_3_5","Select Radius", min=0, max = 700, step = 25, value= 50),
          sliderInput("n_4_5","Select Radius", min=0, max = 700, step = 25, value= 50),
          sliderInput("n_5","Select Radius", min=0, max = 700, step = 25, value= 0)
        ),
        conditionalPanel(
          condition = "input.number == 6",
          sliderInput("n_1_6","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_6","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_3_6","Select Radius", min=0, max = 700, step = 25, value= 50),
          sliderInput("n_4_6","Select Radius", min=0, max = 700, step = 25, value= 50),
          sliderInput("n_5_6","Select Radius", min=0, max = 700, step = 25, value= 50),
          sliderInput("n_6","Select Radius", min=0, max = 700, step = 25, value= 0)
        )
      ),
      #Creating MainPanel with Tabsets
      mainPanel(
        uiOutput("Logo", align="right"),
        #Creating Tabsets 
        tabsetPanel(
          tabPanel("Map",leafletOutput("map") ),

          #display renderPlot
          tabPanel("Barplot",plotOutput("barplot")),
          #display logo
          
          tabPanel("Table", dataTableOutput('datatable_rad'))
        )
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #image from URL https://stackoverflow.com/questions/45709189/renderimage-from-url-and-clickable
    output$Logo <- renderUI({
        # Return a list containining the filename
        imgur1 <- "https://www.qw.tu-berlin.de/fileadmin/_processed_/8/8d/csm_QW_ohne_Text_print_a4670877cd.jpg"
        tags$img(src = imgur1, width=80, height=65)    
    })
    
    
      
    observe({
      if (input$number==1){
        updateSliderInput(session,"n_2", value= 0)
        updateSliderInput(session,"n_3", value= 0)
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==2){  
        val1 <- input$n_1_2
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_3", value= 0)
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==3){
        val1 <- input$n_1_3
        val2 <- input$n_2_3
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==4){
        val1 <- input$n_1_4
        val2 <- input$n_2_4
        val3 <- input$n_3_4
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        updateSliderInput(session,"n_3", value= val3)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==5){
        val1 <- input$n_1_5
        val2 <- input$n_2_5
        val3 <- input$n_3_5
        val4 <- input$n_4_5
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        updateSliderInput(session,"n_3", value= val3)
        updateSliderInput(session,"n_4", value= val4)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==6){
        val1 <- input$n_1_6
        val2 <- input$n_2_6
        val3 <- input$n_3_6
        val4 <- input$n_4_6
        val5 <- input$n_5_6
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        updateSliderInput(session,"n_3", value= val3)
        updateSliderInput(session,"n_4", value= val4)
        updateSliderInput(session,"n_5", value= val5)
      }
    })



    # Make a reactve Radius
    distanz <- reactive({
      test%>%
        filter(test$dist <= as.numeric(max(as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000)))
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
        #Adds Radius/ Circle arround Hamburg to the map
        addMarkers(lng=9.993682, lat=53.551085, icon=hamburg_marker)
        
        
        rad <- as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000
        rad_frame <- data.frame(rad, "index"=c(1:6))
        #Color platte for coloring circles with different radius
        #https://rstudio.github.io/leaflet/colors.html
        #https://cfss.uchicago.edu/notes/leaflet/
        pal <- colorFactor("Dark2", rad_frame$index)
        my_map <- addCircles(map=my_map, data=rad_frame, lng=9.993682, lat=53.551085,radius = ~rad, fillOpacity = 0.02, color = ~pal(rad_frame$index), fillColor = ~pal(rad_frame$index))
        
    })
    
    #Create dataset with number of cars for selected radius
    # r1 --> count(dist)
    
    # Get Radi for Barplot
    #barplot output
    output$barplot <- renderPlot({
      ggplot(distanz())+
        geom_histogram(aes(Bundesland), stat="count")
    })
    #Prepare Dataset 
    get_dataset <- function(){  
      final_rad <- distanz()%>%
          select(Bundesland, dist)%>%
          group_by(Bundesland)%>%
          summarise(totalBundesland = n())%>%
          arrange(desc(totalBundesland))
    }
    
    #filter_dataset <- reactive(x,{
     # get_dataset() %>%
      #  filter(dist <= x)
    #})
    
    #add observations for all selected radius
    
    get_dataset_all <- reactive({
      set1<- get_dataset()
      val1_1=input$n_1
      n_1_filtered <- filter(set1, dist <= val1_1)
      #n_2_filtered <- filter(get_dataset(), dist <= input$n_2)
      #n_3_filtered <- filter(get_dataset(), dist <= input$n_3)
      #n_4_filtered <- filter(get_dataset(), dist <= input$n_4)
      #n_5_filtered <- filter(get_dataset(), dist <= input$n_5)
      #n_6_filtered <- filter(get_dataset(), dist <= input$n_6)
      #a<-get_dataset()%>%
        #mutate(rad1 = map_lgl(dist,(dist-input$n_1>0)))
        #mutate(rad1 = dist - input$n_1 > 0)
      #a <-map_dfc(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6)*1000, filter_dataset())
      #print(head(a))
      get_final_dataset<-cbind(get_dataset(),n_1_filtered)
      get_final_dataset
    })
    
    #get_dataset_all()
    #filter_dataset(500000)
    
    output$datatable_rad <- renderDataTable({
      datatable(
        get_dataset()
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)