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

install_load("shiny", "leaflet", "htmltools", "highcharter", "ggplot2", "maps", "dplyr", "tidyverse", "rvest", "raster", "sf", "rgeos", "plotly", "jpeg", "png", "RColorBrewer", "DT", "janitor", "shinythemes", "shinyWidgets")

#load saved dataframe from Case_Study_Group_50.Rmd
load(file="Final_Data_Group_50.Rda")

#assign loaded dataset to new dataset called 'final_data' used throughout the whole app
#in case the app crashes due to high processing efforts, disable comments and use only part of the dataset
final_data <- Final_Data_Group_50#%>%
  #head(100000)
  #tail(100000)

# Define UI for application 
ui <- fluidPage(
    #change font https://shiny.rstudio.com/articles/css.html
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Anton&display=swap');
        
        h1 {
          font-family: 'Lucida Console', Courier, monospace;
          font-weight: 500;
          line-height: 1.1;
          color: #698b47;
        }
  
      "))
    ),
    # Application title
    headerPanel("Car Recall Action"),
    # Adding a theme that harmonizes with the corporate color
    theme = shinythemes::shinytheme('sandstone'),
    # How to change the color of slider inputs: https://www.rdocumentation.org/packages/shinyWidgets/versions/0.5.3/topics/setSliderColor
    setSliderColor(c("#698b47", "#698b47","#698b47","#698b47","#698b47", "#698b47", "#698b47","#698b47","#698b47","#698b47","#698b47", "#698b47","#698b47","#698b47","#698b47","#698b47", "#698b47","#698b47","#698b47","#698b47", "#698b47", "#698b47"), c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)),
    #Creating a sidebarLayout 
    sidebarLayout(
      #Creating Sidebar
      sidebarPanel(
      # Radius Input
        #multiple choice number of radius
        selectInput("number", "Select number of radii to compare", c(1:6), width= 200),
        #using conditional panels to only show selected amount of sliders for selecting radius
        conditionalPanel(
          condition = "input.number == 1",
          sliderInput("n_1","Select Radius 1 in km", min=25, max = 700, step = 25, value= 50)
        ), 
        conditionalPanel(
          condition = "input.number == 2",
          sliderInput("n_1_2","Select Radius 1 in km", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2","Select Radius 2 in km", min=0, max = 700, step = 25, value=100)
        ),
        conditionalPanel(
          condition = "input.number == 3",
          sliderInput("n_1_3","Select Radius 1 in km", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_3","Select Radius 2 in km", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3","Select Radius 3 in km", min=0, max = 700, step = 25, value= 200)
        ),
        conditionalPanel(
          condition = "input.number == 4",
          sliderInput("n_1_4","Select Radius 1 in km", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_4","Select Radius 2 in km", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3_4","Select Radius 3 in km", min=0, max = 700, step = 25, value= 200),
          sliderInput("n_4","Select Radius 4 in km", min=0, max = 700, step = 25, value= 300)
        ),
        conditionalPanel(
          condition = "input.number == 5",
          sliderInput("n_1_5","Select Radius 1 in km", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_5","Select Radius 2 in km", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3_5","Select Radius 3 in km", min=0, max = 700, step = 25, value= 200),
          sliderInput("n_4_5","Select Radius 4 in km", min=0, max = 700, step = 25, value= 300),
          sliderInput("n_5","Select Radius 5 in km", min=0, max = 700, step = 25, value= 400)
        ),
        conditionalPanel(
          condition = "input.number == 6",
          sliderInput("n_1_6","Select Radius 1 in km", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_6","Select Radius 2 in km", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3_6","Select Radius 3 in km", min=0, max = 700, step = 25, value= 200),
          sliderInput("n_4_6","Select Radius 4 in km", min=0, max = 700, step = 25, value= 300),
          sliderInput("n_5_6","Select Radius 5 in km", min=0, max = 700, step = 25, value= 400),
          sliderInput("n_6","Select Radius 6 in km", min=0, max = 700, step = 25, value= 500)
        ),
        # Add user input to highlight cities and communities with a certain amount of affected vehicles 
        sliderInput("anzahl", "Select a critical number to find all cities and communities with a certain amount of affected registered vehicles", min = 500, max = 7500, value = 5000, step = 100)
      ),
      #Creating MainPanel with Tabsets
      mainPanel(
        #display logo
        uiOutput("Logo", align="right"),
        #Creating Tabsets 
        tabsetPanel(
          tabPanel("Map",leafletOutput("map") ),
          
          #display Barplot
          tabPanel("Barplot",plotOutput("barplot")),
          #display DataTable supporting the Barplot
          tabPanel("Table", dataTableOutput('datatable_rad')),
          #display Basic Dataset
          tabPanel("Basic Dataset", dataTableOutput('basic_dataset'))
        )
      )
    )

)

# Define server logic 
server <- function(input, output, session) {
    
    #image from URL https://stackoverflow.com/questions/45709189/renderimage-from-url-and-clickable
    output$Logo <- renderUI({
        # Return a list containining the filename
        imgur1 <- "https://www.qw.tu-berlin.de/fileadmin/_processed_/8/8d/csm_QW_ohne_Text_print_a4670877cd.jpg"
        tags$img(src = imgur1, width=80, height=65)    
    })
    
    
    #catch default values 
    #https://stackoverflow.com/questions/33662033/shiny-how-to-make-reactive-value-initialize-with-default-value#38565508
    defaults <- isolate(as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6)))

    # Make a reactive Radius
    distanz <- reactive({
      final_data%>%
        filter(final_data$dist <= as.numeric(max(as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000)))
    })
    
    #Define new dataset for reactive function anzahl because otherwise, the app crashes
    #This is used to find all Cities of Interest that have a certain amount of affected vehicles
    #It needs to be reactive so when the user changes to critical number, the number of highlighted cities also changes
    cities_amount <- final_data%>%
      distinct(Ort, .keep_all=TRUE)
    
    anzahl <- reactive({
      filter(cities_amount, n>=input$anzahl)
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
      
      #This marker is used to higlight the Cities of Interest
      auto_marker <- makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
        iconWidth = 15, iconHeight = 25,
        iconAnchorX = 15/2, iconAnchorY = 25
      )
      
      #draw the map and add markers
      # See: https://stackoverflow.com/questions/40861908/shiny-r-implement-slider-input
      distanz <- distanz()
        my_map <- leaflet(distanz)%>%
        
        addTiles()%>%
        #Layer 1 Cluster Anzeige 
        # https://rstudio.github.io/leaflet/markers.html  
        addMarkers(lng=~Laengengrad, 
                   lat=~Breitengrad,
                   group="Clustered Markers", 
                   clusterOptions = markerClusterOptions(),
                   #Choosing popups rather than labels because we prefer information only being displayed when clicking on it. Source:https://rstudio.github.io/leaflet/popups.html
                   popup=~paste("Vehicle ID: ",
                                as.character(ID_Fahrzeug),
                                " ",
                                "Motor Production Date: ",
                                (Produktionsdatum),
                                " ",
                                "Distance to Hamburg in km",
                                (dist/1000),
                                #Separating lines by a break, source: https://stackoverflow.com/questions/26368192/how-to-insert-new-line-in-r-shiny-string
                                sep="<br/>")
                   )%>%
        #Adds marker for Hamburg to the map
        addMarkers(lng=9.993682, lat=53.551085, icon=hamburg_marker)
        
        #Adds Radius/ Circle arround Hamburg to the map
        rad <- as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000
        rad_frame <- data.frame(rad, "index"=c(1:6))
        #Color platte for coloring circles with different radius
        #https://rstudio.github.io/leaflet/colors.html
        #https://cfss.uchicago.edu/notes/leaflet/
        pal <- colorFactor("Dark2", rad_frame$index)
        my_map <- addCircles(map=my_map, data=rad_frame, lng=9.993682, lat=53.551085,radius = ~rad, fillOpacity = 0.02, color = ~pal(rad_frame$index), fillColor = ~pal(rad_frame$index), group="Clustered Markers")
        
        #Layer 2 Cities of Interest 
        my_map <- addMarkers(map=my_map, data = anzahl (),
                  lng=~Laengengrad,
                  lat=~Breitengrad,
                  group="Cities of Interest",
                  icon = auto_marker,
                  #Choosing labels rather than because we prefer information being displayed when hovering above the marker, Source:https://rstudio.github.io/leaflet/popups.html
                  label=~paste("Number of affected vehicles in ",
                               Ort,
                               ":",
                               n)
                   )
        #Add map controls for different groups/Layers 
        my_map <- addLayersControl(map=my_map, overlayGroups = c("Clustered Markers","Cities of Interest"),options = layersControlOptions(collapsed = FALSE))
    })
    
    #Save selected Layers 
    #https://stackoverflow.com/questions/41468538/is-it-possible-to-access-r-leaflet-layer-controls-in-shiny-outside-of-leaflet
    selected_groups <- reactive ({
      groups_sel <- input$map_groups
      groups_sel
    })
    
    
    #Change values on Number of Radius. Allows for values to stay the same when selecting a higher amount of radius to compare to 
    observe({
      if (input$number==1){
        # When radius 1 is set, it is also saved for when there is more than one radius but is still adjustable
        updateSliderInput(session,"n_1_2", value= input$n_1)
        updateSliderInput(session,"n_1_3", value= input$n_1)
        updateSliderInput(session,"n_1_4", value= input$n_1)
        updateSliderInput(session,"n_1_5", value= input$n_1)
        updateSliderInput(session,"n_1_6", value= input$n_1)
        # All radii that are not needed are set to 0
        updateSliderInput(session,"n_2", value= 0)
        updateSliderInput(session,"n_3", value= 0)
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==2){  
        val1 <- input$n_1_2
        # When radius 1 is adjusted, it is also saved for the other numbers of radii
        updateSliderInput(session,"n_1_3", value= val1)
        updateSliderInput(session,"n_1_4", value= val1)
        updateSliderInput(session,"n_1_5", value= val1)
        updateSliderInput(session,"n_1_6", value= val1)
        # When radius 2 is set, it is also saved for when there is more than two radii but is still adjustable
        updateSliderInput(session,"n_2_3", value= input$n_2)
        updateSliderInput(session,"n_2_4", value= input$n_2)
        updateSliderInput(session,"n_2_5", value= input$n_2)
        updateSliderInput(session,"n_2_6", value= input$n_2)
        # All radii that are not needed are set to 0
        updateSliderInput(session,"n_1", value= val1)
        #updateSliderInput(session,"n_2", value= defaults[2])
        updateSliderInput(session,"n_3", value= 0)
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==3){
        val1 <- input$n_1_3
        val2 <- input$n_2_3
        
        #The inputs are adjusted according to the pattern above
        updateSliderInput(session,"n_1_4", value= val1)
        updateSliderInput(session,"n_1_5", value= val1)
        updateSliderInput(session,"n_1_6", value= val1)
        
        updateSliderInput(session,"n_2_4", value= val2)
        updateSliderInput(session,"n_2_5", value= val2)
        updateSliderInput(session,"n_2_6", value= val2)
        
        updateSliderInput(session,"n_3_4", value= input$n_3)
        updateSliderInput(session,"n_3_5", value= input$n_3)
        updateSliderInput(session,"n_3_6", value= input$n_3)
        
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        #updateSliderInput(session,"n_3", value= defaults[3])
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==4){
        val1 <- input$n_1_4
        val2 <- input$n_2_4
        val3 <- input$n_3_4
        
        #The inputs are adjusted according to the pattern above
        updateSliderInput(session,"n_1_5", value= val1)
        updateSliderInput(session,"n_1_6", value= val1)
        
        updateSliderInput(session,"n_2_5", value= val2)
        updateSliderInput(session,"n_2_6", value= val2)
        
        updateSliderInput(session,"n_3_5", value= val3)
        updateSliderInput(session,"n_3_6", value= val3)
        
        updateSliderInput(session,"n_4_5", value= input$n_4)
        updateSliderInput(session,"n_4_6", value= input$n_4)
        
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        updateSliderInput(session,"n_3", value= val3)
        #updateSliderInput(session,"n_4", value= defaults[4])
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==5){
        val1 <- input$n_1_5
        val2 <- input$n_2_5
        val3 <- input$n_3_5
        val4 <- input$n_4_5
        
        #The inputs are adjusted according to the pattern above
        updateSliderInput(session,"n_1_6", value= val1)

        updateSliderInput(session,"n_2_6", value= val2)
        
        updateSliderInput(session,"n_3_6", value= val3)
        
        updateSliderInput(session,"n_4_6", value= val4)
        
        updateSliderInput(session,"n_5_6", value= input$n_5)
        
        updateSliderInput(session,"n_1", value= val1)
        updateSliderInput(session,"n_2", value= val2)
        updateSliderInput(session,"n_3", value= val3)
        updateSliderInput(session,"n_4", value= val4)
        #updateSliderInput(session,"n_5", value= defaults[5])
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
        #updateSliderInput(session,"n_6", value= defaults[6])
      }
    })
    

    #Prepare Dataset due to max radius, only keep necessary info 
    get_basic_dataset <- reactive({
      basic_rad <- distanz()%>%
        select(Bundesland, dist)
      basic_rad
    })
    
    #get the values retrieved from the input sliders 
    inputvector <- reactive ({
      input_vector <- as.numeric(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000
      input_vector
    })
    
    #add observations (amount of vehicles per radius) for all selected radius for displaying a datatable supporting the barplot
    get_dataset_all <- reactive({
      n_1_filtered <- filter(get_basic_dataset(), dist <= inputvector()[1])%>%
        group_by(Bundesland)%>%
        summarise(Radius_1 = n())%>%
        arrange(desc(Radius_1))%>%
        ungroup()
      n_2_filtered <- filter(get_basic_dataset(), dist <= inputvector()[2])%>%
        group_by(Bundesland)%>%
        summarise(Radius_2 = n())%>%
        arrange(desc(Radius_2))%>%
        ungroup()
      n_3_filtered <- filter(get_basic_dataset(), dist <= inputvector()[3])%>%
        group_by(Bundesland)%>%
        summarise(Radius_3 = n())%>%
        arrange(desc(Radius_3))%>%
        ungroup()
      n_4_filtered <- filter(get_basic_dataset(), dist <= inputvector()[4])%>%
        group_by(Bundesland)%>%
        summarise(Radius_4 = n())%>%
        arrange(desc(Radius_4))%>%
        ungroup()
      n_5_filtered <- filter(get_basic_dataset(), dist <= inputvector()[5])%>%
        group_by(Bundesland)%>%
        summarise(Radius_5 = n())%>%
        arrange(desc(Radius_5))%>%
        ungroup()
      n_6_filtered <- filter(get_basic_dataset(), dist <= inputvector()[6])%>%
        group_by(Bundesland)%>%
        summarise(Radius_6 = n())%>%
        arrange(desc(Radius_6))%>%
        ungroup()
      final_rad_all <- list(n_1_filtered,n_2_filtered,n_3_filtered,n_4_filtered,n_5_filtered,n_6_filtered)%>%
        #https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list#34393416
        reduce(full_join, by="Bundesland")%>%
        #https://rdrr.io/cran/janitor/man/remove_empty.html
        remove_empty("cols")
        #fill with "0's"
        final_rad_all[-1][is.na(final_rad_all[-1])]<-0
        #https://stackoverflow.com/questions/16363922/convert-a-vector-into-a-list-each-element-in-the-vector-as-an-element-in-the-li
        sum_1 <- list(Bundesland="Sum")
        sum_2 <- as.list(colSums(final_rad_all[-1]))
        sums <- c(sum_1,sum_2)
        #https://stackoverflow.com/questions/28467068/how-can-a-add-a-row-to-a-data-frame-in-r
        final_rad_all[nrow(final_rad_all)+1,]=sums
        #https://stackoverflow.com/questions/11036989/replace-all-0-values-to-na
        final_rad_all[final_rad_all==0]<- NA
      final_rad_all
    })

    #output Datatable supporting barplot
    output$datatable_rad <- renderDataTable({
      datatable(
        # Disable pagination: https://shiny.rstudio.com/gallery/datatables-options.html
        # Disable sorting: https://stackoverflow.com/questions/37848626/suppressing-sorting-in-datatables-in-shiny/37852445
        options = list(paging = FALSE, ordering = FALSE),
        # Disable numbering of the rows: https://stackoverflow.com/questions/55229736/how-to-remove-the-first-column-index-from-data-table-in-r-shiny
        rownames = FALSE,
        get_dataset_all()%>%
          rename("State" = Bundesland)
        )%>%
        # How to highlight the row "Sum": https://rstudio.github.io/DT/010-style.html
        formatStyle(
          'State',
          target = 'row',
          backgroundColor = styleEqual("Sum", "#698b47"),
          color = styleEqual("Sum", "white")
        )
    })
    
    #Basic Datatable to prove visualisations
    output$basic_dataset <- renderDataTable({
      datatable(rownames = FALSE,
                final_data %>%
                  mutate(dist_zu_ham = dist/1000)%>% #Distance to Hamburg is converted from meters to kilometers
                  select(Ort, Bundesland, Laengengrad, Breitengrad, ID_Motor,Produktionsdatum, ID_Fahrzeug, Zulassung, dist_zu_ham, n)%>% #only necessary attributes are selected and thereby rearranged
                  arrange(Ort)%>% #rows are arranged by Ort name
                  rename("City/Community" = Ort, State = Bundesland, Longitude = Laengengrad, Latitude = Breitengrad, "Distance to Hamburg in km" = dist_zu_ham, "Vehicle ID" = ID_Fahrzeug, "Production Date of the Motor" = Produktionsdatum, "Registration Date of the Vehicle" = Zulassung, "Motor ID" = ID_Motor, "Cases in City/Community"=n) #Variables are renamed from German to English
                )
    })
    
    #preparing a dataset for barplotting 
    get_plot_dataset_advanced_factor <- reactive({
      inputvector_import <- as.integer(inputvector())
      names(inputvector_import)=c(1:6)
      inputvector_sorted <- inputvector_import
      inputvector_sorted[inputvector_sorted==0]<-NA
      inputvector_sorted <- inputvector_sorted[!is.na(inputvector_sorted)]
      #Get dataset 
      final_rad_all_plot_advanced_factor <- get_basic_dataset()
      
      #Radius 1
      if(input$number == 1|2|3|4|5|6){
        if(inputvector_import["1"]!=0){
          final_rad_all_plot_advanced_factor$Radius1 <- as.logical(cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted["1"]),labels=TRUE))
          final_rad_all_plot_advanced_factor$Radius1[final_rad_all_plot_advanced_factor$Radius1==TRUE]<-1
        }
      }
      
      #Radius 2
      if(input$number == 2|3|4|5|6){
        if(inputvector_import["2"]!=0){
          final_rad_all_plot_advanced_factor$Radius2 <- as.logical(cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted["2"]),labels=TRUE))
          final_rad_all_plot_advanced_factor$Radius2[final_rad_all_plot_advanced_factor$Radius2==TRUE]<-2
        }
      }
      # 
      #Radius3
      if(input$number == 3|4|5|6){
        if(inputvector_import["3"]!=0){
          final_rad_all_plot_advanced_factor$Radius3 <- as.logical(cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted["3"]),labels=TRUE))
          final_rad_all_plot_advanced_factor$Radius3[final_rad_all_plot_advanced_factor$Radius3==TRUE]<-3
        }
      }
      
      #Radius4
      if(input$number == 4|5|6){
        if(inputvector_import["4"]!=0){
          final_rad_all_plot_advanced_factor$Radius4 <- as.logical(cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted["4"]),labels=TRUE))
          final_rad_all_plot_advanced_factor$Radius4[final_rad_all_plot_advanced_factor$Radius4==TRUE]<-4
        }
      }
      
      #Radius5
      if(input$number == 5|6){
        if(inputvector_import["5"]!=0){
          final_rad_all_plot_advanced_factor$Radius5 <- as.logical(cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted["5"]),labels=TRUE))
          final_rad_all_plot_advanced_factor$Radius5[final_rad_all_plot_advanced_factor$Radius5==TRUE]<-5
        }
      }
      
      #Radius6
      if(input$number == 6){
        if(inputvector_import["6"]!=0){
          final_rad_all_plot_advanced_factor$Radius6 <- as.logical(cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted["6"]),labels=TRUE))
          final_rad_all_plot_advanced_factor$Radius6[final_rad_all_plot_advanced_factor$Radius6==TRUE]<-6
        }
      }
      
      
      final_rad_all_plot_advanced_factor
    })
    
    
    # Output Barplot 
    output$barplot <- renderPlot({
      df2 <- get_plot_dataset_advanced_factor()
      p <- ggplot() +
        # Changing labels to English, changing Plot Theme, source: https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
        labs(title = "Number of registered vehicles within each radius", x = "Radius Number", y = "Number of registered vehicles", fill = "State")+
        theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16, color = "#698b47"),
              axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16, color = "#698b47"),
              plot.title = element_text(size = 20, face = "bold", color = "#698b47"),
              legend.title = element_text(size = 16, color = "#698b47"),
              legend.text = element_text(size = 14))
      #plotting histogram for every single Radius and adding it to the ggplot
      p = p+geom_histogram(data = subset(df2, Radius1 %in% c(1)),aes(Radius1, fill=Bundesland),stat="count")
      if("Radius2" %in% colnames(df2)){p = p + geom_histogram(data = subset(df2, Radius2 %in% c(2)), aes(Radius2, fill=Bundesland),stat="count")}
      if("Radius3" %in% colnames(df2)){p = p + geom_histogram(data = subset(df2, Radius3 %in% c(3)), aes(Radius3, fill=Bundesland),stat="count")}
      if("Radius4" %in% colnames(df2)){p = p + geom_histogram(data = subset(df2, Radius4 %in% c(4)), aes(Radius4, fill=Bundesland),stat="count")}
      if("Radius5" %in% colnames(df2)){p = p + geom_histogram(data = subset(df2, Radius5 %in% c(5)), aes(Radius5, fill=Bundesland),stat="count")}
      if("Radius6" %in% colnames(df2)){p = p + geom_histogram(data = subset(df2, Radius6 %in% c(6)), aes(Radius6, fill=Bundesland),stat="count")}
      p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)