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

install_load("shiny", "leaflet", "htmltools", "highcharter","ggplot2", "maps","dplyr","tidyverse","rvest","raster","sf","rgeos","plotly","jpeg","png","RColorBrewer","DT","janitor", "shinythemes")

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
          font-family: 'Verdana', cursive;
          font-weight: 500;
          line-height: 1.1;
          color: #7cfc00;
        }
  
      "))
    ),
    # Application title
    headerPanel("Placeholder"),
    # Adding a theme
    theme = shinythemes::shinytheme('sandstone'),
    #Creating a sidebarLayout 
    sidebarLayout(
      #Creating Sidebar
      sidebarPanel(
      # Radius Input
        #selectInput("value", "Select Radius in km:", c(seq(25000, 700000, by=25000)), selected=50000, multiple=TRUE, width = 150), 
        #multiple choice number of radius
        selectInput("number", "select number of radius to compare", c(1:6), width= 200),
        conditionalPanel(
          condition = "input.number == 1",
          sliderInput("n_1","Select Radius", min=25, max = 700, step = 25, value= 50)
        ), 
        conditionalPanel(
          condition = "input.number == 2",
          sliderInput("n_1_2","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2","Select Radius", min=0, max = 700, step = 25, value=100)
        ),
        conditionalPanel(
          condition = "input.number == 3",
          sliderInput("n_1_3","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_3","Select Radius", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3","Select Radius", min=0, max = 700, step = 25, value= 200)
        ),
        conditionalPanel(
          condition = "input.number == 4",
          sliderInput("n_1_4","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_4","Select Radius", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3_4","Select Radius", min=0, max = 700, step = 25, value= 200),
          sliderInput("n_4","Select Radius", min=0, max = 700, step = 25, value= 300)
        ),
        conditionalPanel(
          condition = "input.number == 5",
          sliderInput("n_1_5","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_5","Select Radius", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3_5","Select Radius", min=0, max = 700, step = 25, value= 200),
          sliderInput("n_4_5","Select Radius", min=0, max = 700, step = 25, value= 300),
          sliderInput("n_5","Select Radius", min=0, max = 700, step = 25, value= 400)
        ),
        conditionalPanel(
          condition = "input.number == 6",
          sliderInput("n_1_6","Select Radius", min=25, max = 700, step = 25, value= 50),
          sliderInput("n_2_6","Select Radius", min=25, max = 700, step = 25, value= 100),
          sliderInput("n_3_6","Select Radius", min=0, max = 700, step = 25, value= 200),
          sliderInput("n_4_6","Select Radius", min=0, max = 700, step = 25, value= 300),
          sliderInput("n_5_6","Select Radius", min=0, max = 700, step = 25, value= 400),
          sliderInput("n_6","Select Radius", min=0, max = 700, step = 25, value= 500)
        ),
        # Add user input to highlight cities and communities with a certain amount of affected vehicles (4.d)
        sliderInput("anzahl", "Select the critical number of affected registered vehicles. Cities that have this amount of affected cars (or more) are highlighted in the tab 'Cities affected'", min = 20, max = 100, value = 50)
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
          
          tabPanel("Table", dataTableOutput('datatable_rad')),
          tabPanel("Basic Dataset", dataTableOutput('basic_dataset'))
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
    
    
    #catch default values 
    #https://stackoverflow.com/questions/33662033/shiny-how-to-make-reactive-value-initialize-with-default-value#38565508
    defaults <- isolate(as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6)))
    
    
    
    observe({
      if (input$number==1){
        updateSliderInput(session,"n_1_2", value= input$n_1)
        updateSliderInput(session,"n_1_3", value= input$n_1)
        updateSliderInput(session,"n_1_4", value= input$n_1)
        updateSliderInput(session,"n_1_5", value= input$n_1)
        updateSliderInput(session,"n_1_6", value= input$n_1)
        updateSliderInput(session,"n_2", value= 0)
        updateSliderInput(session,"n_3", value= 0)
        updateSliderInput(session,"n_4", value= 0)
        updateSliderInput(session,"n_5", value= 0)
        updateSliderInput(session,"n_6", value= 0)
      }
      if(input$number==2){  
        val1 <- input$n_1_2
        updateSliderInput(session,"n_1_3", value= val1)
        updateSliderInput(session,"n_1_4", value= val1)
        updateSliderInput(session,"n_1_5", value= val1)
        updateSliderInput(session,"n_1_6", value= val1)
        #uhguzhd
        updateSliderInput(session,"n_2_3", value= input$n_2)
        updateSliderInput(session,"n_2_4", value= input$n_2)
        updateSliderInput(session,"n_2_5", value= input$n_2)
        updateSliderInput(session,"n_2_6", value= input$n_2)
        #uzgfuizwfv
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
        updateSliderInput(session,"n_1_4", value= val1)
        updateSliderInput(session,"n_1_5", value= val1)
        updateSliderInput(session,"n_1_6", value= val1)
        #iuhjgviudf
        updateSliderInput(session,"n_2_4", value= val2)
        updateSliderInput(session,"n_2_5", value= val2)
        updateSliderInput(session,"n_2_6", value= val2)
        #guzuugi
        updateSliderInput(session,"n_3_4", value= input$n_3)
        updateSliderInput(session,"n_3_5", value= input$n_3)
        updateSliderInput(session,"n_3_6", value= input$n_3)
        
        #giuciuebhvcis
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
        
        updateSliderInput(session,"n_1_5", value= val1)
        updateSliderInput(session,"n_1_6", value= val1)
        #iuhjgviudf
        
        updateSliderInput(session,"n_2_5", value= val2)
        updateSliderInput(session,"n_2_6", value= val2)
        #giuui
        updateSliderInput(session,"n_3_5", value= val3)
        updateSliderInput(session,"n_3_6", value= val3)
        #guzuugi
        
        updateSliderInput(session,"n_4_5", value= input$n_4)
        updateSliderInput(session,"n_4_6", value= input$n_4)
        
        #giuciuebhvcis
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
        
        
        updateSliderInput(session,"n_1_6", value= val1)
        #iuhjgviudf
        
        
        updateSliderInput(session,"n_2_6", value= val2)
        #giuui
        
        updateSliderInput(session,"n_3_6", value= val3)
        
        updateSliderInput(session,"n_4_6", value= val4)
        #guzuugi
        
        
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



    # Make a reactve Radius
    distanz <- reactive({
      test%>%
        filter(test$dist <= as.numeric(max(as.integer(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000)))
    })
    
    #prepare dataset for cities of interest 
    anzahl <- reactive({
      cities_amount <- test%>%
        count(Ort)%>%
        filter(n>=input$anzahl)%>%
        left_join(test, by="Ort")
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
        
        #Layer 2 Cities of interest 
        my_map <- addMarkers(map=my_map, data = anzahl (),
                  lng=~Laengengrad, 
                  lat=~Breitengrad,
                  group="Cities of interest",
                  icon = auto_marker
                   )
        #Add map controls for different groups/Layers 
        my_map <- addLayersControl(map=my_map, overlayGroups = c("Clustered Markers","Cities of interest"),options = layersControlOptions(collapsed = FALSE))
        
    })
    

    #Prepare Dataset due to max radius, only keep necessary info 
    get_basic_dataset <- reactive({
      basic_rad <- distanz()%>%
        select(Bundesland, dist)
      basic_rad
    })
    
    
    inputvector <- reactive ({
      input_vector <- as.numeric(c(input$n_1,input$n_2,input$n_3,input$n_4,input$n_5,input$n_6))*1000
      input_vector
    })
    
    #add observations for all selected radius
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
        final_rad_all[is.na(final_rad_all)]<-0
        #https://stackoverflow.com/questions/16363922/convert-a-vector-into-a-list-each-element-in-the-vector-as-an-element-in-the-li
        sum_1 <- list(Bundesland="Summe")
        sum_2 <- as.list(colSums(final_rad_all[-1]))
        sums <- c(sum_1,sum_2)
        #https://stackoverflow.com/questions/28467068/how-can-a-add-a-row-to-a-data-frame-in-r
        final_rad_all[nrow(final_rad_all)+1,]=sums
        #https://stackoverflow.com/questions/11036989/replace-all-0-values-to-na
        final_rad_all[final_rad_all==0]<- NA
      final_rad_all
    })

    output$datatable_rad <- renderDataTable({
      datatable(
        # Disable pagination: https://shiny.rstudio.com/gallery/datatables-options.html
        # Disable sorting: https://stackoverflow.com/questions/37848626/suppressing-sorting-in-datatables-in-shiny/37852445
        options = list(paging = FALSE, ordering = FALSE),
        # Disable numbering of the rows: https://stackoverflow.com/questions/55229736/how-to-remove-the-first-column-index-from-data-table-in-r-shiny
        rownames = FALSE,
        get_dataset_all())%>%
        # How to highlight the row "Summe": https://rstudio.github.io/DT/010-style.html
        formatStyle(
          'Bundesland',
          target = 'row',
          backgroundColor = styleEqual("Summe", 'lawngreen')
        )
    })
    
    #Basic Datatable to prove visualisations
    output$basic_dataset <- renderDataTable({
      datatable(test %>%
                  select(Ort, Bundesland, Laengengrad, Breitengrad, dist))
      
    })
    
    #barplot output
    
    #for advanced barplotting 
    get_plot_dataset_advanced <- reactive({
      final_rad_all_plot_advanced <- get_basic_dataset()
      inputvector_sorted <- as.integer(inputvector())
      names(inputvector_sorted)=c(1:6)
      inputvector_sorted <- sort(inputvector_sorted,decreasing = FALSE)
      inputvector_sorted[inputvector_sorted==0]<-NA
      inputvector_sorted<-inputvector_sorted[!is.na(inputvector_sorted)]
      #Problem wenn zwei oder mehrere Radien komplett gleich sind-> custom error message
      #https://www.sitepoint.com/shiny-and-r-how-to-add-themes-and-customize-error-messages/
      a <- if(TRUE %in% duplicated(inputvector_sorted)){FALSE}else{TRUE}
      validate(
       need(a !=FALSE, 
        "All set Radius must be set unique, please change your input"
         )
      )
      final_rad_all_plot_advanced$Radius <- cut(final_rad_all_plot_advanced$dist, c(0,inputvector_sorted),labels=names(inputvector_sorted))
      final_rad_all_plot_advanced
    })
    
    #factor for every single radius 
    # Radiuss 1 immer ganz links Radius 6 immer ganz rechts --> sorting 
    # akkumlierte ansicht 
    # filled by bundesland 
    # 
    get_plot_dataset_advanced_factor <- reactive({
      inputvector_sorted <- as.integer(inputvector())
      names(inputvector_sorted)=c(1:6)
      inputvector_sorted <- sort(inputvector_sorted,decreasing = FALSE)
      inputvector_sorted[inputvector_sorted==0]<-NA
      inputvector_sorted<-inputvector_sorted[!is.na(inputvector_sorted)]
      final_rad_all_plot_advanced_factor <- get_basic_dataset()
      #Kleinster Radiusbis Größter 
      #1
      #final_rad_all_plot_advanced_factor$Radius1 <- cut(final_rad_all_plot_advanced_factor$dist, c(0,inputvector_sorted[1]),labels=TRUE)
      #Radius 1
      a<-final_rad_all_plot_advanced_factor$dist<=inputvector_sorted[1]
      final_rad_all_plot_advanced_factor$Radius1 <- a
      #
      final_rad_all_plot_advanced_factor <- final_rad_all_plot_advanced_factor%>%
        group_by(Radius1, Bundesland)%>%
        #mutate(count_rad1 = sum(Radius1))
        summarise(count_rad1 = n())%>%
        ungroup()
      final_rad_all_plot_advanced_factor$count_rad1 <- final_rad_all_plot_advanced_factor$count_rad1 * final_rad_all_plot_advanced_factor$Radius1
      # #2
      # if(input$number == 2|3|4|5|6){
      #   b <- as.numeric(final_rad_all_plot_advanced_factor$dist<=inputvector_sorted[2])
      #   final_rad_all_plot_advanced_factor$Radius2 <- b
      # }
      final_rad_all_plot_advanced_factor
    })
    
    observe(print(get_plot_dataset_advanced_factor()))
    
    #prepare dataset for plotting 
    get_plot_dataset <- reactive({
      final_rad_all_plot <- get_dataset_all()%>%
        filter(Bundesland == "Summe")
      name_cols <- colnames(final_rad_all_plot[-1])
      print(name_cols)
      #https://stackoverflow.com/questions/6778908/transpose-a-data-frame
      final_rad_all_plot <- as.data.frame(t(final_rad_all_plot[,-1]))
      colnames(final_rad_all_plot)<-"Anzahl"
      #mutate(final_rad_all_plot, Namen=name_cols)
      final_rad_all_plot$Namen <- name_cols
      final_rad_all_plot
    })
    
    #observe(print(get_plot_dataset()))
    
    output$barplot <- renderPlot({
      df1 <- get_plot_dataset_advanced()
      df2 <- get_plot_dataset_advanced_factor()
      ggplot()+
        geom_bar(data=df1,aes(Radius, fill = Bundesland))
        #geom_histogram(data = df2,aes(Radius1, fill=Bundesland),stat="count")
        #geom_histogram(data = df2,aes(Radius2, fill=Bundesland),stat="count")+ 
        
    })
    
    # output$barplot <- renderPlot({
    #   df <- get_plot_dataset()
    #   ggplot(df)+
    #     geom_col(aes(Namen,Anzahl, fill=Anzahl))
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)