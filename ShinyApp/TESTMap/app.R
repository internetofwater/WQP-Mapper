#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (interactive()) {
  
  library(shiny)
  library(leaflet)
  library(sf)
  library(dataRetrieval)
  library(dplyr)
  library(tidyr)
  library(shinyWidgets)
  library(leafem)
  
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
    # Application title
    titlePanel("TEST Map"),
    
    fluidRow(
      column(
        width = 5, offset = 1,
        pickerInput(
          inputId = "p1",
          label = "Choose Parameter(s) of Interest",
          choices = c("First, select a watershed by clicking on the map"),
          multiple=TRUE,
          options=pickerOptions(actionsBox=TRUE,
                       liveSearch=TRUE,
                       showTick=TRUE,
                       virtualScroll=TRUE)#names(WQP_split)
        )
      ),
      fluidRow(
        column(
          width = 10, offset = 1,
          sliderInput(
            inputId = "up",
            label = "Activity Start Date",
            width = "50%",
            min = 1980,
            max = 2020,
            value = 1980,
            step = 0.1
          )
        )
      ),
      leafletOutput("mymap"),
    ))
  
  # Define server logic 
  server <- function(input, output, session) {
    
    # points <- eventReactive(input$recalc, {
    #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    # }, ignoreNULL = FALSE)
    
    latInput <- reactive({
      switch(input$lat, 
             '36.0356035' = 36.0356035,
             '40.6972313874088' = 40.6972313874088)
    })
    longInput <- reactive({
      switch(input$long,
             '-78.9001728' = -78.9001728,
             '-73.99565830140675' = -73.99565830140675) 
    })
    
    # Assign Variables 
    #lat <- input$latInput #36.0356035 # 40.6972313874088 
    #long <- input$longInput #-78.9001728  # -73.99565830140675
    
    # Get HUC12 from ESRI API
    # H <- reactive({ read_sf(paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
    # click()$lng,",",click()$lat,
    # "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")) })
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        setView(lng=-79.8373764,lat=35.5465094,zoom=7)
      # addTiles() %>% 
      
    })
    
    observeEvent(input$mymap_click, {
      click <- input$mymap_click
      H <- read_sf(paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
                          click$lng,",",click$lat,
                          "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson"))
      bbox <- sf::st_bbox(H)
      H <- st_transform(H, 4326)
      WQPSites <- whatWQPsites (bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)) %>% 
       dplyr::select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationTypeName, HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName)
      Sites <- WQPSites %>% 
        as.data.frame() %>%
        st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269, dim = "XY") %>%
        st_transform(4326) %>%
        st_filter(H)
      
      WQPData <- readWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)) %>%
        dplyr::select(MonitoringLocationIdentifier, ActivityTypeCode, ActivityStartDate, CharacteristicName, ProviderName)
      
      # WQPDataFiltered <- WQPData %>%
      #     filter(CharacteristicName == "Phosphorus" | CharacteristicName == "Nitrate" |  CharacteristicName == "Organic Nitrogen")
      
      WQP_split <- split(WQPData, f=WQPData$CharacteristicName)
      WQP_split2 <- split(WQPData, f=WQPData$CharacteristicName)
      #  names <- names(WQP_split)
      
      ### Update colum input
      # Method 1
      updatePickerInput(session = session, inputId = "p1",
                        choices = names(WQP_split2))
      
      # Method 2
    #  disabled_choices <- !rownames(WQP_split) %in% rownames(WQP_split2)
     # updatePickerInput(
    #    session = session, inputId = "p2",
     #   choices = rownames(WQP_split),
     #     disabled = disabled_choices,
     #     style = ifelse(disabled_choices,
       #                  yes = "color: rgba(119, 119, 119, 0.5);",
     #                    no = "")
     #   )
    #  )
      
     # proxy <- leafletProxy("mymap")
      leafletProxy("mymap") %>% 
        clearMarkers() %>%
        clearShapes() %>%
        removeHomeButton() %>%
        addPolygons(data = H, popup = H$NAME) %>% addHomeButton(ext = extent(Sites), group= "Selected HUC12") %>%
        addMarkers(data = Sites, popup = Sites$MonitoringLocationIdentifier)  
    }, ignoreInit = TRUE)
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}

