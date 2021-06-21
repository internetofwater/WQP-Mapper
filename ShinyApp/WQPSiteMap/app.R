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
    library(raster)
    
    r_colors <- rgb(t(col2rgb(colors()) / 255))
    names(r_colors) <- colors()
    
    # Define UI for application that draws a histogram
    ui <- fluidPage(
        
        # Application title
        titlePanel("Water Quality Portal Monitoring Site Map"),
        
        mainPanel("Select a location on the map to identify the watershed boundary and retrieve
                  monitoring site locations from the Water Quality Portal. Zoom in and out and pan across to select 
                  any watershed in the United States."),
        
            leafletOutput("mymap"),
    
        
        fluidRow(
            column(12,
                   dataTableOutput('table'))),
        )
    
    # Define server logic 
    server <- function(input, output, session) {
        
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
            
            ### Bounding Circle 
            circle <- lwgeom::st_minimum_bounding_circle(H)
            
            ### Transform crs 
            area <- st_area(circle)
            area <- units::set_units(area, mi^2)
            radius <- as.numeric(sqrt(area/pi))
            
            WQPSites <- whatWQPsites(lat = click$lat, long = click$lng, within = radius) %>% 
                dplyr::select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationTypeName, HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName)
            
            Sites <- WQPSites %>% 
                as.data.frame() %>%
                st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269, dim = "XY") %>%
                st_transform(4326) %>%
                st_filter(H)
            
            WatershedSites <- filter(WQPSites, MonitoringLocationIdentifier %in% Sites$MonitoringLocationIdentifier)
            
            # proxy <- leafletProxy("mymap")
            leafletProxy("mymap") %>% 
                clearMarkers() %>%
                clearShapes() %>%
                removeHomeButton() %>%
                addPolygons(data = H, popup = H$NAME) %>% addHomeButton(ext = extent(Sites), group= "Selected HUC12") %>%
                addMarkers(data = Sites, popup = Sites$MonitoringLocationIdentifier)  
            
            output$table <- renderDataTable(WatershedSites)
            
        }, ignoreInit = TRUE)
        
            
        
        # observeEvent(input$p1, {
        #     WQPDataFiltered <- WQPSites %>% 
        #         filter(CharacteristicName %in% input$p1)
        #     
        #     output$table <- renderDataTable(WQPDataFiltered)}, ignoreInit = TRUE)
        
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
}