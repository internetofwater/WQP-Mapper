#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dataRetrieval)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(leafem)
library(raster)
library(here)

# Define UI for application that draws a histogram
if (interactive()) {
  ui <- fluidPage(

    # Application title
    titlePanel("Ellerbe Creek Water Quality Portal Data"),

    fluidRow(
        column(
            width = 10, offset = 1,
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
        
        ### Allow user to select date range 
        # fluidRow(
        #     column(
        #         width = 10, offset = 1,
        #         dateRangeInput("dates", h3("Activity Start Date"),
        #                        label = "Select Date Range",
        #                        start = "1960-01-01",
        #                        end = Sys.Date()))
        # ),
        
        leafletOutput("mymap"),
        
        fluidRow(
          column(12,
                 dataTableOutput('table')
          )
        ),
    ))


# Define server logic
server <- function(input, output, session) {
    
    WQPData <- read.csv("CopyOfWQPData_NHD.csv") %>%
        dplyr::select(MonitoringLocationIdentifier, ActivityTypeCode, ActivityStartDate, CharacteristicName, ProviderName) 
    
    WQPSites <- read.csv("CopyOfWQPsites_NHD.csv") %>% 
        dplyr::select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationTypeName, HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName)
    
    EC_12HUC <- st_read(here("Data", "ellerbe_watershed12_sf", "ellerbe_watershed12_sf.shp"))
    
    H <- st_transform(EC_12HUC, 4326)
    
    Sites <- WQPSites %>% 
        as.data.frame() %>%
        st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269, dim = "XY") %>%
        st_transform(4326) %>%
        st_filter(H)
    
    ECWQPData <- filter(WQPData, MonitoringLocationIdentifier %in% Sites$MonitoringLocationIdentifier)
    ECSites <- filter(WQPSites, MonitoringLocationIdentifier %in% Sites$MonitoringLocationIdentifier)
    
    output$mymap <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
         addPolygons(data = H, popup = H$NAME)%>%
         addMarkers(data = Sites, popup = Sites$MonitoringLocationIdentifier) 
         })

    WQP_split <- split(ECWQPData, f=WQPData$CharacteristicName)
    
    ### Update column input
    
    updatePickerInput(session = session, inputId = "p1",
                      choices = names(WQP_split))
    
    # updateDateRangeInput(session = session, "inDateRange",
    #                      label = paste("Date range label", WQPData$ActivityStartDate),
    #                      start = paste("2013-01-", WQPData$ActivityStartDate, sep=""),
    #                      end = paste("2013-12-", WQPData$ActivityStartDate, sep=""))
    
    observeEvent(input$p1, {
        WQPDataFiltered <- ECWQPData %>%
            filter(CharacteristicName %in% input$p1)

        FilteredSites <- Sites %>%
            filter(MonitoringLocationIdentifier %in% WQPDataFiltered$MonitoringLocationIdentifier)

        leafletProxy("mymap") %>%
            clearMarkers() %>%
            clearShapes() %>%
            removeHomeButton() %>%
            addPolygons(data = H, popup = H$NAME) %>% addHomeButton(ext = extent(Sites), group= "Selected HUC12") %>%
            addMarkers(data = FilteredSites, popup = FilteredSites$MonitoringLocationIdentifier)
    }, ignoreInit = TRUE)
    
    output$table <- renderDataTable(ECSites)
    
    observeEvent(input$p1, {
      WQPDataFiltered <- ECWQPData %>% 
        filter(CharacteristicName %in% input$p1)
      
      output$table <- renderDataTable(WQPDataFiltered)}, ignoreInit = TRUE)
    
    #### Attempts at allowing user to filter by date 
    
    #     WQPDataFiltered <- reactive({
    #         m <- filter(WQPData,
    #                     CharacteristicName %in% input$p1 &
    #                         Date > as.Date(input$dates[1]) & 
    #                         Date < as.Date(input$dates[2]))
    #     })
    #     
    #     FilteredSites <- Sites %>% 
    #         filter(MonitoringLocationIdentifier %in% WQPDataFiltered$MonitoringLocationIdentifier)
    #     
    #     output$map <- renderLeaflet({ leafletProxy("mymap") %>%  
    #         clearMarkers() %>%
    #         clearShapes() %>%
    #         removeHomeButton() %>%
    #         addPolygons(data = H, popup = H$NAME) %>% addHomeButton(ext = extent(Sites), group= "Selected HUC12") %>%
    #                 addMarkers(data = FilteredSites, popup = FilteredSites$MonitoringLocationIdentifier)
    #         }, ignoreInit = TRUE)
    # 
    # leafletOutput('map')
    
    
    # observeEvent(input$p1, {
    #   WQPDataFiltered <- WQPData %>%
    #   #filter(CharacteristicName %in% input$p1)
    #   #filter(DateCharacter, between(date, input$date[1], input$date[2]))
    #   #filter(Date > as.Date(input$dates[1]) & Date < as.Date(input$dates[2]))
    # 
    #   output$table <- renderDataTable(WQPDataFiltered)}, ignoreInit = TRUE)

}
    

# Run the application 
shinyApp(ui = ui, server = server)

}
