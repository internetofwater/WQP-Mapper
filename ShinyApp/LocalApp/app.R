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

# Load Data 
# WQPData <- read.csv("CopyOfWQPData_NHD.csv") %>%
#     dplyr::select(MonitoringLocationIdentifier, ActivityTypeCode, ActivityStartDate, CharacteristicName, ProviderName)
# 
# WQPSites <- read.csv("ShinyApp/LocalApp/CopyOfWQPsites_NHD.csv") %>% 
#     dplyr::select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationTypeName, HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName)
# 
# EC_12HUC <- st_read(here("Data", "ellerbe_watershed12_sf", "ellerbe_watershed12_sf.shp"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Local App"),

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
            #   column(
            #     width = 10, offset = 1,
            #     sliderInput(
            #       inputId = "up",
            #       label = "Activity Start Date",
            #       width = "50%",
            #       min = 1980,
            #       max = 2020,
            #       value = 1980,
            #       step = 0.1
            #     )
            #   ),
            column(
                width = 10, offset = 1,
                dateRangeInput("dates", h3("Activity Start Date")))
        ),
        leafletOutput("mymap"),
    ))


# Define server logic required to draw a histogram
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
    
    output$mymap <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
         addPolygons(data = H, popup = H$NAME)%>%
         addMarkers(data = Sites, popup = Sites$MonitoringLocationIdentifier) 
         })

    WQP_split <- split(WQPData, f=WQPData$CharacteristicName)
    WQP_split2 <- split(WQPData, f=WQPData$CharacteristicName)
    
    ### Update colum input
    # Method 1
    updatePickerInput(session = session, inputId = "p1",
                      choices = names(WQP_split2))
    
    saveData <- function(data) {
        data <- t(data)
        # Create a unique file name 
        fileName <- sprintf("%s.csv", H$NAME)
        # Write the file to the local system
        write.csv(
            x = data,
            file = file.path(fileName), 
            row.names = FALSE, quote = TRUE
        )
    }
    
    observeEvent(input$p1, {
        WQPDataFiltered <- WQPData %>%
            #filter(CharacteristicName == ".beta.-Hexachlorocyclohexane")
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
    
}
    

# Run the application 
shinyApp(ui = ui, server = server)