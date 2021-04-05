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

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("TEST Map"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "lat",
                        label = "latitude",
                        choices = c("36.0356035", "40.6972313874088"),
                        selected = "36.0356035"),
            selectInput(inputId = "long",
                        label = "longitude",
                        choices = c("-78.9001728",  "-73.99565830140675"),
                        selected = "-78.9001728"),
            selectInput(inputId = "CharacteristicName",
                        label = "Characteristic",
                        choices = c("Sulfate",  "Alkalinity"),
                        selected = "Sulfate")
        ),
        
        leafletOutput("mymap")
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
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
            addProviderTiles(providers$Esri.WorldTopoMap)
        # addTiles() %>% 
        
    })
    
    observeEvent(input$mymap_click, {
        click <- input$mymap_click
        H <- read_sf(paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
                            click$lng,",",click$lat,
                            "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson"))
        
        proxy <- leafletProxy("mymap")
        proxy %>% 
            addPolygons(data = H, popup = "Watershed")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
