#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapview)
library(leaflet)
library(sf)

#### Load data 
WQPData <- read.csv("subset.WQPdata_NHD.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Water Quality Portal Mapper"),

    # Sidebar with a slider input for number of bins 
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

        # Show a plot of the generated distribution
        mainPanel(mapviewOutput("mapPlot")
        )))
# Define server logic required to draw a histogram
server <- function(input, output) {
    URL <- paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
                      "-78.9001728",",","36.0356035",
                      "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")
    H <- read_sf(URL)
    mapPlot <- mapview(H)
    output$mapPlot <- renderMapview(mapPlot)
    mapviewOutput(mapPlot, width = "100%", height = 400)
}

# Run the application 
shinyApp(ui = ui, server = server)
