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
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$h2("Update pickerInput"),
    
    fluidRow(
        column(
            width = 5, offset = 1,
            pickerInput(
                inputId = "p1",
                label = "classic update",
                choices = names(WQP_split)
            )
        )
    )
    
    # fluidRow(
    #     column(
    #         width = 10, offset = 1,
    #         sliderInput(
    #             inputId = "up",
    #             label = "Select between models with mpg greater than :",
    #             width = "50%",
    #             min = min(mtcars$mpg),
    #             max = max(mtcars$mpg),
    #             value = min(mtcars$mpg),
    #             step = 0.1
#             )
#         )
#     )
#     
 )

# Define server logic 
server <- function(input, output, session) {

    x <- -78.9001728
    y <- 36.0356035
    
    
    observeEvent(input$up, {
        H <- read_sf(paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
                            x,",",y,
                            "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson"))
        bbox <- sf::st_bbox(H)
        
        WQPData <- readWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)) %>%
            select(MonitoringLocationIdentifier, ActivityTypeCode, ActivityStartDate, CharacteristicName, ProviderName)
        
        WQP_split <- split(WQPData, f=WQPData$CharacteristicName)
        # names <- names(WQP_split)
        
        # mtcars2 <- mtcars[mtcars$mpg >= input$up, ]
        
        # Method 1
        updatePickerInput(session = session, inputId = "p1",
                          choices = names(WQP_split))
        
        # Method 2
        # disabled_choices <- !rownames(mtcars) %in% rownames(mtcars2)
        # updatePickerInput(
        #     session = session, inputId = "p2",
        #     choices = rownames(mtcars),
        #     choicesOpt = list(
        #         disabled = disabled_choices,
        #         style = ifelse(disabled_choices,
        #                        yes = "color: rgba(119, 119, 119, 0.5);",
        #                        no = "")
        #     )
        # )
    }, ignoreInit = TRUE)
    
}


# Run the application 
shinyApp(ui = ui, server = server)

}
