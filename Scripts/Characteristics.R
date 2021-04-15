# Characteristic Prep for Shiny
# Rachel Landman 
# Spring 2021

library(dataRetrieval)
library(sf)
library(mapview)
library(dplyr)
library(here)
library(tidyr)

getwd()

x <- -78.9001728
y <- 36.0356035

H <- read_sf(paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
                    x,",",y,
                    "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson"))
bbox <- sf::st_bbox(H)


WQPData <- readWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)) %>%
  select(MonitoringLocationIdentifier, ActivityTypeCode, ActivityStartDate, CharacteristicName, ProviderName)

WQP_split <- split(WQPData, f=WQPData$CharacteristicName)
names <- names(WQP_split)

WQPData_filtered <- WQPData %>% 
  filter(CharacteristicName == "Phosphorus" | CharacteristicName == "Nitrate" |  CharacteristicName == "Organic Nitrogen")

WQPDataFiltered <- WQPData %>%
  filter(CharacteristicName == "Phosphorus" | CharacteristicName == "Nitrate" |  CharacteristicName == "Organic Nitrogen")

