# NHD plus Script 
# Rachel Landman 
# Spring 2021


library(nhdplusTools)
library(sf)
library(dplyr)
library(rvest)
library(httr)
library(jsonlite)

#Get Feature
point <- get_nldi_feature(list("featureSource" = "nwissite", featureID = "USGS-020850391920"))

mapview::mapview(point)

#Get Basin

nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-020850391920")
discover_nldi_characteristics()
basin <- get_nldi_basin(nldi_feature = nldi_nwis)

basin %>%
  st_geometry() %>%
  plot()
basin

# Get Data Using API 
theURL <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?"
res = GET(theURL)
res
rawToChar(res$content)
data =  xml2::read_html(rawToChar(res$content))
names(data)
data$doc

# Attempt 2 
lat <- 36.0356035
long <- -78.9001728
URL2 <- "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?"
res2 <- GET("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=HUC12 = '030300020603'"))
basin <- sf::read_sf(res2)

# Attempt using NLDI format 
point_nhd_basin_get <- function(lat = lat, lon = lon){
  query <- paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?",
                  lon," ",lat,")")
  comid <- jsonlite::fromJSON(URLencode(query))
  query <- gsub("navigation","basin?f=json",comid$features$properties$navigation)
  basin2 <- sf::read_sf(query)
  return(basin2)
}

b <- point_nhd_basin_get(lat = lat, lon = lon)


mapview::mapview(basin)


res2 <- ("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?geometryType=esriGeometryPoint&geometry=-78.9,36.03")

URL <- paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
              long,",",lat,
              "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")
H <- read_sf(URL)
mapview::mapview(H)

bbox <- sf::st_bbox(H)
bbox

WQPsites <- whatWQPsites(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

