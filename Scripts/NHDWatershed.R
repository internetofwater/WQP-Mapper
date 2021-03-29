# NHD Esri API 
# Rachel Landman 
# Spring 2021

# Load packages
library(sf)
library(dataRetrieval)

# Assign Variables 
lat <- 36.0356035 # 40.6972313874088 
long <- -78.9001728  # -73.99565830140675

# Get HUC12 from ESRI API
URL <- paste0("https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/11/query?where=&text=&objectIds=&time=&geometry=",
              long,",",lat,
              "&geometryType=esriGeometryPoint&inSR=4326&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")
H <- read_sf(URL)
mapview::mapview(H)

bbox <- st_bbox(H)
bbox

# Test to see if it works with data retreival
WQPsites <- whatWQPsites(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))
