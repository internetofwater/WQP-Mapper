# Data Retrieval Script 
# Rachel Landman 
# Spring 2021

# Load Packages (you might have to install the packages if you don't have them)

library(dataRetrieval)
library(sf)
library(mapview)

getwd()

### GET WATERSHED BOUNDARY FROM NLDI #####
# Step 1: Pick point
# 36.0356035,-78.9001728

## Ellerbe Creek HUC8
# 03020201

# Step 2: Pass to NLDI
#https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?f=json&coords=POINT(-78.9%2036.0356)

x <- -78.9001728
y <- 36.0356035
# gets HC12 basin from NLDI point

point_nldi_basin_get <- function(lat = y, lon = x){
  query <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?f=json&coords=POINT(",
                  x," ",y,")")
  comid <- jsonlite::fromJSON(URLencode(query))
  query <- gsub("navigation","basin?f=json",comid$features$properties$navigation)
  basin <- sf::read_sf(query)
  return(basin)
}
b <- point_nldi_basin_get(lat = y, lon = x)

# Step 3: Convert watershed to a bbox to feed into WQP 
bbox <- sf::st_bbox(b)
bbox

# Step 4: Identify WQP sites within the watershed 
WQPsites <- whatWQPsites (bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

write.csv(WQPsites, file = "WQPsites.csv")

# Step 5: whatWQPdata - This returns a data frame with all of the sites that were measured. 
# Also, in that table, there is a measure of activityCount (how often the site was sampled)
# resultCount (how many individual results are available).
dataAvailableWQP <- whatWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

write.csv(dataAvailableWQP, file = "WQPdataavailable.csv")

# Step 6: readWQPdata - all the data available from the WQP within the bounding box
# data from the WQP using generalized Web service calls
WQPData <- readWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

# Step 7: Subset and Merge data tables 
library(dplyr)
joined.siteData <- full_join(WQPsites, dataAvailableWQP, by = NULL, copy = FALSE, suffix = c("MonitoringLocationIdentifier", "MonitoringLocationIdentifier"))
subset.siteData <- joined.siteData[c(1:5, 7, 12:13, 36, 40:45)]

subset.WQPData <- WQPData[c(1:4, 7, 13:14, 20, 22, 27:35, 37, 39)]

merged.WQPData <- merge(subset.WQPData, subset.siteData, by= 
                      c("MonitoringLocationIdentifier","OrganizationFormalName", "OrganizationIdentifier" ))

joinedData <- left_join(subset.siteData, subset.WQPData, by = 
            "MonitoringLocationIdentifier")

# List of characteristics and organizations 
Characteristics <- unique(merged.WQPData[c("CharacteristicName")])
Organizations <- unique(merged.WQPData[c("OrganizationFormalName")])

##### TROUBLESHOOTING ####
colnames(merged.WQPData)
unique(subset.siteData$LatitudeMeasure)
unique(subset.siteData$MonitoringLocationIdentifier)

unique(merged.WQPData$LatitudeMeasure)
unique(merged.WQPData$MonitoringLocationIdentifier)

unique(WQPData$MonitoringLocationIdentifier)
#####

# what samples 
# The function whatWQPsamples returns information on the individual samples collected at a site
WQPSamples <- whatWQPsamples(bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

#Get URL for dataset
attr(WQPData, "url")
siteInfo <- attr(WQPData, "siteInfo")

# Step 8: Plot Data with site dataset

# Coord System for WQP data is NAD83 EPSG:4269
data_tabular <-
  as.data.frame(joined.siteData) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular)

# use names() to get spatial data columns
names(data_tabular)
# y is LatitudeMeasure value and LongitudeMeasure is x value

# convert to sf object
data_tabular_as_sf <- st_as_sf(data_tabular, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269, dim = "XY") # lat long so set CRS = NAD83

# check class and CRS
class(data_tabular_as_sf)
st_crs(data_tabular_as_sf)

#Change crs
data_tabular_as_sf_wgs84 <- data_tabular_as_sf %>%
  st_transform(4326)

st_crs(data_tabular_as_sf_wgs84)

#Select data within boundary 
#### NEED TO BUFFER CATCHMENT 
ec_data <- data_tabular_as_sf_wgs84 %>%
  st_intersection(b)

# Plot with Mapview 
# Sites within the bounding box

mapview::mapview(data_tabular_as_sf_wgs84,zcol = "activityCount")
mapview::mapview(b)

# Sites within the watershed 

mapview::mapview(list("Sites"=ec_data, "Watershed"=b), 
                 col.regions=c("red", "blue"), 
                 homebutton = mapviewGetOption("homebutton"), cex=5)


# Step 8: Plot Data with merged dataset

# Coord System for WQP data is NAD83 EPSG:4269
data_tabular2 <-
  as.data.frame(merged.WQPData) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular)

# use names() to get spatial data columns
names(data_tabular)
# y is LatitudeMeasure value and LongitudeMeasure is x value

# convert to sf object
data_tabular_as_sf2 <- st_as_sf(data_tabular, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269, dim = "XY") # lat long so set CRS = NAD83

# check class and CRS
class(data_tabular_as_sf)
st_crs(data_tabular_as_sf)

#Change crs
data_tabular_as_sf_wgs842 <- data_tabular_as_sf %>%
  st_transform(4326)

st_crs(data_tabular_as_sf_wgs84)

#Select data within boundary 
#### NEED TO BUFFER CATCHMENT 
ec_data2 <- data_tabular_as_sf_wgs84 %>%
  st_intersection(b)

# Plot with Mapview 
# Sites within the bounding box
mapview::mapview(data_tabular_as_sf_wgs842,zcol = "activityCount")

# Sites within the watershed 
mapview::mapview(list("Sites"=ec_data2, "Watershed"=b), 
                 col.regions=c("red", "blue"), 
                 homebutton = mapviewGetOption("homebutton"), cex=5)
       