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
WQPsites <- whatWQPsites (bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

write.csv(WQPsites, file = "WQPsites.csv")

# Step 5: whatWQPdata - This returns a data frame with all of the sites that were measured. 
# Also, in that table, there is a measure of activityCount (how often the site was sampled)
# resultCount (how many individual results are available).
dataAvailableWQP <- whatWQPdata(bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

write.csv(dataAvailableWQP, file = "WQPdataavailable.csv")

# Step 6: readWQPdata - all the data available from the WQP within the bounding box
# data from the WQP using generalized Web service calls
WQPData <- readWQPdata(bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

# List of characteristics and organizations 
Characteristics <- unique(WQPData[c("CharacteristicName")])
Organizations <- unique(WQPData[c("OrganizationFormalName")])


# what samples 
# The function whatWQPsamples returns information on the individual samples collected at a site
WQPSamples <- whatWQPsamples(bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

#Get URL for dataset
attr(WQPData, "url")
siteInfo <- attr(WQPData, "siteInfo")

# Step 7: Plot Data 

# Coord System for WQP data is NAD83 EPSG:4269
data_tabular <-
  as.data.frame(dataAvailableWQP) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular)

# use names() to get spatial data columns
names(data_tabular)
# lat is y value and lon is x value

# convert to sf object
data_tabular_as_sf <- st_as_sf(data_tabular, coords = c("lon", "lat"), crs = 4269, dim = "XY") # lat long so set CRS = NAD83

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
mapview::mapview(data_tabular_as_sf_wgs84, zcol = "activityCount")

# Sites within the watershed 
mapview::mapview(list("Watershed"=b, "Sites"=ec_data), 
                 col.regions=c("blue", "red"), 
                 homebutton = mapviewGetOption("homebutton"), cex=5)

