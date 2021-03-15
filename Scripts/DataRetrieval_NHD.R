# Data Retrieval Script 
# Rachel Landman 
# Spring 2021

# Load Packages (you might have to install the packages if you don't have them)
library(dataRetrieval)
library(sf)
library(mapview)
library(dplyr)
library(here)

getwd()

### Use NHD Watershed Boundaries

EC_12HUC <- st_read(here("Data", "ellerbe_watershed12_sf", "ellerbe_watershed12_sf.shp"))

# Step 3: Convert watershed to a bbox to feed into WQP 
bbox <- sf::st_bbox(EC_12HUC)
bbox

#[-78.84116917762904, 36.0070796158602] , [-78.84129411082718, 36.00679405420703] , [-78.84152610703751, 36.006026679214386]
WQPsites <- whatWQPsites (bBox = c(-78.84116917762904, 36.00679405420703, 
                                   -78.84152610703751, 36.0070796158602))


# Step 4: Identify WQP sites within the watershed 
WQPsites <- whatWQPsites (bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

write.csv(WQPsites, file = "WQPsites_NHD.csv")

# Step 5: whatWQPdata - This returns a data frame with all of the sites that were measured. 
# Also, in that table, there is a measure of activityCount (how often the site was sampled)
# resultCount (how many individual results are available).
dataAvailableWQP <- whatWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

write.csv(dataAvailableWQP, file = "WQPdataavailable_NHD.csv")

# Step 6: readWQPdata - all the data available from the WQP within the bounding box
# data from the WQP using generalized Web service calls
WQPData <- readWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

#write.csv(WQPData, file = "WQPData_NHD.csv")

# Step 7: Subset and Merge data tables 
joined.siteData <- full_join(WQPsites, dataAvailableWQP, by = NULL, copy = FALSE, suffix = c("MonitoringLocationIdentifier", "MonitoringLocationIdentifier"))
subset.siteData <- joined.siteData %>% select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName, activityCount, resultCount, StateName, CountyName)

subset.WQPData <- WQPData %>% select(OrganizationIdentifier, ActivityTypeCode, ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ProviderName)

merged.WQPData <- merge(subset.WQPData, subset.siteData, by= 
                          c("MonitoringLocationIdentifier", "OrganizationIdentifier" ))

joinedData <- left_join(subset.siteData, subset.WQPData, by = 
                          "MonitoringLocationIdentifier")

# List of characteristics and organizations 
Characteristics <- unique(joinedData[c("CharacteristicName")])
MonitoringLocation <- unique(joinedData[c("MonitoringLocationIdentifier")])

Characteristics <- unique(merged.WQPData[c("CharacteristicName")])
MonitoringLocations <- unique(merged.WQPData[c("MonitoringLocationIdentifier")])


# what samples 
# The function whatWQPsamples returns information on the individual samples collected at a site
WQPSamples <- whatWQPsamples(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

#Get URL for dataset

attr(joinedData, "url")
siteInfo <- attr(joinedData, "siteInfo")

# Step 8: Plot Data 

# Coord System for WQP data is NAD83 EPSG:4269
data_tabular <-
  as.data.frame(joinedData) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular)

# use names() to get spatial data columns
names(data_tabular)
# lat is y value and lon is x value

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
  st_intersection(EC_12HUC)

# Plot with Mapview 
# Sites within the bounding box
mapview::mapview(data_tabular_as_sf_wgs84, zcol = "activityCount")

# Sites within the watershed 

mapview(list("Sites"=data_tabular_as_sf_wgs84, "Watershed"=EC_12HUC), 
                 col.regions=c("red", "blue"), 
                 homebutton = mapviewGetOption("homebutton"), cex=5)




#### SITES ONLY ###
# Step 8: Plot Data 

# Coord System for WQP data is NAD83 EPSG:4269
data_tabular <-
  as.data.frame(subset.siteData) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular)

# use names() to get spatial data columns
names(data_tabular)
# lat is y value and lon is x value

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
  st_intersection(EC_12HUC)

# Plot with Mapview 
# Sites within the bounding box
mapview::mapview(data_tabular_as_sf_wgs84, zcol = "activityCount")

# Sites within the watershed 

mapview(list("Sites"=ec_data, "Watershed"=EC_12HUC), 
        col.regions=c("red", "blue"), 
        homebutton = mapviewGetOption("homebutton"), cex=5)



