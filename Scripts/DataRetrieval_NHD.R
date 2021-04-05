# Data Retrieval Script 
# Rachel Landman 
# Spring 2021

# Load Packages (you might have to install the packages if you don't have them)
library(dataRetrieval)
library(sf)
library(mapview)
library(dplyr)
library(here)
library(tidyr)

getwd()

### Use NHD Watershed Boundaries

EC_12HUC <- st_read(here("Data", "ellerbe_watershed12_sf", "ellerbe_watershed12_sf.shp"))

# Step 3: Convert watershed to a bbox to feed into WQP 
bbox <- sf::st_bbox(EC_12HUC)
bbox

#[-78.84116917762904, 36.0070796158602] , [-78.84129411082718, 36.00679405420703] , [-78.84152610703751, 36.006026679214386]
WQPsites <- whatWQPsites (bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))


# Step 4: Identify WQP sites within the watershed 
WQPsites <- whatWQPsites (bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

# write.csv(WQPsites, file = "Outputs/WQPsites_NHD.csv")

# Step 5: whatWQPdata - This returns a data frame with all of the sites that were measured. 
# Also, in that table, there is a measure of activityCount (how often the site was sampled)
# resultCount (how many individual results are available).
dataAvailableWQP <- whatWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

# write.csv(dataAvailableWQP, file = "Outputs/WQPdataavailable_NHD.csv")

# Step 6: readWQPdata - all the data available from the WQP within the bounding box
# data from the WQP using generalized Web service calls
WQPData <- readWQPdata(bBox = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax))

# write.csv(WQPData, file = "Outputs/WQPData_NHD.csv")

# Step 7: Subset data tables 
subset.WQPsites <- WQPsites %>% select(OrganizationIdentifier, MonitoringLocationIdentifier, MonitoringLocationTypeName, HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, ProviderName)
# write.csv(subset.WQPsites, file = "Outputs/subset.WQPsites_NHD.csv")

subset.WQPdataAvailable <- dataAvailableWQP %>% select(MonitoringLocationIdentifier, activityCount, resultCount)
# write.csv(subset.WQPdataAvailable, file = "Outputs/subset.WQPdataAvailable_NHD.csv")

subset.WQPdata <- WQPData %>% select(MonitoringLocationIdentifier, ActivityTypeCode, ActivityStartDate, CharacteristicName, ProviderName)
# write.csv(subset.WQPdata, file = "Outputs/subset.WQPdata_NHD.csv")

# Step 8: Join Data and summarize

# Total monitoring sites within bounding box
joined.siteData <- full_join(subset.WQPsites, subset.WQPdataAvailable, by = NULL, copy = FALSE, suffix = c("MonitoringLocationIdentifier", "MonitoringLocationIdentifier"))

#Total data available withing bounding box 
WQPData.ALL <- left_join(subset.WQPdata, joined.siteData, by = NULL, copy = FALSE, suffix = c("MonitoringLocationIdentifier", "MonitoringLocationIdentifier"))

# Pivot wider 
### DataALL.wide <- pivot_wider(WQPData.ALL, id_cols = NULL, names_from = Characteristics, values_from = count("CharacteristicName"))


WQPdata_summary <- WQPData.ALL %>% group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  mutate(minDate = min(ActivityStartDate), maxdate = max(ActivityStartDate)) %>% 
  ungroup() %>% group_by(MonitoringLocationIdentifier, CharacteristicName, ActivityStartDate) %>% add_tally() 


# List of characteristics and organizations 
Characteristics <- unique(WPQData.ALL[c("CharacteristicName")])
MonitoringLocationALL <- unique(joined.siteData[c("MonitoringLocationIdentifier")])
MonitoringLocationwithData <- unique(WPQData.ALL[c("MonitoringLocationIdentifier")])


# Step 9: Plot Data 

# Coord System for WQP data is NAD83 EPSG:4269
data_tabular <-
  as.data.frame(WQPdata_summary) # use this to drop the tibble classes (i.e., tbl_df and tbl)

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

data_tabular_as_sf_albers <- data_tabular_as_sf %>%
  st_transform(102008)

EC_12HUC_albers <- EC_12HUC %>%
  st_transform(102008)


st_crs(data_tabular_as_sf_albers)

st_crs(EC_12HUC_albers)

#Select data within boundary 
#### NEED TO BUFFER CATCHMENT 
ec_data <- data_tabular_as_sf_albers %>%
  st_intersection(EC_12HUC_albers)

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
data_tabular2 <-
  as.data.frame(joined.siteData) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular2)

# use names() to get spatial data columns
names(data_tabular2)
# lat is y value and lon is x value

# convert to sf object
data_tabular_as_sf2 <- st_as_sf(data_tabular2, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269, dim = "XY") # lat long so set CRS = NAD83

# check class and CRS
class(data_tabular_as_sf2)
st_crs(data_tabular_as_sf2)

#Change crs
data_tabular_as_sf_albers2 <- data_tabular_as_sf2 %>%
  st_transform(102008)

st_crs(data_tabular_as_sf_albers2)

#Select data within boundary 
#### NEED TO BUFFER CATCHMENT 
ec_data_sites <- data_tabular_as_sf_albers2 %>%
  st_intersection(EC_12HUC_albers)

# Plot with Mapview 
# Sites within the bounding box
mapview::mapview(data_tabular_as_sf_wgs84, zcol = "activityCount")

# Sites within the watershed 

mapview(list("Sites"=ec_data_sites, "Watershed"=EC_12HUC_albers), 
        col.regions=c("red", "blue"), 
        homebutton = mapviewGetOption("homebutton"), cex=5)

# Join data 
WQPData.ALL2 <- left_join(ec_data_sites, subset.WQPdata, by = NULL, copy = FALSE, suffix = c("MonitoringLocationIdentifier", "MonitoringLocationIdentifier"))

# Pivot wider 
### DataALL.wide <- pivot_wider(WQPData.ALL, id_cols = NULL, names_from = Characteristics, values_from = count("CharacteristicName"))

WQPdata_summary2 <- WQPData.ALL2 %>% group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  mutate(minDate = min(ActivityStartDate), maxdate = max(ActivityStartDate)) %>% 
  ungroup() %>% group_by(MonitoringLocationIdentifier, CharacteristicName, ActivityStartDate) %>% add_tally() 


class(WQPdata_summary2)
st_crs(WQPdata_summary2)

mapview(WQPdata_summary2)


# Make each characteristic its own dataframe (List of dataframes)
Characteristics <- split( WQPdata_summary2 , f = WQPdata_summary2$CharacteristicName )

# call specific dataframes from list 
varname <- "Barium"
mapview(Characteristics[varname])
