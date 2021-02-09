# SetUp 
library(dataRetrieval)
library(sf)
library(dplyr)
library(ggplot2)
library(ggmap)

# Step 1: Pick point
# 36.0356035,-78.9001728

## Ellerbe HUC8
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
bbox <- sf::st_bbox(b)
bbox
mapview::mapview(b)

# Identify NWIS and WQP sites within the watershed 
### They are the same

NWISsites <- whatNWISsites (bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

WQPsites <- NWISsites <- whatWQPsites (bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))

# Identify Data Available
dataAvailable <- whatNWISdata(bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))
# Parameter Codes
dataAvailable$parm_cd

# WQP
dataAvailableWQP <- whatWQPdata(bBox = c(-78.98523, 36.01312, -78.88892, 36.04652))
# Parameter Codes
dataAvailable$parm_cd

##############################################################################
################################    Test 1    ################################
##############################################################################

#Plot Data 
# Coord System NAD83 EPSG:4269
data_tabular <-
  as.data.frame(dataAvailable) # use this to drop the tibble classes (i.e., tbl_df and tbl)

# look at class
class(data_tabular)

# use names() to get spatial data columns
names(data_tabular)
# LAT_GAGE is y value and LNG_GAGE is x value

# convert to sf object
data_tabular_as_sf <- st_as_sf(data_tabular, coords = c("dec_long_va", "dec_lat_va"), crs = 4269, dim = "XY") # lat long so set CRS = NAD83

# check class and CRS
class(data_tabular_as_sf)
head(data_tabular_as_sf)
st_crs(data_tabular_as_sf)

#Change crs
data_tabular_as_sf_wgs84 <- data_tabular_as_sf %>%
  st_transform(4326)

st_crs(data_tabular_as_sf_wgs84)

#Select data within boundary 
ec_data <- data_tabular_as_sf_wgs84 %>%
  st_intersection(b)

mapview::mapview(b)

mapview::mapview(data_tabular_as_sf_wgs84)

mapview::mapview(ec_data, zcol = "parm_cd", legend = TRUE)


### Read site info ###
siteNumbers <- ec_data$site_no
siteINFO <- readNWISsite(siteNumbers)
siteINFO

### Read site info ###
parameterCd <- ec_data$parm_cd
parameterINFO <- readNWISpCode(parameterCd)








