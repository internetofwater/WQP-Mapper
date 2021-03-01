# Data Retrieval Script 
# Rachel Landman 
# Spring 2021

# Load Packages (you might have to install the packages if you don't have them)
library(sf)
library(mapview)
library(dplyr)
library(ggplot2)
library(here)
library(tidyr)
library(mapview)

# Add NC 12 HUC watersheds
nc_12HUC <- st_read(here("Data","12Digit_HUC_Subwatersheds", "12Digit_HUC_Subwatersheds.shp"))

ellerbe_gages_raw <- read.csv(here("Data", "Ellerbe_12DigitHUC_Stream_Stations.csv")) %>% as.data.frame()

# convert to sf object
ellerbe_gages_as_sf <- st_as_sf(ellerbe_gages_raw, 
                                coords = c("LongitudeMeasure",
                                           "LatitudeMeasure"), 
                                crs = 4326, dim = "XY") 

# check CRS
st_crs(b)
st_crs(ellerbe_gages_as_sf)
st_crs(nc_12HUC)

# select ellerbe HUC
ellerbe_watershed8 <- nc_12HUC %>%
  filter(HUC_8 == "03020201") %>%
  st_geometry()

ellerbe_watershed12 <- nc_12HUC %>%
  filter(HUC_12 == "030202010403")%>%
  st_geometry()

class(ellerbe_watershed12)
class(ellerbe_watershed8)

mapview(list("WatershedNHD"=ellerbe_watershed12, "WatershedNLDI"= b),
        col.regions=c("red", "blue"))
        