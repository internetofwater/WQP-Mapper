# NHD plus Script 
# Rachel Landman 
# Spring 2021


library(nhdplusTools)
library(sf)
library(dplyr)


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