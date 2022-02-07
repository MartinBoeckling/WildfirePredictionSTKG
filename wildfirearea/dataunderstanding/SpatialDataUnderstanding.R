'This R script provides the functions for the data understanding of the spatial
data that is used in the master thesis. The data will be analyzed by its values
and provides the visualizations used for the data understanding phase. The 
following data categories are used within this script:
- Elevation data (Path/to/file)
- Land Cover & Vegetation (Path/to/file)
- Openstreet map
- Weather
- Wildfire
'
#-----------------------------import packages-----------------------------------
library(ggplot2)
library(dichromat)
library(tidyverse)
library(sf)
library(sp)
library(raster)
#----------------------------Script configuration-------------------------------

colorPalette <- grey.colors(n=20)
#-------------------------------Elevation---------------------------------------
# read elevation dataset
rasterElevation <- raster::raster('~/GitHub/wildfirearea/Data/Elevation/90 m DEM of California, USA/commondata/data0/ca_dem/w001001.adf')
# convert rasterElevation type of raster to dataframe
# convert to SpatialPoints Dataframe
rasterElevationPoint <- rasterToPoints(rasterElevation, spatial = TRUE)
# convert Spatial Points dataframe to solely dataframe
rasterElevationDf <- data.frame(rasterElevationPoint)

# visualize elevation raster
plot(rasterElevation, col=colorPalette)
#-------------------------------Weather-----------------------------------------
weather <- read_csv('~/GitHub/wildfirearea/Data/Weather/weather.csv')
