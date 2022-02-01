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
library(tidyverse)
library(sf)
library(sp)
library(raster)
#-------------------------------Elevation---------------------------------------
# read elevation dataset
rasterElevation <- raster('~/GitHub/wildfirearea/Data/Elevation/90 m DEM of California, USA/commondata/data0/ca_dem/w001001.adf')

# visualize elevation raster
ggplot() +
  geom_raster()
#-------------------------------Weather-----------------------------------------
