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
# import packages---------------------------------------------------------------
library(ggplot2)
library(dichromat)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(data.table)
library(DataExplorer)
# Script configuration----------------------------------------------------------

colorPalette <- grey.colors(n=20)

# Land Cover --------------------------------------------------------------

# Elevation ---------------------------------------------------------------
# read elevation dataset
rasterElevation <- raster::raster('~/GitHub/wildfirearea/Data/Elevation/90 m DEM of California, USA/commondata/data0/ca_dem/w001001.adf')
# convert rasterElevation type of raster to dataframe
# convert to SpatialPoints Dataframe
rasterElevationPoint <- raster::rasterToPoints(rasterElevation, spatial = TRUE)
# convert Spatial Points dataframe to solely dataframe
rasterElevationDf <- data.frame(rasterElevationPoint)

# visualize elevation raster
plot(rasterElevation, col=colorPalette)
# Weather ----------------------------------------------------------------------
'The weather dataset contains weather measurements from the California area in
which one row in the dataset represents the measurement from one station for one
day. The columns in the dataset can be categorized into three groups:
- Station related attributes (ELEVATION, LONGITUDE, LATITUDE, NAME, STATION)
- Weather related measurements (TMAX, TAVG, TMIN, PRCP, SNOW, TAVG, ...)
- Weather attributes (TMAX_ATTRIBUTES, TAVG_ATTRIBUTES, TMIN_ATTRIBUTES, ...)'

# read in weather data from csv file
weather <- data.table::fread('~/GitHub/wildfirearea/data/weather/weather.csv')
# NA Overview ==================================================================
# exclude related attributes of weather variables
weather <- weather %>%
  select(!contains('ATTRIBUTES'))
# plot count of missing elements
# extract na fraction of weather dataframe
weatherNADf <- arrange(
  data.frame(NAFraction =
  sapply(weather, function(column) sum(is.na(column))/nrow(weather))
  ), NAFraction)
weatherNADf <- rownames_to_column(weatherNADf, var = 'Column')
# plot na fraction
ggplot(data = weatherNADf, aes(x=Column, y=NAFraction)) +
  geom_bar(stat='identity') +
  xlab('Percentage of missing observations') +
  ylab()
  coord_flip()

# Variable distribution ========================================================
