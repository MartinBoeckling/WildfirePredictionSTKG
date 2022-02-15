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
# import packages --------------------------------------------------------------
library(ggplot2)
library(lubridate)
library(dichromat)
library(dplyr)
library(tibble)
library(rgdal)
library(sf)
library(sp)
library(raster)
library(data.table)
# Script configuration ---------------------------------------------------------

colorPalette <- grey.colors(n=20)

# Functions --------------------------------------------------------------------
'Function to plot Bar diagram of NA Fraction per column in a dataset
- Requirement: 
  - Input: needs to be of class data.frame
  - Used package: tibble
- Input: Object of class data.frame
- Output: Display ggplot bar plot
'
NAColumnPlot <- function(df){
  # check if input variable is of class data.frame
  stopifnot('Input dataframe needs to be of class data.frame' = 'data.frame' %in% class(df))
  # extract column wise percentage of NA Values by summing boolean return of function is.na up
  NADf <- data.frame(NAFraction =
                       sapply(df, function(column) sum(is.na(column))/nrow(df))
    )
  # 
  NADf <- tibble::rownames_to_column(NADf, var = 'Column')
  # plot na fraction in a bar diagram
  ggplot(data = NADf, aes(x=reorder(Column, -NAFraction), y=NAFraction)) +
    geom_bar(stat='identity', fill=colorPalette[1], color='white') +
    coord_flip() +
    xlab('Column') +
    ylab('Percentage of missing observations') +
    theme_minimal()
}
HistogramPlot <- function(df){
  
}
# Land Cover --------------------------------------------------------------

# Elevation ---------------------------------------------------------------
# read elevation dataset
rasterElevation <- raster::raster('~/GitHub/wildfirearea/data/elevation/Tiff elevation/CaliforniaElevation.tif')
# convert rasterElevation type of raster to dataframe
# convert to SpatialPoints Dataframe
rasterElevationPoint <- raster::rasterToPoints(rasterElevation, spatial = TRUE)
# convert Spatial Points dataframe to solely dataframe
rasterElevationDf <- as.data.frame(rasterElevation)

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
## NA Overview -----------------------------------------------------------------
# exclude related attributes of weather variables
weather <- weather %>%
  select(!contains('ATTRIBUTES'))
# plot count of missing elements
# extract na fraction of weather dataframe
NAColumnPlot(weather)

## Variable distribution -------------------------------------------------------
'The variable distribution related to the weather variables play a key role for 
the data preparation. To extend the punctual measurement to the research area
the data preparation incorporates the interpolation of the measurements. For the
variable the distribution builds the base to decide which interpolation method
will be used.'
### Station attributes ---------------------------------------------------------
#### Elevation -----------------------------------------------------------------
summary(weather$ELEVATION)
ggplot(data = weather, aes(x=ELEVATION)) +
  geom_histogram(binwidth = 30, fill=colorPalette[1]) +
  xlab('Elevation in meters') +
  ylab('Count') +
  theme_minimal()

#### Distribution of stations --------------------------------------------------
# read in california boundary from shapefile
california_boundary <- st_read('~/Github/wildfirearea/data/californiaBoundary/CA_State_TIGER2016.shp')
californiaSpol <- as_Spatial(california_boundary)
californiaSpol <- spTransform(californiaSpol, CRS(prjLonLat))

californiaSpol <- sf::st_as_sf(californiaSpol)
stationOverview <- weather %>%
  select(STATION, LATITUDE, LONGITUDE) %>%
  distinct()
nrow(stationOverview)

ggplot(californiaSpol) +
  geom_sf(colour='black', fill='white') +
  geom_point(data = stationOverview, aes(x= LONGITUDE, y=LATITUDE)) +
  theme_minimal() +
  xlab('Longitude') +
  ylab('Latitude')
### Temperature ----------------------------------------------------------------
#### TMAX
# print distribution statistics to console
summary(weather$TMAX)

# filter data out that is over maximum and minimum
weather %>%
  filter(TMAX <= 56.67 & TMAX >= -42.78) %>%
  ggplot(., aes(x=TMAX)) +
    geom_histogram(binwidth = 5, fill=colorPalette[1]) +
    theme_minimal() +
    xlab('maximum Temperature in °C') +
    ylab('Count')

# 
weather %>%
  filter(TMIN <= 56.67 & TMIN >= -42.78) %>%
  filter(TMAX <= 56.67 & TMAX >= -42.78) %>%
  mutate(YEAR = as.character(year(DATE))) %>%
  # plot histogram over distribution
  ggplot(., aes(x = TMIN, y = TMAX)) +
    geom_point(aes(color = YEAR), alpha = .5) +
  scale_color_manual(values = colorPalette)

#### TMIN
summary(weather$TMIN)
# filter data out that is over maximum and minimum
weather %>%
  filter(TMIN <= 56.67 & TMIN >= -42.78) %>%
# plot histogram over distribution
  ggplot(., aes(x=TMIN)) +
  geom_histogram(binwidth = 5, fill=colorPalette[1]) +
  theme_minimal() +
  xlab('minimum Temperature in °C') +
  ylab('Count')

#### TAVG
# print distribution statistics to console
summary(weather$TAVG)
# filter data out that is over maximum and minimum
weather %>%
  filter(TAVG <= 56.67 & TAVG >= -42.78) %>%
  ggplot(., aes(x=TAVG)) +
  geom_histogram(binwidth = 5, fill=colorPalette[1]) +
  theme_minimal() +
  xlab('average Temperature in °C') +
  ylab('Count')

# 