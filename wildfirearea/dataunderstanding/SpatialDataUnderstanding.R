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

colorPalette <- grDevices::grey.colors(n=20)

# Functions --------------------------------------------------------------------

'Function to plot Bar diagram of NA Fraction per column in a dataset
- Requirement: 
  - Input: needs to be of class data.frame
  - Used package: tibble, ggplot2
- Input: Object of class data.frame
- Output: Display ggplot bar plot
'
naColumnPlot <- function(df){
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

'Function to plot histogram of continous variable in a dataset
- Requirement:
  - Input needs to be class of data.frame
  - Used Package: dplyr, ggplot2
- Input:
  - df: Dataframe
  - column: Column part of dataframe
  - minValue: Lowest eligible value within specified column
  - maxValue: Highest eligble value within specified column
  - binWidth: specification of binwidth for histogram
  - xaxisDesc: X-axis label description
- Output: display ggplot object'
histogramPlot <- function(df, column, minValue, maxValue, binWidth, xaxisDesc){
  stopifnot('Input dataframe needs to be of class data.frame' = 'data.frame' %in% class(df))
  df %>%
    filter(!!as.symbol(column) <= maxValue & !!as.symbol(column) >= minValue) %>%
    # plot histogram over distribution
    ggplot(., aes_string(x=column)) +
    geom_histogram(binwidth=binWidth, fill=colorPalette[1]) +
    xlab(xaxisDesc) +
    ylab('Count') +
    theme_minimal()
}

heatmapDatePlot <- function(df, column, minValue, maxValue, aggFun, xaxisDesc, yaxisDesc){
  stopifnot('Input dataframe needs to be of class data.frame' = 'data.frame' %in% class(df))
  stopifnot('Input dataframe needs to be present in input df' = column %in% colnames(df))
  df %>%
    select(DATE, !!as.symbol(column)) %>%
    filter(!!as.symbol(column) <= maxValue & !!as.symbol(column) >= minValue) %>%
    mutate(MONTH = month(DATE), YEAR = year(DATE)) %>%
    group_by(MONTH, YEAR) %>%
    summarise(!!as.symbol(column) = min(!!as.symbol(column))) %>%
    ggplot(., aes(factor(MONTH), factor(YEAR), fill=!!as.symbol(column))) +
    scale_fill_gradient(low=colorPalette[20], high=colorPalette[1]) +
    xlab(xaxisDesc) +
    ylab(yaxisDesc) +
    theme_minimal()
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
#### Elevation
summary(weather$ELEVATION)
ggplot(data = weather, aes(x=ELEVATION)) +
  geom_histogram(binwidth = 30, fill=colorPalette[1]) +
  xlab('Elevation in meters') +
  ylab('Count') +
  theme_minimal()

#### Distribution of stations
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
# general limit for temperature
maxTemperature <- 56.67
minTemperature <- -42.78
#### TMAX
# print distribution statistics to console
summary(weather$TMAX)
# plot histogram with histogramPlot function
histogramPlot(weather, "TMAX", minTemperature, maxTemperature, 5, 'max temperature in °C')
# distribution of data over month and year

#### TMIN
summary(weather$TMIN)
# plot histogram with histogramPlot function
histogramPlot(weather, "TMIN", minTemperature, maxTemperature, 5, 'min temperature in °C')

#### TAVG
# print distribution statistics to console
summary(weather$TAVG)
# plot histogram with histogramPlot function
histogramPlot(weather, "TAVG", minTemperature, maxTemperature, 5, 'avg temperature in °C')


### Precipitation --------------------------------------------------------------
#### PRCP
# print distribution statistics to console
minPRCP <- 0
maxPRCP <- 656.08
summary(weather$PRCP)
