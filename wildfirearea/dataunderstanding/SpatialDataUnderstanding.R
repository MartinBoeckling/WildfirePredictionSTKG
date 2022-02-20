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

heatmapDatePlot <- function(df, column, minValue, maxValue, funString, xaxisDesc, yaxisDesc){
  stopifnot('Input dataframe needs to be of class data.frame' = 'data.frame' %in% class(df))
  stopifnot('Input column needs to be present in input dataframe' = column %in% colnames(df))
  aggFun <- parse(text = funString)
  df %>%
    select(DATE, !!as.symbol(column)) %>%
    filter(!!as.symbol(column) <= maxValue & !!as.symbol(column) >= minValue) %>%
    mutate(MONTH = month(DATE), YEAR = year(DATE)) %>%
    group_by(MONTH, YEAR) %>%
    summarise(aggColumn = eval(aggFun)) %>%
    ggplot(., aes(factor(MONTH), factor(YEAR), fill=aggColumn)) +
    geom_tile() +
    scale_fill_gradient(low=colorPalette[20], high=colorPalette[1]) +
    xlab(xaxisDesc) +
    ylab(yaxisDesc) +
    labs(fill=column) +
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
heatmapDatePlot(weather, 'TMAX', minTemperature, maxTemperature, 'max(TMAX)', 'Month', 'Year')

#### TMIN
summary(weather$TMIN)
# plot histogram with histogramPlot function
histogramPlot(weather, "TMIN", minTemperature, maxTemperature, 5, 'min temperature in °C')
# distribution of data over month and year
heatmapDatePlot(weather, 'TMIN', minTemperature, maxTemperature, 'min(TMIN)', 'Month', 'Year')

#### TAVG
# print distribution statistics to console
summary(weather$TAVG)
# plot histogram with histogramPlot function
histogramPlot(weather, "TAVG", minTemperature, maxTemperature, 5, 'avg temperature in °C')
# distribution of data over month and year
heatmapDatePlot(weather, 'TAVG', minTemperature, maxTemperature, 'mean(TAVG)', 'Month', 'Year')

### Precipitation --------------------------------------------------------------
#### PRCP
# print distribution statistics to console
minPRCP <- 0
maxPRCP <- 656.08
summary(weather$PRCP)
# plot histogram with histogramPlot function
histogramPlot(weather, "PRCP", minPRCP, maxPRCP, 5, 'Precipitation in mm')
# distribution of data over month and year
heatmapDatePlot(weather, 'PRCP', minPRCP, maxPRCP, 'max(PRCP)', 'Month', 'Year')

#### DAPR
summary(weather$DAPR)

#### DWPR
summary(weather$DWPR)

#### MDPR
summary(weather$MDPR)
# plot histogram with histogramPlot function
histogramPlot(weather, "MDPR", minPRCP, maxPRCP, 10, 'Precipitation in mm')
# distribution of data over month and year
heatmapDatePlot(weather, 'MDPR', minPRCP, maxPRCP, 'mean(MDPR)', 'Month', 'Year')

### Snowfall -------------------------------------------------------------------
#### SNOW
minSNOW <- 0
maxSNOW <- 1701.8
summary(weather$SNOW)
histogramPlot(weather, "SNOW", minSNOW, maxSNOW, 10, 'Snowfall in mm')
# heatmap average snowfall over month and year
heatmapDatePlot(weather, 'SNOW', minSNOW, maxSNOW, 'mean(SNOW)', 'Month', 'Year')
# heatmap median snowfall over month and year
heatmapDatePlot(weather, 'SNOW', minSNOW, maxSNOW, 'median(SNOW)', 'Month', 'Year')
# heatmap max snowfall over month and year
heatmapDatePlot(weather, 'SNOW', minSNOW, maxSNOW, 'max(SNOW)', 'Month', 'Year')
# heatmap minimum snowfall over month and year
heatmapDatePlot(weather, 'SNOW', minSNOW, maxSNOW, 'min(SNOW)', 'Month', 'Year')

#### SNWD
minSNWD <- 0
maxSNWD <- 11455.4
summary(weather$SNWD)
histogramPlot(weather, "SNWD", minSNWD, maxSNWD, 50, 'Snowdepth in mm')
# heatmap average snow depth over month and year
heatmapDatePlot(weather, 'SNWD', minSNWD, maxSNWD, 'mean(SNWD)', 'Month', 'Year')
# heatmap max snow depth over month and year
heatmapDatePlot(weather, 'SNWD', minSNWD, maxSNWD, 'max(SNWD)', 'Month', 'Year')
# heatmap min snow depth over month and year
heatmapDatePlot(weather, 'SNWD', minSNWD, maxSNWD, 'min(SNWD)', 'Month', 'Year')

### Cloudiness --------------------------------------------------------------------
# Cloudiness limit parameters
minCloudiness <- 0
maxCloudiness <- 100
#### ACMH
summary(weather$ACMH)
histogramPlot(weather, 'ACMH', minCloudiness, maxCloudiness, 10, 'manual Cloudiness observation in %')
heatmapDatePlot(weather, 'ACMH', minSNWD, maxSNWD, 'mean(ACMH)', 'Month', 'Year')
#### ACSH
summary(weather$ACSH)
histogramPlot(weather, 'ACSH', minCloudiness, maxCloudiness, 10, 'manual Cloudiness observation in %')
heatmapDatePlot(weather, 'ACSH', minSNWD, maxSNWD, 'mean(ACSH)', 'Month', 'Year')

### Wind --------------------------------------------------------------------------
# Wind limit parameters
minWind <- 0
maxWind <- 88.961
#### AWND
summary(weather$AWND)
histogramPlot(weather, 'AWND', minWind, maxWind, 3, 'average windspeed in meter per second')
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'mean(AWND)', 'Month', 'Year')
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'median(AWND)', 'Month', 'Year')
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'max(AWND)', 'Month', 'Year')
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'min(AWND)', 'Month', 'Year')
#### DAWM
summary(weather$DAWM)
histogramPlot(weather, 'DAWM', minWind, maxWind, 3, 'days of multi-day wind measurement')
heatmapDatePlot(weather, 'DAWM', minWind, maxWind, 'mean(DAWM)', 'Month', 'Year')

#### FMTM
minHour <- 0
maxHour <- 2400
summary(weather$FMTM)
histogramPlot(weather, 'FMTM', minHour, maxHour, 100, 'Highest wind speed time in hhmm')
heatmapDatePlot(weather, 'FMTM', minHour, maxHour, 'mean(FMTM)', 'Month', 'Year')

