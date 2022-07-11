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
library(stars)
library(sf)
library(sp)
library(raster)
library(data.table)
# Script configuration ---------------------------------------------------------

colorPalette <- grDevices::grey.colors(n=25)

# Functions --------------------------------------------------------------------

'Function to plot Bar diagram of NA Fraction per column in a dataset
- Requirement: 
  - Input: needs to be of class data.frame
  - Used package: tibble, ggplot2
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
  - Input df needs to be class of data.frame
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
    dplyr::select(DATE, !!as.symbol(column)) %>%
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

# Land Cover -------------------------------------------------------------------
landcoverFileList <- list.files(path = 'data/landCover', pattern = '.tif$', full.names = TRUE)
landCover <- stars::read_stars(landcoverFileList, along = 'band')
downsampledLandCover <- st_downsample(landCover, 10)
landCoverDf <- as.data.frame(downsampledLandCover)
landCoverDf$band <- recode(landCoverDf$band, '1' = '2001', '2' = '2004', '3' = '2006', '4' = '2008',
                    '5' = '2011', '6' = '2013', '7' = '2016', '8' = '2019')

NAWeatherDf <- data.frame(NAFraction =
                     sapply(weather, function(column) sum(is.na(column))/nrow(df))
)

# plot missing values over each column
NAColumnPlot(landCoverDf)

# plot barplot for landcover category count
ggplot(landCoverDf, aes(x=attr)) +
  geom_bar() +
  facet_wrap(~band, ncol=2) +
  xlab('Landcover type') +
  ylab('Count') +
  coord_flip()
# Powerlines -------------------------------------------------------------------
# read in powerline data
powerline <- sf::read_sf('data/openDataCalifornia/California_Electric_Transmission_Lines/Transmission_Line.shp')
## NA Overview -----------------------------------------------------------------
# plot NA statistics
naColumnPlot(powerline)
## Variable distribution -------------------------------------------------------
powerlineCategoricalColumns <- colnames(powerline)[grepl('factor|logical|character',sapply(powerline,class))]
powerlineNumericalColumns <- colnames(powerline)[!grepl('factor|logical|character',sapply(powerline,class))]

for (powerlineColumn in powerlineNumericalColumns) {
  histogramPlot(powerline, powerlineColumn, )
}
### plot powerlines located in California area
ggplot() +
  geom_sf(data = californiaBoundary, fill='white') +
  geom_sf(data = powerline, colour=colorPalette[1]) +
  xlab('Longitude') +
  ylab('Latitude') +
  theme_minimal()
# Open Street map

## NA Overview

# Elevation --------------------------------------------------------------------
# read elevation dataset
rasterElevation <- stars::read_stars('~/GitHub/wildfirearea/data/elevation/Tiff elevation/CaliforniaElevation.tif')
# downsample raster size
downsampledRaster <- st_downsample(rasterElevation, 15)
# convert rasterElevation type of raster to dataframe
rasterElevationDf <- as.data.frame(downsampledRaster)
## NA Overview -----------------------------------------------------------------
# plot NA statistics
naColumnPlot(rasterElevationDf)
## Variable Distribution -------------------------------------------------------
# summary statistics to elevation
summary(rasterElevationDf$CaliforniaElevation.tif)
# plot histogram of elevation value
minElevation <- -86
maxElevation <- 4421
histogramPlot(rasterElevationDf, 'CaliforniaElevation.tif', minElevation, maxElevation, 100, 'Elevation in meter')

# visualize elevation raster
plot(rasterElevation)
# Weather ----------------------------------------------------------------------
'The weather dataset contains weather measurements from the California area in
which one row in the dataset represents the measurement from one station for one
day. The columns in the dataset can be categorized into three groups:
- Station related attributes (ELEVATION, LONGITUDE, LATITUDE, NAME, STATION)
- Weather related measurements (TMAX, TAVG, TMIN, PRCP, SNOW, TAVG, ...)
- Weather attributes (TMAX_ATTRIBUTES, TAVG_ATTRIBUTES, TMIN_ATTRIBUTES, ...)'

# read in weather data from csv file
weather <- data.table::fread('~/GitHub/wildfirearea/data/weather/weather.csv')
weather <- weather %>%
  filter(DATE >= as.Date('2010-01-01'))
## NA Overview -----------------------------------------------------------------
# exclude related attributes of weather variables
weather <- weather %>%
  dplyr::select(!contains('ATTRIBUTES'))
# plot count of missing elements
# extract na fraction of weather dataframe
NADf <- data.frame(NAFraction =
                     sapply(weather, function(column) sum(is.na(column))/nrow(weather))
)
NADf <- tibble::rownames_to_column(NADf, var = 'Column')

naColumnPlot(weather)

## Variable distribution -------------------------------------------------------
'The variable distribution related to the weather variables play a key role for 
the data preparation. To extend the punctual measurement to the research area
the data preparation incorporates the interpolation of the measurements. For the
variable the distribution builds the base to decide which interpolation method
will be used.'
### Station attributes ---------------------------------------------------------
#### Distribution of stations
# read in california boundary from shapefile
california_boundary <- st_read('~/Github/wildfirearea/data/californiaBoundary/CA_State_TIGER2016.shp')
californiaSpol <- as_Spatial(california_boundary)
californiaSpol <- spTransform(californiaSpol, CRS(prjLonLat))

californiaSpol <- sf::st_as_sf(californiaSpol)

dateSequence <- seq.Date(as.Date('2010-01-01'), as.Date('2021-12-31'), by='days')
stationOverview <- weather %>%
  dplyr::group_by(STATION, LATITUDE, LONGITUDE, ELEVATION) %>%
  summarise(completeDate = identical(as.Date(dateSequence), as.Date(DATE)))

completeDateCoverage <- stationOverview %>%
  filter(completeDate == TRUE)

nrow(stationOverview)

ggplot(californiaSpol) +
  geom_sf(colour='black', fill='white') +
  geom_point(data = stationOverview, aes(x= LONGITUDE, y=LATITUDE), size=0.6) +
  theme_minimal() +
  xlab('Longitude') +
  ylab('Latitude')

#### Elevation
summary(stationOverview$ELEVATION)
ggplot(data = stationOverview, aes(x=ELEVATION)) +
  geom_histogram(binwidth = 30, fill=colorPalette[1]) +
  xlab('Elevation in meters') +
  ylab('Count') +
  theme_minimal()
### Temperature ----------------------------------------------------------------
# general limit for temperature
maxTemperature <- 56.67
minTemperature <- -42.78
#### TMAX
# print distribution statistics to console
summary(weather$TMAX)
# plot histogram with histogramPlot function
histogramPlot(weather, "TMAX", minTemperature, maxTemperature, 5, 'maximum temperature in °C')
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

#### TOBS
# print distribution statistics to console
summary(weather$TOBS)
# plot histogram with histogramPlot function
histogramPlot(weather, "TOBS", minTemperature, maxTemperature, 5, 'temperature at observation in °C')
# distribution of data over month and year
heatmapDatePlot(weather, 'TOBS', minTemperature, maxTemperature, 'mean(TOBS)', 'Month', 'Year')

### Precipitation --------------------------------------------------------------
#### PRCP
# print distribution statistics to console
minPRCP <- 0
maxPRCP <- 656.08
summary(weather$PRCP)
# plot histogram with histogramPlot function
histogramPlot(weather, "PRCP", minPRCP, maxPRCP, 5, 'Precipitation in mm')
# distribution of data over month and year
heatmapDatePlot(weather, 'PRCP', minPRCP, maxPRCP, 'mean(PRCP)', 'Month', 'Year')

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
# plot histogram with histogramPlot function
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
# plot histogram with histogramPlot function
histogramPlot(weather, "SNWD", minSNWD, maxSNWD, 50, 'Snowdepth in mm')
# heatmap average snow depth over month and year
heatmapDatePlot(weather, 'SNWD', minSNWD, maxSNWD, 'mean(SNWD)', 'Month', 'Year')
# heatmap max snow depth over month and year
heatmapDatePlot(weather, 'SNWD', minSNWD, maxSNWD, 'max(SNWD)', 'Month', 'Year')
# heatmap min snow depth over month and year
heatmapDatePlot(weather, 'SNWD', minSNWD, maxSNWD, 'min(SNWD)', 'Month', 'Year')

### Cloudiness -----------------------------------------------------------------
# Cloudiness limit parameters
minCloudiness <- 0
maxCloudiness <- 100
#### ACMH
summary(weather$ACMH)
# plot histogram with histogramPlot function
histogramPlot(weather, 'ACMH', minCloudiness, maxCloudiness, 10, 'manual Cloudiness observation in %')
# heatmap average manual observation cloudiness from midnight over month and year
heatmapDatePlot(weather, 'ACMH', minSNWD, maxSNWD, 'mean(ACMH)', 'Month', 'Year')
#### ACSH
summary(weather$ACSH)
# plot histogram with histogramPlot function
histogramPlot(weather, 'ACSH', minCloudiness, maxCloudiness, 10, 'manual Cloudiness observation in %')
# heatmap of average ACMH over month and year
heatmapDatePlot(weather, 'ACSH', minSNWD, maxSNWD, 'mean(ACSH)', 'Month', 'Year')

### Wind --------------------------------------------------------------------------
# Wind limit parameters
minWind <- 0
maxWind <- 88.961
#### AWND
summary(weather$AWND)
# plot histogram with histogramPlot function
histogramPlot(weather, 'AWND', minWind, maxWind, 3, 'average windspeed in meter per second')
# heatmap of average AWND over month and year
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'mean(AWND)', 'Month', 'Year')
# heatmap of median AWND over month and year
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'median(AWND)', 'Month', 'Year')
# heatmap of maximum AWND over month and year
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'max(AWND)', 'Month', 'Year')
# heatmap of minimum AWND over month and year
heatmapDatePlot(weather, 'AWND', minWind, maxWind, 'min(AWND)', 'Month', 'Year')

#### DAWM
summary(weather$DAWM)
# plot histogram with histogramPlot function
histogramPlot(weather, 'DAWM', minWind, maxWind, 3, 'days of multi-day wind measurement')
# heatmap of average DAWM over month and year
heatmapDatePlot(weather, 'DAWM', minWind, maxWind, 'mean(DAWM)', 'Month', 'Year')

#### WDF2
# limit parameters of lowest and highest possible degree
minDegree <- 0
maxDegree <- 360
# summary statistics
summary(weather$WDF2)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WDF2', minDegree, maxDegree, 10, 'direction of fastest 2 minute wind in degree')
# heatmap of average WDF2 over month and year
heatmapDatePlot(weather, 'WDF2', minWind, maxWind, 'mean(WDF2)', 'Month', 'Year')

#### WDF5
# summary statistics
summary(weather$WDF5)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WDF5', minDegree, maxDegree, 10, 'direction of fastest 5 minute wind in degree')
# heatmap of average WDF5 over month and year
heatmapDatePlot(weather, 'WDF5', minWind, maxWind, 'mean(WDF5)', 'Month', 'Year')

#### WDFG
summary(weather$WDFG)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WDFG', minDegree, maxDegree, 10, 'direction of peak wind gust in degree')
# heatmap of average WDFG over month and year
heatmapDatePlot(weather, 'WDFG', minWind, maxWind, 'mean(WDFG)', 'Month', 'Year')

#### WDFM
minWindMovement <- 0
maxWindMovement <- max(weather$WDMV, na.rm=TRUE)
summary(weather$WDMV)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WDMV', minWindMovement, maxWindMovement, 10, '24 hour wind movement in km')
# heatmap of average WDMV over month and year
heatmapDatePlot(weather, 'WDMV', minWind, maxWind, 'mean(WDMV)', 'Month', 'Year')

#### WSF2
summary(weather$WSF2)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WSF2', minWind, maxWind, 5, 'fastest 2 minutes wind speed in meters per second')
# heatmap of average WSF2 over month and year
heatmapDatePlot(weather, 'WSF2', minWind, maxWind, 'mean(WSF2)', 'Month', 'Year')

#### WSFG
summary(weather$WSFG)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WSFG', minWind, maxWind, 5, 'peak gust wind speed in meters per second')
# heatmap of average WSFG over month and year
heatmapDatePlot(weather, 'WSFG', minWind, maxWind, 'mean(WSFG)', 'Month', 'Year')

#### WSFI
summary(weather$WSFI)
# plot histogram with histogramPlot function
histogramPlot(weather, 'WSFI', minWind, maxWind, 5, 'highest instantaneous wind speed in meters per second')
# heatmap of average WSFI over month and year
heatmapDatePlot(weather, 'WSFI', minWind, maxWind, 'mean(WSFI)', 'Month', 'Year')

#### FMTM
# limit parameters for minimum clock time and maximum clock time
minHour <- 0
maxHour <- 2400
summary(weather$FMTM)
# plot histogram with histogramPlot function
histogramPlot(weather, 'FMTM', minHour, maxHour, 100, 'Highest wind speed time in hhmm')
# heatmap of average FMTM over month and year
heatmapDatePlot(weather, 'FMTM', minHour, maxHour, 'mean(FMTM)', 'Month', 'Year')



### Evaporation Pan ------------------------------------------------------------
# limit parameters for evaporation
minEvap <- 0
maxEvap <- max(weather$EVAP, na.rm=TRUE)
#### EVAP
# print summary statistics for evaporation
summary(weather$EVAP)
# plot histogram with histogramPlot function
histogramPlot(weather, 'EVAP', minEvap, maxEvap, 5, 'evaporation in mm')
# heatmap of average EVAP over month and year
heatmapDatePlot(weather, 'EVAP', minEvap, maxEvap, 'mean(EVAP)', 'Month', 'Year')

#### MNPN
# print summary statistics of minimum temperature in evaporation pan
summary(weather$MNPN)
# plot histogrtam with histogramPlot function
histogramPlot(weather, 'MNPN', minTemperature, maxTemperature, 5, 'minimum tempreature in evaporation pan in °C')
# heatmap of average MNPN over month and year
heatmapDatePlot(weather, 'MNPN', minEvap, maxEvap, 'mean(MNPN)', 'Month', 'Year')

#### MXPN
# print summary statistics of maximum temperature in evaporation pan
summary(weather$MXPN)
# plot histogrtam with histogramPlot function
histogramPlot(weather, 'MXPN', minTemperature, maxTemperature, 5, 'maximum tempreature in evaporation pan in °C')
# heatmap of average MNPN over month and year
heatmapDatePlot(weather, 'MXPN', minEvap, maxEvap, 'mean(MXPN)', 'Month', 'Year')

### Ground Parameters--------------------------------------------------------------
#### SN02
# print summary statistics of SN02 variable
summary(weather$SN02)
# plot histogram with histogramPlot function
histogramPlot(weather, "SN02", minTemperature, maxTemperature, 10, 'minimum unknown soil temperature in 10cm depth in °C')
# distribution of SN02 over month and year
heatmapDatePlot(weather, 'SN02', minTemperature, maxTemperature, 'mean(SN02)', 'Month', 'Year')

#### SN03
# print summary statistics of SN03 variable
summary(weather$SN03)
# plot histogram with histogramPlot function
histogramPlot(weather, "SN03", minTemperature, maxTemperature, 10, 'minimum unknown soil temperature in 20 cm depth in °C')
# distribution of SN03 over month and year
heatmapDatePlot(weather, 'SN03', minTemperature, maxTemperature, 'mean(SN03)', 'Month', 'Year')

#### SN32
summary(weather$SN32)
# plot histogram with histogramPlot function
histogramPlot(weather, "SN32", minTemperature, maxTemperature, 10, 'minimum bare ground temperature in 10 cm depth in °C')
# distribution of SN32 over month and year
heatmapDatePlot(weather, 'SN32', minTemperature, maxTemperature, 'mean(SN32)', 'Month', 'Year')

#### SN33
# print summary statistics
summary(weather$SN33)
# plot histogram with histogramPlot function
histogramPlot(weather, "SN33", minTemperature, maxTemperature, 10, 'minimum bare ground temperature in 20 cm depth in °C')
# distribution of SN32 over month and year
heatmapDatePlot(weather, 'SN33', minTemperature, maxTemperature, 'mean(SN33)', 'Month', 'Year')

#### SN35
# print summary statistics
summary(weather$SN35)
# plot histogram with histogramPlot function
histogramPlot(weather, "SN35", minTemperature, maxTemperature, 5, 'minimum bare ground temperature in 100 cm depth in °C')
# distribution of SN32 over month and year
heatmapDatePlot(weather, 'SN35', minTemperature, maxTemperature, 'mean(SN35)', 'Month', 'Year')

#### SX02
# print summary statistics of SN02 variable
summary(weather$SX02)
# plot histogram with histogramPlot function
histogramPlot(weather, "SX02", minTemperature, maxTemperature, 5, 'maximum unknown soil temperature in 10cm depth in °C')
# distribution of SX02 over month and year
heatmapDatePlot(weather, 'SX02', minTemperature, maxTemperature, 'mean(SX02)', 'Month', 'Year')

#### SX03
# print summary statistics of SN03 variable
summary(weather$SX03)
# plot histogram with histogramPlot function
histogramPlot(weather, "SX03", minTemperature, maxTemperature, 5, 'maximum unknown soil temperature in 20 cm depth in °C')
# distribution of SX03 over month and year
heatmapDatePlot(weather, 'SX03', minTemperature, maxTemperature, 'mean(SX03)', 'Month', 'Year')

#### SX32
summary(weather$SX32)
# plot histogram with histogramPlot function
histogramPlot(weather, "SX32", minTemperature, maxTemperature, 10, 'maximum bare ground temperature in 10 cm depth in °C')
# distribution of SX32 over month and year
heatmapDatePlot(weather, 'SX32', minTemperature, maxTemperature, 'mean(SX32)', 'Month', 'Year')

#### SX33
# print summary statistics
summary(weather$SX33)
# plot histogram with histogramPlot function
histogramPlot(weather, "SX33", minTemperature, maxTemperature, 10, 'maximum bare ground temperature in 20 cm depth in °C')
# distribution of SX32 over month and year
heatmapDatePlot(weather, 'SX33', minTemperature, maxTemperature, 'mean(SX33)', 'Month', 'Year')

#### SX35
# print summary statistics
summary(weather$SX35)
# plot histogram with histogramPlot function
histogramPlot(weather, "SX35", minTemperature, maxTemperature, 5, 'minimum bare ground temperature in 100 cm depth in °C')
# distribution of SX32 over month and year
heatmapDatePlot(weather, 'SX35', minTemperature, maxTemperature, 'mean(SX35)', 'Month', 'Year')

### Special Weather type
#### WT01
# distribution of data over month and year
heatmapDatePlot(weather, 'WT01', minPRCP, maxPRCP, 'sum(WT01)', 'Month', 'Year')

#### WT02
# distribution of data over month and year
heatmapDatePlot(weather, 'WT02', minPRCP, maxPRCP, 'sum(WT02)', 'Month', 'Year')

#### WT03
# distribution of data over month and year
heatmapDatePlot(weather, 'WT03', minPRCP, maxPRCP, 'sum(WT03)', 'Month', 'Year')

#### WT04
# distribution of data over month and year
heatmapDatePlot(weather, 'WT04', minPRCP, maxPRCP, 'sum(WT04)', 'Month', 'Year')

#### WT05
# distribution of data over month and year
heatmapDatePlot(weather, 'WT05', minPRCP, maxPRCP, 'sum(WT05)', 'Month', 'Year')

#### WT06
# distribution of data over month and year
heatmapDatePlot(weather, 'WT06', minPRCP, maxPRCP, 'sum(WT06)', 'Month', 'Year')

#### WT07
# distribution of data over month and year
heatmapDatePlot(weather, 'WT07', minPRCP, maxPRCP, 'sum(WT07)', 'Month', 'Year')

#### WT08
# distribution of data over month and year
heatmapDatePlot(weather, 'WT08', minPRCP, maxPRCP, 'sum(WT08)', 'Month', 'Year')

#### WT09
# distribution of data over month and year
heatmapDatePlot(weather, 'WT09', minPRCP, maxPRCP, 'sum(WT09)', 'Month', 'Year')

#### WT10
# distribution of data over month and year
heatmapDatePlot(weather, 'WT10', minPRCP, maxPRCP, 'sum(WT10)', 'Month', 'Year')

#### WT11
# distribution of data over month and year
heatmapDatePlot(weather, 'WT11', minPRCP, maxPRCP, 'sum(WT11)', 'Month', 'Year')

#### WT13
# distribution of data over month and year
heatmapDatePlot(weather, 'WT13', minPRCP, maxPRCP, 'sum(WT13)', 'Month', 'Year')

#### WT14
# distribution of data over month and year
heatmapDatePlot(weather, 'WT14', minPRCP, maxPRCP, 'sum(WT14)', 'Month', 'Year')

#### WT16
# distribution of data over month and year
heatmapDatePlot(weather, 'WT16', minPRCP, maxPRCP, 'sum(WT16)', 'Month', 'Year')

#### WT17
# distribution of data over month and year
heatmapDatePlot(weather, 'WT17', minPRCP, maxPRCP, 'sum(WT17)', 'Month', 'Year')

#### WT18
# distribution of data over month and year
heatmapDatePlot(weather, 'WT18', minPRCP, maxPRCP, 'sum(WT18)', 'Month', 'Year')

#### WT19
# distribution of data over month and year
heatmapDatePlot(weather, 'WT19', minPRCP, maxPRCP, 'sum(WT19)', 'Month', 'Year')

#### WV01
# distribution of data over month and year
heatmapDatePlot(weather, 'WV01', minPRCP, maxPRCP, 'sum(WV01)', 'Month', 'Year')

#### WV03
# distribution of data over month and year
heatmapDatePlot(weather, 'WV03', minPRCP, maxPRCP, 'sum(WV03)', 'Month', 'Year')

#### WV07
# distribution of data over month and year
heatmapDatePlot(weather, 'WV07', minPRCP, maxPRCP, 'sum(WV07)', 'Month', 'Year')

#### WV20
# distribution of data over month and year
heatmapDatePlot(weather, 'WV20', minPRCP, maxPRCP, 'sum(WV20)', 'Month', 'Year')

# Wildfire ---------------------------------------------------------------------
wildfireDirs <- list.dirs('data/wildfire', recursive = FALSE)
wildfireDf <- data.frame()
for (wildfireDir in wildfireDirs){
  year <- strsplit(wildfireDir, '/')[[1]][3]
  print(year)
  wildfireShpFiles <- list.files(wildfireDir, pattern = '.shp', full.names = TRUE)
  wildfireDfList <- lapply(wildfireShpFiles, read_sf)
  wildfireDfSingle <- sf::st_as_sf(data.table::rbindlist(wildfireDfList))
  wildfireDfSingle <- wildfireDfSingle %>%
    mutate(DATE = as.Date(paste0(year, '-01-01')) + BurnDate)
  wildfireDf <- rbind(wildfireDf, wildfireDfSingle)
}

sum(st_is_valid(wildfireDf))
