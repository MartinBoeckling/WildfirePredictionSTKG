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
library(dplyr)
library(rgdal)
library(sf)
library(sp)
library(raster)
library(data.table)
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
## NA Overview -----------------------------------------------------------------
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
  geom_bar(stat='identity', fill=colorPalette[5]) +
  coord_flip() +
  xlab('Weather column') +
  ylab('Percentage of missing observations')
  

## Variable distribution -------------------------------------------------------
'The variable distribution related to the weather variables play a key role for 
the data preparation. To extend the punctual measurement to the research area
the data preparation incorporates the interpolation of the measurements. For the
variable the distribution builds the base to decide which interpolation method
will be used.'
### Station attributes -------------------------------------------------
#### Elevation
summary(weather$ELEVATION)
ggplot(data = weather, aes(x=ELEVATION)) +
  geom_histogram(binwidth = 30)

#### Distribution of stations
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
summary(weather$TMAX)
ggplot(data = weather, aes(x=TMAX)) +
  geom_histogram()
  
#### TMIN
summary(weather$TMIN)

#### TAVG
summary(weather$TAVG)
ggplot(data = weather, aes(x=TAVG)) +
  geom_histogram()

### Precipitation --------------------------------------------------------------
summary(weather$PRCP)
ggplot(data = weather, aes(x=PRCP)) +
  geom_histogram()