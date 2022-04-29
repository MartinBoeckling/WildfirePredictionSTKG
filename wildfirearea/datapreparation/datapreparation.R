'Data Preparation script

Description: This script prepares the data for the fourth phase of the CRISP-DM
process, the modelling phase. The script transforms the data necessary for the '
# import packages --------------------------------------------------------------
library(automap)
library(data.table)
library(dplyr)
library(exactextractr)
library(fields)
library(future.apply)
library(gstat)
library(lubridate)
library(osmdata)
library(pbmcapply)
library(raster)
library(sf)
library(sp)
library(tibble)
library(tidyr)

# Script functions -------------------------------------------------------------
'Title:
Wind Direction function

Description:
Convert the wind direction degree value to wind direction category based on
value threshold.

Input:
  - valueItem: value with wind direction in degree

Output:
  - return of value with wind direction as category'
windDirection <- function(valueItem){
  case_when(
    valueItem > 11.25 & valueItem <= 33.75 ~ 'NNE',
    valueItem > 33.75 & valueItem <= 56.25 ~ 'NE',
    valueItem > 56.25 & valueItem <= 78.75 ~ 'ENE',
    valueItem > 78.75 & valueItem <= 101.25 ~ 'E',
    valueItem > 101.25 & valueItem <= 123.75 ~ 'ESE',
    valueItem > 123.75 & valueItem <= 146.25 ~ 'SE',
    valueItem > 146.25 & valueItem <= 168.75 ~ 'SSE',
    valueItem > 168.75 & valueItem <= 191.25 ~ 'S',
    valueItem > 191.25 & valueItem <= 213.75 ~ 'SSW',
    valueItem > 213.75 & valueItem <= 236.25 ~ 'SW',
    valueItem > 236.25 & valueItem <= 258.75 ~ 'WSW',
    valueItem > 258.75 & valueItem <= 281.25 ~ 'W',
    valueItem > 281.25 & valueItem <= 303.75 ~ 'WNW',
    valueItem > 303.75 & valueItem <= 326.25 ~ 'NW',
    valueItem > 326.25 & valueItem <= 348.75 ~ 'NNW',
    valueItem > 348.75 ~ 'N',
    valueItem <= 11.25  ~ 'N',
    TRUE ~ NA_character_
  )
}

'
Title:
Dynamic kriging function

Description: 
The krigingFunction is a dynamic function to perform simple, ordinary or universal
kriging depending on the formula submitted to the function. The three parameters 
for the variogram (nugget, range, sill) are determined dynamically to always
determine the best fit for those parameters.
The function prepares the data necessary for the interpolation by removing duplicate
locations and stations present in the provided dataframe.

Input: 
  - inputLocations: Location of having the new data
  - inputWeather: Weather dataframe
  - inputFormula: Fortmula given for the modelling of the Variogram and Kriging
  - inputVGMModel: String with the used variogram model
  - inputColumn: Column for which the interpolation happens
  - inputDate: Date for which the interpolation is performed
  - minValue: determine the minimum valid value for the present data
  - maxValue: determine the maximum valid value for the present data

Output:
  - Dataframe with predicted variables for inputLocations with Date and Grid Cell IDs'

krigingFunction <- function(inputLocations, inputWeather, inputFormula,
                            inputColumn, inputDate, minValue,
                            maxValue) {
  # check if input is in required format
  stopifnot('Input location object needs to be of type sf' = 'sf' %in% class(inputLocations))
  stopifnot('Input weather needs to be of class data.frame' = 'data.frame' %in% class(inputWeather))
  stopifnot('Input column not of class character' = class(inputColumn) == 'character')
  stopifnot('Input Column not in weather dataframe' = inputColumn %in% colnames(inputWeather))
  stopifnot('Input Formula not of class character' = class(inputFormula) == 'character')
  stopifnot('Input date not present in weather dataframe' = as.Date(inputDate) %in% unique(inputWeather$DATE))
  # transform string into a R formula
  krigingFormula <- as.formula(inputFormula)
  # prepare dataframe for interpolation
  interpolateDf <- inputWeather %>%
    # select necessary columns for the interpolation
    dplyr::select(STATION, LATITUDE,
                  LONGITUDE, ELEVATION, DATE,
                  !!as.symbol(inputColumn), COASTDISTANCE) %>%
    # filter maximum Values out of column
    dplyr::filter(!!as.symbol(inputColumn) >= minValue & !!as.symbol(inputColumn) <= maxValue) %>%
    # filter needed date
    dplyr::filter(DATE ==  inputDate) %>%
    # keep only distinct station ids in data. If duplicate is observed, first
    # row is being 
    dplyr::distinct(STATION, .keep_all = TRUE)
  
  # transform dataframe into sf object
  interpolateSf <- sf::st_as_sf(interpolateDf, coords=c('LONGITUDE', 'LATITUDE'),
                                crs=prjLonLat, remove = FALSE)
  # delete duplicate locations in interpolateSf
  interpolateSf <- interpolateSf %>%
    distinct(geometry, .keep_all = TRUE)
  # check if column has only 0 values to prevent kriging error
  if (identical(unique(as.data.frame(interpolateSf)[, inputColumn]), 0)){
    # construct dataframe for 0 values
    krigingPred <- inputLocations[, c('ID', 'geometry')]
    krigingPred$DATE <- inputDate
    krigingPred[, inputColumn] <- 0
    krigingPred <- krigingPred %>%
      dplyr::select(!!as.symbol(inputColumn), DATE, ID, geometry)
  } else{
    # transform point dataframe into specified projection
    inputLocations <- sf::st_transform(x = inputLocations, crs = prjLonLat)
    # build variogram on weather dataframe with optimal values
    interpolateVariogram  <- automap::autofitVariogram(krigingFormula, as(interpolateSf, 'Spatial'))$var_model
    # cross validation for kriging error
    # build kriging on created variogram model and calculate predictions based on
    # variogram
    krigingPred <- gstat::krige(formula=krigingFormula,
                                locations=as(interpolateSf, 'Spatial'),
                                newdata=as(inputLocations, 'Spatial'),
                                model=interpolateVariogram,
                                debug.level = 0)
    krigingPred <- sf::st_as_sf(krigingPred)
    krigingPred$DATE <- inputDate
    krigingPred$ID <- inputLocations$ID
    krigingPred <- krigingPred %>%
      dplyr::select(-c(var1.var)) %>%
      dplyr::rename(!!inputColumn := 'var1.pred')
  }
  return(krigingPred)
}

'Title:
RMSE Calculation

Description:
The RMSE function calculates the root mean squared error based on the given residuals

Input:
- Residuals: Error from Cross validation
Output:
- Function output: Root mean squared error value'
RMSE <- function(residuals){
  sqrt(sum((residuals)^2)/length(residuals))
}

'Title:
Validation kriging formula

Description:
The validation kriging formula function performs a LOOCV to determine which
formula combination performs the best in regard to the input date and returns
RMSE for the specific date and the different formula combinations

Input:
  - inputWeather: Dataframe consisting of weather dataframe with station location
  - inputColumn: Column in inputWeather dataframe for which the column needs to
    be interpolated
  - inputDate: Date for which the weather variable needs to be interpolated
  - minValue: minimum value for value series
  - maxValue: maximum value for value series

Output:
  - krigeRMSEDf: Dataframe consisting of date, formula and rmse column'
krigeValidation <- function(inputWeather, inputColumn, inputDate,
                            minValue, maxValue){
  interpolateDf <- inputWeather %>%
    # select necessary columns for the interpolation
    dplyr::select(STATION, LATITUDE,
                  LONGITUDE, ELEVATION, DATE,
                  !!as.symbol(inputColumn), COASTDISTANCE) %>%
    # filter maximum Values out of column
    dplyr::filter(!!as.symbol(inputColumn) >= minValue & !!as.symbol(inputColumn) <= maxValue) %>%
    # filter needed date
    dplyr::filter(DATE ==  inputDate) %>%
    # keep only distinct station ids in data. If duplicate is observed, first
    # row is being 
    dplyr::distinct(STATION, .keep_all = TRUE)
  
  # transform dataframe into sf object
  interpolateSf <- sf::st_as_sf(interpolateDf, coords=c('LONGITUDE', 'LATITUDE'),
                                crs=prjLonLat, remove = FALSE)
  # delete duplicate locations in interpolateSf
  interpolateSf <- interpolateSf %>%
    distinct(geometry, .keep_all = TRUE)
  # determine formula combinations based on given formula elements based on pairwise
  # combination
  formulaCombinations <- paste0(paste0(inputColumn, ' ~ '),
                                unlist(lapply(c(1:3), FUN = function(x) apply(t(combn(formulaElements, x)), 1, paste, collapse = ' + '))))
  formulaCombinations <- c(formulaCombinations, paste0(inputColumn, ' ~ 1'))
  
  if (identical(unique(as.data.frame(interpolateSf)[, inputColumn]), 0)){
    # construct RMSE values with 0 values length formula combinations
    rmseValues <- rep(0, times = length(formulaCombinations))
  } else {
    # initialize rmse value vector
    rmseValues <- c()
    # iterate over kriging formulas
    for (krigingFormula in formulaCombinations) {
      krigingFormula <- as.formula(krigingFormula)
      # calculate variogram based on automap that selects best values
      interpolateVariogram  <- automap::autofitVariogram(krigingFormula, as(interpolateSf, 'Spatial'))$var_model
      # calculate LOOCV on data
      cvKrige <- gstat::krige.cv(formula=krigingFormula,
                                 locations=as(interpolateSf, 'Spatial'),
                                 model=interpolateVariogram,
                                 debug.level = 0,
                                 nfold=nrow(as(interpolateSf, 'Spatial')@data),
                                 verbose = FALSE)
      # calculate RMSE for total iteration
      krigeRMSE <- RMSE(cvKrige@data$residual)
      rmseValues <- c(rmseValues, krigeRMSE)
    }
  }
  krigeRMSEDf <- data.frame(date = inputDate, formula = formulaCombinations,
                            rmse = rmseValues)
  return(krigeRMSEDf)
}

'Title:
Inverse distance weighting function

Description:
The inverse distance weighting (IDW) function interpolates a value series based
on the IDW interpolation. The function optimizes the inverse distance power (idp)
performing a LOOCV with returning a RMSE for the idp. The idp with the minimum 
RMSE is used for the IDW interpolation.

Input:
  - inputWeather: Dataframe consisting of weather dataframe with station location
  - inputColumn: Column in inputWeather dataframe for which the column needs to
    be interpolated
  - inputDate: Date for which the weather variable needs to be interpolated
  - minValue: minimum value for value series
  - maxValue: maximum value for value series


Output:

'
idwFunction <- function(inputDf, inputColumn, inputDate,
                        inputLocations, inputFormula, minValue,
                        maxValue){
  
  idwFormula <- as.formula(inputFormula)

  interpolateDf <- inputDf %>%
    # select necessary columns for the interpolation
    dplyr::select(STATION, LATITUDE,
                  LONGITUDE, DATE, !!as.symbol(inputColumn)) %>%
    # filter maximum Values out of column
    dplyr::filter(!!as.symbol(inputColumn) >= minValue & !!as.symbol(inputColumn) <= maxValue) %>%
    # filter needed date
    dplyr::filter(DATE ==  inputDate) %>%
    # keep only distinct station ids in data. If duplicate is observed, first
    # row is kept
    dplyr::distinct(STATION, .keep_all = TRUE)
  
  # transform dataframe into sf object
  interpolateSf <- sf::st_as_sf(interpolateDf, coords=c('LONGITUDE', 'LATITUDE'),
                           crs=prjLonLat, remove = FALSE)
  
  idpSeq <- c(0.1, seq(0.5, 5, 0.5))
  
  cvGrid <- expand.grid(IDP = idpSeq)
  
  cvGrid$RMSE <- NA
  
  for (i in 1:nrow(cvGrid)){
    idwInterpolation <- gstat(formula = idwFormula,
                              data = interpolateSf,
                              set = list(idp = cvGrid[i, 'IDP']))
    
    idwCrossval <- gstat.cv(idwInterpolation,
                            beta = cvGrid[i, 'IDP'],
                            debug.level = 0,
                            verbose = FALSE)
    
    cvGrid[i, 'RMSE'] <- RMSE(idwCrossval$residual)
    
  }
  optIdx <- which.min(cvGrid$RMSE)
  optIDP <- cvGrid$IDP[optIdx]
  optRMSE <- cvGrid$RMSE[optIdx]
  optIDW <- gstat(formula = idwFormula,
                  data = interpolateSf,
                  set = list(idp = optIDP))
  
  idwPredict <- predict(object = optIDW,
                        newdata = inputLocations,
                        debug.level = 0)
  idwPredict <- idwPredict %>%
    dplyr::select(var1.pred, geometry) %>%
    dplyr::mutate(DATE = inputDate, ID = inputLocations$ID) %>%
    dplyr::rename(!!inputColumn := var1.pred) %>%
    dplyr::select(!!as.symbol(inputColumn), DATE, ID, geometry)
  
  return(idwPredict)
}
'Title:
Thin plate spine interpolation

Description:
This function performs an thin plate spline interpolation over a given value
series of location based measurements. The thin plate spline interpolation
performs a spline based interpolation over the given location values. The
smoothing parameter gets validated on the LOOCV evaluation from the TPS function. 

Input:
  - inputWeather: Dataframe consisting of weather dataframe with station location
  - inputColumn: Column in inputWeather dataframe for which the column needs to
    be interpolated
  - inputDate: Date for which the weather variable needs to be interpolated
  - inputLocations: Sf dataframe consisting with centroid locations based on 
    hexagon
  - minValue: minimum value for value series
  - maxValue: maximum value for value series

Output:
  - krigingPred: Sf Dataframe consisting of predicted category, inputDate, hexagon
    ID and centroid column
'
tpsInterpolation <- function(inputWeather, inputColumn, inputDate,
                             inputLocations, minValue, maxValue){
  interpolateDf <- inputWeather %>%
    # select necessary columns for the interpolation
    dplyr::select(STATION, LATITUDE,
                  LONGITUDE, ELEVATION, DATE,
                  !!as.symbol(inputColumn), COASTDISTANCE) %>%
    # filter maximum Values out of column
    dplyr::filter(!!as.symbol(inputColumn) >= minValue & !!as.symbol(inputColumn) <= maxValue) %>%
    # filter needed date
    dplyr::filter(DATE ==  inputDate) %>%
    # keep only distinct station ids in data. If duplicate is observed, first
    # row is being 
    dplyr::distinct(STATION, .keep_all = TRUE)
  
  if (nrow(interpolateDf) < 5){
    set.seed(15)
    rowMult <- 5-nrow(interpolateDf)
    sampleIndex <- sample(rownames(interpolateDf), rowMult, replace = TRUE)
    sampleDf <- interpolateDf[sampleIndex, ]
    sampleDf$LONGITUDE <- sampleDf$LONGITUDE + 0.0001
    sampleDf$LATITUDE <- sampleDf$LATITUDE + 0.0001
    interpolateDf <- rbind(sampleDf, interpolateDf)
  }
  
  interpolateLocations <- matrix(c(interpolateDf$LONGITUDE, interpolateDf$LATITUDE),
                      byrow = FALSE, ncol=2)
  
  tpsFun <- fields::Tps(x = interpolateLocations, interpolateDf$EVAP, lon.lat = TRUE,
                        miles = FALSE, verbose = FALSE, give.warnings = FALSE)
  
  predLocations <- matrix(c(inputLocations$LONGITUDE, inputLocations$LATITUDE),
                          byrow=FALSE, ncol = 2)
  
  predTPS <- predict.Krig(tpsFun, predLocations, verbose = FALSE)
  
  predTPSDf <- data.frame(as.vector(predTPS), inputDate, inputLocations$ID,
                          inputLocations$geometry)
  names(predTPSDf) <- c(inputColumn, 'DATE', 'ID', 'geometry')
  return(predTPSDf)
}

'Title:
Indicator kriging interpolation

Description:
The indicator kriging method is a kriging based method to interpolate binary
categorical data. The indicator kriging method builds up on the ordinary kriging
method. The method calculates the variogram based on the categorical data and
models the spatial covariance based on the automap function autofitVariogram
method. The build up kriging method uses a formula for ordinary kriging to predict
the probability per input location.

Input:
  - inputWeather: Dataframe consisting of weather dataframe with station location
  - inputColumn: Column in inputWeather dataframe for which the column needs to
    be interpolated
  - inputLocations: Sf dataframe consisting with centroid locations based on 
    hexagon
  - inputDate: Date for which the weather variable needs to be interpolated

Output:
  - krigingPred: Sf Dataframe consisting of predicted category, inputDate, hexagon
    ID and centroid column
'
indicatorKriging <- function(inputWeather, inputColumn, inputLocations,
                             inputDate){
  formula <- paste(inputColumn, '~', '1')
  krigingFormula <- as.formula(formula)
  # prepare dataframe for interpolation
  interpolateDf <- inputWeather %>%
    # select necessary columns for the interpolation
    dplyr::select(STATION, LATITUDE,
                  LONGITUDE, ELEVATION, DATE,
                  !!as.symbol(inputColumn), COASTDISTANCE) %>%
    # filter maximum & minimum Values out of column (min: 0, max: 1)
    dplyr::filter(!!as.symbol(inputColumn) >= 0 & !!as.symbol(inputColumn) <= 1) %>%
    # filter needed date
    dplyr::filter(DATE == inputDate) %>%
    # keep only distinct station ids in data. If duplicate is observed, first
    # row is being 
    dplyr::distinct(STATION, .keep_all = TRUE)
  
  # transform dataframe into sf object
  interpolateSf <- sf::st_as_sf(interpolateDf, coords=c('LONGITUDE', 'LATITUDE'),
                                crs=prjLonLat, remove = FALSE)
  # delete duplicate locations in interpolateSf
  interpolateSf <- interpolateSf %>%
    distinct(geometry, .keep_all = TRUE)
  if (identical(unique(as.data.frame(interpolateSf)[, inputColumn]), 0)){
    # construct dataframe for 0 values
    krigingPred <- inputLocations[, c('ID', 'geometry')]
    krigingPred$DATE <- inputDate
    krigingPred[, inputColumn] <- 0
    krigingPred <- krigingPred %>%
      dplyr::select(!!as.symbol(inputColumn), DATE, ID, geometry)
  } else{
    inputLocations <- sf::st_transform(x = inputLocations, crs = prjLonLat)
    # build variogram on weather dataframe with optimal values
    interpolateVariogram  <- automap::autofitVariogram(krigingFormula, as(interpolateSf, 'Spatial'))$var_model
    
    krigingPred <- gstat::krige(formula=krigingFormula,
                                locations=as(interpolateSf, 'Spatial'),
                                newdata=as(inputLocations, 'Spatial'),
                                model=interpolateVariogram,
                                debug.level = 0)
    krigingPred <- sf::st_as_sf(krigingPred)
    if (unique(is.na(krigingPred$var1.pred))){
      krigingPred$var1.pred <- 0
    }
    krigingPred$DATE <- inputDate
    krigingPred$ID <- inputLocations$ID
    krigingPred$var1.pred <- pmin(1, krigingPred$var1.pred)
    krigingPred$var1.pred <- pmax(0, krigingPred$var1.pred)
    krigingPred$var1.pred <- round(krigingPred$var1.pred)
    krigingPred <- krigingPred %>%
      dplyr::select(-c(var1.var)) %>%
      dplyr::rename(!!inputColumn := 'var1.pred')
  }
  return(krigingPred)
}

# Script parameters ------------------------------------------------------------
# determine parameters for map projection used over the complete script
prjMeter <- 'EPSG:3785'
prjLonLat <- 'EPSG:4269'
# determination if data should be aggregated before interpolation
aggMonth <- TRUE
# create sequence of dates in time range of 2000 to 2015 depending on aggregation

dailyDateSequence <- seq(from=as.Date('2010-01-01'), to=as.Date('2021-12-31'), by='days')

monthlyDateSequence <- seq(from=as.Date('2010-01-01'), to=as.Date('2021-12-31'), by='months')


## Interpolation parameters ----------------------------------------------------
# valid formula elements used for kriging selection
formulaElements <- c('LONGITUDE + LATITUDE', 'ELEVATION', 'COASTDISTANCE')
# date sequence for interpolation
if (aggMonth == TRUE) {
  interpolateDateSequence <- monthlyDateSequence
} else{
  interpolateDateSequence <- dailyDateSequence
}
# date sequence for kriging validation
set.seed(15)
sampleDate <- sort(sample(interpolateDateSequence,
                          size = round(0.5*length(interpolateDateSequence))))

# determine cores to be used for multiprocessing

if (.Platform$OS.type == "windows") {
  warning('Due to Windows as OS no multiprocessing possible')
  cores <- 1
} else {
  cores <- detectCores() - 2
}
### Temperature limit ----------------------------------------------------------
maxTemperature <- 56.67
minTemperature <- -42.78

### Precipitation limit --------------------------------------------------------
minPrcp <- 0
maxPrcp <- 656.08

### SNOW -----------------------------------------------------------------------
minSNOW <- 0
maxSNOW <- 1701.8
minSNWD <- 0
maxSNWD <- 11455.4

# Grid creation ----------------------------------------------------------------
'Grid consisting of hexagonal grid cells. '
## Setup region area -----------------------------------------------------------
californiaBoundary <- sf::st_read('data/californiaBoundary/CA_State_TIGER2016.shp')
californiaSP <- sf::as_Spatial(californiaBoundary)

## setup hexagonal grid --------------------------------------------------------
# define area in square meters
cellArea <- 20000000
# calculate cell distance
'calculation of cellsize based on cellsize argument of spsample
which defines the distance between the center of consecutives hexagons'
cellDistance <- 2 * sqrt(cellArea/((3*sqrt(3)/2))) * sqrt(3)/2

# calculate center points of hexagons in spatial area
set.seed(15)
hexGridCentroids <- sp::spsample(californiaSP, type='hexagonal', cellsize=cellDistance)
# create hexagonal polygons based on calculated centroids
hexGrid <- sp::HexPoints2SpatialPolygons(hexGridCentroids, dx = cellDistance)
# reproject raster to longlat projection
hexGrid <- sp::spTransform(hexGrid, prjLonLat)

# Coast Line -------------------------------------------------------------------
# define bounding box
californiaBbox <- as.vector(sp::bbox(hexGrid))
# extract Open Street map coast line features
# increase timeout to extract all necessary files for query
californiaCoastOSM <- opq(bbox=californiaBbox, timeout = 4000) %>% 
  # define open street map features to be extracted
  add_osm_feature(key ="natural", "coastline") %>% 
  # transform object to simple feature object
  osmdata_sf()
# extract coastLine object to new object
californiaCoastLine <- californiaCoastOSM$osm_lines
# reproject object to EPSG:4269 coordinate reference system
californiaCoastLine <- st_transform(californiaCoastLine, prjLonLat)

# Elevation --------------------------------------------------------------------
## read data -------------------------------------------------------------------
elevationRaster <- raster::raster('data/elevation/californiaElevation.tif')

"## Terrain ---------------------------------------------------------------------
# calculate terrain statistics based from the given elevation raster
terrainList <- raster::terrain(elevationRaster, opt=c('slope', 'aspect', 'flowdir'),
                               unit = 'degrees', neighbors=8, progress='text',
                               filename='data/elevation/terrain.grd')
terrainList <- raster::stack('data/elevation/terrain.tif', bands=c(1, 2, 6))
names(terrainList) <- c('SLOPE', 'ASPECT', 'FLOWDIR')"
## Elevation grid -----------------------------------
# extract elevation values by aggregating average weighting based on area present
# in polygon
elevationGridValues <- exactextractr::exact_extract(elevationRaster, hexGrid, weights = 'area',
                                     fun='weighted_mean', progress=TRUE)


# extract terrain values from terrain raster by aggregating average weighting
# based on area present in polygon
"terrainGridValues <- exactextractr::exact_extract(terrainList, hexGrid, weights = 'area',
                                                  fun='weighted_mean', progress=TRUE,
                                                  stack_apply=TRUE)
"
# transform SpatialPolygon object to SpatialPolygonDataframe object
# extract Grid ID for object merge
pid <- sapply(slot(hexGrid, "polygons"), function(x) slot(x, "ID"))
# merge SpatialPolygon with DataFrame
elevationGridValues <- data.frame('ELEVATION' = elevationGridValues, row.names = pid)

# merge terrain dataframe with IDs
"terrainGridValues <- data.frame('SLOPE' = terrainGridValues$weighted_mean.SLOPE,
                                    'ASPECT' = terrainGridValues$weighted_mean.ASPECT,
                                    'FLOWDIR' = terrainGridValues$weighted_mean.FLOWDIR,
                                    row.names = pid)"
# merge elevation and terrain value based on common index
"elevationGridValues <- cbind(elevationGridValues, terrainGridValues)"
# merge elevationGridValues vector with spatial polygons object

hexGridElevation <- sp::SpatialPolygonsDataFrame(hexGrid, elevationGridValues)

# calculate distance to shore line
# transform Spatial Polygons dataframe to simple feature object
hexGridCentroidsSf <- sf::st_as_sf(hexGridCentroids)
hexGridCentroidsSf <- sf::st_transform(hexGridCentroidsSf, prjLonLat)
# calculate distance of elevation Grid to california Coast Line
gridShoreDistance <- sf::st_distance(hexGridCentroidsSf, californiaCoastLine)
# extract minimum distance to shore by each grid centroid
gridShoreDistance <- apply(gridShoreDistance, 1, min)
# build Data Frame with column ID and coastDistance
gridShoreDf <- data.frame(ID = pid, COASTDISTANCE = gridShoreDistance)
# convert hexGridElevation SpatialPolygon object to SF object
hexGridElevationSf <- sf::st_as_sf(hexGridElevation)
# join data
hexGridElevationSf <- hexGridElevationSf %>%
  rownames_to_column('ID') %>%
  left_join(gridShoreDf, by='ID') %>%
  dplyr::select(-ID)


# weather ----------------------------------------------------------------------
## read data -------------------------------------------------------------------
weather <- data.table::fread('data/weather/weather.csv')
weather <- weather %>%
  filter(DATE >= '2010-01-01')
weather <- as.data.frame(weather)
'Prepare weather dataset based upon the non aggregated daily weather dataset
and the monthly aggregated weather dataset before the interpolation'

## Data Preparation -----------------------------------------------------------
### define variable quality rules 

# select all  weather variable columns
weatherVariables <- weather %>%
  dplyr::select(!contains('ATTRIBUTES')) %>%
  colnames()

# replace NA values of categorical variables with 0
# Weather type
weatherTypeVariable <- weather %>%
  dplyr::select((contains('WT') | contains('WV')) & !contains('ATTRIBUTES')) %>%
  colnames()

# iterate over weather type columns to replace NA values with 0
for (wtColumn in weatherTypeVariable) {
  weather[wtColumn][is.na(weather[wtColumn])] <- 0
}

# check if column covers complete date range defined in variable dateSequence
# setup empty vector for append of TRUE & FALSE values
# TRUE - Column covers complete time range
# FALSE - Column covers not complete time range
validWeatherColumns <- c()

# iterate over all columns with weather variables
for (column in weatherVariables) {
  # build distinct date vector of all non-NA data
  columnDate <- weather %>%
    dplyr::select(DATE, !!as.symbol(column)) %>%
    mutate(DATE = as.Date(DATE)) %>%
    na.omit() %>%
    distinct(DATE) %>%
    pull(DATE)
  # sort values ascending
  columnDate <- columnDate[order(columnDate)]
  # append status if date sequence is identical to column date
  validWeatherColumns <- append(validWeatherColumns, identical(dailyDateSequence, columnDate))
}

# create attributes for weather related variables
validWeatherAttributes <- paste(weatherVariables[validWeatherColumns][-c(1:6)], 'ATTRIBUTES', sep='_')
# create vector of valid weather columns
validWeatherColumns <- c(weatherVariables[validWeatherColumns],validWeatherAttributes )
# select weather column of variables covering all dates
weather <- weather %>%
  dplyr::select(all_of(validWeatherColumns))

# replace wind degrees with categories containing wind direction
weather <- weather %>%
  mutate_at(c('WDF2', 'WDF5'), windDirection)

### Monthly weather -------------------------------------------------------------
if (aggMonth == TRUE) {
  print('Monthly aggregation')
  minAgg <- weather %>%
    mutate(DATE = floor_date(DATE, 'month')) %>%
    group_by(STATION, DATE, LONGITUDE, LATITUDE, ELEVATION) %>%
    summarise_at(c('MNPN', 'TMIN'), min)

  maxAgg <- weather %>%
    mutate(DATE = floor_date(DATE, 'month')) %>%
    group_by(STATION, DATE, LONGITUDE, LATITUDE, ELEVATION) %>%
    summarise_at(c('MXPN', 'TMAX', 'WT01', 'WT02', 'WT03', 'WT04', 'WT05', 'WT06',
                   'WT07', 'WT08', 'WT09', 'WT10', 'WT11', 'WT13', 'WT14', 'WT16',
                   'WT17', 'WT18', 'WT19', 'WT21', 'WT22', 'WV01', 'WV03', 'WV07',
                   'WV20'), max)
  
  meanAgg <- weather %>%
    mutate(DATE = floor_date(DATE, 'month')) %>%
    group_by(STATION, DATE, LONGITUDE, LATITUDE, ELEVATION) %>%
    summarise_at(c('AWND', 'EVAP', 'PRCP', 'SNOW', 'SNWD', 'TAVG', 'TOBS',
                   'WDMV', 'WESD', 'WSF2', 'WSF5', 'WESF'), mean)
  
  countWDF2 <- weather %>%
    mutate(DATE = floor_date(DATE, 'month')) %>%
    group_by(STATION, DATE, LONGITUDE, LATITUDE, ELEVATION, WDF2) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = WDF2, values_from = COUNT, names_prefix = 'WDF2_', values_fill = 0) %>%
    dplyr::select(-WDF2_NA)
  
  countWDF5 <- weather %>%
    mutate(DATE = floor_date(DATE, 'month')) %>%
    group_by(STATION, DATE, LONGITUDE, LATITUDE, ELEVATION, WDF5) %>%
    summarise(COUNT = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = WDF5, values_from = COUNT, names_prefix = 'WDF5_', values_fill = 0) %>%
    dplyr::select(-WDF5_NA)
  
  
  weather <- minAgg %>%
    left_join(maxAgg) %>%
    left_join(meanAgg) %>%
    left_join(countWDF2) %>%
    left_join(countWDF5) %>%
    dplyr::ungroup()
}
### Feature Creation -----------------------------------------------------------
# calculation shore line distance

stationDf <- weather %>%
  dplyr::select(STATION, LATITUDE, LONGITUDE, ELEVATION) %>%
  distinct()

stationDf <- sf::st_as_sf(stationDf, coords=c('LONGITUDE', 'LATITUDE'),
                          crs=prjLonLat)


coastlineDistance <- st_distance(stationDf, californiaCoastLine)
stationDf$COASTDISTANCE <- apply(coastlineDistance, 1, min)

weather <- weather %>%
  left_join(stationDf, by=c('STATION', 'ELEVATION'))
### Interpolation --------------------------------------------------------------
# create centroid spatial point in sf format
# unify 
hexGridElevationSf <- sf::st_transform(x = hexGridElevationSf, crs=prjLonLat)
hexGridElevationSf$LONGITUDE <- st_coordinates(st_centroid(hexGridElevationSf$geometry))[,1]
hexGridElevationSf$LATITUDE <- st_coordinates(st_centroid(hexGridElevationSf$geometry))[,2]

hexGridElevationSf <- st_transform(hexGridElevationSf, prjLonLat)

hexGridCentroidsSf <- hexGridCentroidsSf %>%
  rownames_to_column('ID') %>%
  mutate(ID = paste0('ID', ID)) %>%
  st_join(hexGridElevationSf)

# create interpolation dataframe
interpolateDf <- data.frame('column' = c('TMAX', 'TMIN', 'TAVG', 'TOBS',
                                         'PRCP', 'SNOW', 'SNWD', 'MNPN',
                                         'MXPN', 'WESD'),
                            'minValue' = c(minTemperature, minTemperature, minTemperature, minTemperature,
                                           minPrcp, minSNOW, minSNWD, minTemperature,
                                           minTemperature, minSNWD),
                            'maxValue' = c(maxTemperature, maxTemperature, maxTemperature, maxTemperature,
                                           maxPrcp, maxSNOW, maxSNWD, maxTemperature,
                                           maxTemperature, maxSNWD))

wdfColumns <- weather %>%
  dplyr::select(contains('WDF') & !contains('ATTRIBUTES')) %>%
  colnames()

wdfDataframe <- data.frame('column' = wdfColumns,
                           'minValue' = 0,
                           'maxValue' = 31)

interpolateDf <- rbind(interpolateDf, wdfDataframe)

#### Inverse Distance Weighting ------------------------------------------------
# iterate over interpolate dataframe
for (row in 11:nrow(interpolateDf)) {
  # extract row of dataframe
  rowValues <- interpolateDf[row, ]
  # extract column that is getting interpolated
  interpolateColumn <- rowValues %>%
    dplyr::pull('column')
  # extract minimum value associated to interpolate column
  valueMin <- rowValues %>%
    dplyr::pull('minValue')
  # extract maximum value associated to interpolate column
  valueMax <- rowValues %>%
    dplyr::pull('maxValue')
  # print iteration information
  print(paste0(interpolateColumn, ' - ', row, ' of ', nrow(interpolateDf), ' rows '))
  # perform IDW interpolation for each date sequence which returns a list of dataframes
  idwPredList <- pbmclapply(interpolateDateSequence,
                            function(x) idwFunction(weather, interpolateColumn, x,
                                                    hexGridCentroidsSf, paste0(interpolateColumn, ' ~ 1'),
                                                    valueMin ,valueMax),
                            mc.cores = cores)
  # bind list of dataframes to single dataframe
  idwPredDf <- data.table::rbindlist(idwPredList)
  # store interpolation dataframe as an R datastructure with the name of the 
  # interpolation column
  saveRDS(idwPredDf, paste0('data/interpolation/idw/', interpolateColumn, '.rds'))
  # remove prediction list and dataframe
  rm(idwPredList, idwPredDf)
}

#### Indicator Kriging Interpolation -------------------------------------------
# extract columns that will be used for Indicator Kriging
indicatorColumns <- weather %>%
  dplyr::select((contains('WT') | contains('WV') & !contains('ATTRIBUTES'))) %>%
  colnames()
# iterate over eligible columns for indicator kriging
for (indicatorColumn in indicatorColumns) {
  print(indicatorColumn)
  # perform indicator kriging
  wtPredList <- pbmclapply(interpolateDateSequence,
                         function(x) indicatorKriging(weather, indicatorColumn, hexGridCentroidsSf,
                                                      x),
                         mc.cores=cores)
  # bind list of dataframes to single dataframe
  wtPredDf <- data.table::rbindlist(wtPredList)
  # save dataframe in R Data Structure
  saveRDS(wtPredDf, paste0('data/interpolation/',indicatorColumn, '.rds'))
}

#### Kriging Interpolation -----------------------------------------------------
# create kriging dataframe to specify parameters
krigingDf <- data.frame('column' = c('TMAX', 'TMIN', 'TAVG', 'TOBS',
                                     'PRCP', 'SNOW', 'SNWD', 'MNPN',
                                     'MXPN', 'WESD'),
                        'minValue' = c(minTemperature, minTemperature, minTemperature, minTemperature,
                                       minPrcp, minSNOW, minSNWD, minTemperature,
                                       minTemperature, minSNWD),
                        'maxValue' = c(maxTemperature, maxTemperature, maxTemperature, maxTemperature,
                                       maxPrcp, maxSNOW, maxSNWD, minTemperature,
                                       maxTemperature, maxSNWD))

# iterate over kriging dataframe rows to perform interpolation
for (row in 1:nrow(krigingDf)) {
  # extract single row from dataframe to get necessary values
  rowValues <- krigingDf[row, ]
  # extract columns as vector
  interpolateColumn <- rowValues %>%
    dplyr::pull('column')
  valueMin <- rowValues %>%
    dplyr::pull('minValue')
  valueMax <- rowValues %>%
    dplyr::pull('maxValue')
  print(interpolateColumn)
  print('Validation started')
  # start validation to determine best kriging formula
  validationList <- pbmclapply(sampleDate,
                             function(x) krigeValidation(weather, interpolateColumn, x,
                                                         valueMin, valueMax),
                             mc.cores=cores)
  # transform list of dataframes to single dataframe
  validationDf <- data.table::rbindlist(validationList)
  # save RDS with single validation dataframe
  saveRDS(validationDf, paste0('data/interpolation/validation/', interpolateColumn, '.rds'))
  # aggregate RMSE values to extract most optimal kriging formula
  krigingFormula <- validationDf %>%
    group_by(formula) %>%
    summarise(meanRMSE = mean(rmse)) %>%
    slice(which.min(meanRMSE)) %>%
    pull(formula)
  print('Kriging started')
  # perform kriging based on column and minimum and maximum values
  krigingPredList <- pbmclapply(interpolateDateSequence,
                              function(x) krigingFunction(hexGridCentroidsSf, weather, krigingFormula,
                                                          interpolateColumn, x,
                                                          valueMin, valueMax),
                              mc.cores=cores)
  # bind list of dataframes to single dataframe
  krigingPredDf <- data.table::rbindlist(krigingPredList)
  # extract dates with invalid values and construct dataframe containing columns
  # DATE, ID and INDEX
  invalidCell <- krigingPredDf %>%
    tibble::rownames_to_column('INDEX') %>%
    dplyr::filter(!!as.symbol(interpolateColumn) < valueMin | !!as.symbol(interpolateColumn)> valueMax) %>%
    mutate(INDEX = as.integer(INDEX)) %>%
    dplyr::select(DATE, ID, INDEX)
  print('IDW started')
  # extract dates for which the inverse distance weighting needs to be calculated
  idwDate <- unique(invalidCell$DATE)
  # perform idw interpolation for invalid values
  idwPredList <- pbmclapply(idwDate,
                          function(x) idwFunction(weather, interpolateColumn, x,
                                                  hexGridCentroidsSf, paste0(interpolateColumn, ' ~ 1'),
                                                  valueMin ,valueMax),
                          mc.cores = cores)
  # transform list of dataframes to single dataframe
  idwPredDf <- data.table::rbindlist(idwPredList)
  # join kriging dataframe with idw dataframe to interpolate dataframe
  interpolateDf <- krigingPredDf %>%
    left_join(idwPredDf, by=c('ID', 'DATE'))
  # assign new column names after the join to variables for easier indexing
  interpolateX <- paste0(interpolateColumn, '.x')
  interpolateY <- paste0(interpolateColumn, '.y')
  # extract index of invalid values from the invalidCell dataframe
  invalidCellIndex <- invalidCell %>%
    dplyr::pull(INDEX)
  # replace values from those indeces where invalid values are present
  interpolateDf[invalidCellIndex][[interpolateX]] <- interpolateDf[invalidCellIndex][[interpolateY]]
  # restructure interpolation dataframe 
  interpolateDf <- interpolateDf %>%
    dplyr::rename(!!interpolateColumn := !!interpolateX, 'geometry' = 'geometry.x') %>%
    dplyr::select(!!as.symbol(interpolateColumn), DATE, ID, geometry)
  # store interpolation dataframe into RDS structure
  saveRDS(interpolateDf, paste0('data/interpolation/', interpolateColumn, '.rds'))
}

# Spatial aggregation ----------------------------------------------------------
# transform hexGrid to Sf object
hexGridSf <- sf::st_as_sf(hexGrid, crs=prjLonLat)
# join hexGrid with Centroid Dataframe
hexGridSf <- hexGridSf %>%
  sf::st_join(hexGridCentroidsSf)
## Landscape -------------------------------------------------------------------
# read data files
landscapeFiles <- list.files(path = 'data/landCover', pattern = '.tif$', full.names = TRUE)
# create raster stack for all landscape files
landscapeData <- raster::stack(landscapeFiles)

# create legend and class
landscapeLegend <- data.frame('value' = c(0, 11, 12, 21, 22, 23, 24, 31, 41, 42, 43,
                                          51, 52, 71, 72, 73, 74, 81, 82, 90, 95),
                              'mainClass' = c('Unclassified', 'Water', 'Water',
                                              'Developed', 'Developed', 'Developed',
                                              'Developed', 'Barren', 'Forest',
                                              'Forest', 'Forest', 'Shrubland',
                                              'Shrubland', 'Herbaceous', 'Herbaceous',
                                              'Herbaceous', 'Herbaceous', 'Planted/Cultivated	',
                                              'Planted/Cultivated	', 'Wetlands', 'Wetlands'),
                              'subClass' = c('Unclassified', 'Open Water', 'Perennial Ice/Snow',
                                             'Developed, Open Space', 'Developed, Low Intensity',
                                             'Developed, Medium Intensity', 'Developed High Intensity',
                                             'Barren Land (Rock/Sand/Clay)', 'Deciduous Forest',
                                             'Evergreen Forest', 'Mixed Forest', 'Dwarf Scrub',
                                             'Shrub/Scrub', 'Grassland/Herbaceous', 'Sedge/Herbaceous',
                                             'Lichens', 'Moss', 'Pasture/Hay', 'Cultivated Crops',
                                             'Woody Wetlands', 'Emergent Herbaceous Wetlands'))

# calculate area of categories in cell
for (layer in 1:length(landscapeData@layers)) {
  # extract name of file
  name <- strsplit(landscapeData@layers[[layer]]@file@name, '\\\\')[[1]]
  name <- name[length(name)]
  name <- strsplit(name, '[.]')[[1]][1]
  print(name)
  landscapeValuesList <- exactextractr::exact_extract(landscapeData[[layer]], hexGridSf,
                                                      include_cols=c('ID'))
  
  landscapeAggList <- pbmclapply(landscapeValuesList,
                                 function(df) df %>%
                                   group_by(ID, value) %>%
                                   summarise(.groups='keep',coverage_fraction = sum(coverage_fraction))%>% 
                                   ungroup(),
                                 mc.cores=cores)
  
  landscapeAggDf <- data.table::rbindlist(landscapeAggList)
  landscapeAggDf <- landscapeAggDf %>%
    mutate(area = coverage_fraction * 900) %>%
    left_join(landscapeLegend, by='value') %>%
    dplyr::select(-c(mainClass, coverage_fraction, value)) %>%
    tidyr::pivot_wider(names_from = subClass,
                       values_from = area,
                       values_fill = 0)
  saveRDS(landscapeAggDf, paste0('data/landCover/polygon/PolygonLayer',name, '.rds'))
  rm(landscapeValuesList, landscapeAggList, landscapeAggDf)
  gc()
}

# Wildfire ---------------------------------------------------------------------

# Test area --------------------------------------------------------------------



  