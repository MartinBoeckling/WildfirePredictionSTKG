# import packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(pbmcapply)
library(lubridate)

# script functions -------------------------------------------------------------
landscapeGeneration <- function(fileParameter, aggregateArea){
  # split vector into relevant areas
  # extract landscape path
  landscapePath <- fileParameter[1]
  # extract startdate
  startDate <- as.Date(fileParameter[2])
  # extract enddate
  endDate <- as.Date(fileParameter[3])
  # read in landscape data
  landscapeData <- readRDS(landscapePath)
  # extract date of landscape file based on file name
  landscapeDate <- as.numeric(gsub(".*?([0-9]+).*", "\\1", landscapePath))
  # create landscape sequence
  landscapeDateSeq <- seq(from=startDate, to=endDate, by='months')
  # replicate landscape dataframe by date
  landscapeDateSeqRepl <- rep(landscapeDateSeq, nrow(landscapeData))
  # aggregate data
  if (aggregateArea) {
    landscapeData <- landscapeData %>%
      pivot_longer(!ID, names_to='LANDCOVER', values_to = 'area') %>%
      group_by(ID) %>%
      filter(area == max(area)) %>%
      slice(rep(1:n(), length(landscapeDateSeq))) %>%
      dplyr::select(-area)
  } else{
    landscapeData <- landscapeData %>%
      slice(rep(1:n(), length(landscapeDateSeq)))
  }
  landscapeData$DATE <- landscapeDateSeqRepl
  return(landscapeData)
}

# script parameters ------------------------------------------------------------
# list all IDW Interpolation files
idwInterpolationList <- list.files('data/interpolation/idw', full.names = TRUE)
# list all Kriging interpolation files
krigingInterpolationList <- list.files('data/interpolation', full.names = TRUE, include.dirs = FALSE,
                                       pattern = '.rds')
# list all landcover files in a list
landcoverList <- list.files('data/landCover/polygon', full.names = TRUE)
# create monthly date sequence for date generation
monthlyDateSequence <- seq(from=as.Date('2010-01-01'), to=as.Date('2021-12-31'), by='months')
# determine cores to be used for multiprocessing
if (.Platform$OS.type == "windows") {
  warning('Due to Windows as OS no multiprocessing possible')
  cores <- 1
} else {
  warning('Set core variable carefully to prevent memory leakage for fork operations')
  cores <- detectCores() - 2
}
# general landscape settings
landscapeSettings <- list(c(landcoverList[1], '2010-01-01', '2012-12-01'),
                          c(landcoverList[2], '2013-01-01', '2015-12-01'),
                          c(landcoverList[3], '2016-01-01', '2018-12-01'),
                          c(landcoverList[4], '2019-01-01', '2021-12-01'))

# network setting
graphSimplified = FALSE
graphCluster = FALSE

if(graphSimplified){
  if(graphCluster){
    osmNetworkPath <- 'data/network/simplified/osmCluster.csv'
  } else{
    osmNetworkPath <- 'data/network/simplified/vectorDf.csv'
  }
} else{
  if(graphCluster){
    osmNetworkPath <- 'data/network/double/osmCluster.csv'  
  } else{
    osmNetworkPath <- 'data/network/double/vectorDf.csv'
  }
}

# Base Case --------------------------------------------------------------------
## Use Case 1 ------------------------------------------------------------------
weatherDf <- data.frame()
# join data for weather variables
for (idwIteration in 1:length(idwInterpolationList)){
  print(paste(idwIteration, 'of', length(idwInterpolationList), 'iteration'))
  idwData <- idwInterpolationList[idwIteration]
  interpolateData <- readRDS(idwData)
  if (nrow(weatherDf)==0){
    weatherDf <- interpolateData
  }else{
    weatherDf <- weatherDf %>%
      left_join(interpolateData, by=c('ID', 'DATE', 'geometry'))
  }
}


landscapeDataList <- pbmclapply(landscapeSettings, function(x) landscapeGeneration(x, TRUE), 
                                mc.cores = cores)
landscapeDf <- rbindlist(landscapeDataList, use.names = TRUE)

# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')

usecase1Df <- weatherDf %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  left_join(landscapeDf, by=c('ID', 'DATE')) %>%
  dplyr::select(-contains('geometry'))

fwrite(usecase1Df, 'data/usecase/usecase1.csv')
## Use Case 2 ------------------------------------------------------------------
weatherDf <- data.frame()
# join data for weather variables
for (idwIteration in 1:length(idwInterpolationList)){
  print(paste(idwIteration, 'of', length(idwInterpolationList), 'iteration'))
  idwData <- idwInterpolationList[idwIteration]
  interpolateData <- readRDS(idwData)
  if (nrow(weatherDf)==0){
    weatherDf <- interpolateData
  }else{
    weatherDf <- weatherDf %>%
      left_join(interpolateData, by=c('ID', 'DATE', 'geometry'))
  }
}

landscapeDataList <- pbmclapply(landscapeSettings, function(x) landscapeGeneration(x, FALSE), 
                                mc.cores = cores)
landscapeDf <- rbindlist(landscapeDataList, use.names = TRUE)
# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')

usecase2Df <- weatherDf %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  left_join(landscapeDf, by=c('ID', 'DATE')) %>%
  dplyr::select(-contains('geometry'))

fwrite(usecase2Df, 'data/usecase/usecase2.csv')


## Use Case 3 ------------------------------------------------------------------
# join data for weather variables
weatherDf <- data.frame()

for (krigingIteration in 1:length(krigingInterpolationList)){
  print(paste(krigingIteration, 'of', length(krigingInterpolationList), 'iteration'))
  krigingData <- krigingInterpolationList[krigingIteration]
  interpolateData <- readRDS(krigingData)
  interpolateData <- interpolateData %>%
    select(-geometry)
  if (nrow(weatherDf)==0){
    weatherDf <- interpolateData
  }else{
    weatherDf <- weatherDf %>%
      left_join(interpolateData, by=c('ID', 'DATE'))
  }
  rm(interpolateData)
  gc()
}

landscapeDataList <- pbmclapply(landscapeSettings, function(x) landscapeGeneration(x, TRUE), 
                                mc.cores = cores)
landscapeDf <- rbindlist(landscapeDataList, use.names = TRUE)
# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')

usecase3Df <- weatherDf %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  left_join(landscapeDf, by=c('ID', 'DATE')) %>%
  dplyr::select(-contains('geometry'))

fwrite(usecase3Df, 'data/usecase/usecase3.csv')

## Use Case 4 ------------------------------------------------------------------
# join data for weather variables
weatherDf <- data.frame()

for (krigingIteration in 1:length(krigingInterpolationList)){
  print(paste(krigingIteration, 'of', length(krigingInterpolationList), 'iteration'))
  krigingData <- krigingInterpolationList[krigingIteration]
  interpolateData <- readRDS(krigingData)
  interpolateData <- interpolateData %>%
    select(-geometry)
  if (nrow(weatherDf)==0){
    weatherDf <- interpolateData
  }else{
    weatherDf <- weatherDf %>%
      left_join(interpolateData, by=c('ID', 'DATE'))
  }
  rm(interpolateData)
  gc()
}

landscapeDataList <- pbmclapply(landscapeSettings, function(x) landscapeGeneration(x, FALSE), 
                                mc.cores = cores)
landscapeDf <- rbindlist(landscapeDataList, use.names = TRUE)
# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')

usecase4Df <- weatherDf %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  left_join(landscapeDf, by=c('ID', 'DATE')) %>%
  dplyr::select(-contains('geometry'))

fwrite(usecase4Df, 'data/usecase/usecase4.csv')

# Hybrid Case ------------------------------------------------------------------
''
## Use Case 5 ------------------------------------------------------------------
usecase5Df <- fread('data/usecase/usecase1.csv')
osmNetworkDf <- fread(osmNetworkPath)
usecase5Df <- usecase5Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)

fwrite(x = usecase5Df, 'data/usecase/usecase5.csv')
rm(usecase5Df)
gc()
## Use Case 6 ------------------------------------------------------------------
usecase6Df <- fread('data/usecase/usecase2.csv')
osmNetworkDf <- fread(osmNetworkPath)
usecase6Df <- usecase6Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)

fwrite(x = usecase6Df, 'data/usecase/usecase6.csv')
rm(usecase6Df)
gc()
## Use Case 7 ------------------------------------------------------------------
usecase7Df <- fread('data/usecase/usecase3.csv')
osmNetworkDf <- fread(osmNetworkPath)
usecase7Df <- usecase7Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)

fwrite(x = usecase7Df, 'data/usecase/usecase7.csv')
rm(usecase7Df)
gc()
## Use Case 8 ------------------------------------------------------------------

usecase8Df <- fread('data/usecase/usecase4.csv')
osmNetworkDf <- fread(osmNetworkPath)
usecase8Df <- usecase8Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)

fwrite(x = usecase8Df, 'data/usecase/usecase8.csv')
rm(usecase8Df)
gc()
# Network ----------------------------------------------------------------------
openstreetmapGraphEdge <- readRDS('data/network/edgeDataFrame.rds')

openstreetmapGraphEdge <- openstreetmapGraphEdge %>%
  dplyr::select(-YEAR)
openstreetmapGraphEdge <- openstreetmapGraphEdge %>%
  distinct()

fwrite(openstreetmapGraphEdge, 'data/network/openstreetmapGraph.csv')
