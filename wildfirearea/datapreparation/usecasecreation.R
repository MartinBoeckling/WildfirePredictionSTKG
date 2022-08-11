# import packages --------------------------------------------------------------
library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(tibble)
library(pbmcapply)
library(purrr)
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
graphSimplified = TRUE
graphCluster = FALSE

if(graphSimplified){
  if(graphCluster){
    osmNetworkPath <- 'data/network/simplified/osmCluster.csv'
    datasetIdAdd <- 1
  } else{
    osmNetworkPath <- 'data/network/simplified/vectorDf.csv'
    datasetIdAdd <- 3
  }
} else{
  if(graphCluster){
    osmNetworkPath <- 'data/network/double/osmCluster.csv'  
    datasetIdAdd <- 0
  } else{
    osmNetworkPath <- 'data/network/double/vectorDf.csv'
    datasetIdAdd <- 2
  }
}

if(graphSimplified){
  osmNetworkPathGraph <- 'data/network/simplified/openstreetmapGraph.csv'
} else{
  osmNetworkPathGraph <- 'data/network/double/openstreetmapGraph.csv'
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
dataDefaultId <- 5
usecase5Df <- fread('data/usecase/dataset1.csv')
osmNetworkDf <- fread(osmNetworkPath)
osmNetworkDf <- osmNetworkDf %>%
  rename('YEAR' = 'DATE')
usecase5Df <- usecase5Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)


filePath <- paste('data/usecase/dataset', as.character((dataDefaultId+datasetIdAdd)), '.csv', sep='')
fwrite(x = usecase5Df, filePath)
rm(usecase5Df)
gc()
## Use Case 6 ------------------------------------------------------------------
dataDefaultId <- 9
usecase6Df <- fread('data/usecase/dataset2.csv')
osmNetworkDf <- fread(osmNetworkPath)
osmNetworkDf <- osmNetworkDf %>%
  rename('YEAR' = 'DATE')
usecase6Df <- usecase6Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)

filePath <- paste('data/usecase/dataset', as.character((dataDefaultId+datasetIdAdd)), '.csv', sep='')
fwrite(x = usecase6Df, filePath)
rm(usecase6Df)
gc()
## Use Case 7 ------------------------------------------------------------------
dataDefaultId <- 13
usecase7Df <- fread('data/usecase/dataset3.csv')
osmNetworkDf <- fread(osmNetworkPath)
osmNetworkDf <- osmNetworkDf %>%
  rename('YEAR' = 'DATE')
usecase7Df <- usecase7Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)
filePath <- paste('data/usecase/dataset', as.character((dataDefaultId+datasetIdAdd)), '.csv', sep='')
fwrite(x = usecase7Df, filePath)
rm(usecase7Df)
gc()
## Use Case 8 ------------------------------------------------------------------
dataDefaultId <- 17
usecase8Df <- fread('data/usecase/dataset4.csv')
osmNetworkDf <- fread(osmNetworkPath)
osmNetworkDf <- osmNetworkDf %>%
  rename('YEAR' = 'DATE')
usecase8Df <- usecase8Df %>%
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>%
  dplyr::left_join(osmNetworkDf, by=c("YEAR", "ID")) %>%
  dplyr::select(-YEAR)

filePath <- paste('data/usecase/dataset', as.character((dataDefaultId+datasetIdAdd)), '.csv', sep='')
fwrite(x = usecase8Df, filePath)
rm(usecase8Df)
gc()
# Network ----------------------------------------------------------------------
networkLandscapeSettings <- list(c('2010-01-01', '2012-12-01'),
                                 c('2013-01-01', '2015-12-01'),
                                 c('2016-01-01', '2018-12-01'),
                                 c('2019-01-01', '2021-12-01'))
## Use case 9 ------------------------------------------------------------------
monthYearDf <- data.frame(DATE = seq(from=as.Date('2010-01-01'), to=as.Date('2021-12-01'), by='months'))
monthYearDf <- monthYearDf %>%
  mutate(YEAR = lubridate::year(DATE))
# read in openstreetmap network
osmNetworkDf <- fread(osmNetworkPathGraph)
# expand year based dataframe to monthly base and select only distinct rows
osmNetworkDf <- osmNetworkDf %>%
  left_join(monthYearDf, by='YEAR') %>%
  dplyr::select(from, description, to, ID, DATE) %>%
  distinct()

# read in network from IDW interpolated data
idwNetworkDf <- fread('data/network/idwNetwork.csv')
# reorder columns and remove duplicate rows
idwNetworkDf <- idwNetworkDf %>%
  dplyr::select(from, description, to, ID, DATE) %>%
  distinct()
  
# read in landscape data
landscapeData <- fread('data/network/aggregateLandscapeEdgeDf.csv')
landscapeYears <- unique(landscapeData$YEAR)
landscapeList <- list()
for (iteration in 1:length(landscapeYears)){
  landscapeYear <- landscapeYears[iteration]
  landscapeSettingRow <- networkLandscapeSettings[[iteration]]
  print(landscapeYear)
  # extract startdate
  startDate <- as.Date(landscapeSettingRow[1])
  # extract enddate
  endDate <- as.Date(landscapeSettingRow[2])
  # create landscape sequence
  landscapeDateSeq <- seq(from=startDate, to=endDate, by='months')
  
  filteredLandscapeData <- landscapeData %>%
    filter(YEAR == landscapeYear) %>%
    distinct()
  
  # replicate landscape dataframe by date
  landscapeDateSeqRepl <- sort(rep(landscapeDateSeq, nrow(filteredLandscapeData)))
  
  filteredLandscapeData <- filteredLandscapeData %>%
    slice(rep(1:n(), length(landscapeDateSeq))) %>%
    mutate(DATE = landscapeDateSeqRepl)
  
  landscapeList <- c(landscapeList, list(filteredLandscapeData))
}

landscapeDf <- data.table::rbindlist(landscapeList)

landscapeDf <- landscapeDf %>%
  dplyr::select(from, description, to, ID, DATE) %>%
  distinct()

usecase9Df <- bind_rows(osmNetworkDf, idwNetworkDf, landscapeDf)

rm(list = ls()[!ls() %in% c('usecase9Df')])

gc()

### RDF2Vec ---------------------------------------------------------------------

uniqueDate <- unique(usecase10Df$DATE)
dir.create(path = 'data/usecase/usecase9RDF2Vec', showWarnings = FALSE)

for (date in uniqueDate){
  print(date)
  filteredDf <- usecase9Df %>%
    filter(DATE == date)
  fwrite(filteredDf, paste0('data/usecase/usecase9RDF2Vec/',as.IDate(date), '.csv'),)
}

# read in vector representation extracted from RDF2Vec approach
usecase9Df <- fread('data/network/usecase9/vectorDf.csv')
usecase9Df$DATE <- as.IDate(usecase9Df$DATE)

# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')
wildfire$DATE <- as.IDate(wildfire$DATE)

usecase9Df <- usecase9Df %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  dplyr::select(-geometry)

fwrite(usecase9Df, 'data/usecase/usecase9Vector.csv')

### STGNN ----------------------------------------------------------------------
# create edge index for each individual edge

# wildfire
# read wildfire dataset
wildfire <- readRDS('data/wildfire/wildfire.rds')

#transform sf dataframe to dataframe without geometry
st_geometry(wildfire) <- NULL
# change dataframe column Date to IDate format 
wildfire <- wildfire %>%
  mutate(DATE = as.IDate(DATE))

usecase9Df <- usecase9Df %>%
  mutate(DATE = as.IDate(DATE)) %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE))

uniqueCategories <- unique(usecase9Df$description)

uniqueDate <- unique(usecase9Df$DATE)
dir.create(path = 'data/usecase/usecase9STGNN', showWarnings = FALSE)

for (date in uniqueDate){
  print(as.IDate(date))
  filteredDf <- usecase9Df %>%
    filter(DATE == date)
  fwrite(filteredDf, paste0('data/usecase/usecase9STGNN/',as.IDate(date), '.csv'),)
}


rm(usecase9Df, landscapeDf, landscapeList, landscapeYears)

## Use Case 10 -----------------------------------------------------------------
monthYearDf <- data.frame(DATE = seq(from=as.Date('2010-01-01'), to=as.Date('2021-12-01'), by='months'))
monthYearDf <- monthYearDf %>%
  mutate(YEAR = lubridate::year(DATE))
# read in openstreetmap network
osmNetworkDf <- fread(osmNetworkPathGraph)
# expand year based dataframe to monthly base and select only distinct rows
osmNetworkDf <- osmNetworkDf %>%
  left_join(monthYearDf, by='YEAR') %>%
  dplyr::select(from, description, to, ID, DATE) %>%
  distinct()

# read in network from IDW interpolated data
idwNetworkDf <- fread('data/network/idwNetwork.csv')
# reorder columns and remove duplicate rows
idwNetworkDf <- idwNetworkDf %>%
  dplyr::select(from, description, to, ID, DATE) %>%
  distinct()

# read in landscape data
landscapeData <- fread('data/network/landscapeEdgeDf.csv')
landscapeYears <- unique(landscapeData$YEAR)
landscapeList <- list()
for (iteration in 1:length(landscapeYears)){
  landscapeYear <- landscapeYears[iteration]
  landscapeSettingRow <- networkLandscapeSettings[[iteration]]
  print(landscapeYear)
  # extract startdate
  startDate <- as.Date(landscapeSettingRow[1])
  # extract enddate
  endDate <- as.Date(landscapeSettingRow[2])
  # create landscape sequence
  landscapeDateSeq <- seq(from=startDate, to=endDate, by='months')
  
  filteredLandscapeData <- landscapeData %>%
    filter(YEAR == landscapeYear) %>%
    distinct()
  
  # replicate landscape dataframe by date
  landscapeDateSeqRepl <- sort(rep(landscapeDateSeq, nrow(filteredLandscapeData)))
  
  filteredLandscapeData <- filteredLandscapeData %>%
    slice(rep(1:n(), length(landscapeDateSeq))) %>%
    mutate(DATE = landscapeDateSeqRepl)
  
  landscapeList <- c(landscapeList, list(filteredLandscapeData))
}

landscapeDf <- data.table::rbindlist(landscapeList)

landscapeDf <- landscapeDf %>%
  mutate(ID = from) %>%
  dplyr::select(from, description, to, ID, DATE) %>%
  distinct()

rm(list = ls()[!ls() %in% c('osmNetworkDf', 'idwNetworkDf', 'landscapeDf')])

gc()

usecase10Df <- bind_rows(osmNetworkDf, idwNetworkDf, landscapeDf)

rm(list = ls()[!ls() %in% c('usecase10Df')])

gc()


uniqueDate <- unique(usecase10Df$DATE)
folderPath <- 'data/usecase/usecase10RDF2Vec'
dir.create(path = folderPath, showWarnings = FALSE)

for (date in uniqueDate){
  print(date)
  filteredDf <- usecase10Df %>%
    filter(DATE == date)
  fwrite(filteredDf, paste0(folderPath, '/', as.IDate(date), '.csv'),)
}

# read in vector representation extracted from RDF2Vec approach
usecase10Df <- fread('data/network/usecase10/vectorDf.csv')
usecase10Df$DATE <- as.IDate(usecase10Df$DATE)

# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')
wildfire$DATE <- as.IDate(wildfire$DATE)

usecase10Df <- usecase10Df %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  dplyr::select(-geometry)

fwrite(usecase10Df, 'data/usecase/dataset10.csv')
