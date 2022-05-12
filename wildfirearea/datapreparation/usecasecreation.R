# import packages
library(data.table)
library(dplyr)
library(sf)
library(tidyr)

# Base Case --------------------------------------------------------------------
# list all IDW Interpolation files
idwInterpolationList <- list.files('data/interpolation/idw', full.names = TRUE)
# list all landcover files in a list
landcoverList <- list.files('data/landCover/polygon', full.names = TRUE)
# create monthly date sequence for date generation
monthlyDateSequence <- seq(from=as.Date('2010-01-01'), to=as.Date('2021-12-31'), by='months')

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

landscapeDf <- data.frame()
# aggregate landscape dataframe
landscapeDates <- as.numeric(gsub(".*?([0-9]+).*", "\\1", landcoverList))
for (landscapeIteration in 1:length(landcoverList)) {
  print(paste(landscapeIteration, 'of', length(landcoverList), 'iteration'))
  landscapePath <- landcoverList[landscapeIteration]
  landscapeData <- readRDS(landscapePath)
  landscapeDate <- landscapeDates[landscapeIteration]
  if (landscapeIteration == 1){
    startDate <- monthlyDateSequence[1]
    endDate <- as.Date(paste(landscapeDates[landscapeIteration+1]-1, '12', '31', sep='-'))
  }else if (landscapeIteration == length(landcoverList)){
    startDate <- as.Date(paste(landscapeDates[landscapeIteration], '01', '01', sep='-'))
    endDate <- monthlyDateSequence[length(monthlyDateSequence)]
  } else{
    startDate <- as.Date(paste(landscapeDates[landscapeIteration], '01', '01', sep='-'))
    endDate <- as.Date(paste(landscapeDates[landscapeIteration+1]-1, '12', '31', sep='-'))
  }
  landscapeDateSeq <- seq(from=startDate, to=endDate, by='months')
  
  landscapeDateSeqRepl <- rep(landscapeDateSeq, nrow(landscapeData))
  landscapeData <- landscapeData %>%
    pivot_longer(!ID, names_to='LANDCOVER', values_to = 'area') %>%
    group_by(ID) %>%
    filter(area == max(area)) %>%
    slice(rep(1:n(), length(landscapeDateSeq))) %>%
    dplyr::select(-area)
  
  landscapeData$DATE <- landscapeDateSeqRepl
  landscapeDf <- rbind(landscapeDf, landscapeData)
}

# wildfire
wildfire <- readRDS('data/wildfire/wildfire.rds')
usecase1Df <- weatherDf %>%
  left_join(wildfire, by=c('ID', 'DATE')) %>%
  mutate(WILDFIRE = ifelse(is.na(WILDFIRE), 0, WILDFIRE)) %>%
  left_join(landscapeDf, by=c('ID', 'DATE')) %>%
  dplyr::select(-contains('geometry'))

fwrite(usecase1Df, 'data/usecase/usecase1.csv')

  
