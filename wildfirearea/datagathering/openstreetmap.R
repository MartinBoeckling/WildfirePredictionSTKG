library(osmdata)
library(readxl)
library(sf)
library(dplyr)


osmApi <- function(timeSpan, osmKey, osmValue, osmDate) {
  # create key folder
  key = osmKey[1]
  year = strsplit(osmDate[1], '-')[[1]][1]
  value = osmValue[1]
  print(paste(key, value, year, sep='-'))
  folderName <- paste('data/openstreetmap', key, value,  sep = '/')
  dir.create(folderName, showWarnings = FALSE, recursive = TRUE)
  osmObject <- opq(bbox=californiaBbox, timeout = timeSpan, datetime = osmDate) %>% 
    # define open street map features to be extracted
    add_osm_feature(key = osmKey, value = osmValue) %>% 
    # transform object to simple feature object
    osmdata_sf()
  
  fileName <- paste0(folderName, '/', value, year, '.rds')
  saveRDS(osmObject, file = fileName)
  # return value of query
}

californiaBoundary <- sf::st_read('data/californiaBoundary/CA_State_TIGER2016.shp')
prjLonLat <- 'EPSG:4269'
californiaBoundary <- sf::st_transform(californiaBoundary, crs = prjLonLat)
californiaBbox <- sf::st_bbox(californiaBoundary)
osmSettings <- read_excel('data/openstreetmap/OSMSettings2010.xlsx')
osmSettings <- osmSettings %>%
  dplyr::filter(grepl('2016|2017|2018|2019|2020|2021', osmDate))
osmSettings <- osmSettings[96:198,]
apply(osmSettings, 1,
      function(x) osmApi(1200, x['osmKey'], x['osmValue'], x['osmDate']))

