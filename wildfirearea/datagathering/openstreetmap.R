library(osmdata)
library(readxl)



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
  
  fileName <- paste0(folderName, '/', value, year, '.osm')
  saveRDS(osmObject, file = fileName)
  # return value of query
}

osmSettings <- read_excel('data/openstreetmap/OSMSettings.xlsx')
osmSettings <- osmSettings[465:544,]
apply(osmSettings, 1,
      function(x) osmApi(900, x['osmKey'], x['osmValue'], x['osmDate']))
