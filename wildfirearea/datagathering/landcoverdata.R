library(FedData)
library(sf)
library(raster)
library(stars)
# read in california boundary from shapefile
californiaBoundary <- st_read('~/Github/wildfirearea/data/californiaBoundary/CA_State_TIGER2016.shp')
californiaSpol <- as_Spatial(californiaBoundary)
californiaSpol <- spTransform(californiaSpol, CRS(prjLonLat))

# eligible years for landcover data
years <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)

for (year in years) {
  filePath <- paste0('~/Github/wildfirearea/data/landCover/nlcd', year, '.tif')
  nlcdRaster <- get_nlcd(template = californiaSpol, 
                         label = 'California', 
                         year = year)
  writeRaster(nlcdRaster, filePath)
  print(year)
}

