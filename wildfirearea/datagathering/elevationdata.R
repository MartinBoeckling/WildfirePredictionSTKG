#------------------------------import packages----------------------------------
library(stars)
library(raster)
#------------------------------Script preparation-------------------------------
# specify base path
basePath <- '~/GitHub/wildfirearea/Data/Elevation/Tiff elevation/Single Files'
# define list of all single tif files and merge with base path
fileList <- list.files(path = basePath, pattern = '.tif', full.names = TRUE)
fileName <- paste0(getwd(), '/Data/Elevation/Tiff elevation/CaliforniaElevation.tif')
# merge file together with mosaic
californiaElevationFile <- st_mosaic(.x=fileList, dst = fileName, file_ext = 'tif')
californiaElevation <- raster('C:/Users/D070547/Documents/GitHub/wildfirearea/Data/Elevation/Tiff elevation/CaliforniaElevation.tif')
test <- projectRaster(californiaElevation, crs = crs('EPSG:3785'))
