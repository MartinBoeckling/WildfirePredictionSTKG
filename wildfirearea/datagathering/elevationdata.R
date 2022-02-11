#------------------------------import packages----------------------------------
library(stars)
library(raster)
#------------------------------Script preparation-------------------------------
# specify base path
basePath <- '~/GitHub/wildfirearea/data/elevation/Tiff elevation/Single Files'
# define list of all single tif files and merge with base path
fileList <- list.files(path = basePath, pattern = '.tif', full.names = TRUE)
fileName <- paste0(getwd(), '/data/elevation/Tiff elevation/ccaliforniaElevation.tif')
# merge file together with mosaic
californiaElevationFile <- stars::st_mosaic(.x=fileList,
                                            dst = fileName,
                                            file_ext = 'tif')
californiaElevationFile <- stars::read_stars('~/GitHub/wildfirearea/data/elevation/Tiff elevation/CaliforniaElevation.tif')
