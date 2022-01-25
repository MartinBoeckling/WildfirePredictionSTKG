#----------------------------Introduction-------------------------------
'This file represents the generation and preparation of the spatial data being 
part of the data preparation. The goal of this script is to transform the data
in a format appropriate to the '

# load packages for spatial operations
library(sf)
library(sp)
library(raster)

#-------------------- Basic parameters-------------------------
prj_dd <- 'EPSG:3785'
#--------------------Create base map----------------------------
# load California map from ggplot
california_boundary <- st_read('~/Document/Github/wildfirearea/data/spatial data/california boundary/CA_State_TIGER2016.shp')
california_spol <- as_Spatial(california_boundary)

# create hexagonal grid
# area in square meters
area <- 10000000
'calculation of cellsize based on cellsize argument of spsample
which defines the distance between the center of consecutives hexagons'
cellSize <- 2 * sqrt(area/((3*sqrt(3)/2))) * sqrt(3)/2

hex_points <- spsample(california_spol, type='hexagonal', cellsize=cellSize)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = cellSize)

plot(california_spol, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T, lwd=0.25)
#----------------------Elevation-------------------------
# read elevation
rasterElevation <- raster('~/GitHub/wildfirearea/Data/Elevation/90 m DEM of California, USA/commondata/data0/ca_dem/w001001.adf')
# transform CRS to standard 
rasterElevation <- projectRaster(from=rasterElevation, crs = prj_dd)
# extract values from raster to hexagonal grid
elevationDf <- raster::extract(x = rasterElevation, y = hex_grid, fun = mean, na.rm = TRUE, cellnumbers = TRUE, df = TRUE)
# change values of ID to matching items in data frame
rownames(elevationDf) <- paste('ID', rownames(elevationDf), sep='')
# combine dataframe to spatial polygons data
hexGridElevation <- SpatialPolygonsDataFrame(hex_grid, elevationDf)
