# load packages for spatial operations
library(sf)
library(sp)

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
california_elevation <- get_elev_raster(california, prj = prj_dd, z = 8)
plot(california_elevation)
plot(california_sp, add=TRUE)
