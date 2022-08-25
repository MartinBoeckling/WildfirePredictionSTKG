# import packages --------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RSQLite)
library(sf)
library(tidyr)
library(dbplyr)
library(visNetwork)
library(stars)

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

# Script parameters ------------------------------------------------------------
californiaBoundary <- sf::st_read('data/californiaBoundary/CA_State_TIGER2016.shp')
colorPalette <- grDevices::grey.colors(n=25)
# list all landcover files in a list
landcoverList <- list.files('data/landCover/polygon', full.names = TRUE)
# Wildfire visualization -------------------------------------------------------
wildfire <- readRDS('data/wildfire/wildfire.rds')
ggplot(data=wildfire) +
  geom_sf(fill = colorPalette[1], lwd=0.05) +
  geom_sf(data = californiaBoundary, fill = NA) +
  ylab('Longitude') +
  xlab('Latitude') +
  theme_minimal()

# weather ----------------------------------------------------------------------
tmax <- readRDS('data/interpolation/TMAX.rds')
tmax <- as.data.frame(tmax) %>%
  select(c('TMAX', 'DATE', 'ID')) %>%
  filter(DATE == '2010-07-01') %>%
  left_join(hexGridSf, by='ID') %>%
  select(c('TMAX', 'DATE', 'ID', 'geometry')) %>%
  st_as_sf()

ggplot(data=tmax) +
  geom_sf(color = NA, aes(fill = TMAX), lwd=0.0) +
  geom_sf(data = californiaBoundary, fill = NA) +
  scale_fill_continuous(low=colorPalette[1], high=colorPalette[25]) +
  ylab('Longitude') +
  xlab('Latitude') +
  theme_minimal()

prcp <- readRDS('data/interpolation/tmin.rds')
prcp <- as.data.frame(prcp) %>%
  select(c('PRCP', 'DATE', 'ID')) %>%
  filter(DATE == '2010-01-01') %>%
  left_join(hexGridSf, by='ID') %>%
  select(c('PRCP', 'DATE', 'ID', 'geometry')) %>%
  st_as_sf()

ggplot(data=prcp) +
  geom_sf(color = NA, aes(fill = PRCP), lwd=0.0) +
  geom_sf(data = californiaBoundary, fill = NA) +
  scale_fill_continuous(low=colorPalette[1], high=colorPalette[25]) +
  ylab('Longitude') +
  xlab('Latitude') +
  theme_minimal()

# network visualization --------------------------------------------------------
openstreetmapGraphEdge <- readRDS('data/network/edgeDataframe.rds')
openstreetmapGraphEdge <- openstreetmapGraphEdge %>%
  rename('title'='description')
nodeCandidates <- unique(c(unique(openstreetmapGraphEdge$from), unique(openstreetmapGraphEdge$to)))
openstreetmapGraphNode <- data.frame(id = nodeCandidates, label= nodeCandidates)
openstreetmapGraphNode <- openstreetmapGraphNode[1:1000, ]
openstreetmapGraphEdge <- openstreetmapGraphEdge %>%
  filter(from %in% openstreetmapGraphNode$id & to %in% openstreetmapGraphNode$id)
visualizationNetwork <- visNetwork(openstreetmapGraphNode, openstreetmapGraphEdge, width='100%') %>%
  visEdges(arrows = "from") %>%
  visPhysics(solver = "forceAtlas2Based", stabilization = FALSE) %>%
  visConfigure(enabled = TRUE, filter='physics') %>%
  visOptions(highlightNearest = TRUE)

visSave(visualizationNetwork, file = "data/network/osmNetwork.html", selfcontained = TRUE)

# OpenStreetMap Visualization --------------------------------------------------
osmNetwork <- readRDS('data/openstreetmap/highway/primary/primary2021.osm')
osmNetwork <- osmNetwork$osm_lines
ggplot() +
  geom_sf(data=osmNetwork) +
  theme_minimal()


# Grid visualization -----------------------------------------------------------
californiaBoundary <- sf::st_read('data/californiaBoundary/CA_State_TIGER2016.shp')
californiaBoundary <- sf::st_transform(californiaBoundary, crs=prjLonLat)
californiaSP <- sf::as_Spatial(californiaBoundary)
prjLonLat <- 'EPSG:4269'
# calculate center points of hexagons in spatial area
set.seed(15)
hexGridCentroids <- sp::spsample(californiaSP, type='hexagonal', n = 500)
hexGrid <- sp::HexPoints2SpatialPolygons(hexGridCentroids)
hexGrid <- sp::spTransform(hexGrid, prjLonLat)

hexGrid <- sf::st_as_sf(hexGrid)

ggplot() +
  geom_sf(data=californiaBoundary, fill='white') +
  geom_sf(data=hexGrid, fill=NA, color='darkgrey', lwd=1) +
  theme_minimal()


# Elevation visualization ------------------------------------------------------
ggplot() +
  geom_sf(data=hexGridSf, aes(fill=ELEVATION, colour=ELEVATION)) +
  scale_fill_continuous(name='Elevation in meters', low=colorPalette[1], high=colorPalette[25]) +
  scale_colour_continuous(name='Elevation in meters', low=colorPalette[1], high=colorPalette[25]) +
  theme_minimal()

# Wildfire ---------------------------------------------------------------------
wildfireData <- readRDS('data/wildfire/wildfire.rds')

ggplot() +
  geom_sf(data=californiaBoundary, fill='white') +
  geom_sf(data  = wildfireData, aes(fill=DATE, colour=DATE), size=0) +
  scale_fill_date(name='Date', low=colorPalette[1], high=colorPalette[25]) +
  scale_colour_date(name='Date', low=colorPalette[1], high=colorPalette[25]) +
  theme_minimal()

# Wildfire reasoning -----------------------------------------------------------
connection <- dbConnect(drv=RSQLite::SQLite(), 'C:/Users/D070547/Downloads/RDS-2013-0009.5_SQLITE/Data/FPA_FOD_20210617.sqlite')
## list all tables
fires <- tbl(connection, "Fires") %>% collect()

# Landscape --------------------------------------------------------------------
landscapeData <- readRDS("data/landCover/polygon/PolygonLayernlcd2011.rds")
landscapeData <- landscapeData %>%
  pivot_longer(!ID, names_to='LANDCOVER', values_to = 'area') %>%
  group_by(ID) %>%
  filter(area == max(area)) %>%
  dplyr::select(-area)

landscapeData <- landscapeData %>%
  left_join(hexGridSf, by='ID')
landscapeData <- st_as_sf(landscapeData)

ggplot() +
  geom_sf(data  = landscapeData, aes(fill=LANDCOVER, colour=LANDCOVER), size=0) +
  scale_fill_discrete(name='Landcover') +
  scale_colour_discrete(name='Landcover') +
  theme_minimal()
