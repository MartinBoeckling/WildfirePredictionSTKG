# import packages --------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)
# Script parameters ------------------------------------------------------------
californiaBoundary <- sf::st_read('data/californiaBoundary/CA_State_TIGER2016.shp')
colorPalette <- grDevices::grey.colors(n=25)
# Wildfire visualization -------------------------------------------------------
wildfire <- readRDS('data/wildfire/wildfire.rds')
ggplot(data=wildfire) +
  geom_sf(fill = colorPalette[1], lwd=0.05) +
  geom_sf(data = californiaBoundary, fill = NA) +
  ylab('Longitude') +
  xlab('Latitude') +
  theme_minimal()

# weather ----------------------------------------------------------------------
tmax <- readRDS('data/interpolation/idw/TMAX.rds')
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

prcp <- readRDS('data/interpolation/idw/PRCP.rds')
prcp <- as.data.frame(prcp) %>%
  select(c('PRCP', 'DATE', 'ID')) %>%
  filter(DATE == '2010-07-01') %>%
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