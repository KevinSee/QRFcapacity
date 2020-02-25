# Author: Kevin See
# Purpose: Get QRF estimates for upper Salmon Chinook redds
# Created: 2/11/2020
# Last Modified: 2/25/2020
# Notes: Request from Chris Jordan

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(QRFcapacity)

# set default theme for ggplot
theme_set(theme_bw())
data("chnk_domain")
data("gaa")
#-----------------------------------------------------------------
# read in all QRF Chinook redd estimates and filter for the Upper Salmon
upSalm_pts = st_read('output/gpkg/Redds_Capacity.gpkg') %>%
  mutate(Lifestage = 'Redds') %>%
  filter(HUC6NmNRCS %in% c('Salmon')) %>%
  rbind(st_read('output/gpkg/Sum_Juv_Capacity.gpkg') %>%
          mutate(Lifestage = 'Summer Juvenile') %>%
          filter(HUC6NmNRCS %in% c('Salmon'))) %>%
  rbind(st_read('output/gpkg/Win_Juv_Capacity.gpkg') %>%
          mutate(Lifestage = 'Summer Juvenile') %>%
          filter(HUC6NmNRCS %in% c('Salmon'))) %>%
  select(Lifestage, everything()) %>%
  st_transform(st_crs(chnk_domain))

st_crs(upSalm_pts)$epsg = 5070

# add the ComID from GAAs
upSalm_pts %<>%
  left_join(gaa %>%
              select(Site, COMID_V1))

# save file
st_write(upSalm_pts,
         dsn = 'outgoing/shapefiles/QRF_Capacity_Salmon.gpkg',
         driver = 'GPKG',
         delete_layer = T)

# save file
st_write(upSalm_pts,
         dsn = 'outgoing/shapefiles/QRF_Capacity_Salmon.shp',
         driver = 'ESRI Shapefile',
         delete_layer = T)

