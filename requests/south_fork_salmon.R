# Author: Kevin See
# Purpose: Get QRF estimates for South Fork Salmon steelhead and Chinook
# Created: 9/22/2020
# Last Modified: 9/22/2020
# Notes: Ryan Kinzer requested this for the Nez Perce in response to Midas mine project

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
# library(janitor)
# library(magrittr)
library(sf)
# library(QRFcapacity)

#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds')[2]

rch_200_cap = st_read(paste0('output/gpkg/Rch_Cap_', mod_choice, '.gpkg'))

# clip out south fork salmon, by Ryan's request
sf_salm = rch_200_cap %>%
  filter(grepl("South Fork Salmon River", sthd_NWR_NAME) |
           grepl("Secesh", sthd_NWR_NAME))

sf_salm %>%
  select(chnk_NWR_NAME) %>%
  plot()

#save as shapefile
st_write(sf_salm,
         dsn = paste0('output/shapefiles/Rch_Cap_', mod_choice, '_South_Fork_Salmon.shp'),
         driver = 'ESRI Shapefile',
         append = F)

# save as geopackage
st_write(sf_salm,
         dsn = paste0('output/shapefiles/Rch_Cap_', mod_choice, '_South_Fork_Salmon.gpkg'),
         driver = 'GPKG',
         append = F)

names(sf_salm)
