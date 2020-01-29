# Author: Kevin See
# Purpose: Get QRF estimates for upper Columbia
# Created: 1/15/2020
# Last Modified: 1/15/2020
# Notes: Greer wanted these by population for the UC RTT

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

#-----------------------------------------------------------------
# read in all QRF estimates and filter for the Upper Columbia
uc_pts = st_read('output/gpkg/Sum_Juv_Capacity.gpkg') %>%
  mutate(Lifestage = 'Summer Juv.') %>%
  rbind(st_read('output/gpkg/Win_Juv_Capacity.gpkg') %>%
          mutate(Lifestage = 'Winter Juv.')) %>%
  rbind(st_read('output/gpkg/Redds_Capacity.gpkg') %>%
          mutate(Lifestage = 'Redds')) %>%
  filter(HUC8NmNRCS %in% c('Wenatchee', 'Methow', 'Okanogan') |
           HUC10NmNRC %in% c('Entiat River', 'Mad River')) %>%
  select(Lifestage, everything()) %>%
  st_transform(st_crs(chnk_domain))

# get HUC 12 polygons
huc12_sf = st_read('/Users/seek/OneDrive - Merck Sharp & Dohme, Corp/Data/WatershedBoundaries/WBDHU12.shp') %>%
  st_transform(st_crs(chnk_domain))

ent_huc12 = huc12_sf %>%
  filter(grepl("1702001001", HUC12) |
           grepl("1702001002", HUC12))

wen_huc12 = huc12_sf %>%
  filter(grepl("17020011", HUC12)) %>%
  mutate_at(vars(NAME),
            list(fct_drop))

met_huc12 = huc12_sf %>%
  filter(grepl("17020008", HUC12)) %>%
  mutate_at(vars(NAME),
            list(fct_drop))

# pull out points for each population
wen_pts = uc_pts %>%
  st_join(wen_huc12 %>%
            select(HUC12, NAME, AREASQKM)) %>%
  filter(!is.na(NAME))

met_pts = uc_pts %>%
  st_join(met_huc12 %>%
            select(HUC12, NAME, AREASQKM)) %>%
  filter(!is.na(NAME))

ent_pts = uc_pts %>%
  st_join(ent_huc12 %>%
            select(HUC12, NAME, AREASQKM)) %>%
  filter(!is.na(NAME))

st_crs(wen_pts)$epsg = 5070
st_crs(met_pts)$epsg = 5070
st_crs(ent_pts)$epsg = 5070

# st_write(ent_pts,
#          dsn = 'outgoing/shapefiles/QRF_Capacity_Entiat.shp',
#          driver = 'ESRI Shapefile',
#          delete_layer = T)

st_write(ent_pts,
         dsn = 'outgoing/shapefiles/QRF_Capacity_Entiat.gpkg',
         driver = 'GPKG',
         delete_layer = T)

st_write(wen_pts,
         dsn = 'outgoing/shapefiles/QRF_Capacity_Wenatchee.gpkg',
         driver = 'GPKG',
         delete_layer = T)

st_write(met_pts,
         dsn = 'outgoing/shapefiles/QRF_Capacity_Methow.gpkg',
         driver = 'GPKG',
         delete_layer = T)



ent_huc12_cap = ent_huc12 %>%
  mutate_at(vars(NAME),
            list(fct_drop)) %>%
  split(list(.$NAME)) %>%
  map_df(.id = "NAME",
         .f = function(x) {
           ent_pts %>%
             filter(chnk == 'Yes' | steel == 'Yes') %>%
             split(list(.$Lifestage)) %>%
             map_df(.id = 'Lifestage',
                    .f = function(y) {
                      calc_watershed_cap(x,
                                         chnk_domain,
                                         y)
                      
                    })
         }) %>%
  mutate(Species = 'Chinook') %>%
  bind_rows(ent_huc12 %>%
              mutate_at(vars(NAME),
                        list(fct_drop)) %>%
              split(list(.$NAME)) %>%
              map_df(.id = "NAME",
                     .f = function(x) {
                       ent_pts %>%
                         filter(chnk == 'Yes' | steel == 'Yes') %>%
                         split(list(.$Lifestage)) %>%
                         map_df(.id = 'Lifestage',
                                .f = function(y) {
                                  calc_watershed_cap(x,
                                                     sthd_domain,
                                                     y,
                                                     capacity_name = 'sthd_per_m',
                                                     capacity_se_name = 'sthd_per_m_se')
                                  
                                })
                     }) %>%
              mutate(Species = 'Steelhead')) %>%
  select(Species, everything())

wen_huc12_cap = wen_huc12 %>%
  mutate_at(vars(NAME),
            list(fct_drop)) %>%
  split(list(.$NAME)) %>%
  map_df(.id = "NAME",
         .f = function(x) {
           wen_pts %>%
             filter(chnk == 'Yes' | steel == 'Yes') %>%
             split(list(.$Lifestage)) %>%
             map_df(.id = 'Lifestage',
                    .f = function(y) {
                      calc_watershed_cap(x,
                                         chnk_domain,
                                         y)
                      
                    })
         }) %>%
  mutate(Species = 'Chinook') %>%
  bind_rows(wen_huc12 %>%
              mutate_at(vars(NAME),
                        list(fct_drop)) %>%
              split(list(.$NAME)) %>%
              map_df(.id = "NAME",
                     .f = function(x) {
                       wen_pts %>%
                         filter(chnk == 'Yes' | steel == 'Yes') %>%
                         split(list(.$Lifestage)) %>%
                         map_df(.id = 'Lifestage',
                                .f = function(y) {
                                  calc_watershed_cap(x,
                                                     sthd_domain,
                                                     y,
                                                     capacity_name = 'sthd_per_m',
                                                     capacity_se_name = 'sthd_per_m_se')
                                  
                                })
                     }) %>%
              mutate(Species = 'Steelhead')) %>%
  select(Species, everything())

met_huc12_cap = met_huc12 %>%
  mutate_at(vars(NAME),
            list(fct_drop)) %>%
  split(list(.$NAME)) %>%
  map_df(.id = "NAME",
         .f = function(x) {
           met_pts %>%
             filter(chnk == 'Yes' | steel == 'Yes') %>%
             split(list(.$Lifestage)) %>%
             map_df(.id = 'Lifestage',
                    .f = function(y) {
                      calc_watershed_cap(x,
                                         chnk_domain,
                                         y)
                      
                    })
         }) %>%
  mutate(Species = 'Chinook') %>%
  bind_rows(met_huc12 %>%
              mutate_at(vars(NAME),
                        list(fct_drop)) %>%
              split(list(.$NAME)) %>%
              map_df(.id = "NAME",
                     .f = function(x) {
                       met_pts %>%
                         filter(chnk == 'Yes' | steel == 'Yes') %>%
                         split(list(.$Lifestage)) %>%
                         map_df(.id = 'Lifestage',
                                .f = function(y) {
                                  calc_watershed_cap(x,
                                                     sthd_domain,
                                                     y,
                                                     capacity_name = 'sthd_per_m',
                                                     capacity_se_name = 'sthd_per_m_se')
                                  
                                })
                     }) %>%
              mutate(Species = 'Steelhead')) %>%
  select(Species, everything())

# save tables 
wen_huc12_cap %>%
  filter(!is.na(n_pts)) %>%
  mutate_at(vars(tot_cap, tot_cap_se),
            list(round)) %>%
  mutate_at(vars(area, tot_length),
            list(round),
            digits = 2) %>%
  write_csv('outgoing/Wenatchee_QRF_capacity_table.csv')

met_huc12_cap %>%
  filter(!is.na(n_pts)) %>%
  mutate_at(vars(tot_cap, tot_cap_se),
            list(round)) %>%
  mutate_at(vars(area, tot_length),
            list(round),
            digits = 2) %>%
  write_csv('outgoing/Methow_QRF_capacity_table.csv')

ent_huc12_cap %>%
  filter(!is.na(n_pts)) %>%
  mutate_at(vars(tot_cap, tot_cap_se),
            list(round)) %>%
  mutate_at(vars(area, tot_length),
            list(round),
            digits = 2) %>%
  write_csv('outgoing/Entiat_QRF_capacity_table.csv')

