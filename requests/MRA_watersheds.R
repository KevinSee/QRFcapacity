# Author: Kevin See
# Purpose: Get QRF estimates for Salmon MPG
# Created: 4/13/2020
# Last Modified: 4/13/2020
# Notes: These are for the MRA report

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(QRFcapacity)

# set default theme for ggplot
theme_set(theme_bw())

#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds')[2]

load(paste0('output/modelFits/extrap_mastPts_', mod_choice, '.rda'))
data("gaa")
data("chnk_domain")
data("sthd_domain")

site_cap = all_preds %>%
  left_join(gaa %>%
              select(Site, 
                     HUC_6, HUC6NmNRCS, HUC_8, HUC8NmNRCS, HUC_10, HUC10NmNRC, HUC_12, HUC12NmNRC,
                     Lat, Lon)) %>%
  select(Site, starts_with('HUC'), everything()) %>%
  filter(!is.na(Lon), !is.na(Lat)) %>%
  st_as_sf(coords = c('Lon', 'Lat'),
           crs = 4326) %>%
  st_transform(crs = st_crs(chnk_domain))

#-----------------------------------------------------------------
# filter QRF capacities and species' domains to the upper Salmon area
ups_cap = site_cap %>%
  filter(HUC6NmNRCS == 'Salmon',
         HUC8NmNRCS %in% c('Lemhi',
                           'Pahsimeroi') |
           (HUC8NmNRCS == 'Upper Salmon' &
              ! HUC10NmNRC %in% c('Valley Creek', 
                                  'Yankee Fork')),
  )

ups_chnk_dom = chnk_domain %>%
  filter(MPG == 'Upper Salmon River') %>%
  filter(grepl('Lemhi', NWR_NAME) |
           grepl('Pahsimeroi', NWR_NAME) |
           grepl('above Redfish Lake', NWR_NAME))

ups_sthd_dom = sthd_domain %>%
  filter(MPG == 'Salmon River') %>%
  filter(grepl('Lemhi', NWR_NAME) |
           grepl('Pahsimeroi', NWR_NAME) |
           grepl('Salmon River Upper Mainstem', NWR_NAME))

#-----------------------------------------------------------------
# determine if each point is in each species' domain

ups_chnk_buff = ups_chnk_dom %>%
  select(chnk_ESU_DPS = ESU_DPS,
         chnk_MPG = MPG,
         chnk_NWR_POPID = NWR_POPID,
         chnk_NWR_NAME = NWR_NAME,
         chnk_use = UseType) %>%
  st_buffer(dist = 200,
            endCapStyle = 'FLAT')

ups_sthd_buff = ups_sthd_dom %>%
  select(sthd_ESU_DPS = ESU_DPS,
         sthd_MPG = MPG,
         sthd_NWR_POPID = NWR_POPID,
         sthd_NWR_NAME = NWR_NAME,
         sthd_use = UseType) %>%
  st_buffer(dist = 200,
            endCapStyle = 'FLAT')

ups_cap %<>%
  st_join(ups_chnk_buff,
          largest = T)

ups_cap %<>%
  st_join(ups_sthd_buff,
          largest = T)

# save it
# as GPKG
st_write(ups_cap,
         dsn = paste0('output/gpkg/UpperSalmon_MastPts_Cap_', mod_choice, '.gpkg'),
         driver = 'GPKG')
