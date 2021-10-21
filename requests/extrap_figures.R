# Author: Kevin See
# Purpose: Make some extrapolation figures
# Created: 10/20/2021
# Last Modified: 10/20/2021
# Notes: Initially created for a presentation for BPA

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(QRFcapacity)
library(here)

# set default theme for ggplot
theme_set(theme_bw())

#-----------------------------------------------------------------
# read in Chinook capacity estimates
cap_df = here("output/gpkg/QRF_estimates",
              "StreamNetwork_Capacity_chnk.gpkg") %>%
  st_read()


#-----------------------------------------------------------------
# watershed boundaries
huc12_sf = here("data/raw/watershed_boundaries",
                'WBDHU12.shp') %>%
  st_read() %>%
  st_transform(st_crs(cap_df))

# pull out Grande Ronde
wtsd_huc_sf = huc12_sf %>%
  filter(str_detect(HUC12,
                    "^17060104"))

wtsd_bndry = wtsd_huc_sf %>%
  st_union() %>%
  nngeo::st_remove_holes()

# clip capacity layer
wtsd_cap = cap_df %>%
  st_intersection(wtsd_bndry)

# pick a background river color
river_color = "lightskyblue1"


chnk_sum_map = wtsd_cap %>%
  ggplot() +
  geom_sf(data = wtsd_bndry,
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = wtsd_cap %>%
            filter(spp_domain),
          aes(color = sum_juv_per_m2),
          size = 1) +
  theme(legend.position = "bottom") +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Chinook - Upper Grande Ronde",
       color = expression(`Summer Juv.` / m^2))

chnk_sum_map
ggsave(filename = '~/Desktop/gr_chnk_map.pdf',
       chnk_sum_map,
       width = 6,
       height = 6)


chnk_sum_cap = calc_watershed_cap(wtsd_bndry,
                                  wtsd_cap,
                                  capacity_name = "sum_juv_per_m",
                                  capacity_se_name = "sum_juv_per_m_se",
                                  by_stream = T)

wtsd_cap %>%
  st_drop_geometry() %>%
  filter(spp_domain) %>%
  mutate(cap_tot = win_juv_per_m * reach_leng,
         cap_tot_se = win_juv_per_m_se * reach_leng) %>%
  group_by(NWR_NAME) %>%
  summarize(km = sum(reach_leng) / 1000,
            cap = sum(cap_tot),
            cap_se = sqrt(sum(cap_tot_se^2)),
            cap_cv = cap_se / cap)



#-----------------------------------------------------------------
# map habitat sites vs. map
mod_choice = c('juv_summer',
                'juv_summer_dash',
                'redds',
                'juv_winter')[1]
                  
load(paste0('output/modelFits/extrap_200rch_RF_', mod_choice, '.rda'))

lem_pts = model_rf_df$data[[1]] %>%
  filter(Watershed == "Lemhi") %>%
  st_as_sf(coords = c("LON_DD", "LAT_DD"),
           crs = 4326) %>%
  st_transform(st_crs(cap_df))


# pull out Lemhi
lem_bndry = huc12_sf %>%
  filter(str_detect(HUC12,
                    "^17060204")) %>%
  st_union() %>%
  nngeo::st_remove_holes()

# clip capacity layer
lem_cap = cap_df %>%
  st_intersection(lem_bndry)


lem_map1 = lem_cap %>%
  ggplot() +
  geom_sf(data = lem_bndry,
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = lem_pts,
          aes(color = qrf_cap * Lgth_Wet / Area_Wet),
          size = 4) +
  theme(legend.position = "bottom") +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Habitat Sites",
       color = expression(`Summer Juv.` / m^2))

lem_map2 = lem_cap %>%
  ggplot() +
  geom_sf(data = lem_bndry,
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = lem_cap %>%
            filter(spp_domain),
          aes(color = sum_juv_per_m2),
          size = 1) +
  theme(legend.position = "bottom") +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Capacity Extrapolation",
       color = expression(`Summer Juv.` / m^2))

lem_map = ggpubr::ggarrange(plotlist = list(lem_map1,lem_map2),
                  ncol = 2,
                  nrow = 1,
                  legend = "bottom",
                  common.legend = T)


wen_pts = model_rf_df$data[[1]] %>%
  filter(Watershed == "Wenatchee") %>%
  st_as_sf(coords = c("LON_DD", "LAT_DD"),
           crs = 4326) %>%
  st_transform(st_crs(cap_df))


# pull out Lemhi
wen_bndry = huc12_sf %>%
  filter(str_detect(HUC12,
                    "^17020011")) %>%
  st_union() %>%
  nngeo::st_remove_holes()

# clip capacity layer
wen_cap = cap_df %>%
  st_intersection(wen_bndry)


wen_map1 = wen_cap %>%
  ggplot() +
  geom_sf(data = wen_bndry,
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = wen_pts,
          aes(color = qrf_cap * Lgth_Wet / Area_Wet),
          size = 4) +
  theme(legend.position = "bottom",
        axis.text = element_blank()) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Habitat Sites",
       color = expression(`Summer Juv.` / m^2))

wen_map2 = wen_cap %>%
  ggplot() +
  geom_sf(data = wen_bndry,
          fill = NA,
          color = "gray50") +
  geom_sf(color = river_color) +
  geom_sf(data = wen_cap %>%
            filter(spp_domain),
          aes(color = sum_juv_per_m2),
          size = 1) +
  theme(legend.position = "bottom",
        axis.text = element_blank()) +
  scale_color_viridis_c(direction = -1) +
  labs(title = "Capacity Extrapolation",
       color = expression(`Summer Juv.` / m^2))

wen_map = ggpubr::ggarrange(plotlist = list(wen_map1,
                                            wen_map2),
                            ncol = 2,
                            nrow = 1,
                            legend = "bottom",
                            common.legend = T)

wen_map
ggsave(filename = '~/Desktop/wen_maps.pdf',
       wen_map,
       width = 7,
       height = 6)

ggsave(filename = '~/Desktop/wen_map_pts.pdf',
       wen_map1,
       width = 4,
       height = 6)
