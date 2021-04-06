# Author: Kevin See
# Purpose: Get QRF estimates for all IRA watersheds
# Created: 4/3/2021
# Last Modified: 4/3/2021
# Notes: Need to confirm spatial extents used in the IRA for each watershed

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
# 200 m Reach Extrapolation
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds',
               "juv_winter")[1]

# load(paste0('output/modelFits/extrap_200rch_RF_', mod_choice, '.rda'))
# data("rch_200")

rch_cap = st_read(paste0("output/gpkg/Rch_Cap_RF_", mod_choice,
                         ".gpkg"))


#-----------------------------------------------------------------
# pull out areas in the IRA report
rch_cap %>%
  filter(chnk_MPG == "Upper Salmon River") %>%
  xtabs(~ chnk_NWR_NAME, .)

ira_list = list("East Fork Salmon" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - East Fork Salmon River",
                         chnk),
                "Valley Creek" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - Valley Creek",
                         chnk),
                "Yankee Fork" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - Yankee Fork",
                         chnk),
                "Nork Fork Salmon" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - North Fork Salmon River",
                         GNIS_Name %in% c("Dahlonega Creek",
                                          "Hughes Creek",
                                          # "Moose Creek",
                                          "North Fork Salmon River",
                                          "Sheep Creek",
                                          "Twin Creek"),
                         chnk),
                "Lemhi" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - Lemhi River",
                         chnk),
                "Pahsimeroi" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - Pahsimeroi River",
                         chnk),
                "Panther Creek" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - Panther Creek",
                         GNIS_Name %in% c("Big Deer Creek",
                                          "Blackbird Creek",
                                          "Clear Creek",
                                          "Moyer Creek",
                                          "Musgrove Creek",
                                          "Napias Creek",
                                          "Panther Creek"),
                         chnk),
                "Upper Salmon" = rch_cap %>%
                  filter(chnk_NWR_NAME == "Chinook Salmon (Snake River Spring/Summer-run ESU) - Salmon River Upper Mainstem above Redfish Lake",
                         chnk))

ira_plots = ira_list %>%
  map(.f = function(x) {
    plot_title = x %>% 
      pull(chnk_NWR_NAME) %>%
      unique() %>%
      str_remove("Chinook Salmon \\(Snake River Spring/Summer-run ESU\\) \\- ")
    x %>%
      ggplot() +
      geom_sf(aes(color = chnk_per_m2)) +
      scale_color_viridis_c(direction = -1) +
      labs(title = plot_title) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
  })

ggpubr::ggarrange(plotlist = ira_plots,
                  nrow = 3,
                  ncol = 3,
                  common.legend = T,
                  legend = 'right')

i = 2
ira_list[[i]] %>%
  ggplot() +
  geom_sf(aes(color = chnk_per_m2)) +
  # geom_sf(aes(color = GNIS_Name)) +
  scale_color_viridis_c(direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

wtsd_poly = ira_list[[i]] %>%
  st_union() %>%
  st_convex_hull()

calc_watershed_cap(wtsd_poly,
                   ira_list[[i]],
                   by_stream = T) %>%
  mutate(across(tot_length,
                ~ . / 1000))
