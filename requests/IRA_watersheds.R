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

if(mod_choice == "juv_summer") {
  rch_pts = st_read("output/gpkg/Sum_Juv_Capacity.gpkg") %>%
    st_transform(st_crs(rch_cap))
} else if(mod_choice == "juv_summer_dash") {
  rch_pts = st_read("output/gpkg/Sum_Juv_Capacity_DASH.gpkg") %>%
    st_transform(st_crs(rch_cap))
} else if(mod_choice == "redds") {
  rch_pts = st_read("output/gpkg/Redds_Capacity.gpkg") %>%
    st_transform(st_crs(rch_cap))
} else if(mod_choice == "juv_winter") {
  rch_pts = st_read("output/gpkg/Win_Juv_Capacity.gpkg") %>%
    st_transform(st_crs(rch_cap))
}

#-----------------------------------------------------------------
# pull out areas in the IRA report
# rch_cap %>%
#   filter(chnk_MPG == "Upper Salmon River") %>%
#   xtabs(~ chnk_NWR_NAME, .)

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
                         chnk,
                         HUC8_code == "17060204"),
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

wtsd_poly_list = ira_list %>%
  map(.f = function(x) {
    x %>%
      st_union() %>%
      st_convex_hull()
  })

ira_pts = wtsd_poly_list %>%
  map(.f = function(x) {
    st_intersection(rch_pts, x)# %>%
      # filter(chnk == "Yes")
  })

# get Chinook range from 200m reaches
chnk_wtsd_range = ira_list %>%
  map(.f = function(x) {
    x %>%
      mutate(Species = "Chinook Salmon",
             SciName = "Oncorhynchus tshawytscha") %>%
      select(StreamName = GNIS_Name,
             Species, SciName,
             UseType = chnk_use,
             ESU_DPS = chnk_ESU_DPS,
             MPG = chnk_MPG,
             NWR_POPID = chnk_NWR_POPID,
             NWR_NAME = chnk_NWR_NAME)
  })
chnk_domain = do.call(rbind, chnk_wtsd_range)

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

names(ira_list)
i = 5
ggplot() +
  geom_sf(data = wtsd_poly_list[[i]]) +
  geom_sf(data = ira_list[[i]],
          aes(color = chnk_per_m2)) +
  geom_sf(data = ira_pts[[i]],
          aes(color = chnk_per_m2)) +
  scale_color_viridis_c(direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# calculate total capacity by stream
# using 200 m reaches
calc_watershed_cap(wtsd_poly_list[[i]],
                   ira_list[[i]],
                   by_stream = T) %>%
  mutate(across(tot_length,
                ~ . / 1000)) %>%
  mutate(tot_cap_cv = tot_cap_se / tot_cap,
         cap_per_km   = tot_cap / tot_length)

# using master sample points
calc_watershed_cap(wtsd_polygon = wtsd_poly_list[[i]],
                   capacity_sf = ira_pts[[i]],
                   spp_range = chnk_domain,
                   by_stream = T) %>%
  mutate(across(tot_length,
                ~ . / 1000)) %>%
  select(StreamName, n_pts:tot_cap_se) %>%
  mutate(tot_cap_cv = tot_cap_se / tot_cap,
         cap_per_km = tot_cap / tot_length)

ggplot() +
  geom_sf(data = chnk_wtsd_range[[i]] %>%
            mutate(across(StreamName,
                          fct_drop)),
          aes(color = StreamName)) #+
  # geom_sf(data = ira_list[[i]] %>%
  #           rename(StreamName = GNIS_Name) %>%
  #           mutate(across(StreamName,
  #                         fct_drop)),
  #         aes(color = StreamName))


#-----------------------------------------------------------------
# calculate total capacity based on points and reaches
cap_comp = tibble(watershed = names(ira_list)) %>%
  add_column(rchs = ira_list,
             pts = ira_pts,
             wtsd_poly = wtsd_poly_list) %>%
  mutate(cap_rch = map2(rchs, wtsd_poly,
                        .f = function(x, y) {
                          calc_watershed_cap(y, x)
                        }),
         cap_pts = map2(pts, wtsd_poly,
                        .f = function(x, y) {
                          calc_watershed_cap(y, x,
                                             spp_range = chnk_domain)
                        })) %>%
  pivot_longer(cols = starts_with("cap"),
               names_to = "source",
               values_to = "cap_df") %>%
  unnest(cols = cap_df)

# make a few plots
cap_comp %>%
  mutate(source = str_remove(source, "^cap_")) %>%
  select(-area,
         -n_rchs, -n_pts) %>%
  pivot_wider(names_from = "source",
              values_from = starts_with("tot")) %>%
  ggplot(aes(x = tot_cap_pts,
             y = tot_cap_rch,
             color = watershed)) +
  geom_abline(linetype = 2) +
  geom_errorbar(aes(ymin = tot_cap_rch + tot_cap_se_rch * qnorm(0.025),
                    ymax = tot_cap_rch + tot_cap_se_rch * qnorm(0.975)),
                width = 0) +
  geom_errorbarh(aes(xmin = tot_cap_pts + tot_cap_se_pts * qnorm(0.025),
                     xmax = tot_cap_pts + tot_cap_se_pts * qnorm(0.975)),
                 height = 0) +
  geom_point() +
  labs(x = "Master Sample Points",
       y = "200m Reaches",
       color = "Watershed",
       title = "Total Watershed Capacity")

cap_comp %>%
  mutate(source = str_remove(source, "^cap_")) %>%
  select(-area,
         -n_rchs, -n_pts) %>%
  pivot_wider(names_from = "source",
              values_from = starts_with("tot")) %>%
  ggplot(aes(x = tot_length_pts,
             y = tot_length_rch,
             color = watershed)) +
  geom_abline(linetype = 2) +
  geom_point(size = 4) +
  labs(x = "Master Sample Points",
       y = "200m Reaches",
       color = "Watershed",
       title = "Total Stream Length")

cap_comp %>%
  mutate(source = str_remove(source, "^cap_")) %>%
  select(-area,
         -n_rchs, -n_pts) %>%
  pivot_wider(names_from = "source",
              values_from = starts_with("tot")) %>%
  ggplot(aes(x = tot_cap_pts / tot_length_pts,
             y = tot_cap_rch / tot_length_rch,
             color = watershed)) +
  geom_abline(linetype = 2) +
  geom_point(size = 4) +
  labs(x = "Master Sample Points",
       y = "200m Reaches",
       color = "Watershed",
       title = "Avg Capacity / M")

cap_comp %>%
  mutate(source = str_remove(source, "^cap_")) %>%
  select(-area,
         -n_rchs, -n_pts) %>%
  pivot_wider(names_from = "source",
              values_from = starts_with("tot")) %>%
  mutate(diff = tot_cap_rch - tot_cap_pts,
         rel_diff = diff / tot_cap_pts) %>%
  ggplot(aes(y = watershed,
             x = rel_diff,
             fill = watershed)) +
  geom_col() +
  labs(y = "Watershed",
       fill = "Watershed",
       x = "Relative Difference")
  