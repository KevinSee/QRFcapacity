# Author: Kevin See
# Purpose: Get QRF estimates for Upper Grande Ronde
# Created: 9/29/22
# Last Modified: 2/22/23
# Notes: This is done as part of a review of a paper by Roni

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
               'redds')[2]

# load(paste0('output/modelFits/extrap_200rch_RF_', mod_choice, '.rda'))
load(paste0('output/modelFits/extrap_200rch_', mod_choice, '.rda'))

data("rch_200")

rch_200_cap = rch_200 %>%
  select(UniqueID, GNIS_Name, reach_leng:HUC8_code, 
         chnk, chnk_use, chnk_ESU_DPS:chnk_NWR_NAME,
         sthd, sthd_use, sthd_ESU_DPS:sthd_NWR_NAME) %>%
  left_join(all_preds %>%
              select(-HUC8_code)) %>%
  filter(reach_leng < 500)

# pulling estimates directly from StreamNet files
sn_sf <- st_read("data/prepped/StreamNet/StreamNetwork_Capacity_RF_Chnk.gpkg")

rch_200_cap = rch_200 %>%
  select(UniqueID, GNIS_Name, reach_leng:HUC8_code, 
         chnk, chnk_use, chnk_ESU_DPS:chnk_NWR_NAME,
         sthd, sthd_use, sthd_ESU_DPS:sthd_NWR_NAME) %>%
  left_join(sn_sf |> 
              st_drop_geometry() |> 
              as_tibble() |> 
              filter(spp_domain) |> 
              select(UniqueID,
                     chnk_per_m = sum_juv_per_m,
                     chnk_per_m_se = sum_juv_per_m_se,
                     chnk_per_m2 = sum_juv_per_m2,
                     chnk_per_m2_se = sum_juv_per_m2_se)) |> 
  filter(reach_leng < 500)

sn_sf |> 
  filter(UniqueID %in% my_ids,
         spp_domain) |> 
  st_drop_geometry() |> 
  summarize(across(ends_with("per_m2"),
                list(mean = mean,
                     median = median)))

sn_sf |> 
  filter(UniqueID %in% my_ids,
         spp_domain) |> 
  st_drop_geometry() |> 
  mutate(across(ends_with("per_m"),
                ~ . * reach_leng)) |> 
  summarize(across(ends_with("per_m"),
                   sum),
            across(ends_with("per_m"),
                   round_half_up))

#-----------------------------------------------------------------
# filter QRF capacities and species' domains to the upper Grande Ronde area
ugr_cap <- rch_200_cap %>%
  filter(str_detect(chnk_NWR_NAME, "Grande Ronde"),
         chnk) %>%
  select(-starts_with("sthd"))


# st_write(ugr_cap,
#          dsn = "O:Desktop/UGR_Chnk_Cap.gpkg",
#          driver = 'GPKG')




mtr_bb <- ugr_cap %>%
  filter(UniqueID %in% c(364385,
                         358135)) %>%
  st_bbox() %>%
  st_as_sfc()

my_ids <- ugr_cap %>%
  st_intersection(mtr_bb) %>%
  filter(GNIS_Name == "Grande Ronde River") %>%
  pull(UniqueID)

mtr_cap <- ugr_cap %>%
  filter(UniqueID %in% my_ids)

mtr_cap |> 
  filter(is.na(chnk_per_m)) |> 
  st_drop_geometry() |> 
  select(UniqueID) |> 
  left_join(mtr_cap_champ) |> 
  rbind(mtr_cap |> 
          filter(!is.na(chnk_per_m))) |> 
  st_as_sf() -> mtr_cap

# load(paste0('output/modelFits/extrap_mastPts_', mod_choice, '.rda'))
# mtr_pts <- all_preds %>%
#   filter(Watershed == "Upper Grande Ronde") %>%
#   left_join(champ_site_2011_17_avg %>%
#               select(Site,
#                      LAT_DD,
#                      LON_DD)) %>%
#   filter(!is.na(LON_DD),
#          !is.na(LAT_DD)) %>%
#   st_as_sf(coords = c("LON_DD",
#                       "LAT_DD"),
#            crs = 4326) %>%
#   st_transform(st_crs(mtr_bb)) %>%
#   st_intersection(mtr_bb)



#-----------------
# how long is this stretch of river?
mtr_cap %>%
  pull(geometry) %>%
  st_length() %>%
  sum() %>%
  units::drop_units() %>%
  measurements::conv_unit("m", "km") %>%
  round(2)

# average capacity?
mtr_cap %>%
  st_drop_geometry() %>%
  summarize(across(c(chnk_per_m,
                     chnk_per_m2),
                   list(mean = mean,
                        median = median),
                   na.rm = T))

# total capacity
mtr_cap %>%
  mutate(chnk_tot = chnk_per_m * reach_leng,
         chnk_tot_se = chnk_per_m_se * reach_leng) %>%
  st_drop_geometry() %>%
  summarize(across(c(chnk_per_m,
                     chnk_per_m2),
                   mean,
                   na.rm = T),
            across(chnk_tot,
                   sum,
                   na.rm = T),
            across(chnk_tot_se,
                   ~ sqrt(sum(.^2, na.rm = T)))) %>%
  mutate(lci_tot = qnorm(0.025, chnk_tot, chnk_tot_se),
         uci_tot = qnorm(0.975, chnk_tot, chnk_tot_se))

# using average fish / m^2 and total area from Roni paper
mtr_cap %>%
  st_drop_geometry() %>%
  # group_by(model) %>%
  summarize(across(c(chnk_per_m2),
                   median,
                   na.rm = T)) %>%
  mutate(chnk_tot = chnk_per_m2 * 19697)

# quick map
mtr_cap %>%
  ggplot(aes(color = chnk_per_m2)) +
  geom_sf() +
  scale_color_viridis_c() +
  theme(axis.text = element_blank())

mtr_cap %>%
  st_drop_geometry() %>%
  select(model, starts_with("chnk_per_m"))

mtr_size <- tibble(site_length = mtr_cap %>%
                     pull(geometry) %>%
                     st_length() %>%
                     sum() %>%
                     units::drop_units(),
                   site_area = 19697)


#----------------------------------------------
# examine some of the fish and habitat data from this stretch
data("champ_site_rch")
data("champ_site_2011_17")

champ_site_rch %>%
  inner_join(mtr_cap %>%
               st_drop_geometry() %>%
               select(UniqueID)) %>%
   left_join(champ_site_2011_17) %>%
  select(Site, VisitYear,
         SampleDate,
         VisitObjective)

mtr_dens_est <- champ_site_rch %>%
  inner_join(mtr_cap %>%
               st_drop_geometry() %>%
               select(UniqueID)) %>%
  left_join(fish_sum_est) %>%
  filter(Species == "Chinook") %>%
  mutate(fish_dens_2 = N / FishWettedArea) %>%
  select(Site,
         Year,
         FishCrew,
         Method,
         # Species,
         Pass1.M,
         Nmethod,
         N, 
         fish_dens,
         fish_dens_2)

mtr_dens_est |> 
  group_by(Site) |> 
  summarize(across(fish_dens_2,
                   list(min = min,
                        mean = mean,
                        median = median,
                        max = max),
                   .names = "{.fn}"))

ggplot(mtr_dens_est,
       aes(x = fish_dens_2,
           fill = Site)) +
  geom_histogram(bins = 10,
                 position = "dodge") +
  labs(x = "Fish Density (per m2)",
       color = "CHaMP Site")
  


mtr_dens_est %>%
  group_by(Site) %>%
  # group_by(Year) %>%
  summarize(n_sites = n(),
            across(matches("fish_dens"),
                   median)) %>%
                   # mean)) %>%
                   # max)) %>%
  bind_cols(mtr_size) %>%
  mutate(cap_1 = fish_dens * site_length,
         cap_2 = fish_dens_2 * site_area) %>%
  arrange(cap_2)

champ_site_rch %>%
  inner_join(mtr_cap %>%
               st_drop_geometry() %>%
               select(UniqueID)) %>%
  left_join(champ_site_2011_17_avg) %>%
  select(Site, 
         ValleyClass, Channel_Type,
         Grad,
         SlowWater_Pct,
         FstTurb_Pct,
         FstNT_Pct,
         WetWdth_Int,
         WetWdth_Avg,
         BfWdthInt,
         DpthBf_Max,
         DpthBf_Avg)


fish_sum_est %>%
  filter(Species == "Chinook") %>%
  mutate(fish_dens_2 = N / FishWettedArea) %>%
  filter(fish_dens_2 != Inf) %>%
  xtabs(~ fish_dens_2 >= 1, .)
  # filter(fish_dens_2 > 1) %>%
  select(Site, Year, Watershed, 
         Method,
         Nmethod,
         N, fish_dens_2) %>%
  ggplot(aes(x = fish_dens_2)) +
  geom_histogram() +
  geom_vline(xintercept = 1,
             linetype = 2,
             color = "red")
