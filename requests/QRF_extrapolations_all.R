# Author: Kevin See
# Purpose: Compile QRF extrapolations for entire CRB
# Created: 7/29/2021
# Last Modified: 12/17/2021
# Notes: To fulfill BPA contract

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(QRFcapacity)
library(here)

#-----------------------------------------------------------------
# read in estimates at all master sample points
# qrf_pts = st_read('output/gpkg/Sum_Juv_Capacity.gpkg') %>%
#   mutate(Lifestage = 'sum_juv') %>%
#   rbind(st_read('output/gpkg/Win_Juv_Capacity.gpkg') %>%
#           mutate(Lifestage = 'win_juv')) %>%
#   rbind(st_read('output/gpkg/Redds_Capacity.gpkg') %>%
#           mutate(Lifestage = 'redd')) %>%
#   select(Lifestage, everything()) %>%
#   st_transform(crs = 5070)

qrf_pts = st_read('output/gpkg/MastPts_Cap_juv_summer.gpkg') %>%
  mutate(Lifestage = 'sum_juv') %>%
  rbind(st_read('output/gpkg/MastPts_Cap_juv_winter.gpkg') %>%
          mutate(Lifestage = 'win_juv')) %>%
  rbind(st_read('output/gpkg/MastPts_Cap_redds.gpkg') %>%
          mutate(Lifestage = 'redd')) %>%
  select(Lifestage, everything()) %>%
  st_transform(crs = 5070)


# how many distinct sites?
n_sites = n_distinct(qrf_pts$Site)

# split into 2 objects, one for each species, with one row per site
chnk_pts = qrf_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(Lifestage:HUC12NmNRC,
         spp_domain = chnk_range,
         model,
         starts_with("chnk")) %>%
  # select(-inCovarRange) %>%
  pivot_longer(starts_with("chnk"),
               names_to = "name",
               values_to = "value") %>%
  mutate(across(name,
                str_remove,
                "chnk_")) %>%
  unite(metric, Lifestage, name, sep = "_") %>% 
  pivot_wider(names_from = metric,
              values_from = value) %>%
  add_column(Species = "Chinook",
             .before = 0) %>%
  select(Species:model,
         matches("_per_"),
         everything())

qrf_pts %>%
  select(Site,
         geom) %>%
  distinct() %>%
  left_join(chnk_pts,
            by = "Site") -> chnk_pts

sum(duplicated(chnk_pts$Site))
identical(nrow(chnk_pts), n_sites)

sthd_pts = qrf_pts %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(Lifestage:HUC12NmNRC,
         spp_domain = sthd_range,
         model,
         starts_with("sthd")) %>%
  # select(-inCovarRange) %>%
  pivot_longer(starts_with("sthd"),
               names_to = "name",
               values_to = "value") %>%
  mutate(across(name,
                str_remove,
                "sthd_")) %>%
  unite(metric, Lifestage, name, sep = "_") %>% 
  pivot_wider(names_from = metric,
              values_from = value) %>%
  add_column(Species = "Steelhead",
             .before = 0) %>%
  select(Species:model,
         matches("_per_"),
         everything())

sum(duplicated(sthd_pts$Site))
identical(nrow(sthd_pts), n_sites)

qrf_pts %>%
  select(Site,
         geom) %>%
  distinct() %>%
  left_join(sthd_pts,
            by = "Site") -> sthd_pts

# save as geopackages
st_write(chnk_pts,
         here("output/gpkg/MasterPts_Capacity_Chnk.gpkg"),
         driver = 'GPKG',
         delete_layer = T)

st_write(sthd_pts,
         here("output/gpkg/MasterPts_Capacity_Sthd.gpkg"),
         driver = 'GPKG',
         delete_layer = T)

rm(qrf_pts, chnk_pts, sthd_pts, n_sites)

#-----------------------------------------------------------------
# load 200m reach data
data("rch_200")

# read in estimates at all 200 m reaches

# # results from random forest model
# qrf_rch = st_read(here("output/gpkg/Rch_Cap_RF_juv_summer.gpkg")) %>%
#   add_column(Lifestage = "sum_juv",
#              .before = 0) %>%
#   rbind(st_read(here("output/gpkg/Rch_Cap_RF_juv_winter.gpkg")) %>%
#           add_column(Lifestage = "win_juv",
#                      .before = 0)) %>%
#   rbind(st_read(here("output/gpkg/Rch_Cap_RF_redds.gpkg")) %>%
#           add_column(Lifestage = "redd",
#                      .before = 0)) %>%
#   st_transform(crs = 5070)

# results from linear model
qrf_rch = st_read(here("output/gpkg/Rch_Cap_juv_summer.gpkg")) %>%
  add_column(Lifestage = "sum_juv",
             .before = 0) %>%
  rbind(st_read(here("output/gpkg/Rch_Cap_juv_winter.gpkg")) %>%
          add_column(Lifestage = "win_juv",
                     .before = 0)) %>%
  rbind(st_read(here("output/gpkg/Rch_Cap_redds.gpkg")) %>%
          add_column(Lifestage = "redd",
                     .before = 0)) %>%
  st_transform(crs = 5070)

# how many distinct reaches?
n_rchs = n_distinct(qrf_rch$UniqueID)

# split into 2 objects, one for each species, with one row per site
chnk_rch = rch_200 %>%
  # st_drop_geometry() %>%
  # as_tibble() %>%
  select(UniqueID,
         reach_leng,
         HUC6_name,
         matches("chnk")) %>%
  rename_with(.fn = ~ str_remove(.x, "chnk_"),
              .cols = starts_with("chnk")) %>%
  select(-run) %>%
  rename(spp_use = use,
         spp_domain = chnk) %>%
  inner_join(qrf_rch %>%
               st_drop_geometry() %>%
               as_tibble() %>%
               select(UniqueID,
                      Lifestage,
                      starts_with("chnk_")) %>%
               select(UniqueID,
                      Lifestage,
                      matches("per")) %>%
               rename_with(.fn = ~ str_remove(.x, "chnk_"),
                           .cols = starts_with("chnk")) %>%
               pivot_longer(starts_with("per"),
                            names_to = "name",
                            values_to = "value") %>%
               unite(metric, Lifestage, name, sep = "_") %>% 
               pivot_wider(names_from = metric,
                           values_from = value) %>%
               add_column(Species = "Chinook",
                          .before = 0),
             by = "UniqueID") %>%
  relocate(Species, 
           .before = 1) %>%
  relocate(spp_use, spp_domain,
           .after = reach_leng)

qrf_rch %>%
  select(UniqueID,
         geom) %>%
  distinct() %>%
  left_join(chnk_rch,
            by = "Site") -> chnk_rch



sum(duplicated(chnk_rch$UniqueID))
identical(nrow(chnk_rch), n_rchs)

qrf_rch$UniqueID[!qrf_rch$UniqueID %in% chnk_rch$UniqueID]
chnk_rch$UniqueID[!chnk_rch$UniqueID %in% qrf_rch$UniqueID]

# sum(is.na(chnk_rch$sum_juv_per_m))

sthd_rch = rch_200 %>%
  select(UniqueID,
         reach_leng,
         HUC6_name,
         matches("sthd")) %>%
  rename_with(.fn = ~ str_remove(.x, "sthd_"),
              .cols = starts_with("sthd")) %>%
  select(-run) %>%
  rename(spp_use = use,
         spp_domain = sthd) %>%
  inner_join(qrf_rch %>%
               as_tibble() %>%
               select(UniqueID,
                      Lifestage,
                      starts_with("sthd_")) %>%
               select(UniqueID,
                      Lifestage,
                      matches("per")) %>%
               rename_with(.fn = ~ str_remove(.x, "sthd_"),
                           .cols = starts_with("sthd")) %>%
               pivot_longer(starts_with("per"),
                            names_to = "name",
                            values_to = "value") %>%
               unite(metric, Lifestage, name, sep = "_") %>% 
               pivot_wider(names_from = metric,
                           values_from = value) %>%
               add_column(Species = "Steelhead",
                          .before = 0),
             by = "UniqueID") %>%
  relocate(Species, 
           .before = 1) %>%
  relocate(spp_use, spp_domain,
           .after = reach_leng)

sum(duplicated(sthd_rch$UniqueID))
identical(nrow(sthd_rch), n_rchs)  

sthd_rch %>%
  filter(is.na(sum_juv_per_m)) %>%
  # filter(UniqueID %in% UniqueID[duplicated(UniqueID)]) %>%
  arrange(UniqueID) %>%
  as.data.frame() %>% 
  head(10)


# save as geopackages
# try splitting it up and appending each one subsequently, to help speed it up.
chnk_rch_split = chnk_rch %>%
  group_split(HUC6_name)
for(i in 1:length(chnk_rch_split)) {
  cat(paste("Working on group", i, "out of", length(chnk_rch_split), "with", nrow(chnk_rch_split[[i]]), " rows\n"))
  
  st_write(chnk_rch_split[[i]],
           dsn = here("output/gpkg/StreamNetwork_Capacity_Chnk.gpkg"),
           driver = 'GPKG',
           append = if_else(i == 1, F, T))
}
rm(chnk_rch_split)

sthd_rch_split = sthd_rch %>%
  group_split(HUC6_name)
for(i in 1:length(sthd_rch_split)) {
  cat(paste("Working on group", i, "out of", length(sthd_rch_split), "with", nrow(sthd_rch_split[[i]]), " rows\n"))
  
  st_write(sthd_rch_split[[i]],
           dsn = here("output/gpkg/StreamNetwork_Capacity_Sthd.gpkg"),
           driver = 'GPKG',
           append = if_else(i == 1, F, T))
}
rm(sthd_rch_split)

# st_write(chnk_rch,
#          here("output/gpkg/StreamNetwork_Capacity_Chnk.gpkg"),
#          driver = 'GPKG',
#          delete_layer = T)

# st_write(sthd_rch,
#          here("output/gpkg/StreamNetwork_Capacity_Sthd.gpkg"),
#          driver = 'GPKG',
#          delete_layer = T)

#-----------------------------------------------------------------
# start some metadata
tibble(`Column Name` = names(chnk_pts)) %>%
  left_join(readxl::read_excel("data/raw/master_sample_pts/GAA_Metadata_20150506.xlsx") %>%
              filter(grepl("NmNRC", FieldName) |
                       FieldName == "Site_ID") %>%
              mutate(FieldName = recode(FieldName,
                                        "Site_ID" = "Site")) %>%
              select(`Column Name` = FieldName,
                     Definition = Description) %>%
              distinct())

list("MasterPts" = tibble(`Column Name` = names(chnk_pts),
                          Definition = NA),
     "StreamNetwork" = tibble(`Column Name` = names(chnk_rch),
                              Definition = NA)) %>%
  writexl::write_xlsx(here("output/gpkg/QRF_Extrapolation_Metadata.xlsx"))

