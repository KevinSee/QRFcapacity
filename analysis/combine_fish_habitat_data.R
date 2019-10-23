# Author: Kevin See
# Purpose: Combine fish and habitat data
# Created: 9/11/2019
# Last Modified: 10/22/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)
library(maptools)

#-----------------------------------------------------------------
# summer juvenile data 2011-2014
#-----------------------------------------------------------------
data("fish_sum_est")
data("champ_site_2011_14")
# for temperature metrics
data("int_crb_temp")

fh_sum_champ_2014 = fish_sum_est %>%
  filter(Valid) %>%
  filter(Year <= 2014) %>%
  filter(!is.na(FishSiteLength)) %>%
  rename(fishSampDate = SampleDate) %>%
  select(-Stream) %>%
  inner_join(champ_site_2011_14 %>%
               rename(habSampDate = SampleDate,
                      Year = VisitYear) %>%
               filter(`Primary Visit` == 'Yes',
                      VisitStatus == 'Released to Public')) %>%
  mutate(siteSppYr = paste(Site, Species, Year, sep = '_')) %>%
  mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
         time_diff = abs(as.integer(time_diff))) %>%
  mutate(fish_dens = N / FishSiteLength) %>%
  group_by(siteSppYr) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  filter(time_diff == min(time_diff, na.rm = T)) %>%
  ungroup()

fh_sf = fh_sum_champ_2014 %>%
  select(Site, Year, Watershed, FishSite, Lon, Lat) %>%
  distinct() %>%
  st_as_sf(coords = c('Lon', 'Lat'),
           crs = 4326) %>%
  st_transform(st_crs(int_crb_temp))

ggplot(fh_sf) +
  geom_sf(data = int_crb_temp,
          color = 'lightblue') +
  geom_sf(aes(color = Watershed)) +
  theme_bw() +
  theme(axis.text = element_blank())


temp_spatial = int_crb_temp %>%
  mutate(id = 1:n()) %>%
  as_Spatial()

fh_sf = fh_sf %>%
  as_Spatial() %>%
  snapPointsToLines(lines = temp_spatial,
                    maxDist = 1000,
                    idField = 'id') %>%
  st_as_sf()

qplot(snap_dist, data = fh_sf)

fh_sf %<>%
  rename(id = nearest_line_id) %>%
  left_join(int_crb_temp %>%
              mutate(id = 1:n()) %>%
              as_tibble() %>%
              select(id, CANOPY, NorWeST_area, Flow_Aug:S36_2015))

fh_temp = fh_sf %>%
  as_tibble() %>%
  select(Site:FishSite,
         S21_2011, S33_2012:S36_2015) %>%
  gather(scenario, Aug_temp, -(Site:FishSite)) %>%
  mutate(temp_yr = str_sub(scenario, -4)) %>%
  mutate_at(vars(temp_yr),
            list(as.numeric)) %>%
  filter(Year == temp_yr) %>%
  select(Site:FishSite, Aug_temp)

fh_sum_champ_2014 %>%
  left_join(fh_temp) %>%
  filter(!is.na(Aug_temp)) %>%
  tabyl(Watershed, Year)

# for each site, pull out the year with the highest fish density
fh_sum_champ_2014 %<>%
  mutate(siteSpp = paste(Site, Species)) %>%
  group_by(siteSpp) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-siteSppYr, -siteSpp, -time_diff)

use_data(fh_sum_champ_2014,
         version = 2,
         overwrite = T)

#-----------------------------------------------------------------
# summer juvenile data 2011-2017
#-----------------------------------------------------------------
data("fish_sum_est")
data("champ_site_2011_17")

fh_sum_champ_2017 = fish_sum_est %>%
  filter(Valid) %>%
  filter(!is.na(FishSiteLength)) %>%
  rename(fishSampDate = SampleDate) %>%
  select(-Stream) %>%
  inner_join(champ_site_2011_17 %>%
               rename(habSampDate = SampleDate,
                      Year = VisitYear) %>%
               filter(VisitObjective == 'Primary Visit',
                      VisitStatus == 'Released to Public')) %>%
  mutate(siteSppYr = paste(Site, Species, Year, sep = '_')) %>%
  mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
         time_diff = abs(as.integer(time_diff))) %>%
  mutate(fish_dens = N / FishSiteLength) %>%
  group_by(siteSppYr) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  filter(time_diff == min(time_diff, na.rm = T)) %>%
  ungroup()

# for each site, pull out the year with the highest fish density
fh_sum_champ_2017 %<>%
  mutate(siteSpp = paste(Site, Species)) %>%
  group_by(siteSpp) %>%
  filter(fish_dens == max(fish_dens, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-siteSppYr, -siteSpp, -time_diff)

use_data(fh_sum_champ_2017,
         version = 2,
         overwrite = T)

#-----------------------------------------------------------------
# redd data, using CHaMP 2011-2014
#-----------------------------------------------------------------
# use habitat data averaged across all CHaMP surveys

data("redds_site_max")
data("champ_site_2011_14")
data("champ_site_2011_14_avg")

# 
# # match year with max redds with CHaMP survey in closest year
# fh_redds_champ_2014 = redds_site_max %>%
#   mutate(fishSampDate = ymd(paste0(maxYr, '0715'))) %>%
#   inner_join(champ_site_2011_14 %>%
#                rename(habSampDate = SampleDate,
#                       Year = VisitYear,
#                       Site = SiteName,
#                       Watershed = WatershedName) %>%
#                filter(`Primary Visit` == 'Yes',
#                       VisitStatus == 'Released to Public'),
#              by = c('Site',
#                     'Watershed')) %>%
#   mutate(siteSpp = paste(Site, Species, sep = '_')) %>%
#   mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
#          time_diff = abs(as.integer(time_diff))) %>%
#   group_by(siteSpp) %>%
#   filter(time_diff == min(time_diff, na.rm = T)) %>%
#   ungroup()
# 
# fh_redds_champ_2014 %>%
#   group_by(Species, maxYr) %>%
#   summarise(nSurv = n(),
#             nDiffYr = sum(maxYr != year(habSampDate))) %>%
#   filter(nDiffYr < nSurv)
# 
# redds_site_max %>%
#   anti_join(fh_redds_champ_2014)


# match year with max redds to average CHaMP values
fh_redds_champ_2014 = redds_site_max %>%
  inner_join(champ_site_2011_14_avg %>%
               rename(Site = SiteName,
                      Watershed = WatershedName)) %>%
  mutate(fish_dens = maxReddsPerKm / 1000)

redds_site_max %>%
  anti_join(fh_redds_champ_2014)

# save as R data object
use_data(fh_redds_champ_2014,
         version = 2,
         overwrite = T)

#-----------------------------------------------------------------
# redd data, using CHaMP 2011-2017
#-----------------------------------------------------------------
# use habitat data averaged across all CHaMP surveys

data("redds_site_max")
data("champ_site_2011_17")
data("champ_site_2011_17_avg")


# # match year with max redds with CHaMP survey in closest year
# fh_redds_champ_2017 = redds_site_max %>%
#   mutate(fishSampDate = ymd(paste0(maxYr, '0715'))) %>%
#   inner_join(champ_site_2011_17 %>%
#                rename(habSampDate = SampleDate,
#                       Year = VisitYear) %>%
#                filter(VisitObjective == 'Primary Visit',
#                       VisitStatus == 'Released to Public'),
#              by = c('Site',
#                     'Watershed')) %>%
#   mutate(siteSpp = paste(Site, Species, sep = '_')) %>%
#   mutate(time_diff = difftime(fishSampDate, habSampDate, units = 'days'),
#          time_diff = abs(as.integer(time_diff))) %>%
#   group_by(siteSpp) %>%
#   filter(time_diff == min(time_diff, na.rm = T)) %>%
#   ungroup()
# 
# fh_redds_champ_2017 %>%
#   group_by(Species, maxYr) %>%
#   summarise(nSurv = n(),
#             nDiffYr = sum(maxYr != year(habSampDate))) %>%
#   filter(nDiffYr < nSurv)
# 
# redds_site_max %>%
#   anti_join(fh_redds_champ_2017)


# match year with max redds to average CHaMP values
fh_redds_champ_2017 = redds_site_max %>%
  inner_join(champ_site_2011_17_avg) %>%
  mutate(fish_dens = maxReddsPerKm / 1000)

redds_site_max %>%
  anti_join(fh_redds_champ_2017)

# save as R data object
use_data(fh_redds_champ_2017,
         version = 2,
         overwrite = T)


#-----------------------------------------------------------------
# winter juvenile data
#-----------------------------------------------------------------
data("fish_win_est")
data("champ_cu")
data("champ_site_2011_17")

# focus on latest channel unit data
# make some things match the fish data
cu_df = champ_cu %>%
  mutate(Tier1 = recode(Tier1,
                        'Fast-NonTurbulent/Glide' = 'Run',
                        'Fast-Turbulent' = 'Riffle',
                        'Slow/Pool' = 'Pool',
                        'Small Side Channel' = 'SSC')) %>%
  left_join(champ_site_2011_17 %>%
              filter(VisitObjective == 'Primary Visit',
                     VisitStatus == 'Released to Public') %>%
              select(VisitID, 
                     Site, 
                     Watershed,
                     SampleDate,
                     SubD50,
                     Sin,
                     CU_Freq)) %>%
  group_by(Site, ChUnitNumber) %>%
  filter(SampleDate == max(SampleDate, na.rm = T)) %>%
  ungroup() %>%
  mutate(FishCovAll = 100 - FishCovNone,
         UcutArea_Pct = UcutArea_Pct * 100) %>%
  # add cobble and boulder substrate together
  mutate(SubEstCandBldr = SubEstCbl + SubEstBldr)


fh_win_champ_2017 = fish_win_est %>%
  rename(fishSampDate = SampleDate,
         Tier1_fish = Tier1,
         Tier2_fish = Tier2) %>%
  select(-Stream) %>%
  inner_join(cu_df %>%
               rename(habSampDate = SampleDate) %>%
               mutate_at(vars(ChUnitNumber),
                         list(as.character))) %>%
  mutate(fish_dens = N / AreaTotal)


use_data(fh_win_champ_2017,
         version = 2,
         overwrite = T)
