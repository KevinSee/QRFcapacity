# Author: Kevin See
# Purpose: Combine fish and habitat data
# Created: 9/11/2019
# Last Modified: 9/13/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(sf)

#-----------------------------------------------------------------
# summer juvenile data
#-----------------------------------------------------------------
data(fishSumEst)
data(site_data)
# data(chnkDomain)
# data(sthdDomain)


fish_hab_sum = fishSumEst %>%
  filter(Valid) %>%
  filter(!is.na(FishSiteLength)) %>%
  rename(fishSampDate = SampleDate) %>%
  select(-Stream) %>%
  inner_join(site_data %>%
               rename(habSampDate = SampleDate,
                      Year = VisitYear) %>%
               filter(VisitObjective == 'Primary Visit',
                      VisitStatus == 'Released to Public')) %>%
  mutate(siteSppYr = paste(Site, Species, Year, sep = '_')) %>%
  mutate(timeDiff = difftime(fishSampDate, habSampDate, units = 'days'),
         timeDiff = abs(as.integer(timeDiff))) %>%
  mutate(fishDens = N / FishSiteLength) %>%
  group_by(siteSppYr) %>%
  filter(fishDens == max(fishDens, na.rm = T)) %>%
  filter(timeDiff == min(timeDiff, na.rm = T)) %>%
  ungroup()

# for each site, pull out the year with the highest fish density
fish_hab_sum %<>%
  mutate(siteSpp = paste(Site, Species)) %>%
  group_by(siteSpp) %>%
  filter(fishDens == max(fishDens, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-siteSppYr, -siteSpp, -timeDiff)

use_data(fish_hab_sum,
         version = 2,
         overwrite = T)

#-----------------------------------------------------------------
# redd data
#-----------------------------------------------------------------
# use habitat data averaged across all CHaMP surveys

data(maxRedds)
data(avgHab_v2)

# fish_hab_redd = maxRedds %>%
#   mutate(fishSampDate = ymd(paste0(maxYr, '0715'))) %>%
#   inner_join(site_data %>%
#                rename(habSampDate = SampleDate,
#                       Year = VisitYear) %>%
#                filter(VisitObjective == 'Primary Visit',
#                       VisitStatus == 'Released to Public'),
#              by = c('Site',
#                     'Watershed')) %>%
#   mutate(siteSpp = paste(Site, Species, sep = '_')) %>%
#   mutate(timeDiff = difftime(fishSampDate, habSampDate, units = 'days'),
#          timeDiff = abs(as.integer(timeDiff))) %>%
#   group_by(siteSpp) %>%
#   filter(timeDiff == min(timeDiff, na.rm = T)) %>%
#   ungroup()
#   
#   rename(Year = maxYr)
# 
# fish_hab_redd %>%
#   filter(timeDiff > 180) %>%
#   xtabs(~ maxYr, .)
#   
# maxRedds %>%
#   anti_join(fish_hab_redd) %>%
#   xtabs(~ maxYr, .)

fish_hab_redd = maxRedds %>%
  inner_join(avgHab_v2) %>%
  mutate(fish_dens = maxReddsPerKm / 1000)

use_data(fish_hab_redd,
         version = 2,
         overwrite = T)


#-----------------------------------------------------------------
# winter juvenile data
#-----------------------------------------------------------------
data(fishWinEst)

fish_hab_win = fishWinEst %>%
  filter(Valid) %>%
  filter(!is.na(FishSiteLength)) %>%
  rename(fishSampDate = SampleDate) %>%
  select(-Stream) %>%
  inner_join(site_data %>%
               rename(habSampDate = SampleDate,
                      Year = VisitYear) %>%
               filter(VisitObjective == 'Primary Visit',
                      VisitStatus == 'Released to Public')) %>%
  mutate(siteSppYr = paste(Site, Species, Year, sep = '_')) %>%
  mutate(timeDiff = difftime(fishSampDate, habSampDate, units = 'days'),
         timeDiff = abs(as.integer(timeDiff))) %>%
  mutate(fishDens = N / FishSiteLength) %>%
  group_by(siteSppYr) %>%
  filter(fishDens == max(fishDens, na.rm = T)) %>%
  filter(timeDiff == min(timeDiff, na.rm = T)) %>%
  ungroup()