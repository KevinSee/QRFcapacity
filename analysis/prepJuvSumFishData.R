# Author: Kevin See
# Purpose: Prep summer juvenile fish data
# Created: 5/14/2019
# Last Modified: 5/14/19
# Notes: need info for site name, fish crew, sample data, site length, watershed AND
# number marks, captures, recaptures OR
# 1st, 2nd, 3rd pass depletion OR
# snorkel counts / abundance estimate


#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)

#-----------------------------------------------------------------
# Entiat
#-----------------------------------------------------------------
# from USFS
entIMW = excel_sheets('data/raw/fish/IMW_Abundance_Estimate_ForKS_1-12-2016.xlsx') %>%
  as.list() %>%
  map_df(.f = function(x) {
    read_excel('data/raw/fish/IMW_Abundance_Estimate_ForKS_1-12-2016.xlsx',
               x[1],
               skip = 8) %>%
      filter(!is.na(Species)) %>%
      fill(Site) %>%
      # filter(!is.na(`# Marked (M)`)) %>%
      mutate(Watershed = 'Entiat',
             FishCrew = 'USFS') %>% 
      mutate(Site = paste0('ENT00001-', Site)) %>%
      mutate(Season = x) %>%
      select(Season,
             SiteName = Site,
             Watershed,
             FishCrew,
             Species,
             Pass1.M = `# Marked (M)`,
             Pass2.C = `# Day 2 Capture (C)`,
             Pass3.R = `# Day 2 RE (R)`) %>%
      mutate_at(vars(matches('^Pass')),
                list(as.integer)) %>%
      mutate_at(vars(matches('^Pass')),
                list(~ if_else(is.na(.), as.integer(0), .)))
  }) %>%
  mutate(Year = str_sub(Season, 1, 4),
         Season = str_remove(Season, '[:digit:]+'),
         Season = str_trim(Season)) %>%
  mutate(Species = if_else(grepl('Chinook', Species),
                           'Chinook',
                           if_else(grepl('steelhead', Species),
                                   'Steelhead',
                                   as.character(NA)))) %>%
  filter(Season == 'Summer',
         !is.na(Species)) %>%
  # filter out one row that shouldn't be there
  filter(!(Year == 2014 & grepl('M23$', SiteName) & Pass1.M > 1000)) %>%
  select(Year, everything())

