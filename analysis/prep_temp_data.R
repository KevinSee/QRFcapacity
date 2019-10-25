# Author: Kevin See
# Purpose: prep NorWeST temperature data for use in QRF models
# Created: 10/18/2019
# Last Modified: 10/18/19
# Notes: all NorWeST shapefiles were downloaded from https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/ModeledStreamTemperatureScenarioMaps.shtml

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(sf)
# library(lubridate)
# library(magrittr)

theme_set(theme_bw())

#-----------------------------------------------------------------
# set projection we'd like to use consistently
my_crs = 5070

#-----------------------------------------------------------------
# read in shapefiles with mean aug temperature predictions from NorWeST
int_crb_temp = read_sf('data/raw/temperature/NorWeST_PredictedStreamTempLines_Salmon/NorWeST_PredictedStreamTempLines_Salmon.shp') %>%
  st_transform(crs = my_crs) %>%
  mutate(NorWeST_area = 'Salmon') %>%
  select(OBSPRED_ID:BFI, NorWeST_area, GNIS_NAME, COMID, Air_Aug, Flow_Aug, everything()) %>%
  rbind(read_sf('data/raw/temperature/NorWeST_PredictedStreamTempLines_Clearwater/NorWeST_PredictedStreamTempLines_Clearwater.shp') %>%
          st_transform(crs = my_crs) %>%
          mutate(NorWeST_area = 'Clearwater') %>%
          select(one_of(names(.)))) %>%
  rbind(read_sf('data/raw/temperature/NorWeST_PredictedStreamTempLines_MidColumbia_MWMT/NorWeST_PredictedStreamTempLines_MidColumbia_MWMT.shp') %>%
          st_transform(crs = my_crs) %>%
          mutate(NorWeST_area = 'MidColumbia_MWMT') %>%
          rename(Air_Aug = Air_Temp,
                 Flow_Aug = Flow) %>%
          select(one_of(names(.)))) %>%
  rbind(read_sf('data/raw/temperature/NorWeST_PredictedStreamTempLines_UpperColumbiaYakima_MWMT/NorWeST_PredictedStreamTempLines_UpperColumbiaYakima_MWMT.shp') %>%
          st_transform(crs = my_crs) %>%
          mutate(NorWeST_area = 'UpperColumbiaYakima_MWMT') %>%
          rename(Air_Aug = Air_Temp,
                 Flow_Aug = Flow) %>%
          select(one_of(names(.)))) %>%
  rbind(read_sf('data/raw/temperature/NorWeST_PredictedStreamTempLines_MiddleSnake/NorWeST_PredictedStreamTempLines_MiddleSnake.shp') %>%
          st_transform(crs = my_crs) %>%
          mutate(NorWeST_area = 'MiddleSnake') %>%
          add_column(S33_2012 = NA,
                     S34_2013 = NA,
                     S35_2014 = NA,
                     S36_2015 = NA) %>%
          select(one_of(names(.)))) %>%
  # mark any value that's listed as -9999, NA
  mutate_at(vars(S1_93_11:S36_2015),
            list(~if_else(. == -9999,
                         as.numeric(NA),
                         .))) %>%
  # mark any value that's listed as less than 0 degrees, NA
  mutate_at(vars(S1_93_11:S36_2015),
            list(~if_else(. < 0,
                          as.numeric(NA),
                          .)))

#-----------------------------------------------------------------
# save temperature predictions
#-----------------------------------------------------------------
# make available like in a package, by calling "data()"
# used xz compression because it made the smallest .rda object
use_data(int_crb_temp,
         version = 2,
         compress = "xz",
         overwrite = T)

# save as shapefile
write_sf(int_crb_temp,
         dsn = 'data/prepped/NorWeST_temps.shp',
         driver = 'ESRI Shapefile',
         delete_layer = T)

#-----------------------------------------------------------------
# put into long data format
int_crb_temp_long = int_crb_temp %>%
  gather(scenario, aug_temp, starts_with('S'), -SLOPE)

# where do we have missing predictions?
int_crb_temp_long %>%
  filter(is.na(aug_temp)) %>%
  xtabs(~ NorWeST_area + scenario, .)

# plot the spatial extents of a couple scenarios
int_crb_temp %>%
  select(S2_02_11, S36_2015) %>%
  plot()

#-----------------------------------------------------------------
# Richie joined the CHaMP 2011-2017 points to this temperature file
champ_temps = st_read('data/prepped/CHaMP_NorW.shp') %>%
  st_transform(crs = my_crs) %>%
  rename(NorWeST_area = NrWST_r,
         Flow_Aug = Flow_Ag) %>%
  select(-c(FID_1, FID_2:TAILWAT, Y_COORD:BFI, COMID))
# correct some names that ArcGIS messed up
names(champ_temps)[c(13:48)] = names(int_crb_temp)[c(19:54)]

use_data(champ_temps,
         version = 2,
         compress = "xz",
         overwrite = T)
