# Author: Kevin See
# Purpose: Prep redd data
# Created: 5/14/2019
# Last Modified: 5/14/19
# Notes: 


#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)

#-----------------------------------------------------------------
# Read in and prepare CHaMP habitat data
champ_sites = read_csv(file = 'data/raw/fish/CHAMPsites_With_RID_MEAS.csv') %>%
  select(ProgramSiteID, SiteName, WatershedID, WatershedName, Stream, 
         LAT_DD, LON_DD,
         RID, MEAS_champ = MEAS, 
         NEAR_DIST, NEAR_X, NEAR_Y, minArea_m2)

# Read in and prepare redd data
redd_df = read_csv(file = 'data/raw/fish/ReddData_With_RID_MEAS.csv') %>%
  # For Entiat, we decided to remove the summer Chinook redd data. The summer Chinook population in the Entiat is completely a product of hatchery propagation.
  filter(!(Species == 'Chinook' & Basin == 'Entiat' & Run == 'Summer')) %>%
  rename(MEAS_redd = MEAS) %>%
  mutate(reddID = 1:n())

# Set the distance buffer. We will count redds within this distance (meters) above and below the CHaMP x-site
buffer <- 500

redd_champ = full_join(champ_sites %>%
                         select(SiteName, WatershedName:MEAS_champ),
                       redd_df %>%
                         select(-Stream)) %>%
  filter(MEAS_redd <= MEAS_champ + buffer & MEAS_redd >= MEAS_champ - buffer)

# redd counts, by site / species / year
reddsSiteYr = redd_champ %>%
  group_by(WatershedName, SiteName, Species, Year) %>%
  summarise(nRedds = n_distinct(reddID)) %>%
  ungroup() %>%
  arrange(Species, WatershedName, SiteName, Year)

# pull out maximum number of redds found at any CHaMP site
maxRedds = redd_champ %>%
  group_by(SiteName, Species, Year) %>%
  summarise(ReddsPerKm = n_distinct(reddID)) %>%
  ungroup() %>%
  group_by(SiteName, Species) %>%
  filter(ReddsPerKm == max(ReddsPerKm, na.rm = T)) %>%
  # if some years are tied, use the latest year (to better match CHaMP data)
  filter(Year == max(Year)) %>%
  rename(maxReddsPerKm = ReddsPerKm) %>%
  ungroup() %>%
  left_join(champ_sites %>%
              select(SiteName, WatershedName, minArea_m2)) %>%
  mutate(maxReddsPerMsq = maxReddsPerKm / minArea_m2) %>%
  select(Species, WatershedName, SiteName, maxYr = Year, maxReddsPerKm, maxReddsPerMsq) %>%
  arrange(WatershedName, SiteName, Species)

#-----------------------------------------------------------------
# save redd data
use_data(reddsSiteYr, maxRedds,
         version = 2,
         overwrite = T)
