# Author: Kevin See
# Purpose: Compile Chinook spawner recruit data for spawners / parr 
# Created: 10/17/2019
# Last Modified: 10/17/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(Rcapture)

#-----------------------------------------------------------
# pull in some spawner-recruit data from Morgan Bond that he compiled from a variety of agencies
#-----------------------------------------------------------
# best guess for over-winter survival
overwinter_surv = 0.32
sr_data = read_csv('data/raw/spawn_rec/CRB_spawn_juv_kevin_all.csv') %>%
  rename(Fry_spring = `Fry migrants (spring/summer)`,
         Parr_fall = `Fall Parr migrants (Fall)`,
         Smolt_spring = `Spring smolt migrants`,
         Trap_location = `Trap location`,
         lat = `Trap Location Latitude`,
         long = `Trap Location Longitude`,
         Total_migrant_abunance = `Total migrant abundance`,
         Brood_Year = `Brood Year`) %>%
  mutate(Total_migrants = if_else(!is.na(Total_migrant_abunance), 
                                 Total_migrant_abunance, 
                                 if_else(is.na(Parr_fall), 
                                         Smolt_spring, 
                                         as.numeric(NA))),
         subbasin = recode(subbasin,
                           'Toucannon River' = 'Tucannon River'),
         Trap_location = recode(Trap_location,
                                'Toucannon River' = 'Tucannon River')) %>%
  # delete one point that seems crazy
  filter(!(Trap_location == 'Hood River' & Total_migrants > 35000)) %>%
  # delete one location that has questionable data
  filter(!grepl('Clackamas', Trap_location)) %>%
  # try to estimate total summer parr
  mutate(Parr = if_else(is.na(Parr_fall), 
                        Smolt_spring / overwinter_surv, 
                        Parr_fall + Smolt_spring / overwinter_surv),
         Spawners = redds,
         Spawner_type = 'redd counts')


# ODFW redd / parr data
odfw_data = read_excel('data/raw/spawn_rec/Snake_ChS_CATH_GRUM_wWeir_NOSAEJ_TSAEJ_SummerParr_forKevinSee_20141013.xlsx') %>%
  rename(Tot_spawners = TSAEJ,
         Nat_spawners = NOSAEJ,
         Parr = LateSummerParr) %>%
  select(NMFS_Common_Name, BroodYear, OutmigrationYear, Nat_spawners:Parr) %>%
  bind_rows(read_excel('data/raw/spawn_rec/Snake_ChS_NOSAEJ_TSAEJ_fromReddEst_SummerParr_forKevinSee_20151013.xlsx') %>%
              rename(Tot_spawners = TSAEJ,
                     Nat_spawners = NOSAEJ,
                     Parr = LateSummerParr) %>%
              select(NMFS_Common_Name, BroodYear, OutmigrationYear, Nat_spawners:Parr)) %>%
  arrange(NMFS_Common_Name, BroodYear) %>%
  mutate(Spawners = Tot_spawners,
         Spawner_type = 'total spawners')

# Data for the Chiwawa from Tracy Hilman
chiw_data = read_csv('data/raw/spawn_rec/ChiwawaData_TracyHillman.csv') %>%
  rename(BroodYear = BY,
         Spawners = `Stock (Spawners)`,
         Eggs = `Total Eggs`,
         Smolts = `Number of Yearlings (Smolts)`) %>%
  # decide whether to use resident or total parr (includes migrants into the Chiwawa)
  mutate(#Parr = `Resident Parr``,
    Parr = `Total Parr`,
    Spawner_type = 'total spawners')

