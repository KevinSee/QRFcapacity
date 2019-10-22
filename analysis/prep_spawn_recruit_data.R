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
library(PITcleanr)
library(DABOM)

theme_set(theme_bw())

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

#-----------------------------------------
# data for Lemhi
#-----------------------------------------

#-----------------------------------------
# escapement estimates from DABOM
lem_spwn = 2010:2018 %>%
  as.list() %>%
  rlang::set_names() %>%
  map_df(.id = 'BroodYear',
         .f = function(x) {
           yr = x[1]
           
           cat(paste('Compiling', spp, 'in', yr, '\n'))
           
           load(paste0('/Users/kevin/Dropbox/ISEMP/Git/SnakeBasinFishStatus/STADEM_results/LGR_STADEM_Chinook_', yr,'.rda')) 
           load(paste0('/Users/kevin/Dropbox/ISEMP/Git/SnakeBasinFishStatus/DABOM_results/LGR_DABOM_Chinook_', yr,'.rda'))
           
           lem_summ = calcTribEscape_LGD(dabom_mod,
                                         stadem_mod,
                                         stadem_param_nm = 'X.new.wild',
                                         bootstrap_samp = 2000, #2000
                                         node_order = proc_list$NodeOrder,
                                         summ_results = T,
                                         pt_est_nm = 'mean',
                                         cred_int_prob = 0.95) %>%
             mutate(species = spp) %>%
             select(species, everything()) %>%
             filter(area %in% c('Lemhi', 'past_HYC', 'past_LRW'))
           
           rm(yr, dabom_list, dabom_mod, proc_list, stadem_list, stadem_mod)
           return(lem_summ)
         }) %>%
  mutate_at(vars(BroodYear),
            list(as.numeric))

# screw trap estimates by lifestage and population
lem_rst = excel_sheets('data/raw/spawn_rec/Lemhi RST Chinook Outmigration Status.xlsx') %>%
  as.list() %>%
  rlang::set_names() %>%
  map_df(.id = 'Trap',
         .f = function(x) {
           
           col_range = if_else(x == 'L3AO',
                               'A:K',
                               'A:H')
           
           rst_est = read_excel('data/raw/spawn_rec/Lemhi RST Chinook Outmigration Status.xlsx',
                      x,
                      range = cell_cols(col_range))
           if(x == "L3AO") {
             rst_est %<>%
               rename(LB = `95% NL`,
                      UB = `95% NU`) %>%
               select(-C, -M, -R)  
           }
           
           rst_est %>%
             mutate_at(vars(Est:CV),
                       list(as.numeric))
         }) %>%
  rename(BroodYear = `Brood Year`,
         lifestage = `Life Stage`,
         RST_est = Est) %>%
  mutate(Population = if_else(Trap == 'Hayden',
                              'Hayden Creek',
                              if_else(Trap == 'LRW',
                                      'Upper Lemhi',
                                      'Lemhi River'))) %>%
  select(Population, BroodYear, lifestage, RST_est:UB)

# survival estimates by lifestage and population (generated from TribPIT)
lem_surv = read_excel('data/raw/spawn_rec/Lemhi Chinook Survival 191015.xlsx',
                          'Survival estimates',
                          range = 'A13:L21') %>%
  rename(lifestage = `Upper Lemhi sub-population`) %>%
  mutate(Population = 'Upper Lemhi') %>%
  gather(BroodYear, survival, -Population, -lifestage) %>%
  mutate(survival = str_split(survival, '\\(', simplify = T)[,1],
         survival = as.numeric(survival)) %>%
  bind_rows(read_excel('data/raw/spawn_rec/Lemhi Chinook Survival 191015.xlsx',
                       'Survival estimates',
                       range = 'A3:L11') %>%
              rename(lifestage = `Hayden sub-population`) %>%
              mutate(Population = 'Hayden Creek') %>%
              gather(BroodYear, survival, -Population, -lifestage) %>%
              mutate(survival = str_split(survival, '\\(', simplify = T)[,1],
                     survival = as.numeric(survival))) %>%
  bind_rows(read_excel('data/raw/spawn_rec/Lemhi Chinook Survival 191015.xlsx',
                       'Survival estimates',
                       range = 'A23:L29') %>%
              rename(lifestage = `Combined Lemhi subbasin`) %>%
              mutate(Population = 'Lemhi River') %>%
              gather(BroodYear, survival, -Population, -lifestage) %>%
              mutate(survival = str_split(survival, '\\(', simplify = T)[,1],
                     survival = as.numeric(survival))) %>%
  select(Population, BroodYear, lifestage, survival)

# back estimates of parr in each population by brood year
lem_parr = lem_rst %>%
  filter(lifestage != 'Fry') %>%
  mutate_at(vars(RST_est),
            list(~ if_else(is.na(.), 0, .))) %>%
  select(Population:RST_est) %>%
  left_join(lem_surv %>%
              mutate_at(vars(BroodYear),
                        list(as.numeric)) %>%
              mutate(lifestage = recode(lifestage,
                                        'Parr' = 'Presmolt',
                                        'Over - Winter Upper Lemhi' = 'Smolt',
                                        'Over - Winter Hayden Creek' = 'Smolt',
                                        'Over - Winter Lemhi subbasin' = 'Smolt')) %>%
              mutate(lifestage = if_else(lifestage == 'Parr/Presmolt to Mouth of Lemhi' &
                                           Population == 'Lemhi River',
                                         'Presmolt',
                                         lifestage)) %>%
              filter(lifestage %in% c('Presmolt', 'Smolt'))) %>%
  left_join(lem_surv %>%
              mutate(lifestage = recode(lifestage,
                                        'Parr' = 'Presmolt',
                                        'Over - Winter Upper Lemhi' = 'Smolt',
                                        'Over - Winter Hayden Creek' = 'Smolt',
                                        'Over - Winter Lemhi subbasin' = 'Smolt')) %>%
              mutate(lifestage = if_else(lifestage == 'Parr/Presmolt to Mouth of Lemhi' &
                                           Population == 'Lemhi River',
                                         'Presmolt',
                                         lifestage)) %>%
              filter(lifestage %in% c('Presmolt', 'Smolt'),
                     BroodYear == 'Average') %>%
              select(Population, lifestage, avg_survival = survival)) %>%
  mutate_at(vars(survival, avg_survival),
            list(~ if_else(lifestage == 'Parr',
                           1,
                           .))) %>%
  mutate(total = RST_est / survival) %>%
  mutate(total = if_else(is.na(survival),
                         RST_est / avg_survival,
                         total)) %>%
  group_by(Population, BroodYear) %>%
  summarise(Parr = sum(total)) %>%
  ungroup()

# put spawners and parr together
lem_data = lem_parr %>%
  full_join(lem_spwn %>%
              mutate(Population = if_else(area == 'past_HYC',
                                          'Hayden Creek',
                                          if_else(area == 'past_LRW',
                                                  'Upper Lemhi',
                                                  'Lemhi River'))) %>%
              select(Population, BroodYear, Spawners = estimate)) %>%
  mutate(Spawner_type = 'total spawners')

# get locations for RST within Lemhi subbasin
mmr_meta = queryMRRMeta()
lemhi_locs = mmr_meta %>%
  filter(siteCode %in% c('HYDTRP', 
                         'LLRTP',
                         'LEMTRP')) %>%
  # as.data.frame()
  mutate(Population = if_else(siteCode == 'HYDTRP',
                              'Hayden Creek',
                              if_else(siteCode == 'LEMTRP',
                                      'Upper Lemhi',
                                      'Lemhi River'))) %>%
  select(Site = siteCode,
         Population,
         lat = latitude,
         long = longitude)

#-----------------------------------------
# put it all together
#-----------------------------------------
spawn_recr_data = odfw_data %>%
  select(Population = NMFS_Common_Name,
         BroodYear, Spawners, Spawner_type, Parr) %>%
  mutate(Population = recode(Population,
                             'Grande Ronde River Upper Mainstem' = 'Upper Grande Ronde River')) %>%
  bind_rows(chiw_data %>%
              select(BroodYear, Spawners, Spawner_type, Parr) %>%
              mutate(Population = 'Chiwawa R.')) %>%
  bind_rows(lem_data) %>%
  mutate(ParrEst = T) %>%
  bind_rows(anti_join(sr_data %>%
                        mutate(ParrEst = F) %>%
                        select(Population = Trap_location,
                               BroodYear = Brood_Year,
                               Spawners, Spawner_type, Parr, ParrEst),
                      .,
                      by = c('Population', 'BroodYear'))) %>%
  filter(!is.na(Spawners),
         !is.na(Parr),
         Parr > 0) %>%
  left_join(sr_data %>%
              select(Population = Trap_location,
                     BroodYear = Brood_Year,
                     trap_lat = lat,
                     trap_long = long) %>%
              distinct()) %>%
  mutate(trap_lat = ifelse(Population == 'Lemhi River', unique(sr_data$lat[sr_data$Trap_location == 'Lemhi River']), trap_lat),
         trap_long = ifelse(Population == 'Lemhi River', unique(sr_data$long[sr_data$Trap_location == 'Lemhi River']), trap_long)) %>%
  left_join(lemhi_locs) %>%
  mutate(trap_lat = if_else(is.na(trap_lat), lat, trap_lat),
         trap_long = if_else(is.na(trap_long), long, trap_long)) %>%
  select(-Site, -lat, -long) %>%
  select(Population, BroodYear, trap_lat, trap_long, everything()) %>%
  arrange(Population, BroodYear)

filter(spawn_recr_data,
       is.na(trap_lat) | is.na(trap_long)) %>%
  xtabs(~ Population + BroodYear, .)

spawn_recr_data %>%
  filter(!is.na(Parr),
         !is.na(Spawners)) %>%
  group_by(Population) %>%
  summarise(n_yrs = n_distinct(BroodYear),
            n_methods = n_distinct(Spawner_type)) %>%
  arrange(desc(n_yrs))


#-----------------------------------------
# save data
#-----------------------------------------
# save as csv file
write_csv(spawn_recr_data,
          'data/prepped/spawner_recruit_data.csv')

# save to use as data
use_data(spawn_recr_data,
         version = 2,
         overwrite = T)
