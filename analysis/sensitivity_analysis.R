# Author: Kevin See
# Purpose: Sensitivity analysis on  number of QRF covariates
# Created: 4/8/2020
# Last Modified: 4/8/2020
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(QRFcapacity)
library(tidyverse)
library(janitor)
library(magrittr)
library(quantregForest)

# set default theme for ggplot
theme_set(theme_bw())

#-----------------------------------------------------------------
# load model fit
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds')[2]

load(paste0('output/modelFits/qrf_', mod_choice, '.rda'))

#-----------------------------------------------------------------
# relative importance of habtiat covariates
rel_imp_list = qrf_mods %>%
  map(.f = function(x) {
    as_tibble(x$importance,
              rownames = 'Metric') %>%
      mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
      left_join(hab_dict %>%
                  select(Metric = ShortName,
                         Name)) %>%
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., relImp))) %>%
      arrange(desc(Metric)) %>%
      distinct() %>%
      select(Metric, Name, everything())
  })

#-----------------------------------------------------------------
# prep some habitat data
#-----------------------------------------------------------------
# all the related habitat data
if(mod_choice %in% c('juv_summer', 'redds')) {
  data("champ_site_2011_17")
  hab_data = champ_site_2011_17
  
  data("champ_site_2011_17_avg")
  hab_avg = champ_site_2011_17_avg
  
  # add a metric showing "some" riparian canopy
  hab_data %<>%
    mutate(RipCovCanSome = 100 - RipCovCanNone)
  
  hab_avg %<>%
    mutate(RipCovCanSome = 100 - RipCovCanNone)
  
}

if(mod_choice == 'juv_summer_dash') {
  data("champ_dash")
  hab_data = champ_dash
  
  data("champ_dash_avg")
  hab_avg = champ_dash_avg
}

# alter a few metrics
hab_data %<>%
  # scale some metrics by site length
  mutate_at(vars(starts_with('LWVol'),
                 ends_with('_Vol')),
            list(~ . / Lgth_Wet * 100)) %>%
  # add a metric showing "some" fish cover
  mutate(FishCovSome = 100 - FishCovNone)

hab_avg %<>%
  # scale some metrics by site length
  mutate_at(vars(starts_with('LWVol'),
                 ends_with('_Vol')),
            list(~ . / Lgth_Wet * 100)) %>%
  # add a metric showing "some" fish cover
  mutate(FishCovSome = 100 - FishCovNone)


# add temperature metrics
data("champ_temps")
hab_avg %<>%
  left_join(champ_temps %>%
              as_tibble() %>%
              select(Site, avg_aug_temp = S2_02_11) %>%
              distinct())

hab_data %<>%
  left_join(hab_data %>%
              select(VisitID, Year = VisitYear) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(VisitID, avg_aug_temp = S2_02_11))) %>%
  left_join(hab_data %>%
              select(VisitID, Year = VisitYear) %>%
              distinct() %>%
              left_join(champ_temps %>%
                          as_tibble() %>%
                          select(Site:VisitID, S1_93_11:S36_2015) %>%
                          gather(scenario, aug_temp, S1_93_11:S36_2015) %>%
                          mutate(Year = str_sub(scenario, -4)) %>%
                          mutate_at(vars(Year),
                                    list(as.numeric)) %>%
                          filter(!is.na(Year)) %>%
                          select(Site:VisitID, Year, aug_temp))) %>%
  select(-Year)


#-----------------------------------------------------------------
# predict capacity at all CHaMP sites
#-----------------------------------------------------------------
# what quantile is a proxy for capacity?
pred_quant = 0.9

hab_impute = hab_avg %>%
  mutate_at(vars(Watershed, Channel_Type),
            list(fct_drop)) %>%
  impute_missing_data(data = .,
                      covars = unique(sel_hab_mets$Metric),
                      impute_vars = c('Watershed', 
                                      'Elev_M', 
                                      'Channel_Type', 
                                      'CUMDRAINAG'),
                      method = 'missForest') %>%
  select(Site, Watershed, LON_DD, LAT_DD, VisitYear, Lgth_Wet, Area_Wet, one_of(unique(sel_hab_mets$Metric)))



  
#-----------------------------------------------------------------
# step through dropping metrics one by one
#-----------------------------------------------------------------
tibble(Species = names(qrf_mods),
       Full_QRF = qrf_mods,
       rel_imp = rel_imp_list)

drop_df = tibble(covars = rep(NA, length(rel_imp_list$Chinook$Metric))) %>%
  mutate(covars = map(covars,
                      .f = identity))
for(i in 1:nrow(drop_df)) {
  drop_df$covars[i] = list(as.character(rel_imp_list$Chinook$Metric)[1:i])
}
drop_df %<>%
  mutate(qrf_mod = )



qrf_mod_df %>%
  group_by(Species) %>%
  nest() %>%
  rename(qrf_data = data) %>%
  left_join(sel_hab_mets %>%
              group_by(Species) %>%
              nest() %>%
              rename(metrics = data))

tibble(Species = 'Chinook',
       n_covars = sum(sel_hab_mets$Species == 'Chinook'),
       model = list(qrf_mods$Chinook))
