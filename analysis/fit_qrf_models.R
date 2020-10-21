# Author: Kevin See
# Purpose: Fit QRF models
# Created: 10/19/2020
# Last Modified: 10/19/2020
# Notes: fish density is fish / m for summer juvs and redds, but fish / m^2 for winter juvs

#-----------------------------------------------------------------
# load needed libraries
library(QRFcapacity)
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(quantregForest)

# set default theme for ggplot
theme_set(theme_bw())

#-----------------------------------------------------------------
# which QRF model to fit? 
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds',
               'juv_winter')[4]

for(mod_choice in c('juv_summer',
                    'juv_summer_dash',
                    'redds',
                    'juv_winter')) {
  
  #-----------------------------------------------------------------
  # determine which set of fish/habitat data to use
  if(mod_choice == "juv_summer") {
    data("fh_sum_champ_2017")
    fish_hab = fh_sum_champ_2017 %>%
      mutate_at(vars(Watershed, Year),
                list(as.factor))
  } else if(mod_choice == "juv_summer_dash") {
    data("fh_sum_dash_2014_17")
    fish_hab = fh_sum_dash_2014_17 %>%
      mutate_at(vars(Watershed, Year),
                list(as.factor))
  } else if(mod_choice == "redds") {
    data("fh_redds_champ_2017")
    fish_hab = fh_redds_champ_2017 %>%
      mutate_at(vars(Watershed),
                list(as.factor))
  } else if(mod_choice == "juv_winter") {
    data("fh_win_champ_2017")
    fish_hab = fh_win_champ_2017 %>%
      filter(!is.na(fish_dens)) %>%
      mutate_at(vars(Watershed, Year, Tier1),
                list(as.factor))
    
  }
  
  # and the appropriate habitat dictionary to go with it
  data("hab_dict_2017")
  if(mod_choice == "juv_winter") {
    hab_dict = hab_dict_2017 %>%
      bind_rows(hab_dict_2017 %>%
                  filter(ShortName == 'Q') %>%
                  mutate(ShortName = 'Discharge')) %>%
      filter(ShortName != "DpthThlwgExit" |
               (ShortName == "DpthThlwgExit" & MetricGroupName == 'Channel Unit')) %>%
      bind_rows(tibble(ShortName = 'LWCount',
                       Name = 'Large Wood Count',
                       MetricGroupName = 'Channel Unit',
                       DescriptiveText = 'Total number of qualifying pieces of large wood',
                       UnitOfMeasure = 'count',
                       MetricCategory = 'Wood'))
  } else {
    hab_dict = hab_dict_2017
    # change some of the descriptions for large wood volume
    hab_dict %<>%
      mutate(DescriptiveText = if_else(grepl("^LWVol", ShortName),
                                       paste0(str_remove(DescriptiveText, ".$"),
                                              ", scaled by site length."),
                                       DescriptiveText),
             UnitOfMeasure = if_else(grepl("^LWVol", ShortName),
                                     paste0(UnitOfMeasure,
                                            " per 100 meters"),
                                     UnitOfMeasure),
             UnitOfMeasureAbbrv = if_else(grepl("^LWVol", ShortName),
                                          paste0(UnitOfMeasureAbbrv,
                                                 "/100m"),
                                          UnitOfMeasureAbbrv)) %>%
      # add description for some riparian canopy
      bind_rows(hab_dict_2017 %>%
                  filter(ShortName == "RipCovCanNone") %>%
                  mutate(ShortName = "RipCovCanSome",
                         Name = "Riparian Cover: Some Canopy",
                         DescriptiveText = "Percent of riparian canopy with some vegetation.")) %>%
      # add description for no riparian ground cover
      bind_rows(hab_dict_2017 %>%
                  filter(ShortName == "RipCovGrnd") %>%
                  mutate(ShortName = "RipCovGrndNone",
                         Name = "Riparian Cover: No Ground",
                         DescriptiveText = "Percent of groundcover with no vegetation."))
  }
  
  hab_dict %<>%
    # add description for some fish cover
    bind_rows(hab_dict_2017 %>%
                filter(ShortName == "FishCovNone") %>%
                mutate(ShortName = "FishCovSome",
                       Name = "Fish Cover: Some Cover",
                       DescriptiveText = "Percent of wetted area with some form of fish cover"))
  
  # all the related habitat data
  data("champ_site_2011_17")
  hab_data = champ_site_2011_17
  
  data("champ_site_2011_17_avg")
  hab_avg = champ_site_2011_17_avg
  
  if(mod_choice == "juv_winter") {
    data("champ_cu")
    
    hab_data = champ_cu %>%
      mutate(Tier1 = recode(Tier1,
                            'Fast-NonTurbulent/Glide' = 'Run',
                            'Fast-Turbulent' = 'Riffle',
                            'Slow/Pool' = 'Pool',
                            'Small Side Channel' = 'SSC'),
             Tier1 = fct_explicit_na(Tier1)) %>%
      # add some site-scale metrics
      inner_join(champ_site_2011_17 %>%
                   filter(VisitObjective == 'Primary Visit',
                          VisitStatus == 'Released to Public') %>%
                   filter(!Watershed %in% c('Big-Navarro-Garcia (CA)',
                                            'CHaMP Training',
                                            'Region 17')) %>%
                   select(Site, Watershed, VisitID, VisitYear,
                          Channel_Type, Elev_M, CUMDRAINAG, 
                          LON_DD, LAT_DD,
                          DistPrin1,
                          Discharge = Q,
                          Lgth_Wet, Area_Wet,
                          CU_Freq,
                          Sin,
                          SubD50))
    
    hab_avg = champ_cu %>%
      mutate(Tier1 = recode(Tier1,
                            'Fast-NonTurbulent/Glide' = 'Run',
                            'Fast-Turbulent' = 'Riffle',
                            'Slow/Pool' = 'Pool',
                            'Small Side Channel' = 'SSC'),
             Tier1 = fct_explicit_na(Tier1)) %>%
      # # use VisitID as proxy for sample date
      # group_by(Site) %>%
      # filter(VisitID == max(VisitID)) %>%
      # ungroup()
      inner_join(champ_site_2011_17 %>%
                   filter(VisitObjective == 'Primary Visit',
                          VisitStatus == 'Released to Public') %>%
                   filter(Watershed %in% unique(hab_data$Watershed)) %>%
                   select(VisitID, Site, SampleDate)) %>%
      group_by(Site) %>%
      filter(SampleDate == max(SampleDate, na.rm = T)) %>%
      ungroup() %>%
      select(-SampleDate) %>%
      # add some site-scale metrics
      left_join(champ_site_2011_17_avg %>%
                  select(Site, Watershed,
                         Channel_Type, Elev_M, CUMDRAINAG, 
                         LON_DD, LAT_DD,
                         DistPrin1,
                         Discharge = Q,
                         Lgth_Wet, Area_Wet,
                         CU_Freq,
                         Sin,
                         SubD50))
  }
  
  
  # alter a few metrics
  fish_hab %<>%
    # scale some metrics by site length
    mutate_at(vars(starts_with('LWVol'),
                   ends_with('_Vol')),
              list(~ . / Lgth_Wet * 100)) %>%
    # add a metric showing "some" fish cover
    mutate(FishCovSome = 100 - FishCovNone)
  
  hab_data %<>%
    # scale some metrics by site length
    mutate(across(starts_with('LWVol') |
                    ends_with('_Vol'),
                  ~ . / Lgth_Wet * 100)) %>%
    # add a metric showing "some" fish cover
    mutate(FishCovSome = 100 - FishCovNone)
  
  hab_avg %<>%
    # scale some metrics by site length
    mutate(across(starts_with('LWVol') |
                    ends_with('_Vol'),
                  ~ . / Lgth_Wet * 100)) %>%
    # add a metric showing "some" fish cover
    mutate(FishCovSome = 100 - FishCovNone)
  
  # add a metric showing "some" riparian canopy
  if(!mod_choice %in% c("juv_winter", "juv_summer_dash")) {
    
    fish_hab %<>%
      mutate(RipCovCanSome = 100 - RipCovCanNone)
    
    hab_data %<>%
      mutate(RipCovCanSome = 100 - RipCovCanNone)
    
    hab_avg %<>%
      mutate(RipCovCanSome = 100 - RipCovCanNone)
  }
  
  # add temperature metrics
  if(mod_choice != "juv_winter") {
    data("champ_temps")
    hab_avg %<>%
      left_join(champ_temps %>%
                  as_tibble() %>%
                  select(Site, 
                         avg_aug_temp = S2_02_11) %>%
                  distinct())
    
    hab_data %<>%
      left_join(hab_data %>%
                  select(VisitID, Year = VisitYear) %>%
                  distinct() %>%
                  left_join(champ_temps %>%
                              as_tibble() %>%
                              select(VisitID, 
                                     avg_aug_temp = S2_02_11))) %>%
      left_join(hab_data %>%
                  select(VisitID, Year = VisitYear) %>%
                  distinct() %>%
                  left_join(champ_temps %>%
                              as_tibble() %>%
                              select(Site:VisitID, S1_93_11:S36_2015) %>%
                              pivot_longer(cols = -(Site:VisitID),
                                           names_to = "scenario",
                                           values_to = "aug_temp") %>%
                              mutate(Year = str_sub(scenario, -4)) %>%
                              mutate(Year = as.numeric(Year)) %>%
                              filter(!is.na(Year)) %>%
                              select(Site:VisitID, Year, aug_temp))) %>%
      select(-Year)
  }
  
  #-----------------------------------------------------------------
  # clip Chinook data to Chinook domain
  # all winter sites within Chinook domain by design
  if(mod_choice != "juv_winter") {
    data("rch_200")
    data("champ_site_rch")
    
    chnk_sites = champ_site_rch %>%
      inner_join(rch_200 %>%
                   select(UniqueID, chnk)) %>%
      filter(chnk) %>%
      pull(Site) %>%
      as.character()
    
    # add Big Springs and Little Springs sites in the Lemhi
    chnk_sites = c(chnk_sites,
                   hab_data %>%
                     filter(grepl('Big0Springs', Site) | grepl('Little0Springs', Site)) %>%
                     pull(Site) %>%
                     unique()) %>%
      unique()
    
    
    # only keep Chinook data from sites in Chinook domain
    fish_hab %<>%
      filter(Species == 'Steelhead' |
               (Species == 'Chinook' & (Site %in% chnk_sites | fish_dens > 0))) 
    
  }
  
  #-----------------------------------------------------------------
  # select which habitat metrics to use in QRF model
  #-----------------------------------------------------------------
  if(mod_choice == "juv_summer") {
    sel_hab_mets = crossing(Species = c('Chinook', 
                                        'Steelhead'),
                            Metric = c('CU_Freq',
                                       'SlowWater_Pct',
                                       'BfWdth_CV',
                                       'DpthWet_SD',
                                       'WetWDRat_Avg',
                                       'Sin',
                                       'WetBraid',
                                       'FishCovSome',
                                       'UcutLgth_Pct',
                                       'DistPrin1',
                                       'RipCovBigTree',
                                       'RipCovGrnd',
                                       'WetSC_Pct',
                                       'WetWdth_Avg',
                                       'Grad',
                                       'SubD50',
                                       'SubLT6',
                                       'SubEstCbl',
                                       'avg_aug_temp',
                                       'Cond',
                                       # 'LWVol_Bf',
                                       'LWVol_BfSlow'))
  } else if(mod_choice == "juv_summer_dash") {
    sel_hab_mets = crossing(Species = c('Chinook', 
                                        'Steelhead'),
                            Metric = c('CU_Freq',
                                       # 'FstTurb_Freq',
                                       # 'FstNT_Freq',
                                       # 'WetWdth_CV', # nope
                                       'WetBraid',
                                       'Sin_CL',
                                       # 'Sin',
                                       'UcutLgth_Pct',
                                       'FishCovSome',
                                       'WetSC_Pct',
                                       'WetWdth_Int', #area of wetted polygon divided by centerline length, not available for >= 2019?
                                       'Q', # try MeanU instead
                                       'SubEstGrvl',
                                       'avg_aug_temp',
                                       # 'LWFreq_Wet',
                                       # 'LWVol_WetFstTurb',
                                       'LWVol_Wet',
                                       'SlowWater_Pct')) #,
    # 'NatPrin1',
    # 'DistPrin1'))
  } else if(mod_choice == "redds") {
    sel_hab_mets = crossing(Species = c('Chinook', 
                                        'Steelhead'),
                            Metric = c('SubEstGrvl',
                                       'SubLT2',
                                       'CU_Freq',
                                       "DetrendElev_SD",
                                       "FishCovSome",
                                       "UcutLgth_Pct",
                                       "RipCovCanSome",
                                       "Q",
                                       "DistPrin1",
                                       "PoolResidDpth",
                                       "avg_aug_temp",
                                       "LWFreq_Wet"))
  } else if(mod_choice == "juv_winter") {
    sel_hab_mets = crossing(Species = c('Chinook', 
                                        'Steelhead'),
                            Metric = c('Tier1',
                                       'Discharge',
                                       'CU_Freq',
                                       'Sin',
                                       'SubD50',
                                       'DpthResid',
                                       'FishCovSome',
                                       'DistPrin1',
                                       'LWCount',
                                       'SubEstSandFines'))
  }
  
  #-----------------------------------------------------------------
  # Fit QRF model
  #-----------------------------------------------------------------
  # impute missing data in fish / habitat dataset
  
  # impute missing habitat metrics once, for both species
  all_covars = sel_hab_mets %>%
    pull(Metric) %>%
    unique()
  
  tabyl(all_covars %in% names(fish_hab))
  all_covars[!all_covars %in% names(fish_hab)]
  
  # how many data points per metric?
  fish_hab %>%
    group_by(Species) %>%
    summarise(n_sites = n()) %>%
    left_join(sel_hab_mets %>%
                group_by(Species) %>%
                summarise(n_mets = n_distinct(Metric))) %>%
    mutate(pts_per_met = n_sites / n_mets)
  
  impute_covars = c('Watershed', 'Elev_M', 'Sin', 'Year', 'CUMDRAINAG')
  if(mod_choice == "redds") {
    impute_covars = impute_covars[!grepl('Year', impute_covars)]
  }
  if(mod_choice == "juv_summer_dash") {
    impute_covars = str_replace(impute_covars, "^Sin$", "Sin_CL")
  }
  
  qrf_mod_df = fish_hab %>%
    mutate(id = 1:n()) %>%
    select(id,
           all_of(impute_covars),
           all_of(all_covars)) %>%
    impute_missing_data(covars = all_covars,
                        impute_vars = impute_covars,
                        method = 'missForest') %>%
    left_join(fish_hab %>%
                mutate(id = 1:n()) %>%
                select(id, 
                       Species, Site, Watershed,
                       LON_DD, LAT_DD,
                       fish_dens)) %>%
    select(id, Species, Site, Watershed, LON_DD, LAT_DD, fish_dens, one_of(all_covars))
  
  if(mod_choice %in% c('juv_summer',
                       'juv_summer_dash')) {
    qrf_mod_df %<>%
      left_join(fish_hab %>%
                  mutate(id = 1:n()) %>%
                  select(id, 
                         VisitID,
                         Year)) %>%
      select(Species:Watershed,
             Year,
             VisitID,
             everything(),
             -id)
  } else if(mod_choice == "redds") {
    qrf_mod_df %<>%
      left_join(fish_hab %>%
                  mutate(id = 1:n()) %>%
                  select(id, 
                         maxYr)) %>%
      select(Species:Watershed,
             maxYr,
             everything(),
             -id)
  } else if(mod_choice == "juv_winter") {
    qrf_mod_df %<>%
      left_join(fish_hab %>%
                  mutate(id = 1:n()) %>%
                  select(id, 
                         VisitID,
                         Year, ChUnitNumber, SiteUnit)) %>%
      select(Species:Watershed,
             VisitID,
             Year, ChUnitNumber, SiteUnit,
             everything(),
             -id)
  }
  
  rm(all_covars)
  
  # fit the QRF model
  # set the density offset (to accommodate 0s)
  range(fish_hab$fish_dens)
  summary(fish_hab$fish_dens)
  dens_offset = if_else(mod_choice == 'redds',
                        0,
                        0.005)
  
  # fit random forest models
  qrf_mods = qrf_mod_df %>%
    split(list(.$Species)) %>%
    map(.f = function(z) {
      
      covars = sel_hab_mets %>%
        filter(Species == unique(z$Species)) %>%
        pull(Metric)
      
      set.seed(3)
      qrf_mod = quantregForest(x = z %>%
                                 select(one_of(covars)),
                               y = z %>%
                                 mutate(across(fish_dens,
                                               ~ log(. + dens_offset))) %>%
                                 pull(fish_dens),
                               keep.inbag = T,
                               ntree = 1000)
      
      return(qrf_mod)
    })
  
  # save some results
  save(fish_hab, 
       sel_hab_mets,
       qrf_mod_df,
       dens_offset,
       qrf_mods,
       hab_dict,
       hab_data,
       hab_avg,
       file = paste0('output/modelFits/qrf_', mod_choice, '.rda'))
  
}

#-----------------------------------------------------------------
# create a few figures
#-----------------------------------------------------------------
mod_choice = c('juv_summer',
               'juv_summer_dash',
               'redds',
               'juv_winter')[2]

load(paste0('output/modelFits/qrf_', mod_choice, '.rda'))

# relative importance of habitat covariates
rel_imp_p = tibble(Species = names(qrf_mods),
                   qrf_mod = qrf_mods) %>%
  mutate(rel_imp = map(qrf_mod,
                       .f = function(x) {
                         as_tibble(x$importance,
                                   rownames = 'Metric') %>%
                           mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
                           left_join(hab_dict %>%
                                       select(Metric = ShortName,
                                              Name)) %>%
                           mutate(across(c(Metric, Name),
                                         ~ fct_reorder(., relImp))) %>%
                           arrange(Metric) %>%
                           distinct()
                       })) %>%
  unnest(cols = rel_imp) %>%
  select(-qrf_mod) %>%
  mutate(across(c(Metric, Name),
                fct_reorder,
                .x = relImp)) %>%
  ggplot(aes(x = Name,
             y = relImp,
             fill = Species)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(x = 'Metric',
       y = 'Relative Importance')
rel_imp_p

# partial dependence plots
# for Chinook
if(mod_choice == "juv_winter") {
  chnk_pdp = plot_partial_dependence_v2(qrf_mods[['Chinook']],
                                        qrf_mod_df %>%
                                          filter(Species == 'Chinook'),
                                        data_dict = hab_dict,
                                        # log_transform = F,
                                        log_offset = dens_offset,
                                        # scales = "free_x") +
                                        scales = 'free') +
    labs(title = 'Chinook')
} else {
  chnk_pdp = plot_partial_dependence(qrf_mods[['Chinook']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Chinook'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     # scales = "free_x") +
                                     scales = 'free') +
    labs(title = 'Chinook')
}
chnk_pdp

# for steelhead
if(mod_choice == "juv_winter") {
  sthd_pdp = plot_partial_dependence_v2(qrf_mods[['Steelhead']],
                                        qrf_mod_df %>%
                                          filter(Species == 'Steelhead'),
                                        data_dict = hab_dict,
                                        # log_transform = F,
                                        log_offset = dens_offset,
                                        # scales = "free_x") +
                                        scales = 'free') +
    labs(title = 'Steelhead')
} else {
  sthd_pdp = plot_partial_dependence(qrf_mods[['Steelhead']],
                                     qrf_mod_df %>%
                                       filter(Species == 'Steelhead'),
                                     data_dict = hab_dict,
                                     # log_transform = F,
                                     log_offset = dens_offset,
                                     # scales = "free_x") +
                                     scales = 'free') +
    labs(title = 'Steelhead')
}
sthd_pdp
