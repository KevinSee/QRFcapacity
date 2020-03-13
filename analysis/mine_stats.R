# Author: Kevin See
# Purpose: Calculate MINE statistics on various fish/habitat datasets
# Created: 2/13/2020
# Last Modified: 3/13/2020
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(QRFcapacity)
library(tidyverse)
library(minerva)
library(janitor)
library(magrittr)
library(sf)


theme_set(theme_bw())

#-----------------------------------------------------------------
# determine which set of fish/habitat data to use
# summmer juveniles with CHaMP metrics
data("fh_sum_champ_2017")

# summer juveniles with DASH metrics
data("fh_sum_dash_2014_17")

# redds
data("fh_redds_champ_2017")

# winter juveniles
data("fh_win_champ_2017")

# combine all fish-habitat datasets into one list
fish_hab_list = list('Redds' = fh_redds_champ_2017 %>%
                       mutate_at(vars(Watershed),
                                 list(as.factor)) %>%
                       # what kind of redd density metric should we use?
                       mutate(fish_dens = maxReddsPerMsq),
                     'Winter' = fh_win_champ_2017 %>%
                       filter(!is.na(fish_dens)) %>%
                       mutate_at(vars(Watershed, Year, Tier1),
                                 list(as.factor)),
                     'Summer_CHaMP' = fh_sum_champ_2017 %>%
                       mutate_at(vars(Watershed, Year),
                                 list(as.factor)),
                     'Summer_DASH' = fh_sum_dash_2014_17 %>%
                       mutate_at(vars(Watershed, Year),
                                 list(as.factor)))

# alter a few metrics consistently
fish_hab_list %<>%
  map(.f = function(x) {
    # scale some metrics by site length
    x %<>%
      mutate_at(vars(starts_with('LWVol'),
                     ends_with('_Vol')),
                list(~ . / Lgth_Wet * 100))
    
    # add a metric showing "some" riparian canopy
    if(RipCovCanNone %in% names(x)) {
      x %<>%
        mutate(RipCovCanSome = 100 - RipCovCanNone)
    }
  })


#-----------------------------------------------------------------
# and the appropriate habitat dictionrary to go with it
data("hab_dict_2017")
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
                     DescriptiveText = "Percent of riparian canopy with some vegetation."))

#-----------------------------------------------------------------
# clip Chinook data to Chinook domain
data("chnk_domain")

# which sites were sampled for Chinook? 
chnk_samps = fish_hab_list %>%
  map_df(.id = 'dataset',
         .f = function(x) {
           x %>%
             filter(Species == 'Chinook') %>%
             select(Site, LON_DD, LAT_DD, fish_dens) %>%
             distinct()
         }) %>%
  filter(!is.na(LON_DD)) %>%
  st_as_sf(coords = c('LON_DD', 'LAT_DD'),
           crs = 4326) %>%
  st_transform(st_crs(chnk_domain))

# chnk_samps %>%
#   group_by(Site) %>%
#   summarise(n_datasets = n_distinct(dataset)) %>%
#   ungroup() %>%
#   tabyl(n_datasets)

# set snap distance (in meters)
st_crs(chnk_samps)
snap_dist = 1000

# which of those sites are in Chinook domain?
chnk_sites = chnk_samps %>%
  as_Spatial() %>%
  maptools::snapPointsToLines(chnk_domain %>%
                                mutate(id = 1:n()) %>%
                                select(id, MPG) %>%
                                as_Spatial(),
                              maxDist = snap_dist,
                              withAttrs = T,
                              idField = 'id') %>%
  as('sf') %>%
  as_tibble() %>%
  pull(Site) %>%
  unique()

# only keep Chinook data from sites in Chinook domain
fish_hab_list %<>%
  map(.f = function(x) {
    x %>%
      filter(Species == 'Steelhead' |
               (Species == 'Chinook' & Site %in% chnk_sites))
  })

#-----------------------------------------------------------------
# # what are some possible habitat covariates?
# poss_hab_mets = hab_dict %>%
#   filter(MetricCategory != 'Categorical') %>%
#   filter(ShortName %in% names(fish_hab)) %>%
#   pull(ShortName) %>%
#   unique()

# poss_hab_mets = fish_hab_list %>%
#   map(.f = function(x) {
#     hab_dict %>%
#       filter(MetricCategory != 'Categorical') %>%
#       filter(ShortName %in% names(x)) %>%
#       pull(ShortName) %>%
#       unique()
#   })

poss_hab_mets = fish_hab_list %>%
  map_df(.f = function(x) tibble(ShortName = names(x))) %>%
  distinct() %>%
  left_join(hab_dict %>%
              filter(MetricGroupName %in% c('Channel Unit', 'Visit Metric')) %>%
              select(ShortName, MetricGroupName, Name, MetricCategory) %>%
              distinct()) %>%
  filter(is.na(MetricCategory) | MetricCategory != 'Categorical') %>%
  filter((!is.na(Name) |
            ShortName %in% c('Elev_M',
                             'CUMDRAINAG',
                             "DpthThlwg_Avg",
                             "SCSm_Area",
                             "SCSm_Freq",
                             "SCSm_Ct",
                             "SCSm_Vol",
                             "RipCovUstoryNone",
                             "RipCovGrndNone",
                             "SC_Area",
                             "SC_Area_Pct",
                             "ChnlUnitTotal_Ct",
                             "Discharge_fish",
                             "Temp",
                             "PercentIceCover",
                             "LWCount",
                             "SubEstBdrk",
                             "Ucut_Length",
                             "FishCovAll",
                             "SubEstCandBldr",
                             "UcutLgth",
                             "LWcnt_Wet"))) %>%
  mutate(Name = if_else(is.na(Name),
                        ShortName,
                        Name)) %>%
  filter(!ShortName %in% c('Tier1', 'Tier2')) %>%
  mutate(MetricCategory = if_else(grepl('SC', ShortName),
                                  'SideChannel',
                                  MetricCategory),
         MetricCategory = if_else(grepl('Sub', ShortName),
                                  'Substrate',
                                  MetricCategory),
         MetricCategory = if_else(grepl('^Rip', ShortName),
                                  'Riparian',
                                  MetricCategory),
         MetricCategory = if_else(grepl('^FishCov', ShortName) |
                                    grepl('Ucut', ShortName) |
                                    ShortName %in% c('PercentIceCover'),
                                  'Cover',
                                  MetricCategory),
         MetricCategory = if_else(grepl('^LW', ShortName),
                                  'Wood',
                                  MetricCategory),
         MetricCategory = if_else(grepl('Discharge', ShortName) |
                                    ShortName %in% c("DpthThlwg_Avg",
                                                     "Dpth_Max",
                                                     'DpthThlwgExit',
                                                     'DpthResid',
                                                     'TotalVol',
                                                     'CUMDRAINAG'),
                                  'Size',
                                  MetricCategory),
         MetricCategory = if_else(ShortName %in% c('Elev_M', 'Temp'),
                                  'Temperature',
                                  MetricCategory),
         MetricCategory = if_else(ShortName %in% c('ChnlUnitTotal_Ct'),
                                  'ChannelUnit',
                                  MetricCategory))


# poss_hab_mets %>%
#   mutate(MetricGroupName = if_else(is.na(MetricGroupName) &
#                                      (grepl('^SC', ShortName) |
#                                         grepl('^Rip', ShortName) |
#                                         ShortName %in% c('DpthThlwg_Avg')),
#                                    'Visit Metric',
#                                    MetricGroupName),
#          MetricGroupName = if_else(is.na(MetricGroupName) &
#                                      ShortName %in% c("Elev_M"),
#                                    'GAA',
#                                    MetricGroupName))
# 
# 
# 
# 
# poss_hab_mets %>%
#   # filter(is.na(MetricGroupName))
#   tabyl(MetricGroupName)

#-----------------------------------------------------------------
# generate MINE statistics
#-----------------------------------------------------------------
mine_res = crossing(dataset = names(fish_hab_list),
                    species = unique(fish_hab_list$Summer_CHaMP$Species)) %>%
  mutate(fh_data = map2(dataset,
                        species,
                        .f = function(x, y) {
                          fish_hab_list[[x]] %>%
                            filter(Species == y)
                        }),
         metrics = map(fh_data,
                       .f = function(x) {
                         poss_hab_mets %>%
                           filter(ShortName %in% names(x)) %>%
                           pull(ShortName)
                       })) %>%
  mutate(mine_res = map2(fh_data,
                         metrics,
                         .f = function(x, y) {
                           if(sum(x$fish_dens == 0) == 0) {
                             try(x %>%
                                   mutate(fish_dens = log(fish_dens)) %>%
                                   estimate_MIC(covars = y,
                                                response = 'fish_dens'))
                           } else {
                             try(x %>%
                                   mutate(fish_dens = log(fish_dens + 0.001)) %>%
                                   estimate_MIC(covars = y,
                                                response = 'fish_dens'))
                           }
                         }))

# create database for plotting, potentially filtering out some metrics that we wouldn't want to use
mine_plot_list = mine_res %>%
  select(-fh_data, -metrics) %>%
  unnest(cols = mine_res) %>%
  left_join(poss_hab_mets,
            by = c('Metric' = 'ShortName')) %>%
  mutate_at(vars(MetricCategory),
            list(fct_explicit_na),
            na_level = 'Other') %>%
  mutate_at(vars(Name),
            list(as.character)) %>%
  mutate(Name = if_else(is.na(Name),
                        as.character(Metric),
                        Name)) %>%
  #split by dataset
  split(list(.$dataset)) %>%
  map(.f = function(x) {
    x %>%
  # put the metric names in descending order by MIC
  mutate_at(vars(Metric, Name),
            list(~ fct_reorder(., .x = MIC))) %>%
  select(species, MetricCategory, Metric, everything()) %>%
  arrange(species, MetricCategory, desc(MIC)) %>%
      # filter out some metrics with too many NAs or 0s
      filter((perc_NA < 0.5 & non_0 > 100)) %>%
      # filter out metrics with very low variance
      # filter(var < 0.1) %>%
      # filter(obsCV < 0.1)
      # janitor::tabyl(MetricCategory)
      # select(1:11)
      # filter out area and volume metrics
      filter(!grepl('Area$', Metric),
             !grepl('Vol$', Metric),
             !Metric %in% c('Lgth_Wet', 
                            'Lgth_BfChnl',
                            'Lgth_WetChnl',
                            'Area_Wet', 
                            'Area_Bf', 
                            'WetVol', 
                            'BfVol'))
    })


#-----------------------------------------------------
# make some plots of MIC values
#-----------------------------------------------------
mine_p = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      ggplot(aes(x = Name,
                 y = MIC,
                 fill = species)) +
      geom_col(position = position_dodge(1)) +
      coord_flip() +
      facet_wrap(~ MetricCategory,
                 scales = 'free_y',
                 ncol = 3) +
      scale_fill_brewer(palette = 'Set1',
                        guide = guide_legend(nrow = 1)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 5)) +
      labs(title = unique(x$dataset),
           fill = "Species")
  })

mine_p2 = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      ggplot(aes(x = Name,
                 y = MIC,
                 fill = species)) +
      geom_col(position = position_dodge(1)) +
      coord_flip() +
      scale_fill_brewer(palette = 'Set1',
                        guide = guide_legend(nrow = 1),
                        name = 'Species') +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 5))
  })



mine_chnk_p = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      filter(species == "Chinook") %>%
      # filter(MetricCategory != 'Other') %>%
      # put the metric names in descending order by MIC
      mutate_at(vars(Metric, Name),
                list(~ fct_reorder(., .x = MIC))) %>%
      arrange(desc(Metric)) %>%
      ggplot(aes(x = Name,
                 y = MIC,
                 fill = MetricCategory)) +
      geom_col() +
      coord_flip() +
      # scale_fill_viridis_d() +
      scale_fill_brewer(palette = 'Set3',
                        guide = guide_legend(nrow = 2)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 6))
  })

mine_chnk_p

#-----------------------------------------------------------------
# look at correlations between habitat metrics
#-----------------------------------------------------------------
library(corrr)
library(ggcorrplot)
library(GGally)

sel_mets = poss_hab_mets
sel_mets = mine_plot_df %>%
  group_by(MetricCategory) %>%
  slice(1:4) %>%
  ungroup() %>%
  filter(MIC > 0.2) %>%
  pull(Metric) %>%
  unique() %>%
  as.character()

# take a few out that are very correlated or similar to others
sel_mets = sel_mets[!grepl('^SubEmbed', sel_mets)]
sel_mets = sel_mets[!sel_mets %in% c('CU_Freq', 'FstTurb_Freq', 'UcutLgth_Pct', 'Alk', 'WetWdth_Avg', "RipCovCanNone", "RipCovCanSome", "DpthBf_Avg", "BfWDRat_CV", "FishCovAqVeg", "CUMDRAINAG", "SubLT6")]



corr_mat = fish_hab %>%
  select(one_of(sel_mets)) %>%
  corrr::correlate()

corr_mat %>%
  rearrange(absolute = F) %>%
  shave(upper = T) %>% 
  stretch() %>%
  filter(!is.na(r)) %>%
  left_join(hab_dict %>%
              select(x = ShortName,
                     x_category = MetricCategory)) %>%
  left_join(hab_dict %>%
              select(y = ShortName,
                     y_category = MetricCategory)) %>%
  distinct() %>%
  # filter(x_category == y_category) %>%
  arrange(x_category, 
          desc(abs(r))) %>%
  filter(abs(r) > 0.5)

corr_p1 = corr_mat %>%
  rearrange(absolute = F) %>%
  shave(upper = T) %>% 
  rplot(legend = T,
        print_cor = T)

corr_p1

corr_p2 = fish_hab %>%
  select(one_of(sel_mets)) %>%
  cor(use = 'pairwise') %>%
  ggcorrplot(method = 'square',
             hc.order = T,
             lab = T,
             type = "lower")
corr_p2

fish_hab %>%
  select(one_of(sel_mets)) %>%
  ggcorr()



#-----------------------------------------------------------------
# test out a QRF model 
#-----------------------------------------------------------------
sel_hab_mets = crossing(Species = c('Chinook', 
                                    'Steelhead'),
                        Metric = sel_mets)
# impute missing values
qrf_data = impute_missing_data(data = fish_hab %>%
                                   select(-(maxYr:maxReddsPerMsq)) %>%
                                   distinct(),
                                 covars = sel_mets,
                                 impute_vars = c('Watershed', 'Elev_M', 'Sin', 'CUMDRAINAG'),
                                 method = 'missForest') %>%
  left_join(fish_hab %>%
              select(Species:Site, maxYr:maxReddsPerMsq)) %>%
  mutate(fish_dens = maxReddsPerMsq) %>%
  select(Species, Site, Watershed, maxYr, LON_DD, LAT_DD, fish_dens, one_of(sel_mets))


# fit the QRF model
# set the density offset (to accommodate 0s)
# dens_offset = 0.005
dens_offset = 0

# fit random forest models
qrf_mod_df = qrf_data %>%
  group_by(Species) %>%
  nest() %>%
  mutate(mod_data = map2(Species,
                         data,
                         function(x, y) {
                           covars = sel_hab_mets %>%
                             filter(Species == x) %>%
                             pull(Metric)
                           y %>%
                             select(fish_dens, one_of(covars)) %>%
                             return()
                         })) %>%
  mutate(qrf_mod = map(mod_data,
                       .f = function(z) {
                         set.seed(3)
                         qrf_mod = quantregForest(x = z %>%
                                                    select(-fish_dens) %>%
                                                    as.matrix,
                                                  y = z %>%
                                                    mutate_at(vars(fish_dens),
                                                              # list(~ log(. * 1000))) %>%
                                                              list(~ log(. + dens_offset))) %>%
                                                    select(fish_dens) %>%
                                                    as.matrix(),
                                                  keep.inbag = T,
                                                  ntree = 1000)
                         return(qrf_mod)
                       }))

qrf_mods = qrf_mod_df$qrf_mod %>%
  rlang::set_names(qrf_mod_df$Species)


plot_partial_dependence(qrf_mods$Chinook,
                        data = qrf_data %>%
                          filter(Species == 'Chinook'),
                        data_dict = hab_dict,
                        log_offset = dens_offset,
                        scales = 'free')
