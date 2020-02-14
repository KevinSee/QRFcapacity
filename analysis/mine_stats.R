# Author: Kevin See
# Purpose: Calculate MINE statistics on various fish/habitat datasets
# Created: 2/13/2020
# Last Modified: 2/13/2020
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
# summmer juvenilts
data("fh_sum_champ_2017")
fish_hab = fh_sum_champ_2017 %>%
  mutate_at(vars(Watershed, Year),
            list(as.factor)) %>%
  rename(LON_DD = Lon,
         LAT_DD = Lat)

# redds
data("fh_redds_champ_2017")
fish_hab = fh_redds_champ_2017 %>%
  mutate_at(vars(Watershed),
            list(as.factor)) %>%
  # scale some metrics by site length
  mutate_at(vars(starts_with('LWVol'),
                 ends_with('_Vol')),
            list(~ . / Lgth_Wet * 100)) %>%
  mutate(RipCovCanSome = 100 - RipCovCanNone) %>%
  # what kind of redd density metric should we use?
  mutate(fish_dens = maxReddsPerMsq)


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
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "RipCovCanNone") %>%
              mutate(ShortName = "RipCovCanSome",
                     Name = "Riparian Cover: Some Canopy",
                     DescriptiveText = "Percent of riparian canopy with some vegetation."))

#-----------------------------------------------------------------
# clip Chinook data to Chinook domain
data("chnk_domain")

# which sites were sampled for Chinook? 
chnk_samps = fish_hab %>%
  filter(Species == 'Chinook') %>%
  select(Site, LON_DD, LAT_DD, fish_dens) %>%
  distinct() %>%
  st_as_sf(coords = c('LON_DD', 'LAT_DD'),
           crs = 4326) %>%
  st_transform(st_crs(chnk_domain))

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
  pull(Site)

# only keep Chinook data from sites in Chinook domain
fish_hab %<>%
  filter(Species == 'Steelhead' |
           (Species == 'Chinook' & Site %in% chnk_sites))

#-----------------------------------------------------------------
# # what are some possible habitat covariates?
poss_hab_mets = hab_dict %>%
  filter(MetricCategory != 'Categorical') %>%
  filter(ShortName %in% names(fish_hab)) %>%
  pull(ShortName) %>%
  unique()

poss_hab_mets

# generate MINE statistics
mine_res = fish_hab %>%
  split(list(.$Species)) %>%
  map_df(.id = 'Species',
         .f = function(x) {
           x %>%
             mutate(fish_dens = log(fish_dens)) %>%
             estimate_MIC(covars = poss_hab_mets,
                          response = 'fish_dens')
         }) %>%
  left_join(hab_dict %>%
              filter(MetricGroupName == 'Visit Metric') %>%
              select(Metric = ShortName,
                     MetricCategory,
                     Name),
            by = 'Metric') %>%
  mutate(MetricCategory = if_else(Metric == 'CUMDRAINAG',
                                  'Size',
                                  MetricCategory)) %>%
  mutate_at(vars(MetricCategory),
            list(fct_explicit_na),
            na_level = 'Other') %>%
  mutate_at(vars(Name),
            list(as.character)) %>%
  mutate(Name = if_else(is.na(Name),
                        as.character(Metric),
                        Name)) %>%
  # put the metric names in descending order by MIC
  mutate_at(vars(Metric, Name),
            list(~ fct_reorder(., .x = MIC))) %>%
  select(Species, MetricCategory, Metric, everything()) %>%
  arrange(Species, MetricCategory, desc(MIC))

# create database for plotting, potentially filtering out some metrics that we wouldn't want to use
mine_plot_df = mine_res %>%
  # filter out some metrics with too many NAs or 0s
  filter((perc_NA < 0.2 & non_0 > 100)) %>%
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

#-----------------------------------------------------
# make some plots of MIC values
#-----------------------------------------------------
mine_p = mine_plot_df %>%
  ggplot(aes(x = Name,
             y = MIC,
             fill = Species)) +
  geom_col(position = position_dodge(1)) +
  coord_flip() +
  facet_wrap(~ MetricCategory,
             scales = 'free_y',
             ncol = 3) +
  scale_fill_brewer(palette = 'Set1',
                    guide = guide_legend(nrow = 1)) +
  theme(legend.position = 'bottom',
        axis.text = element_text(size = 5))

mine_p2 = mine_plot_df %>%
  ggplot(aes(x = Name,
             y = MIC,
             fill = Species)) +
  geom_col(position = position_dodge(1)) +
  coord_flip() +
  scale_fill_brewer(palette = 'Set1',
                    guide = guide_legend(nrow = 1)) +
  theme(legend.position = 'bottom',
        axis.text = element_text(size = 5))

mine_chnk_p = mine_plot_df %>%
  filter(Species == "Chinook") %>%
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
