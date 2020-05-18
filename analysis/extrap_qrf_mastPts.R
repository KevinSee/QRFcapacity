# Author: Kevin See
# Purpose: Extrapolate QRF model to all 200 m reaches
# Created: 3/27/2020
# Last Modified: 4/6/2020
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(QRFcapacity)
library(tidyverse)
library(janitor)
library(magrittr)
library(sf)
library(quantregForest)
library(survey)

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

pred_hab_sites = hab_impute %>%
  mutate(chnk_per_m = predict(qrf_mods[['Chinook']],
                              newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                              what = pred_quant),
         chnk_per_m = exp(chnk_per_m) - dens_offset,
         chnk_per_m2 = chnk_per_m * Lgth_Wet / Area_Wet) %>%
  mutate(sthd_per_m = predict(qrf_mods[['Steelhead']],
                              newdata = select(., one_of(unique(sel_hab_mets$Metric))),
                              what = pred_quant),
         sthd_per_m = exp(sthd_per_m) - dens_offset,
         sthd_per_m2 = sthd_per_m * Lgth_Wet / Area_Wet)

# only use sites that are in the GAA master sample database
# split by species, and filter by each species domain (based on what's in the GAAs)
# data("chnk_domain")
# snap_dist = 1000
# 
# chnk_sites = pred_hab_sites %>%
#   filter(!is.na(LON_DD)) %>%
#   st_as_sf(coords = c('LON_DD', 'LAT_DD'),
#            crs = 4326) %>%
#   st_transform(crs = st_crs(chnk_domain)) %>%
#   as_Spatial() %>%
#   maptools::snapPointsToLines(chnk_domain %>%
#                                 mutate(id = 1:n()) %>%
#                                 select(id, MPG) %>%
#                                 as_Spatial(),
#                               maxDist = snap_dist,
#                               withAttrs = T,
#                               idField = 'id') %>%
#   as('sf') %>%
#   as_tibble() %>%
#   pull(Site) %>%
#   unique()


data("gaa")
data('champ_site_rch')
data('rch_200')

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

pred_hab_df = pred_hab_sites %>%
  select(-starts_with('chnk')) %>%
  rename(cap_per_m = sthd_per_m,
         cap_per_m2 = sthd_per_m2) %>%
  mutate(Species = 'Steelhead') %>%
  bind_rows(pred_hab_sites %>%
              select(-starts_with('sthd')) %>%
              rename(cap_per_m = chnk_per_m,
                     cap_per_m2 = chnk_per_m2) %>%
              mutate(Species = 'Chinook')) %>%
  select(Species, Site:Area_Wet, starts_with("cap_")) %>%
  inner_join(gaa %>%
               select(Site)) %>%
               # select(Site, chnk, sthd = steel) %>%
               # mutate_at(vars(chnk, sthd),
               #           list(~ if_else(. == "Yes", T, F)))) %>%
  # filter((Species == 'Steelhead' & sthd) |
  #          (Species == 'Chinook' & chnk)) %>%
  filter(Species == 'Steelhead' |
           (Species == 'Chinook' & Site %in% chnk_sites)) %>%
  mutate_at(vars(Watershed),
            list(fct_drop))

#----------------------------------------
# pull in survey design related data
#----------------------------------------
# Calculate GRTS design weights.
data("gaa")

# pull in info about what strata each CHaMP site was assigned to (using 2014 as reference year)
site_strata = pred_hab_df %>%
  select(Species, Site, Watershed) %>%
  distinct() %>%
  left_join(gaa %>%
              select(Site,
                     strata = AStrat2014)) %>%
  mutate(site_num = str_split(Site, '-', simplify = T)[,2]) %>%
  mutate(strata = if_else(Watershed == 'Asotin',
                          site_num,
                          if_else(Watershed == 'Entiat' & grepl('ENT00001', Site),
                                  paste('EntiatIMW', site_num, sep = '_'),
                                  strata))) %>%
  mutate(strata = if_else(grepl('EntiatIMW', strata),
                          str_remove(strata, '[[:digit:]]$'),
                          strata),
         strata = if_else(grepl('EntiatIMW', strata),
                          str_remove(strata, '[[:digit:]]$'),
                          strata)) %>%
  filter(!is.na(strata)) %>%
  mutate(strata = paste(Watershed, strata, sep = '_')) %>%
  select(-site_num)

# read in data from the CHaMP frame
champ_frame_df = read_csv('data/prepped/champ_frame_data.csv') %>%
  mutate(Target2014 = ifelse(is.na(AStrat2014), 'Non-Target', Target2014)) %>%
  mutate(AStrat2014 = ifelse(AStrat2014 == 'Entiat IMW', paste('EntiatIMW', GeoRchIMW, sep = '_'), AStrat2014)) %>%
  mutate(UseTypCHSP = ifelse(CHaMPshed == 'Lemhi' & AStrat2014 == 'Little Springs', 
                             "Spawning and rearing", UseTypCHSP),
         UseTypSTSU = ifelse(CHaMPshed == 'Lemhi' & AStrat2014 %in% c('Big Springs', 'Little Springs'), 
                             "Spawning and rearing", UseTypSTSU)) %>%
  filter(Target2014 == 'Target') %>%
  rename(Watershed = CHaMPshed)

# what strata do we have?
frame_strata = champ_frame_df %>%
  mutate(strata = paste(Watershed, AStrat2014, sep='_')) %>%
  select(Watershed, 
         strata) %>%
  distinct()

# how long is each strata, by species?
chnk_strata_length = champ_frame_df %>%
  filter(!is.na(UseTypCHSP)) %>%
  mutate(strata = paste(Watershed, AStrat2014, sep='_')) %>%
  select(Watershed, matches("Strat"), FrameLeng) %>%
  group_by(Watershed, strata) %>%
  summarise(tot_length_km = sum(FrameLeng) / 1000) %>%
  ungroup() %>%
  mutate_at(vars(Watershed, strata), 
            list(as.factor)) %>%
  arrange(Watershed, strata)

sthd_strata_length = champ_frame_df %>%
  filter(!is.na(UseTypSTSU)) %>%
  mutate(strata = paste(Watershed, AStrat2014, sep='_')) %>%
  select(Watershed, matches("Strat"), FrameLeng) %>%
  group_by(Watershed, strata) %>%
  summarise(tot_length_km = sum(FrameLeng) / 1000) %>%
  bind_rows(tibble(Watershed = 'Asotin',
                   strata = paste('Asotin', c('CC', 'NF', 'SF'), sep = '_'),
                   tot_length_km = 12)) %>%
  ungroup() %>%
  mutate_at(vars(Watershed, strata), 
            list(as.factor)) %>%
  arrange(Watershed, strata)

strata_length = chnk_strata_length %>%
  mutate(Species = 'Chinook') %>%
  bind_rows(sthd_strata_length %>%
              mutate(Species = 'Steelhead')) %>%
  select(Species, everything())

# how many sites in each strata? and what is the length of each strata?
strata_tab = pred_hab_df %>%
  select(Species, Site, Watershed, matches('per_m')) %>%
  left_join(site_strata) %>%
  filter(strata != 'Entiat_Entiat IMW') %>%
  mutate_at(vars(Watershed),
            list(fct_drop)) %>%
  group_by(Species, Watershed, strata) %>%
  summarise(n_sites = n_distinct(Site)) %>%
  ungroup() %>%
  full_join(strata_length) %>%
  mutate(n_sites = if_else(is.na(n_sites),
                           as.integer(0),
                           n_sites)) %>%
  # calculate the weight of each site in each strata
  mutate(site_weight = if_else(n_sites > 0,
                               tot_length_km / n_sites,
                               as.numeric(NA)))


# test to see if we've accounted for all strata and most of each watershed
strata_test = frame_strata %>%
  full_join(strata_tab) %>%
  mutate_at(vars(Watershed),
            list(fct_drop)) %>%
  mutate(n_sites = if_else(is.na(n_sites),
                           as.integer(0),
                           n_sites)) %>%
  select(Species, everything()) %>%
  arrange(Species, Watershed, strata)

# # what frame strata don't have any sites in them?
# strata_test %>%
#   filter(n_sites == 0,
#          !is.na(tot_length_km)) %>%
#   arrange(Species, Watershed, strata) %>%
#   as.data.frame()
# 
# # what strata that we have sites for are not in the frame strata?
# strata_test %>%
#   filter(n_sites > 0,
#          (is.na(tot_length_km) |
#             tot_length_km == 0)) %>%
#   as.data.frame()

# champ_frame_df %>%
#   filter(Watershed == 'Lemhi') %>%
#   select(Watershed, AStrat2014, Target2014, UseTypSTSU, FrameLeng) %>%
#   filter(!grepl('Mainstem', AStrat2014)) %>%
#   group_by(AStrat2014) %>%
#   summarise(use_length = sum(FrameLeng[!is.na(UseTypSTSU)]),
#             nonuse_length = sum(FrameLeng[is.na(UseTypSTSU)]))

# # how much of each watershed is not accounted for with current sites / strata?
# strata_test %>%
#   group_by(Species, Watershed) %>%
#   summarise_at(vars(tot_length_km),
#                list(sum),
#                na.rm = T) %>%
#   left_join(strata_test %>%
#               filter(n_sites == 0) %>%
#               group_by(Species, Watershed) %>%
#               summarise_at(vars(missing_length = tot_length_km),
#                            list(sum),
#                            na.rm = T)) %>%
#   mutate_at(vars(missing_length),
#             list(~ if_else(is.na(.), 0, .))) %>%
#   mutate(perc_missing = missing_length / tot_length_km) %>%
#   mutate_at(vars(perc_missing),
#             list(~ if_else(is.na(.), 0, .))) %>%
#   arrange(desc(perc_missing))


#----------------------------------------
# Prep GAAs for extrapolation
#----------------------------------------
gaa_all = gaa %>%
  # filter out a few areas
  filter(!HUC6NmNRCS %in% c('Upper Sacramento', 'Southern Oregon Coastal', 'Puget Sound', 'Northern California Coastal', 'Oregon Closed Basins')) %>%
  # don't use AEM sites in model
  filter(!grepl('^AEM', Site)) %>%
  # don't use non-GRTS sites
  filter(SiteID_alt != 'NonGRTSSite' | is.na(SiteID_alt)) %>%
  filter(!grepl('mega', Site, ignore.case = T)) %>%
  rename(Channel_Type = ChanlType) %>%
  mutate_at(vars(Channel_Type),
            list(as.factor))

# which GAAs to use
extrap_covars = c('TRange', 
                  # 'GDD', 
                  # 'Precip',
                  'Elev_M', 
                  'CHaMPsheds', 
                  'NatPrin1', 
                  # 'NatPrin2', 
                  'DistPrin1', 
                  # 'BFW_M', 
                  'SrtCumDrn', 
                  'StrmPwr',
                  'Slp_NHD_v1', 
                  'Channel_Type', 
                  # 'MAVELV', 
                  'WIDE_BF',
                  'S2_02_11')

# what type of covariate is each GAA?
extrap_class = gaa_all %>%
  select(one_of(extrap_covars)) %>%
  as.list() %>%
  map_chr(.f = function(x) class(x)[1])

# which ones are numeric?
extrap_num = names(extrap_class)[extrap_class %in% c('integer', 'numeric')]
# which ones are categorical?
extrap_catg = names(extrap_class)[extrap_class %in% c('factor', 'character', 'ordered')]

# # correlation between numeric covariates
# gaa_all %>%
#   select(one_of(extrap_num)) %>%
#   cor(method = 'spearman')

# compare range of covariates from model dataset and prediction dataset
range_comp = bind_rows(gaa_all %>%
                         filter(!Site %in% unique(pred_hab_sites$Site)) %>%
                         select(Site,
                                one_of(extrap_num)) %>%
                         gather(Metric, value, -Site) %>%
                         mutate(Source = 'non-CHaMP Reaches'),
                       gaa_all %>%
                         filter(Site %in% unique(pred_hab_sites$Site)) %>%
                         select(Site,
                                one_of(extrap_num)) %>%
                         distinct() %>%
                         gather(Metric, value, -Site) %>%
                         mutate(Source = 'CHaMP Reaches')) %>%
  mutate_at(vars(Source, Metric),
            list(as.factor))

range_max = range_comp %>%
  group_by(Metric, Source) %>%
  summarise_at(vars(value),
               tibble::lst(min, max),
               na.rm = T) %>%
  filter(Source == 'CHaMP Reaches') %>%
  ungroup() %>%
  gather(type, value, -Metric, -Source)


# covar_range_p = range_comp %>%
#   ggplot(aes(x = Source,
#              y = value,
#              fill = Source)) +
#   geom_boxplot() +
#   facet_wrap(~ Metric,
#              scales = 'free') +
#   geom_hline(data = range_max,
#              aes(yintercept = value),
#              lty = 2,
#              color = 'darkgray') +
#   theme_minimal()

# covar_range_p


# Center the covariates
# filter out reaches with covariates outside range of covariates used to fit extrapolation model
out_range_rchs = gaa_all %>%
  select(one_of(extrap_num), Site) %>%
  gather(Metric, value, -Site) %>%
  left_join(select(range_max, -Source) %>%
              spread(type, value)) %>%
  group_by(Metric) %>%
  filter(value > max |
           value < min) %>%
  ungroup() %>%
  pull(Site) %>%
  unique()

# center covariates
extrap_summ = inner_join(pred_hab_df %>%
                        select(Site) %>%
                        distinct(),
                      gaa_all %>%
                        select(Site, one_of(extrap_num))) %>%
  gather(metric_nm, value, -Site) %>%
  group_by(metric_nm) %>%
  summarise(metric_mean = mean(value, na.rm=T),
            metric_sd = sd(value, na.rm=T)) %>%
  ungroup()

# extrapolation model data set, with normalized covariates
mod_data = inner_join(pred_hab_df,
                      gaa_all %>%
                        select(Site, one_of(extrap_num))) %>%
  gather(metric_nm, value, one_of(extrap_num)) %>%
  left_join(extrap_summ) %>%
  mutate(norm_value = (value - metric_mean) / metric_sd) %>%
  select(-(value:metric_sd)) %>%
  spread(metric_nm, norm_value) %>%
  left_join(gaa_all %>%
              select(Site, one_of(extrap_catg)))

# filter out rows with missing data in covariates
mod_data %<>%
  bind_cols(mod_data %>%
              is.na() %>%
              as_tibble() %>%
              select(one_of(extrap_covars)) %>%
              transmute(n_NA = rowSums(.))) %>%
  filter(n_NA == 0) %>%
  mutate_at(vars(Watershed, one_of(extrap_catg)),
            list(fct_drop))

sum(is.na(mod_data))


# calculate adjusted weights for all predicted QRF capacity sites
mod_data_weights = mod_data %>%
  left_join(site_strata) %>%
  left_join(strata_tab) %>%
  # if site not in a strata, it gets weight proportionate to it's length
  mutate(site_weight = if_else(is.na(site_weight),
                               Lgth_Wet / 1000,
                               site_weight)) %>%
  group_by(Species, Watershed) %>%
  mutate(sum_weights = sum(site_weight)) %>%
  ungroup() %>%
  mutate(adj_weight = site_weight / sum_weights)


gaa_pred = gaa_all %>%
  mutate(in_covar_range = ifelse(Site %in% out_range_rchs, F, T)) %>%
  select(Site, one_of(extrap_covars), Lon, Lat, in_covar_range, HUC6NmNRCS, HUC8NmNRCS, HUC10NmNRC, HUC12NmNRC, chnk, steel) %>%
  gather(metric_nm, value, one_of(extrap_num)) %>%
  left_join(extrap_summ) %>%
  mutate(norm_value = (value - metric_mean) / metric_sd) %>%
  select(-(value:metric_sd)) %>%
  spread(metric_nm, norm_value)

#-------------------------------------------------------------
# Set up the survey design.

# getOption('survey.lonely.psu')
# this will prevent strata with only 1 site from contributing to the variance
options(survey.lonely.psu = 'certainty')
# this centers strata with only 1 site to the sample grand mean; this is conservative
# options(survey.lonely.psu = 'adjust')

# extrapolation model formula
full_form = as.formula(paste('log_qrf_cap ~ -1 + (', paste(extrap_covars, collapse = ' + '), ')'))

# fit various models
model_svy_df = mod_data_weights %>%
  gather(response, qrf_cap, matches('per_m')) %>%
  select(-(n_sites:sum_weights)) %>%
  mutate(log_qrf_cap = log(qrf_cap)) %>%
  group_by(Species, response) %>%
  nest() %>%
  mutate(design = map(data,
                      .f = function(x) {
                        svydesign(id = ~ 1,
                                  data = x,
                                  strata = ~ Watershed,
                                  # strata = ~ strata,
                                  weights = ~ adj_weight)
                      })) %>%
  mutate(mod_no_champ = map(design,
                        .f = function(x) {
                          svyglm(update(full_form, .~ . - CHaMPsheds),
                                 design = x)
                        }),
         mod_champ = map(design,
                            .f = function(x) {
                              svyglm(full_form,
                                     design = x)
                            })) %>%
  arrange(Species, response) %>%
  ungroup()

# make predictions at all possible reaches, using both models
model_svy_df %<>%
  mutate(pred_all_rchs = list(gaa_pred %>%
                                select(Site, one_of(extrap_covars), -CHaMPsheds) %>%
                                na.omit() %>%
                                left_join(gaa_pred)),
         # which reaches have Channel types in the model dataset?
         pred_all_rchs = map2(pred_all_rchs,
                              mod_no_champ,
                              .f = function(x,y) {
                                x %>%
                                  filter(Channel_Type %in% y$xlevels$Channel_Type) %>%
                                  mutate_at(vars(Channel_Type),
                                            list(fct_drop))
                              }),
         # which reaches are in CHaMP watersheds?
         pred_champ_rchs = map2(pred_all_rchs,
                                mod_champ,
                                .f = function(x,y) {
                                  x %>%
                                    filter(CHaMPsheds %in% y$xlevels$CHaMPsheds) %>%
                                    mutate_at(vars(CHaMPsheds),
                                              list(fct_drop))
                                })) %>%
  mutate(pred_no_champ = map2(mod_no_champ,
                              pred_all_rchs,
                              .f = function(x, y) {
                                y %>%
                                  select(Site) %>%
                                  bind_cols(predict(x,
                                                    newdata = y,
                                                    se = T,
                                                    type = 'response') %>%
                                              as_tibble())
                              }),
         pred_champ = map2(mod_champ,
                           pred_champ_rchs,
                           .f = function(x, y) {
                             y %>%
                               select(Site) %>%
                               bind_cols(predict(x,
                                                 newdata = y,
                                                 se = T,
                                                 type = 'response') %>%
                                           as_tibble())
                           }))

# pull out predictions at all reaches, using the model without Watershed as a covariate
z = model_svy_df %>%
  select(Species, type = response, pred_no_champ) %>%
  unnest(cols = pred_no_champ) %>%
  gather(key, value, response, SE) %>%
  mutate(key = if_else(key == 'response',
                       if_else(Species == 'Chinook',
                               str_replace(type, 'cap', 'chnk'),
                               str_replace(type, 'cap', 'sthd')),
                       if_else(Species == 'Chinook',
                               paste0(str_replace(type, 'cap', 'chnk'), "_se"),
                               paste0(str_replace(type, 'cap', 'sthd'), "_se")))) %>%
  select(Site, key, value) %>%
  spread(key, value) %>%
  mutate_at(vars(matches('per_m')),
            list(exp))

# pull out predictions at all CHaMP reaches, using the model with Watershed as a covariate
y = model_svy_df %>%
  select(Species, type = response, pred_champ) %>%
  unnest(cols = pred_champ) %>%
  gather(key, value, response, SE) %>%
  mutate(key = if_else(key == 'response',
                       if_else(Species == 'Chinook',
                               str_replace(type, 'cap', 'chnk'),
                               str_replace(type, 'cap', 'sthd')),
                       if_else(Species == 'Chinook',
                               paste0(str_replace(type, 'cap', 'chnk'), "_se"),
                               paste0(str_replace(type, 'cap', 'sthd'), "_se")))) %>%
  select(Site, key, value) %>%
  spread(key, value) %>%
  mutate_at(vars(matches('per_m')),
            list(exp))


# put all predictions together
all_preds = y %>%
  mutate(model = 'CHaMP') %>%
  bind_rows(z %>%
              mutate(model = 'non-CHaMP')) %>%
  left_join(gaa_all %>%
              select(Site, Watershed = CHaMPsheds)) %>%
  select(Site, Watershed, everything())


# quick comparison of capacity predicitons with both models
comp_pred_p = all_preds %>%
  filter(Site %in% Site[duplicated(Site)]) %>%
  select(Site, Watershed, model, chnk_per_m, chnk_per_m2, sthd_per_m, sthd_per_m2) %>%
  arrange(Watershed, Site, model) %>%
  gather(dens_type, cap, matches('_per_m')) %>%
  spread(model, cap) %>%
  ggplot(aes(x = CHaMP,
             y = `non-CHaMP`)) +
  geom_point() +
  geom_abline(linetype = 2,
              color = 'red') +
  facet_wrap(~ Watershed + dens_type,
             scales = 'free')

# comp_pred_p

# for sites in CHaMP watersheds, use predicions from CHaMP extrapolation model
all_preds %<>%
  filter((Site %in% Site[duplicated(Site)] & model == 'CHaMP') |
           !Site %in% Site[duplicated(Site)])

# add non-CHaMP model predictions in for Asotin
all_preds %<>%
  filter(Watershed != 'Asotin' | is.na(Watershed)) %>%
  bind_rows(all_preds %>%
              filter(Watershed == 'Asotin') %>%
              select(-starts_with('chnk')) %>%
              left_join(z %>%
                          select(Site, starts_with('chnk'))))

sum(duplicated(all_preds$Site))

# for CHaMP sites, use direct QRF esimates, not extrapolation ones (adds a few extra sites)
all_preds %>%
  anti_join(pred_hab_sites %>%
              select(Site)) %>%
  bind_rows(pred_hab_sites %>%
              select(Site, Watershed, matches('per_m')) %>%
              mutate(chnk_per_m_se = 0,
                     chnk_per_m2_se = 0,
                     sthd_per_m_se = 0,
                     sthd_per_m2_se = 0) %>%
              mutate(model = 'QRF')) %>%
  select(Site, Watershed, model, everything()) %>%
  arrange(Watershed, Site) -> all_preds

save(extrap_covars,
     mod_data_weights,
     model_svy_df,
     all_preds,
     file = paste0('output/modelFits/extrap_mastPts_', mod_choice, '.rda'))

#---------------------------
# create a shapefile
load(paste0('output/modelFits/extrap_mastPts_', mod_choice, '.rda'))
data("gaa")
data("chnk_domain")

site_cap = all_preds %>%
  left_join(gaa %>%
              select(Site, 
                     HUC_6, HUC6NmNRCS, HUC_8, HUC8NmNRCS, HUC_10, HUC10NmNRC, HUC_12, HUC12NmNRC,
                     Lat, Lon)) %>%
  select(Site, starts_with('HUC'), everything()) %>%
  filter(!is.na(Lon), !is.na(Lat)) %>%
  st_as_sf(coords = c('Lon', 'Lat'),
           crs = 4326) %>%
  st_transform(crs = st_crs(chnk_domain))

chnk_buff = chnk_domain %>%
  select(chnk_ESU_DPS = ESU_DPS,
         chnk_MPG = MPG,
         chnk_NWR_POPID = NWR_POPID,
         chnk_NWR_NAME = NWR_NAME,
         chnk_use = UseType) %>%
  st_buffer(dist = 200,
            endCapStyle = 'FLAT')

chnk_buff2 = chnk_buff %>%
  mutate_at(vars(starts_with('chnk')),
            list(fct_explicit_na)) %>%
  mutate(area = st_area(.)) %>%
  group_by(chnk_ESU_DPS,
           chnk_MPG,
           chnk_NWR_POPID,
           chnk_NWR_NAME,
           chnk_use) %>%
  summarise(area = sum(area)) %>%
  ungroup()

site_cap2 = site_cap %>%
  st_join(chnk_buff2)

site_chnk_dist = site_cap %>%
  st_nearest_feature(chnk_domain %>%
                       select(chnk_ESU_DPS = ESU_DPS,
                              chnk_MPG = MPG,
                              chnk_NWR_POPID = NWR_POPID,
                              chnk_NWR_NAME = NWR_NAME,
                              chnk_use = UseType))
# site_cap2 %>%
#   bind_cols(chnk_domain %>%
#               select(chnk_ESU_DPS = ESU_DPS,
#                      chnk_MPG = MPG,
#                      chnk_NWR_POPID = NWR_POPID,
#                      chnk_NWR_NAME = NWR_NAME,
#                      chnk_use = UseType) %>%
#               slice(site_chnk_dist))


site_cap2 = site_cap %>%
  st_join(chnk_domain %>%
            select(chnk_ESU_DPS = ESU_DPS,
                   chnk_MPG = MPG,
                   chnk_NWR_POPID = NWR_POPID,
                   chnk_NWR_NAME = NWR_NAME,
                   chnk_use = UseType),
          join = st_is_within_distance,
          dist = 200)

%>%
  group_by(Site) %>%
  filter(!is.na(chnk_use)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(chnk = if_else(!is.na(chnk_use),
                        T, F))

site_cap2 %>%
  st_drop_geometry() %>%
  distinct() %>%
  arrange(Site) %>%
  filter(Site %in% Site[duplicated(Site)]) %>%
  slice(1:4) %>%
  as.data.frame()

nrow(site_cap)
nrow(site_cap2)
site_cap2 %>%
  filter(Site %in% Site[duplicated(Site)])

table(site_cap2$chnk)

rm(mod_data_weights, model_svy_df, extrap_covars)
rm(gaa, all_preds)

# save it
# as GPKG
st_write(site_cap,
         dsn = paste0('output/gpkg/MastPts_Cap_', mod_choice, '.gpkg'),
         driver = 'GPKG')


# as shapefile
st_write(site_cap,
         dsn = paste0('output/shapefiles/MastPts_Cap_', mod_choice, '.shp'),
         driver = 'ESRI Shapefile')

