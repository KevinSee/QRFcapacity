# Author: Kevin See
# Purpose: Test various extrapolation models, using 200m reach dataset
# Created: 10/5/2020
# Last Modified: 10/5/2020
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
library(caret)
library(mgcv)

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

# only use sites that are in the 200 m reach dataset
data("champ_site_rch")

pred_hab_sites %<>%
  inner_join(champ_site_rch) %>%
  mutate_at(vars(Watershed),
            list(fct_drop))

# split by species, and filter by each species domain
data("rch_200")

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
  select(Species, UniqueID, Site:Area_Wet, starts_with("cap_")) %>%
  left_join(rch_200 %>%
              st_drop_geometry() %>%
              select(UniqueID, chnk, sthd)) %>%
  filter((Species == 'Steelhead' & sthd) |
           (Species == 'Chinook' & chnk))

#----------------------------------------
# prep 200 m reaches for extrapolation
#----------------------------------------
rch_200_df = rch_200 %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  mutate_at(vars(regime),
            list(~ as.factor(as.character(.))))

extrap_covars = names(rch_200_df)[c(18:20,
                                    23:29,
                                    37, 40:42)]

# what type of covariate is each GAA?
extrap_class = rch_200_df %>%
  select(one_of(extrap_covars)) %>%
  as.list() %>%
  map_chr(.f = function(x) class(x)[1])

# which ones are numeric?
extrap_num = names(extrap_class)[extrap_class %in% c('integer', 'numeric')]
# which ones are categorical?
extrap_catg = names(extrap_class)[extrap_class %in% c('factor', 'character', 'ordered')]

# compare range of covariates from model dataset and prediction dataset
range_comp = bind_rows(rch_200_df %>%
                         filter(!UniqueID %in% unique(pred_hab_sites$UniqueID)) %>%
                         select(UniqueID,
                                one_of(extrap_num)) %>%
                         gather(Metric, value, -UniqueID) %>%
                         mutate(Source = 'non-CHaMP Reaches'),
                       rch_200_df %>%
                         filter(UniqueID %in% unique(pred_hab_sites$UniqueID)) %>%
                         select(UniqueID,
                                one_of(extrap_num)) %>%
                         distinct() %>%
                         gather(Metric, value, -UniqueID) %>%
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

# # correlation between numeric covariates
# rch_200_df %>%
#   select(one_of(extrap_num)) %>%
#   cor(method = 'spearman',
#       use = "pairwise")


# Center the covariates
# filter out reaches with covariates outside range of covariates used to fit extrapolation model
out_range_rchs = rch_200_df %>%
  select(one_of(extrap_num), UniqueID) %>%
  gather(Metric, value, -UniqueID) %>%
  left_join(select(range_max, -Source) %>%
              spread(type, value)) %>%
  group_by(Metric) %>%
  filter(value > max |
           value < min) %>%
  ungroup() %>%
  pull(UniqueID) %>%
  unique()

# center covariates
extrap_summ = inner_join(pred_hab_df %>%
                           select(UniqueID) %>%
                           distinct(),
                         rch_200_df %>%
                           select(UniqueID, one_of(extrap_num))) %>%
  gather(metric_nm, value, -UniqueID) %>%
  group_by(metric_nm) %>%
  summarise(metric_mean = mean(value, na.rm=T),
            metric_sd = sd(value, na.rm=T)) %>%
  ungroup()

# extrapolation model data set, with normalized covariates
mod_data = inner_join(pred_hab_df,
                      rch_200_df %>%
                        select(UniqueID, one_of(extrap_num))) %>%
  gather(metric_nm, value, one_of(extrap_num)) %>%
  left_join(extrap_summ) %>%
  mutate(norm_value = (value - metric_mean) / metric_sd) %>%
  select(-(value:metric_sd)) %>%
  spread(metric_nm, norm_value) %>%
  left_join(rch_200_df %>%
              select(UniqueID, one_of(extrap_catg)))

sum(is.na(mod_data))

# mod_data %<>%
#   bind_cols(mod_data %>%
#               is.na() %>%
#               as_tibble() %>%
#               select(one_of(extrap_covars)) %>%
#               transmute(n_NA = rowSums(.))) %>%
#   filter(n_NA == 0) %>%
#   mutate_at(vars(Watershed, one_of(extrap_catg)),
#             list(fct_drop)) 

# where to make extrapolation predictions
rch_pred = rch_200_df %>%
  mutate(in_covar_range = ifelse(UniqueID %in% out_range_rchs, F, T)) %>%
  pivot_longer(cols = one_of(extrap_num),
               names_to = "metric_nm",
               values_to = "value") %>%
  left_join(extrap_summ) %>%
  mutate(norm_value = (value - metric_mean) / metric_sd) %>%
  select(-(value:metric_sd)) %>%
  pivot_wider(names_from = "metric_nm",
              values_from = "norm_value")


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

#-------------------------------------------------------------
# Set up the survey design.
#-------------------------------------------------------------
# getOption('survey.lonely.psu')
# this will prevent strata with only 1 site from contributing to the variance
options(survey.lonely.psu = 'certainty')
# this centers strata with only 1 site to the sample grand mean; this is conservative
# options(survey.lonely.psu = 'adjust')

# extrapolation model formula
full_form = as.formula(paste('log_qrf_cap ~ -1 + (', paste(extrap_covars, collapse = ' + '), ')'))

# for GAMs
gam_form = as.formula(paste('log_qrf_cap ~ -1 + ', paste(extrap_catg, collapse = ' + '), ' + ', paste(paste0("te(", extrap_num, ", k=4)"), collapse = " + ")))

#-------------------------------------------------------------
# Test predictions by holding out a subset of CHaMP sites, and predicting capacity there
#-------------------------------------------------------------
# hold out 10% of the CHaMP sites randomly, multiple times
set.seed(8)
cv_data = mod_data_weights %>%
  gather(response, qrf_cap, matches('per_m')) %>%
  # select(-(n_sites:sum_weights)) %>%
  mutate(log_qrf_cap = log(qrf_cap)) %>%
  group_by(Species, response) %>%
  nest() %>%
  mutate(fold_rows = map(data,
                         .f = function(x) createFolds(1:nrow(x)))) %>%
  unnest(cols = fold_rows) %>%
  mutate(fold_num = 1:n()) %>%
  ungroup() %>%
  mutate(fold_mod_data = map2(data, fold_rows,
                              .f = function(x, y) {
                                x %>%
                                  slice(-y)
                              }),
         fold_cv_data = map2(data, fold_rows,
                             .f = function(x, y) {
                               x %>%
                                 slice(y)
                             }))


# using linear model that accounts for design weights
cv_lm = cv_data %>%
  # re-adjust weights
  mutate(fold_mod_data = map(fold_mod_data,
                             .f = function(x) {
                               x %>%
                                 group_by(Watershed) %>%
                                 mutate(sum_weights = sum(site_weight)) %>%
                                 ungroup() %>%
                                 mutate(adj_weight = site_weight / sum_weights)
                             })) %>%
  # incorporate survey design
  mutate(design = map(fold_mod_data,
                      .f = function(x) {
                        svydesign(id = ~ 1,
                                  data = x,
                                  strata = ~ Watershed,
                                  # strata = ~ strata,
                                  weights = ~ adj_weight)
                      })) %>%
  mutate(mod_no_champ = map(design,
                            .f = function(x) {
                              svyglm(full_form,
                                     design = x)
                            }),
         mod_champ = map(design,
                         .f = function(x) {
                           svyglm(update(full_form, .~ . + Watershed),
                                  design = x)
                         })) %>%
  pivot_longer(cols = c(mod_champ, mod_no_champ),
               names_to = "model_type",
               values_to = "model") %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             x %>%
                               bind_cols(predict(y, 
                                                 newdata = x,
                                                 se = T,
                                                 type = 'response') %>%
                                           as_tibble()) %>%
                               rename(log_fit = response,
                                      log_se = SE) %>%
                               mutate(pred_cap = exp(log_fit) * (1 + log_se^2 / 2),
                                      pred_se = pred_cap * log_se)
                           }))

# calculate some summary statistics
cv_lm %>%
  mutate(cv_stats = map(fold_preds,
                        .f = function(x) {
                          x %>%
                            mutate(err = qrf_cap - pred_cap,
                                   rel_err = err / qrf_cap,
                                   inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                                                    qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                                                  T, F)) %>%
                            summarise(avg_qrf = mean(qrf_cap),
                                      median_se = median(pred_se),
                                      median_cv = median(pred_se / pred_cap),
                                      bias = mean(err),
                                      abs_bias = mean(abs(err)),
                                      rel_bias = mean(rel_err),
                                      rel_abs_bias = mean(abs(rel_err)),
                                      cover95 = sum(inCI) / n(),
                                      rmse = sqrt(mean(err^2)),
                                      cv_rmse = rmse / mean(qrf_cap),
                                      nrmse = rmse / IQR(qrf_cap))
                        })) %>%
  select(Species, response, model_type,
         cv_stats) %>%
  unnest(cols = cv_stats) %>%
  group_by(Species, response, model_type) %>%
  summarise(across(where(is.numeric), 
                   mean),
            .groups = "drop")


# using random forest model that ignores design weights
cv_rf = cv_data %>%
  mutate(mod_no_champ = map(fold_mod_data,
                            .f = function(x) {
                              randomForest(full_form,
                                           data = x,
                                           ntree = 5000)
                            }),
         mod_champ = map(fold_mod_data,
                         .f = function(x) {
                           randomForest(update(full_form, .~. + Watershed),
                                        data = x,
                                        ntree = 5000)
                         })) %>%
  pivot_longer(cols = c(mod_champ, mod_no_champ),
               names_to = "model_type",
               values_to = "model") %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             # x %>%
                             #   bind_cols(tibble(response = predict(y,
                             #                                       newdata = x))) %>%
                             #   mutate(pred_cap = exp(response))
                             
                             preds = predict(y,
                                             newdata = x,
                                             predict.all = T)

                             x %>%
                               bind_cols(tibble(response = preds$aggregate,
                                                SE = preds$individual %>%
                                                  apply(1, sd))) %>%
                               rename(log_fit = response,
                                      log_se = SE) %>%
                               mutate(pred_cap = exp(log_fit) * (1 + log_se^2 / 2),
                                      pred_se = pred_cap * log_se)
                           }))


# try random forest without logging QRF capacity
cv_rf2 = cv_data %>%
  mutate(mod_no_champ = map(fold_mod_data,
                            .f = function(x) {
                              randomForest(update(full_form, qrf_cap ~ .),
                                           data = x,
                                           ntree = 5000)
                            }),
         mod_champ = map(fold_mod_data,
                         .f = function(x) {
                           randomForest(update(full_form, qrf_cap ~. + Watershed),
                                        data = x,
                                        ntree = 5000)
                         })) %>%
  pivot_longer(cols = c(mod_champ, mod_no_champ),
               names_to = "model_type",
               values_to = "model") %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             preds = predict(y,
                                             newdata = x,
                                             predict.all = T)
                             
                             x %>%
                               bind_cols(tibble(pred_cap = preds$aggregate,
                                                pred_se = preds$individual %>%
                                                  apply(1, sd)))
                           }))

# calculate some summary statistics
cv_rf %>%
# cv_rf2 %>%
  mutate(cv_stats = map(fold_preds,
                        .f = function(x) {
                          x %>%
                            mutate(err = qrf_cap - pred_cap,
                                   rel_err = err / qrf_cap) %>%
                            mutate(inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                                                    qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                                                  T, F)) %>%
                            summarise(avg_qrf = mean(qrf_cap),
                                      median_se = median(pred_se),
                                      median_cv = median(pred_se / pred_cap),
                                      bias = mean(err),
                                      abs_bias = mean(abs(err)),
                                      rel_bias = mean(rel_err),
                                      rel_abs_bias = mean(abs(rel_err)),
                                      cover95 = sum(inCI) / n(),
                                      rmse = sqrt(mean(err^2)),
                                      cv_rmse = rmse / mean(qrf_cap),
                                      nrmse = rmse / IQR(qrf_cap))
                        })) %>%
  select(Species, response, model_type,
         cv_stats) %>%
  unnest(cols = cv_stats) %>%
  group_by(Species, response, model_type) %>%
  summarise(across(where(is.numeric), 
                   mean),
            .groups = "drop")


# using GAMs that try to incorporate design weights
cv_gam = cv_data %>%
  mutate(mod_no_champ = map(fold_mod_data,
                            .f = function(x) {
                              gam(gam_form,
                                  data = x,
                                  weights = adj_weight)
                            }),
         mod_champ = map(fold_mod_data,
                         .f = function(x) {
                           gam(update(gam_form, .~. + Watershed),
                               data = x,
                               weights = adj_weight)
                         })) %>%
  pivot_longer(cols = c(mod_champ, mod_no_champ),
               names_to = "model_type",
               values_to = "model") %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             x %>%
                               bind_cols(predict(y, 
                                                 newdata = x,
                                                 se = T,
                                                 type = 'response') %>%
                                           as_tibble()) %>%
                               rename(log_fit = fit,
                                      log_se = se.fit) %>%
                               mutate(pred_cap = exp(log_fit) * (1 + log_se^2 / 2),
                                      pred_se = pred_cap * log_se)
                           }))

# calculate some summary statistics
cv_gam %>%
  mutate(cv_stats = map(fold_preds,
                        .f = function(x) {
                          x %>%
                            mutate(err = qrf_cap - pred_cap,
                                   rel_err = err / qrf_cap) %>%
                            mutate(inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                                                    qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                                                  T, F)) %>%
                            summarise(avg_qrf = mean(qrf_cap),
                                      median_se = median(pred_se),
                                      median_cv = median(pred_se / pred_cap),
                                      bias = mean(err),
                                      abs_bias = mean(abs(err)),
                                      rel_bias = mean(rel_err),
                                      rel_abs_bias = mean(abs(rel_err)),
                                      cover95 = sum(inCI) / n(),
                                      rmse = sqrt(mean(err^2)),
                                      cv_rmse = rmse / mean(qrf_cap),
                                      nrmse = rmse / IQR(qrf_cap))
                        })) %>%
  select(Species, response, model_type,
         cv_stats) %>%
  unnest(cols = cv_stats) %>%
  group_by(Species, response, model_type) %>%
  summarise(across(where(is.numeric), 
                   mean),
            .groups = "drop")

#-------------------------------------------------------------
# hold out entire CHaMP watersheds, and use the non-CHaMP model to predict there
set.seed(8)
wtsd_data = mod_data_weights %>%
  gather(response, qrf_cap, matches('per_m')) %>%
  # select(-(n_sites:sum_weights)) %>%
  mutate(log_qrf_cap = log(qrf_cap)) %>%
  group_by(Species, response) %>%
  nest() %>%
  mutate(fold_cv_data = map(data,
                             .f = function(x) {
                               x %>%
                                 group_by(Watershed) %>%
                                 nest() %>%
                                 rename(fold_cv_data = data)
                             })) %>%
  unnest(cols = fold_cv_data) %>%
  mutate(fold_mod_data = map2(data,
                              Watershed,
                              .f = function(x, y) {
                                x %>%
                                  filter(Watershed != y)
                              }))

# using linear model that accounts for design weights
cv_wtsd_lm = wtsd_data %>%
  # incorporate survey design
  mutate(design = map(fold_mod_data,
                      .f = function(x) {
                        svydesign(id = ~ 1,
                                  data = x,
                                  strata = ~ Watershed,
                                  # strata = ~ strata,
                                  weights = ~ adj_weight)
                      })) %>%
  mutate(model_type = "mod_no_champ",
         model = map(design,
                            .f = function(x) {
                              svyglm(full_form,
                                     design = x)
                            })) %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             x %>%
                               bind_cols(predict(y, 
                                                 newdata = x,
                                                 se = T,
                                                 type = 'response') %>%
                                           as_tibble()) %>%
                               rename(log_fit = response,
                                      log_se = SE) %>%
                               mutate(pred_cap = exp(log_fit) * (1 + log_se^2 / 2),
                                      pred_se = pred_cap * log_se)
                           }))

# calculate some summary statistics
cv_wtsd_lm %>%
  mutate(cv_stats = map(fold_preds,
                        .f = function(x) {
                          x %>%
                            mutate(err = qrf_cap - pred_cap,
                                   rel_err = err / qrf_cap,
                                   inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                                                    qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                                                  T, F)) %>%
                            summarise(avg_qrf = mean(qrf_cap),
                                      median_se = median(pred_se),
                                      median_cv = median(pred_se / pred_cap),
                                      bias = mean(err),
                                      abs_bias = mean(abs(err)),
                                      rel_bias = mean(rel_err),
                                      rel_abs_bias = mean(abs(rel_err)),
                                      cover95 = sum(inCI) / n(),
                                      rmse = sqrt(mean(err^2)),
                                      cv_rmse = rmse / mean(qrf_cap),
                                      nrmse = rmse / IQR(qrf_cap))
                        })) %>%
  select(Species, response, model_type,
         cv_stats) %>%
  unnest(cols = cv_stats) %>%
  group_by(Species, response, model_type) %>%
  summarise(across(where(is.numeric), 
                   mean),
            .groups = "drop")


# using random forest model that ignores design weights
cv_wtsd_rf = wtsd_data %>%
  mutate(model_type = "mod_no_champ",
         model = map(fold_mod_data,
                            .f = function(x) {
                              randomForest(full_form,
                                           data = x,
                                           ntree = 5000)
                            })) %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             # x %>%
                             #   bind_cols(tibble(response = predict(y,
                             #                                       newdata = x))) %>%
                             #   mutate(pred_cap = exp(response))
                             
                             preds = predict(y,
                                             newdata = x,
                                             predict.all = T)
                             
                             x %>%
                               bind_cols(tibble(response = preds$aggregate,
                                                SE = preds$individual %>%
                                                  apply(1, sd))) %>%
                               rename(log_fit = response,
                                      log_se = SE) %>%
                               mutate(pred_cap = exp(log_fit) * (1 + log_se^2 / 2),
                                      pred_se = pred_cap * log_se)
                           }))

# calculate some summary statistics
cv_wtsd_rf %>%
  mutate(cv_stats = map(fold_preds,
                        .f = function(x) {
                          x %>%
                            mutate(err = qrf_cap - pred_cap,
                                   rel_err = err / qrf_cap) %>%
                            mutate(inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                                                    qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                                                  T, F)) %>%
                            summarise(avg_qrf = mean(qrf_cap),
                                      median_se = median(pred_se),
                                      median_cv = median(pred_se / pred_cap),
                                      bias = mean(err),
                                      abs_bias = mean(abs(err)),
                                      rel_bias = mean(rel_err),
                                      rel_abs_bias = mean(abs(rel_err)),
                                      cover95 = sum(inCI) / n(),
                                      rmse = sqrt(mean(err^2)),
                                      cv_rmse = rmse / mean(qrf_cap),
                                      nrmse = rmse / IQR(qrf_cap))
                        })) %>%
  select(Species, response, model_type,
         cv_stats) %>%
  unnest(cols = cv_stats) %>%
  group_by(Species, response, model_type) %>%
  summarise(across(where(is.numeric), 
                   mean),
            .groups = "drop")

# using GAMs that try to incorporate design weights
cv_wtsd_gam = wtsd_data %>%
  mutate(model_type = "mod_no_champ",
         model = map(fold_mod_data,
                            .f = function(x) {
                              try(gam(gam_form,
                                  data = x,
                                  weights = adj_weight))
                            })) %>%
  mutate(fold_preds = map2(fold_cv_data,
                           model,
                           .f = function(x, y) {
                             x %>%
                               bind_cols(predict(y, 
                                                 newdata = x,
                                                 se = T,
                                                 type = 'response') %>%
                                           as_tibble()) %>%
                               rename(log_fit = fit,
                                      log_se = se.fit) %>%
                               mutate(pred_cap = exp(log_fit) * (1 + log_se^2 / 2),
                                      pred_se = pred_cap * log_se)
                           }))

# calculate some summary statistics
cv_wtsd_gam %>%
  mutate(cv_stats = map(fold_preds,
                        .f = function(x) {
                          x %>%
                            mutate(err = qrf_cap - pred_cap,
                                   rel_err = err / qrf_cap) %>%
                            mutate(inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                                                    qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                                                  T, F)) %>%
                            summarise(avg_qrf = mean(qrf_cap),
                                      median_se = median(pred_se),
                                      median_cv = median(pred_se / pred_cap),
                                      bias = mean(err),
                                      abs_bias = mean(abs(err)),
                                      rel_bias = mean(rel_err),
                                      rel_abs_bias = mean(abs(rel_err)),
                                      cover95 = sum(inCI) / n(),
                                      rmse = sqrt(mean(err^2)),
                                      cv_rmse = rmse / mean(qrf_cap),
                                      nrmse = rmse / IQR(qrf_cap))
                        })) %>%
  select(Species, response, model_type,
         cv_stats) %>%
  unnest(cols = cv_stats) %>%
  group_by(Species, response, model_type) %>%
  summarise(across(where(is.numeric), 
                   mean),
            .groups = "drop")


#--------------------------------------------------------
# a few plots
#--------------------------------------------------------
# cv_gam %>%
# cv_lm %>%
cv_rf %>%
  select(Species,
         resp_nm = response,
         model_type,
         fold_preds) %>%
  unnest(cols = fold_preds) %>%
  mutate(err = qrf_cap - pred_cap,
         rel_err = err / qrf_cap) %>%
  # filter(abs(rel_err) < 20) %>%
  ggplot(aes(x = Species,
             y = rel_err,
             fill = model_type)) +
  geom_boxplot() +
  geom_hline(yintercept = 0,
             linetype = 2) +
  facet_wrap(~ resp_nm) +
  # coord_cartesian(ylim = c(-5, 1)) +
  labs(y = "Relative Error")


# cv_gam %>%
# cv_lm %>%
cv_rf %>%
# cv_wtsd_rf %>%
# cv_wtsd_lm %>%
  select(Species,
         resp_nm = response,
         # Watershed,
         model_type,
         fold_preds) %>%
  unnest(cols = fold_preds) %>%
  mutate(inCI = if_else(qrf_cap <= (pred_cap + pred_se * qnorm(0.975)) &
                          qrf_cap >= (pred_cap + pred_se * qnorm(0.025)),
                        T, F)) %>%
  ggplot(aes(x = Watershed,
             fill = inCI)) +
  geom_bar(position = position_fill()) +
  geom_hline(yintercept = 0.95, 
             linetype = 2) +
  facet_wrap(~ Species + resp_nm) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(y = "95% Confidence Interval Coverage")
