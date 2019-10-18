# Author: Kevin See
# Purpose: Estimate abundance of winter juvenile fish
# Created: 9/12/2019
# Last Modified: 10/17/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(FSA)
library(MuMIn)


theme_set(theme_bw())

#-----------------------------------------------------------------
# load fish data
data(fish_win_data)

fish_data = fish_win_data %>%
  mutate(Year = if_else(month(Date) > 7,
                        paste(year(Date), year(Date) + 1, sep = '-'),
                        paste(year(Date) - 1, year(Date), sep = '-'))) %>%
  select(Year, everything()) %>%
  rename(SampleDate = Date) %>%
  add_column(N = NA, 
             SE = NA, 
             p = NA, 
             pSE = NA, 
             Nmethod = NA) %>%
  mutate_at(vars(N:pSE),
            list(as.numeric)) %>%
  mutate_at(vars(Nmethod),
            list(as.character))

xtabs(~ FishCrew + Year, fish_data)

#-----------------------------------------------------------------
# Estimate abundance for all depletion / removal surveys
#-----------------------------------------------------------------
fish_data %>%
  filter(Method == 'Depletion') %>%
  select(Year, SiteUnit, Site, ChUnitNumber, SampleDate, FishCrew, Species, Pass1.M, Pass2.C, Pass3.R) %>%
  group_by(Year, SiteUnit, Site, ChUnitNumber, SampleDate, FishCrew, Species) %>%
  nest() %>%
  mutate(deplMod = map(data,
                       .f = function(x) {
                         if(is.na(x$Pass3.R)) {
                           # use 2 pass model from Seber
                           mod = removal(catch = as_vector(x), 
                                         method='Seber2') 
                           return(mod)
                         }
                         
                         if(!is.na(x$Pass3.R)) {
                           # Chose Carle-Strub method based on results from Hedger et al. (2013)
                           mod = removal(catch = as_vector(x), 
                                   method='CarleStrub')
                           return(mod)
                         }
                         
                       })) %>%
  mutate(N = map_dbl(deplMod,
                     .f = function(x) {
                       if_else(is.na(x$est['No']),
                               0,
                               as.numeric(x$est['No']))
                     }),
         SE = map_dbl(deplMod,
                      .f = function(x) x$est['No.se']),
         p = map_dbl(deplMod,
                     .f = function(x) x$est['p']),
         pSE = map_dbl(deplMod,
                       .f = function(x) x$est['p.se']),
         Nmethod = map_chr(deplMod,
                           .f = function(x) x$method[1])) %>%
  select(-data, -deplMod) %>%
  mutate(Nmethod = recode(Nmethod,
                          'Seber2' = 'Seber',
                          'CarleStrub' = 'Carle Strub')) %>%
  left_join(fish_data %>%
              select(-(N:Nmethod))) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'SiteUnit', 'Site', 'ChUnitNumber', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

#-----------------------------------------------------------------
# mark-recapture
fish_data %>%
  filter(Method == 'Mark Recapture',
         Pass1.M > 0,
         Pass2.C > 0,
         Pass3.R <= Pass1.M) %>%
  # filter(Pass3.R > Pass1.M) %>% select(SiteUnit, Species, starts_with('Pass'))
  select(Year, SiteUnit, Site, ChUnitNumber, SampleDate, FishCrew, Species, Pass1.M, Pass2.C, Pass3.R) %>%
  group_by(Year, SiteUnit, Site, ChUnitNumber, SampleDate, FishCrew, Species) %>%
  nest() %>%
  mutate(mrMod = map(data,
                     .f = function(x) {
                       mrClosed(M = x$Pass1.M,
                                n = x$Pass2.C,
                                m = x$Pass3.R,
                                method = 'Chapman',
                                chapman.mod = T) %>%
                         summary(incl.SE = T)
                     })) %>%
  mutate(N = map_dbl(mrMod,
                     .f = function(x) x[1,'N']),
         SE = map_dbl(mrMod,
                      .f = function(x) x[1, 'SE'])) %>%
  select(-data, -mrMod) %>%
  mutate(Nmethod = 'Chapman') %>%
  left_join(fish_data %>%
              select(-(N:Nmethod))) %>%
  mutate(p = Pass1.M / N,
         pSE = Pass1.M * SE / N^2) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'SiteUnit', 'Site', 'ChUnitNumber', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

#-----------------------------------------------------------------
# Develop snorkel correction model
#-----------------------------------------------------------------
spp_calib = fish_data %>%
  filter(SurveyType == 'Calibration',
         !is.na(N),
         count > 0,
         count <= N) %>%
  # drop brook trout
  filter(Species != 'BrookTrout') %>%
  mutate_at(vars(FishCrew, Tier1, Stream, Species),
            funs(fct_drop)) %>%
  mutate(pSnork = count / N)

xtabs(~ Tier1 + Species, spp_calib)

mod_df = spp_calib %>%
  split(list(.$Species)) %>%
  map_df(.f = function(x) {
    
    x %>%
      group_by(Species) %>%
      summarise(binom_full = list(glm(cbind(count, N - count) ~ -1 + (Tier1 + FishCrew + count)^2,
                                      data = .,
                                      family = binomial,
                                      na.action = na.fail)),
                binom_dd = list(dredge(binom_full)),
                binom_best = list(get.models(binom_dd,
                                             subset = delta == 0)[[1]]),
                binom_avg = list(model.avg(binom_dd,
                                           subset = cumsum(weight) < 0.95,
                                           fit = T)),
                lin_full = list(lm(N ~ -1 + count + count:(Tier1 + FishCrew),
                                   data = .,
                                   na.action = na.fail)),
                lin_dd = list(dredge(lin_full,
                                     fixed = 'count')),
                lin_best = list(get.models(lin_dd,
                                           subset = delta == 0)[[1]]),
                lin_avg = list(model.avg(lin_dd,
                                         subset = cumsum(weight) < 0.95,
                                         fit = T)))
  })

head(mod_df$binom_dd[[1]])
head(mod_df$lin_dd[[1]])

pred_df = spp_calib %>%
  group_by(Species) %>%
  nest() %>%
  left_join(mod_df %>%
              select(-ends_with('dd'))) %>%
  mutate_at(vars(binom_full:lin_avg),
            list(~ map2(.x = .,
                        .y = data,
                        .f = predict,
                        type = 'response')))

se_df = spp_calib %>%
  group_by(Species) %>%
  nest() %>%
  left_join(mod_df %>%
              select(-ends_with('dd'))) %>%
  mutate_at(vars(binom_full:lin_avg),
            list(~ map2(.x = .,
                        .y = data,
                        .f = predict,
                        type = 'response',
                        se.fit = T))) %>%
  mutate_at(vars(binom_full:lin_avg),
            list(~ map(.x = .,
                       .f = function(x) x$se.fit)))
names(se_df)[grepl('^binom', names(se_df)) | grepl('lin', names(se_df))] = paste(names(se_df)[grepl('^binom', names(se_df)) | grepl('lin', names(se_df))], 'se', sep = '_')

comp_df = pred_df %>%
  unnest() %>%
  gather(model, pred, starts_with('binom'), starts_with('lin')) %>%
  left_join(se_df %>%
              unnest() %>%
              gather(model, se, starts_with('binom'), starts_with('lin')) %>%
              mutate(model = str_remove(model, '_se$'))) %>%
  mutate(Nhat = if_else(grepl('binom', model),
                         count / pred,
                         pred),
         Nhat_se = if_else(grepl('binom', model),
                           count * se / pred^2,
                           se))

xtabs(~ model + is.na(se), comp_df)

comp_df %>%
  select(Species, SiteUnit, count, Pass1.M, N, SE, model, Nhat) %>%
  spread(model, Nhat) %>%
  select(N, binom_avg:lin_full) %>%
  cor() %>%
  round(2)

#----------------------------------------------
# predict abundance from snorkel counts
#----------------------------------------------
snork_preds = fish_data %>%
  filter(Species %in% levels(spp_calib$Species),
         Tier1 %in% levels(spp_calib$Tier1),
         SurveyType == 'Snorkeling') %>%
  mutate_at(vars(Species, Tier1),
            list(fct_drop)) %>%
  select(Stream, Site, ChUnitNumber, SampleDate, FishCrew, Tier1:count) %>%
  split(list(.$Species)) %>%
  map_df(.f = function(x) {
    pred_model = mod_df %>%
      filter(Species == unique(x$Species)) %>%
      pull(binom_avg)
    pred_model = pred_model[[1]]
    
    x %>%
      bind_cols(predict(pred_model,
                        newdata = x,
                        type = 'response',
                        se.fit = T) %>%
                  as.tibble() %>%
                  rename(phat = fit,
                         phat_se = se.fit))
  }) %>%
  mutate(Nhat = count / phat,
         Nhat_se = count * phat_se / phat^2,
         N_method = 'SnorkCalib')

# no small side channel predictions (not in the spp_calib data.frame)
# model averaged linear model doesn't use Tier 1
ssc_preds = fish_data %>%
  filter(Species %in% levels(spp_calib$Species),
         !Tier1 %in% levels(spp_calib$Tier1),
         SurveyType == 'Snorkeling') %>%
  mutate_at(vars(Species, Tier1),
            list(fct_drop)) %>%
  select(Stream, Site, ChUnitNumber, SampleDate, FishCrew, Tier1:count) %>%
  split(list(.$Species)) %>%
  map_df(.f = function(x) {
    pred_model = mod_df %>%
      filter(Species == unique(x$Species)) %>%
      pull(lin_avg)
    pred_model = pred_model[[1]]
    
    x %>%
      bind_cols(predict(pred_model,
                        newdata = x %>%
                          select(count, FishCrew),
                        type = 'response',
                        se.fit = T) %>%
                  as_tibble() %>%
                  rename(Nhat = fit,
                         Nhat_se = se.fit))
  }) %>%
  mutate(phat = count / Nhat,
         phat_se = count * Nhat_se / Nhat^2,
         N_method = 'SnorkCalib')

# add predictions back to main fish data
fish_data %<>%
  left_join(snork_preds %>%
              bind_rows(ssc_preds)) %>%
  mutate(Nmethod = if_else(!is.na(N_method), N_method, Nmethod),
         N = if_else(is.na(N), Nhat, N),
         SE = if_else(is.na(SE), Nhat_se, SE),
         p = if_else(is.na(p), phat, p),
         pSE = if_else(is.na(pSE), phat_se, pSE)) %>%
  select(-(phat:Nhat_se),
         -N_method)

fish_data %>%
  filter(SurveyType != 'Calibration',
         Species %in% unique(spp_calib$Species),
         is.na(N)) %>%
  xtabs(~ SurveyType + Tier1, .)

#----------------------------------------------
# deal with single pass electro-fishing surveys
#----------------------------------------------

sp_calib = fish_data %>%
    filter(SurveyType == 'Electrofishing',
           Method == 'Mark Recapture',
           Pass1.M > 0, 
           Pass2.C > 0,
           !is.na(N))

sp_covar_means = sp_calib %>%
  select(Discharge, Temp, PercentIceCover) %>%
  gather(covar, value) %>%
  group_by(covar) %>%
  summarise_at(vars(value),
               list(mean = mean,
                    sd = sd))

sp_mod_df = sp_calib %>%
  gather(covar, value, one_of(sp_covar_means$covar)) %>%
  left_join(sp_covar_means) %>%
  mutate_at(vars(value),
            list(~ (. - mean) / sd)) %>%
  select(-mean, -sd) %>%
  spread(covar, value) %>%
  select(Species, N, Pass1.M, Tier1, Discharge) %>% #, Temp) %>%
  split(list(.$Species)) %>%
  map_df(.f = function(x) {
    
    x %>%
      group_by(Species) %>%
      # summarise(binom_full = list(glm(cbind(Pass1.M, N - Pass1.M) ~ -1 + (Tier1 + Discharge + Temp)^2,
      summarise(binom_full = list(glm(cbind(Pass1.M, N - Pass1.M) ~ -1 + (Tier1 + Discharge)^2,
                                      data = .,
                                      family = binomial,
                                      na.action = na.fail)),
                binom_dd = list(dredge(binom_full)),
                binom_best = list(get.models(binom_dd,
                                             subset = delta == 0)[[1]]),
                binom_avg = list(try(model.avg(binom_dd,
                                               # subset = cumsum(weight) < 0.95,
                                               fit = T))),
                # lin_full = list(lm(N ~ -1 + Pass1.M + Pass1.M:(Tier1 + Discharge + Temp),
                lin_full = list(lm(N ~ -1 + Pass1.M + Pass1.M:(Tier1 + Discharge),
                                   data = .,
                                   na.action = na.fail)),
                lin_dd = list(dredge(lin_full,
                                     fixed = 'Pass1.M')),
                lin_best = list(get.models(lin_dd,
                                           subset = delta == 0)[[1]]),
                lin_avg = list(try(model.avg(lin_dd,
                                             # subset = cumsum(weight) < 0.95,
                                             fit = T))))
  })

head(sp_mod_df$binom_dd[[1]])
head(sp_mod_df$binom_dd[[2]])
head(sp_mod_df$lin_dd[[1]])
head(sp_mod_df$lin_dd[[2]])

# where do we need this model? Single pass surveys and funky mark-recapture surveys
sp_preds = fish_data %>%
  filter(SurveyType == 'Electrofishing',
         (Method == 'Single Pass' |
            (Method == 'Mark Recapture' & is.na(N))),
         Species %in% unique(sp_calib$Species),
         Tier1 %in% unique(sp_calib$Tier1)) %>%
  # no Rapid channel units in Chinook model data
  filter(!(Species == 'Chinook' & Tier1 == 'Rapid')) %>%
  mutate_at(vars(Species, Tier1),
            list(fct_drop)) %>%
  select(-(Pass2.C:Nmethod)) %>%
  gather(covar, value, one_of(sp_covar_means$covar)) %>%
  left_join(sp_covar_means) %>%
  mutate_at(vars(value),
            list(~ (. - mean) / sd)) %>%
  select(-mean, -sd) %>%
  spread(covar, value) %>%
  split(list(.$Species)) %>%
  map_df(.f = function(x) {
    pred_model = sp_mod_df %>%
      filter(Species == unique(x$Species)) %>%
      pull(binom_avg)
    pred_model = pred_model[[1]]
    
    x %>%
      bind_cols(predict(pred_model,
                        newdata = x,
                        type = 'response',
                        se.fit = T) %>%
                  as.tibble() %>%
                  rename(phat = fit,
                         phat_se = se.fit))
  }) %>%
  mutate(Nhat = Pass1.M / phat,
         Nhat_se = Pass1.M * phat_se / phat^2,
         N_method = 'Ratio Est.')

# add predictions back to main fish data
fish_data %<>%
  left_join(sp_preds %>%
              select(-one_of(sp_covar_means$covar))) %>%
  mutate(Nmethod = if_else(!is.na(N_method), N_method, Nmethod),
         N = if_else(is.na(N), Nhat, N),
         SE = if_else(is.na(SE), Nhat_se, SE),
         p = if_else(is.na(p), phat, p),
         pSE = if_else(is.na(pSE), phat_se, pSE)) %>%
  select(-(phat:Nhat_se),
         -N_method)

fish_data %>%
  filter(SurveyType != 'Calibration',
         Species %in% unique(spp_calib$Species)) %>%
  xtabs(~ Method + Nmethod, .) %>%
  addmargins()

# save a csv of all data with abundance estimates
write_csv(fish_data,
          'data/prepped/fish_data_winter_all_estimates.csv')

#----------------------------------------------
# deal with duplicate channel unit surveys
#----------------------------------------------
fish_data %<>%
  filter(SurveyType != 'Calibration',
         Species != 'BrookTrout') %>%
  mutate(id = paste(Species, Year, SiteUnit, sep = '_'))

fish_data %>%
  filter(id %in% id[duplicated(id)]) %>% 
  arrange(id) %>%
  group_by(FishCrew) %>%
  summarise(nSurv = n(),
            nID = n_distinct(id),
            nCU = n_distinct(SiteUnit))

# for WDFW, there are 6 channel units sampled on consecutive nights.
# I'm going to keep the one with a higher abundance estimate
fish_data %>%
  anti_join(fish_data %>%
              filter(id %in% id[duplicated(id)],
                     FishCrew == 'WDFW')) %>%
  bind_rows(fish_data %>%
              filter(id %in% id[duplicated(id)],
                     FishCrew == 'WDFW') %>%
              group_by(id) %>%
              filter(N == max(N)) %>%
              slice(1) %>%
              ungroup()) %>%
  arrange(id) -> fish_data

# for QCI, there were 27 channel units sampled more than once
fish_data %>%
  filter(id %in% id[duplicated(id)],
         FishCrew == 'QCI') %>%
  arrange(id) %>%
  select(SiteUnit, Species, SampleDate, Method, count:Nmethod) %>%
  xtabs(~ Species + (N > 0), .)

# Most of these resulted in 0 Chinook anyways. For the ones that didn't, I'll keep which ever sample resulted in the higher N (and for the 0s, I'll keep the last sample)
fish_data %>%
  anti_join(fish_data %>%
              filter(id %in% id[duplicated(id)],
                     FishCrew == 'QCI')) %>%
  bind_rows(fish_data %>%
              filter(id %in% id[duplicated(id)],
                     FishCrew == 'QCI') %>%
              group_by(id) %>%
              filter(N == max(N)) %>%
              filter(SampleDate == max(SampleDate)) %>%
              ungroup()) %>%
  arrange(id) -> fish_data

#----------------------------------------------
# Save estimates
#----------------------------------------------
fish_win_est = fish_data %>%
  select(-id) %>%
  mutate_at(vars(Watershed, Stream, FishCrew, SurveyType, Method, Tier1),
            list(fct_drop))

use_data(fish_win_est,
         version = 2,
         overwrite = T)

