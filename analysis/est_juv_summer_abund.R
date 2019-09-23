# Author: Kevin See
# Purpose: Estimate abundance of summer juvenile fish
# Created: 6/3/2019
# Last Modified: 9/19/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(lme4)
library(merTools)
library(tidyverse)
library(lubridate)
library(magrittr)
library(FSA)

theme_set(theme_bw())

#-----------------------------------------------------------------
# load fish data
data(fish_sum_data)

fish_data = fish_sum_data

#-----------------------------------------------------------------
# 2 pass depletions
fish_data %>%
  filter(Method == 'Depletion',
         is.na(Pass3.R)) %>%
  select(Year, FishSite, SampleDate, FishCrew, Species, Pass1.M, Pass2.C) %>%
  group_by(Year, FishSite, SampleDate, FishCrew, Species) %>%
  nest() %>%
  mutate(deplMod = map(data,
                       .f = function(x) {
                         # use 2 pass model from Seber
                         removal(catch = as_vector(x), 
                                           method='Seber2')
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
                      .f = function(x) x$est['p.se'])) %>%
  select(-data, -deplMod) %>%
  mutate(Nmethod = 'Seber') %>%
  left_join(fish_data %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

#-----------------------------------------------------------------
# 3 pass depletions
fish_data %>%
  filter(Method == 'Depletion',
         !is.na(Pass3.R),
         (is.na(Pass4) | Pass4 == 0),
         is.na(N)) %>%
  select(Year, FishSite, SampleDate, Species, Pass1.M, Pass2.C, Pass3.R) %>%
  group_by(Year, FishSite, SampleDate, Species) %>%
  nest() %>%
  mutate(deplMod = map(data,
                       .f = function(x) {
                         # Chose Carle-Strub method based on results from Hedger et al. (2013)
                         removal(catch = as_vector(x), 
                                 method='CarleStrub')
                       })) %>%
  mutate(N = map_dbl(deplMod,
                     .f = function(x) x$est['No']),
         SE = map_dbl(deplMod,
                      .f = function(x) x$est['No.se']),
         p = map_dbl(deplMod,
                     .f = function(x) x$est['p']),
         pSE = map_dbl(deplMod,
                       .f = function(x) x$est['p.se'])) %>%
  select(-data, -deplMod) %>%
  mutate(Nmethod = 'Carle Strub') %>%
  left_join(fish_data %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

#-----------------------------------------------------------------
# 4 pass depletions
fish_data %>%
  filter(Method == 'Depletion',
         Pass4 > 0,
         is.na(N)) %>%
  select(Year, FishSite, SampleDate, Species, Pass1.M, Pass2.C, Pass3.R, Pass4) %>%
  group_by(Year, FishSite, SampleDate, Species) %>%
  nest() %>%
  mutate(deplMod = map(data,
                       .f = function(x) {
                         # Chose Carle-Strub method based on results from Hedger et al. (2013)
                         removal(catch = as_vector(x), 
                                 method='CarleStrub')
                       })) %>%
  mutate(N = map_dbl(deplMod,
                     .f = function(x) x$est['No']),
         SE = map_dbl(deplMod,
                      .f = function(x) x$est['No.se']),
         p = map_dbl(deplMod,
                     .f = function(x) x$est['p']),
         pSE = map_dbl(deplMod,
                       .f = function(x) x$est['p.se'])) %>%
  select(-data, -deplMod) %>%
  mutate(Nmethod = 'Carle Strub') %>%
  left_join(fish_data %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

#-----------------------------------------------------------------
# mark-recapture
fish_data %>%
  filter(Method == 'Mark Recapture') %>%
  select(Year, FishSite, SampleDate, Species, Pass1.M, Pass2.C, Pass3.R) %>%
  group_by(Year, FishSite, SampleDate, Species) %>%
  nest() %>%
  mutate(mrMod = map(data,
                     .f = function(x) {
                       mrClosed(M = x$Pass1.M,
                                n = x$Pass2.C,
                                m = x$Pass3.R,
                                method = 'Chapman') %>%
                         summary(incl.SE = T)
                     })) %>%
  mutate(N = map_dbl(mrMod,
                     .f = function(x) x[1,'N']),
         SE = map_dbl(mrMod,
                      .f = function(x) x[1, 'SE'])) %>%
  select(-data, -mrMod) %>%
  mutate(Nmethod = 'Chapman') %>%
  left_join(fish_data %>%
              select(-(Nmethod:pSE))) %>%
  mutate(p = Pass1.M / N,
         pSE = Pass1.M * SE / N^2) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

#-----------------------------------------------------------------
# continuous fish surveys (with estimates of capture probability)
fish_data %>%
  filter(Method == 'Continuous') %>%
  mutate(N = Pass1.M / p,
         SE = Pass1.M * pSE / p^2,
         Nmethod = 'Ratio Est.') %>%
  mutate(N = if_else(is.na(p) & Pass1.M == 0,
                     0,
                     N)) %>%
  left_join(fish_data %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fish_data,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fish_data))) -> fish_data

# any missing data from the continuous surveys?
fish_data %>%
  filter(Method == 'Continuous',
         is.na(N)) %>%
  select(Year, Watershed, Stream, Site, Species, Pass1.M:pSE)

# drop one site in mainstem Lemhi. Can't use capture probability from steelhead in mainstem (due to size differences between species)
fish_data %>%
  filter(Method == 'Continuous',
         Species == 'Chinook',
         Stream == 'Lemhi River') %>%
  select(Year, Site, Pass1.M, p, pSE, N, SE) %>%
  arrange(Site, Year) %>%
  group_by(Site) %>%
  summarise(nYrs = n_distinct(Year),
            nNonNA = sum(!is.na(p)),
            p_mean = mean(p, na.rm = T),
            p_sd = sd(p, na.rm = T)) %>%
  mutate(p_cv = p_sd / p_mean)

fish_data %>%
  filter(!(Method == 'Continuous' & is.na(N))) -> fish_data

fish_data %>%
  xtabs(~ Method + is.na(N), .) %>%
  addmargins()

#-----------------------------------------------------------------
# which estimates are considered "valid"?
fish_data %>%
  filter(N < 0 | N == Inf | N == -Inf) %>%
  xtabs(~ Year + Method, ., drop.unused.levels = T)

# mark-recapture with no recaptures
fish_data %>%
  filter(Method == 'Mark Recapture',
         Pass1.M > 0,
         Pass3.R == 0) %>%
  # xtabs(~ Watershed + Species, ., drop.unused.levels = T)
  select(Watershed, FishSite, FishCrew, Pass1.M:pSE)

# mark-recapture with no marks (if any captures on 2nd pass, N == Pass2.C, SE == 0)
fish_data %>%
  filter(Method == 'Mark Recapture',
         Pass1.M == 0) %>%
  xtabs(~ (N > 0) + (Pass2.C == N), .)
  # filter(N > 0) %>%
  # # filter(Pass2.C != N) %>%
  # # xtabs(~ Watershed + Species, ., drop.unused.levels = T)
  # select(Watershed, FishSite, FishCrew, Pass1.M:pSE)

# using criteria from Robson & Regier (1964)
fish_data %>%
  filter(Method == 'Mark Recapture') %>%
  mutate(Valid = if_else(((Pass1.M * Pass2.C) / 4 > N) | Pass3.R >= 7, T, F)) %>%
  xtabs(~ Watershed + Valid + Species, ., drop.unused.levels = T) %>%
  addmargins() %>%
  prop.table(margin = 1) %>%
  round(2)



fish_data %<>%
  mutate(Valid = if_else(Method == 'Mark Recapture',
                         if_else((((Pass1.M * Pass2.C) / 4 > N) | Pass3.R >= 7), T, F),
                         NA),
         Valid = if_else(Method %in% c('Continuous', 'CU Depletion', 'Depletion', 'Snorkel'), T, Valid),
         Valid = if_else(Method == 'Single Pass',
                         if_else(Nmethod == 'Prev. Calc.', T, F),
                         Valid),
         Valid = if_else(is.na(Valid), F, Valid))

#-----------------------------------------------------------------
# pull out valid estimates, save to use

fish_sum_est = fish_data %>%
  mutate(fish_dens = N / FishSiteLength) %>%
  # filter(Valid) %>%
  mutate_at(vars(Species, Watershed, Season, FishCrew, Method),
            list(fct_drop))


fish_sum_est %>%
  filter(is.na(FishSiteLength)) %>%
  pull(Site) %>% unique() %>% sort()

fish_sum_est %>%
  filter(!is.na(FishSiteLength)) %>%
  group_by(Species, Valid) %>%
  summarise(nSites = n_distinct(Site)) %>%
  ungroup() %>%
  spread(Valid, nSites)

use_data(fish_sum_est,
         version = 2,
         overwrite = T)

#-----------------------------------------------------------------
# Modeling capture probability for the single pass surveys
library(MuMIn)

# for most places, use valid mark-recapture data to estimate capture efficiency
# for single pass sites in the Lemhi, use depletion sites for ratio estimator model

data(champ_site_2011_17)
data(fish_sum_est)

# what can we use to predict capture probability?
sp_mod_df = fish_sum_est %>%
  filter((Method == 'Mark Recapture' & Valid & 
            !(FishCrew == 'QCI' & Watershed == 'Lemhi')) |
           (FishCrew == 'QCI' & Watershed == 'Lemhi' & Method == 'Depletion')) %>%
  left_join(champ_site_2011_17 %>%
              filter(VisitStatus == 'Released to Public',
                     VisitObjective == 'Primary Visit') %>%
              select(Site, Watershed, 
                     StreamName = Stream, 
                     Year = VisitYear, 
                     WetWdth_Int, 
                     Channel_Type)) %>%
  mutate(WetWidth = if_else(!is.na(FishWettedArea) & FishWettedArea > 0,
                            FishWettedArea / FishSiteLength,
                            WetWdth_Int)) %>%
  mutate(Stream = if_else(is.na(Stream),
                          StreamName,
                          Stream)) %>%
  select(Year, Site, Stream, Watershed, FishCrew, Channel_Type, FishSiteLength, WetWidth, Species, Pass1.M, N, SE, p, pSE) %>%
  drop_na() %>%
  mutate_at(vars(Pass1.M, N),
            list(floor)) %>%
  mutate(pass1_dens = Pass1.M / FishSiteLength) %>%
  # combine crew and watershed into one covariate
  mutate(CrwWtsd = paste(FishCrew, Watershed, sep = '_')) %>%
  # center some variables
  mutate(WetWidth_center = WetWidth - mean(WetWidth, na.rm = T),
         pass1_dens_center = pass1_dens - mean(pass1_dens, na.rm = T))


# which surveys do we need to make predictions for?
pred_df = fish_sum_est %>%
  filter(!Valid) %>%
  # filter((Method == 'Single Pass' & is.na(N)) |
  #          (Method == 'Mark Recapture' & !Valid)) %>%
  mutate(N = NA,
         SE = NA,
         p = NA,
         pSE = NA) %>%
  # add some information from CHaMP
  left_join(champ_site_2011_17 %>%
              filter(VisitStatus == 'Released to Public',
                     VisitObjective == 'Primary Visit') %>%
              select(Site, Watershed, 
                     StreamName = Stream, 
                     Year = VisitYear, 
                     WetWdth_Int, 
                     Channel_Type) %>%
              distinct()) %>%
  mutate(WetWidth = if_else(!is.na(FishWettedArea) & FishWettedArea > 0,
                            FishWettedArea / FishSiteLength,
                            WetWdth_Int)) %>%
  mutate(Stream = if_else(is.na(Stream),
                          StreamName,
                          Stream)) %>%
  mutate(pass1_dens = Pass1.M / FishSiteLength) %>%
  # combine crew and watershed into one covariate
  mutate(CrwWtsd = paste(FishCrew, Watershed, sep = '_')) %>%
  # center some variables to match sp_mod_df
  mutate(WetWidth_center = WetWidth - mean(sp_mod_df$WetWidth, na.rm = T),
         pass1_dens_center = pass1_dens - mean(sp_mod_df$pass1_dens, na.rm = T))


pred_df %>%
  group_by(Watershed, FishCrew, Species) %>%
  summarise(nPred = n()) %>%
  full_join(sp_mod_df %>%
              group_by(Watershed, FishCrew, Species) %>%
              summarise(nMod = n()))

sp_mod_df %>%
  ggplot(aes(x = Channel_Type,
             y = p,
             fill = CrwWtsd)) +
  geom_boxplot() +
  theme(legend.position = 'bottom') +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~ Species)

sp_mod_df %>%
  ggplot(aes(x = WetWidth,
             y = p)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = lm) +
  facet_wrap(~ FishCrew)

lm(p ~ Channel_Type,
   data = sp_mod_df) %>%
  # anova()
  summary()

xtabs(~ Channel_Type + Species,
      sp_mod_df)

xtabs(~ Watershed + FishCrew + Species, 
      sp_mod_df, 
      drop.unused.levels = T) %>%
  addmargins(margin = c(1,2))

# use random effect for Crew/Watershed
mod_re = glmer(cbind(Pass1.M, (N - Pass1.M)) ~ -1 + (Species + WetWidth_center + pass1_dens)^2 + (1 | CrwWtsd),
               data = sp_mod_df,
               family = 'binomial',
               na.action = 'na.fail')
# use fixed effect for Crew/Watershed
mod_fix = glm(cbind(Pass1.M, (N - Pass1.M)) ~ -1 + (Species + WetWidth_center + pass1_dens)^2 + CrwWtsd,
              data = sp_mod_df,
              family = 'binomial',
              na.action = 'na.fail')

library(ggfortify)
autoplot(mod_fix,
         which = 1:6,
         ncol = 3)

# it appears that row 303 in sp_mod_df is a bit of an outlier
sp_mod_df %>%
  slice(303) %>%
  left_join(fish_sum_data) %>%
  as.data.frame()
# biggest estimate of abundance and pass1 density
sp_mod_df %>% arrange(desc(N))
qplot(sp_mod_df$pass1_dens)
qplot(pred_df$pass1_dens)
which(sp_mod_df$pass1_dens > 2)

# Drop row 303 as outlier from model data set
sp_mod_df2 = sp_mod_df %>%
  slice(-303)
# use random effect for Crew/Watershed
mod_re = glmer(cbind(Pass1.M, (N - Pass1.M)) ~ -1 + (Species + WetWidth_center + pass1_dens_center)^2 + (1 | CrwWtsd),
               data = sp_mod_df2,
               family = 'binomial',
               na.action = 'na.fail')
# use fixed effect for Crew/Watershed
mod_fix = glm(cbind(Pass1.M, (N - Pass1.M)) ~ -1 + (Species + WetWidth_center + pass1_dens_center)^2 + CrwWtsd,
              data = sp_mod_df2,
              family = 'binomial',
              na.action = 'na.fail')

autoplot(mod_fix,
         which = 1:6,
         ncol = 3)

full_mod = mod_re
# full_mod = mod_fix



summary(full_mod)
anova(full_mod, test = 'Chisq')
exp(coef(full_mod)) %>%
  as_tibble(rownames = 'Parameter') %>%
  rename(odds_ratio = value)
exp(fixef(full_mod)[-1])

dd = dredge(full_mod)
head(dd)
importance(dd) %>%
  enframe(name = 'var',
          value = 'relImp') %>%
  mutate(var = fct_reorder(var, relImp)) %>%
  ggplot(aes(x = var, y = relImp)) +
  geom_col() +
  coord_flip() +
  labs(y = 'Relative Importance') +
  theme_bw()

best_mod = get.models(dd, subset = delta == 0)[[1]]

avg_mod = model.avg(dd, 
                   # subset = delta <= 2,
                   fit = T)
exp(coef(avg_mod, full = T)[-1])


fixef(full_mod) %>%
  enframe(name = 'param',
          value = 'full') %>%
  left_join(fixef(best_mod) %>%
              enframe(name = 'param',
                      value = 'best') %>%
              mutate(param = recode(param,
                                    'pass1_dens_center:SpeciesSteelhead' = 'SpeciesSteelhead:pass1_dens_center',
                                    'pass1_dens_center:WetWidth_center' = 'WetWidth_center:pass1_dens_center'))) %>%
  left_join(coef(avg_mod, full = T) %>%
              enframe(name = 'param',
                      value = 'avg') %>%
              mutate(param = recode(param,
                                    'pass1_dens_center:SpeciesSteelhead' = 'SpeciesSteelhead:pass1_dens_center',
                                    'pass1_dens_center:WetWidth_center' = 'WetWidth_center:pass1_dens_center')))

pred_int = predictInterval(update(mod_re, . ~ . + 1),
                           newdata = sp_mod_df2 %>%
                             as.data.frame(),
                           level = 0.95,
                           n.sims = 100,
                           stat = 'median',
                           type = 'probability',
                           seed = 3) %>%
  as_tibble() %>%
  mutate(se = (upr - lwr) / (qnorm(0.975) - qnorm(0.025)),
         lwCI = fit + qnorm(0.025) * se,
         upCI = fit + qnorm(0.975) * se)


plot_df = sp_mod_df2 %>%
  select(Year, Watershed, FishCrew, CrwWtsd, Species) %>%
  bind_cols(tibble(obs = sp_mod_df2$p,
                   obs_se = sp_mod_df2$pSE,
                   pred = predict(full_mod,
                                  type = 'response'),
                   # pred_se = predict(full_mod,
                   #                   type = 'response',
                   #                   se.fit = T)$se.fit,
                   pred_lwCI = pred_int$lwr,
                   pred_upCI = pred_int$upr,
                   resid = residuals(full_mod, 
                                     type = 'response')))

plot_df %>%
  ggplot(aes(x = pred,
             y = resid)) +
  geom_hline(yintercept = 0,
             color = 'red',
             linetype = 2) +
  geom_point() +
  geom_smooth()

plot_df %>%
  ggplot(aes(x = obs,
             y = pred,
             color = Species)) +
  geom_abline(color = 'darkgray',
              linetype = 2) +
  geom_errorbarh(aes(xmin = obs + qnorm(0.025) * obs_se,
                     xmax = obs + qnorm(0.975) * obs_se)) +
  geom_errorbar(aes(ymin = pred_lwCI,
                    ymax = pred_upCI),
                width = 0) +
  geom_point() +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap(~ FishCrew + Watershed,
             scales = 'free') +
  geom_smooth(method = lm) +
  labs(title = 'Capture Probability',
       x = 'Observed',
       y = 'Predicted')

plot_df %>%
  mutate(obs_lwCI = obs + qnorm(0.025) * obs_se,
         obs_upCI = obs + qnorm(0.975) * obs_se) %>%
  mutate(overlapCI = if_else(obs_lwCI < pred_upCI &
                               obs_upCI > pred_lwCI,
                             T, F)) %>%
  group_by(CrwWtsd, Species) %>%
  summarise(nPred = n(),
            nOverLap = sum(overlapCI),
            perOverLap = nOverLap / nPred)


plot_df %>%
  group_by(CrwWtsd, Species) %>%
  summarise(nObs = n(),
            bias_mean = mean(resid),
            bias_median = median(resid),
            RMSE = sqrt(mean(resid^2))) %>%
  # arrange(desc(RMSE))
  arrange(desc(abs(bias_median)))

hist(ranef(full_mod)$CrwWtsd[,1],
     10,
     col = 'blue')

#-------------------------------------------------
# predict capture probability
pred_int = predictInterval(update(mod_re, . ~ . + 1),
                           newdata = pred_df %>%
                             as.data.frame(),
                           level = 0.95,
                           n.sims = 1000,
                           stat = 'mean',
                           type = 'probability',
                           seed = 3) %>%
  as_tibble() %>%
  mutate(pred = predict(mod_re,
                           newdata = pred_df,
                           type = 'response'),
         se = (upr - lwr) / (qnorm(0.975) - qnorm(0.025)))

# pred_int = predict(mod_fix,
#                    newdata = pred_df,
#                    type = 'response',
#                    se.fit = T)[1:2] %>%
#   map_df(.f = identity) %>%
#   rename(pred = fit,
#          se = se.fit)


pred_est = pred_df %>%
  select(-(N:pSE)) %>%
  bind_cols(pred_int %>%
              select(p = pred,
                     pSE = se)) %>%
  mutate(N = Pass1.M / p,
         SE = Pass1.M * pSE / (p^2),
         Nmethod = 'Cap. Prob. Model',
         Valid = TRUE) %>%
  select(one_of(names(pred_df)))


fish_sum_est = fish_sum_est %>%
  filter(Valid) %>%
  bind_rows(pred_est %>%
              select(one_of(names(fish_sum_est))))

use_data(fish_sum_est,
         version = 2,
         overwrite = T)
