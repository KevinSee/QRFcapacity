# Author: Kevin See
# Purpose: Estimate abundance of summer juvenile fish
# Created: 6/3/2019
# Last Modified: 9/12/19
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(FSA)
# library(Rcapture)

theme_set(theme_bw())

#-----------------------------------------------------------------
# load fish data
data(fishSumDf)

#-----------------------------------------------------------------
# 2 pass depletions
fishSumDf %>%
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
  left_join(fishSumDf %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fishSumDf,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fishSumDf))) -> fishSumDf

#-----------------------------------------------------------------
# 3 pass depletions
fishSumDf %>%
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
  left_join(fishSumDf %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fishSumDf,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fishSumDf))) -> fishSumDf

#-----------------------------------------------------------------
# 4 pass depletions
fishSumDf %>%
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
  left_join(fishSumDf %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fishSumDf,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fishSumDf))) -> fishSumDf

#-----------------------------------------------------------------
# mark-recapture
fishSumDf %>%
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
  left_join(fishSumDf %>%
              select(-(Nmethod:pSE))) %>%
  mutate(p = Pass1.M / N,
         pSE = Pass1.M * SE / N^2) %>%
  bind_rows(anti_join(fishSumDf,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fishSumDf))) -> fishSumDf

#-----------------------------------------------------------------
# continuous fish surveys (with estimates of capture probability)
fishSumDf %>%
  filter(Method == 'Continuous') %>%
  mutate(N = Pass1.M / p,
         SE = Pass1.M * pSE / p^2,
         Nmethod = 'Ratio Est.') %>%
  mutate(N = if_else(is.na(p) & Pass1.M == 0,
                     0,
                     N)) %>%
  left_join(fishSumDf %>%
              select(-(Nmethod:pSE))) %>%
  bind_rows(anti_join(fishSumDf,
                      .,
                      by = c('Year', 'FishSite', 'SampleDate', 'Species', 'Method', 'FishCrew'))) %>%
  select(one_of(names(fishSumDf))) -> fishSumDf

# any missing data from the continuous surveys?
fishSumDf %>%
  filter(Method == 'Continuous',
         is.na(N)) %>%
  select(Year, Watershed, Stream, Site, Species, Pass1.M:pSE)

# drop one site in mainstem Lemhi. Can't use capture probability from steelhead in mainstem (due to size differences between species)
fishSumDf %>%
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

fishSumDf %>%
  filter(!(Method == 'Continuous' & is.na(N))) -> fishSumDf

fishSumDf %>%
  xtabs(~ Method + is.na(N), .) %>%
  addmargins()

#-----------------------------------------------------------------
# which estimates are considered "valid"?
fishSumDf %>%
  filter(N < 0 | N == Inf | N == -Inf) %>%
  xtabs(~ Year + Method, ., drop.unused.levels = T)

# mark-recapture with no recaptures
fishSumDf %>%
  filter(Method == 'Mark Recapture',
         Pass1.M > 0,
         Pass3.R == 0) %>%
  # xtabs(~ Watershed + Species, ., drop.unused.levels = T)
  select(Watershed, FishSite, FishCrew, Pass1.M:pSE)

# mark-recapture with no marks (if any captures on 2nd pass, N == Pass2.C, SE == 0)
fishSumDf %>%
  filter(Method == 'Mark Recapture',
         Pass1.M == 0) %>%
  xtabs(~ (N > 0) + (Pass2.C == N), .)
  # filter(N > 0) %>%
  # # filter(Pass2.C != N) %>%
  # # xtabs(~ Watershed + Species, ., drop.unused.levels = T)
  # select(Watershed, FishSite, FishCrew, Pass1.M:pSE)

# using criteria from Robson & Regier (1964)
fishSumDf %>%
  filter(Method == 'Mark Recapture') %>%
  mutate(Valid = if_else(((Pass1.M * Pass2.C) / 4 > N) | Pass3.R >= 7, T, F)) %>%
  xtabs(~ Watershed + Valid + Species, ., drop.unused.levels = T) %>%
  addmargins() %>%
  prop.table(margin = 1) %>%
  round(2)



fishSumDf %<>%
  mutate(Valid = if_else(Method == 'Mark Recapture' & (((Pass1.M * Pass2.C) / 4 > N) | Pass3.R >= 7), T, NA),
         Valid = if_else(Method == 'Single Pass', F, Valid),
         Valid = if_else(Method %in% c('Continuous', 'CU Depletion', 'Depletion', 'Snorkel'), T, Valid),
         Valid = if_else(is.na(Valid), F, Valid))

#-----------------------------------------------------------------
# pull out valid estimates, save to use

fishSumEst = fishSumDf %>%
  mutate(fish_dens = N / FishSiteLength) %>%
  # filter(Valid) %>%
  mutate_at(vars(Species, Watershed, Season, FishCrew, Method),
            list(fct_drop))


fishSumEst %>%
  filter(is.na(FishSiteLength)) %>%
  pull(Site) %>% unique() %>% sort()

fishSumEst %>%
  filter(!is.na(FishSiteLength)) %>%
  group_by(Species, Valid) %>%
  summarise(nSites = n_distinct(Site)) %>%
  ungroup() %>%
  spread(Valid, nSites)

use_data(fishSumEst,
         version = 2,
         overwrite = T)

#-----------------------------------------------------------------
# Modeling capture probability for the single pass surveys
library(lme4)
library(MuMIn)

# for most places, use valid mark-recapture data to estimate capture efficiency
# for single pass sites in the Lemhi, use depletion sites for ratio estimator model



data(siteData)
data(fishSumEst)
# which surveys do we need to make predictions for?
ratPredDf = fishSumEst %>%
  filter((Method == 'Single Pass' & is.na(N)) |
           (Method == 'Mark Recapture' & !Valid)) %>%
  mutate(N = NA,
         SE = NA,
         p = NA,
         pSE = NA) %>%
  # add some information from CHaMP
  left_join(siteData %>%
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
                          Stream))


xtabs(~ Watershed + Year, 
      ratPredDf, 
      drop.unused.levels = T) %>%
  addmargins()

# what can we use to predict capture probability?
ratModDf = fishSumEst %>%
  filter(Method == 'Mark Recapture',
         Valid,
         # Watershed %in% unique(ratPredDf$Watershed),
         !(FishCrew == 'QCI' & Watershed == 'Lemhi')) %>%
  left_join(siteData %>%
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
  # center some variables
  mutate_at(vars(WetWidth),
            list(~ (. - mean(., na.rm = T))))

ratModDf %>%
  ggplot(aes(x = Channel_Type,
             y = p,
             fill = FishCrew)) +
  geom_boxplot() +
  theme(legend.position = 'bottom') +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~ Species)

ratModDf %>%
  ggplot(aes(x = WetWidth,
             y = p)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = lm) +
  facet_wrap(~ FishCrew)

lm(p ~ Channel_Type,
   data = ratModDf) %>%
  # anova()
  summary()

xtabs(~ Channel_Type + Species,
      ratModDf)

xtabs(~ Watershed + FishCrew + Species, 
      ratModDf, 
      drop.unused.levels = T) %>%
  addmargins(margin = c(1,2))

test = glm(cbind(Pass1.M, (N - Pass1.M)) ~ FishCrew + (Species + Pass1.M + WetWidth + Channel_Type)^2,
             data = ratModDf,
             family = 'binomial',
             na.action = 'na.fail')
autoplot(test)

fullMod = glmer(cbind(Pass1.M, (N - Pass1.M)) ~ (Species + Pass1.M + WetWidth + Channel_Type)^2 + (1 | FishCrew),
# fullMod = glm(cbind(Pass1.M, (N - Pass1.M)) ~ (Species + FishCrew + Pass1.M + WetWidth)^2,
                data = ratModDf,
                family = 'binomial',
                na.action = 'na.fail')
summary(fullMod)
exp(fixef(fullMod)[-1])

dd = dredge(fullMod)
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

bestMod = get.models(dd, subset = delta == 0)[[1]]

modAvg = model.avg(dd, 
                   # subset = delta <= 2,
                   fit = T)
exp(coef(modAvg, full = T)[-1])


fixef(fullMod) %>%
  enframe(name = 'param',
          value = 'full') %>%
  left_join(fixef(bestMod) %>%
              enframe(name = 'param',
                      value = 'best') %>%
              mutate(param = recode(param,
                                    'Pass1.M:SpeciesSteelhead' = 'SpeciesSteelhead:Pass1.M'))) %>%
  left_join(coef(modAvg, full = T) %>%
              enframe(name = 'param',
                      value = 'avg') %>%
              mutate(param = recode(param,
                                    'Pass1.M:SpeciesSteelhead' = 'SpeciesSteelhead:Pass1.M')))

plot(fullMod)
hist(ranef(fullMod)$Stream[,1])
hist(ranef(fullMod)$FishCrew[,1])

#-------------------------------------------------
# predict capture probability
summary(ratPredDf)
ratPredDf %>%
  mutate(predFull = predict(fullMod,
                            newdata = .,
                            type = 'response',
                            allow.new.levels = T),
         predBest = predict(bestMod,
                            newdata = .,
                            type = 'response',
                            allow.new.levels = T),
         predAvg = predict(modAvg,
                            newdata = .,
                            type = 'response',
                            allow.new.levels = T)) %>%
  select(starts_with('pred')) %>%
  cor(use = 'pairwise.complete.obs')
  GGally::ggpairs()
