# Author: Mike Ackerman & Kevin See
# Purpose: Examine abundance/densities and explore preferred or target habitat conditions in identified high density areas
# Created: 10/24/2019
# Last Modified: 10/24/2019
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(ggplot2)

#-----------------------------------------------------------------
# load data
#-----------------------------------------------------------------
data("fh_sum_champ_2017")   # summer juvenile parr paired fish/CHaMP habitat data through 2017
data("fh_win_champ_2017")   # winter juvenile presmolts paired fish/CHaMP habitat data through 2017
data("fh_redds_champ_2017") # paired redd/CHaMP habitat data through 2017

# juvenile Chinook salmon parr
ch_sum_champ_2017 = fh_sum_champ_2017 %>%
  filter(Species == 'Chinook')

#dens_outlier_threshhold = 20
ch_sum_dens = ch_sum_champ_2017 %>%
  select(Year, Watershed, N, SE, fish_dens, Area_Wet) %>%
  filter(fish_dens > 0) %>%
  mutate(log_fish_dens = log(fish_dens + 0.005))
  #filter(fish_dens > dens_outlier_threshhold)

# calculate quantiles of log(fish densities) after removing 0s
ch_dens_quants = quantile(ch_sum_dens$log_fish_dens, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

# plot log transformed fish densities with quantiles
ch_sum_p = ggplot(ch_sum_dens, aes(x = log_fish_dens, fill = Watershed)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = ch_dens_quants) +
  theme_classic() +
  labs(x = 'Juvenile Chinook Density (fish/m)',
       y = 'Frequency') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) 
ch_sum_p

ch_df = ch_sum_champ_2017 %>%
  filter(Watershed == "Lemhi") %>%
  filter(fish_dens > 0) %>%
  mutate(log_fish_dens = log(fish_dens + 0.005),
         dens_cat = cut_number(log_fish_dens, 6,
                               labels = c('very low', 'low', 'mod low', 'mod high', 'high', 'very high'))) %>%
  mutate(plot_cat = recode(dens_cat, 'very low' = 'low', 
                                     'low' = 'low',
                                     'mod low' = 'low',
                                     'mod high' = 'high',
                                     'high' = 'high',
                                     'very high' = 'high')) %>%
  filter(plot_cat != 'mod') %>%
  select(Watershed, Year, StreamName, Channel_Type, Lat, Lon,          # site
         N, fish_dens, log_fish_dens, dens_cat, plot_cat,              # fish abundance/density
         FishSiteLength, FishWettedArea, CUMDRAINAG, MeanU, Q,         # size
         WetWdth_Int, WetBraid, WetWdth_Avg, DpthThlwg_Avg,
         DistPrin1, NatPrin1, NatPrin2,                                # PCA
         SlowWater_Pct, SlowWater_Freq, FstTurb_Pct, FstTurb_Freq,     # channel units
         FstNT_Pct, FstNT_Freq, CU_Freq,
         Grad, Sin, DetrendElev_SD, DpthThlwg_UF_CV, DpthWet_SD,       # complexity
         WetWdth_CV, WetWDRat_Avg, PoolResidDpth,  
         SC_Area_Pct, WetSC_Pct, SCSm_Freq,                            # side channel
         SubD16, SubD50, SubD84, SubEstGrvl, SubEstSandFines,          # substrate
         SubEstBldr, SubEstCbl, 
         Cond,                                                         # other
         RipCovBigTree, RipCovConif, RipCovNonWood, RipCovUstory,      # riparian cover
         RipCovWood, RipCovCanNone, RipCovUstoryNone, RipCovGrndNone, 
         LWVol_Wet, LWVol_WetSlow, LWVol_WetFstTurb, LWVol_WetFstNT,   # large wood
         LWFreq_Wet, 
         Ucut_Area,  UcutLgth_Pct, UcutArea_Pct,                       # undercuts
         FishCovLW, FishCovTVeg, FishCovArt, FishCovNone, FishCovAqVeg,# fish cover
         FishCovTotal) 

# lists of metric categories
size = c("FishSiteLength", "FishWettedArea", "CUMDRAINAG", "MeanU", "Q", "WetWdth_Int", "WetBraid", "WetWdth_Avg", 
         "DpthThlwg_Avg")
pca  = c("DistPrin1", "NatPrin1", "NatPrin2")
channel_units = c("SlowWater_Pct", "SlowWater_Freq", "FstTurb_Pct", "FstTurb_Freq", "FstNT_Pct", "FstNT_Freq", 
                  "CU_Freq")
complexity = c("Grad", "Sin", "DetrendElev_SD", "DpthThlwg_UF_CV", "DpthWet_SD",  "WetWdth_CV", 
               "WetWDRat_Avg", "PoolResidDpth")
side_channel = c("SC_Area_Pct", "WetSC_Pct", "SCSm_Freq")
substrate = c("SubD16", "SubD50", "SubD84", "SubEstGrvl", "SubEstSandFines", "SubEstBldr", "SubEstCbl")
other = c("Cond")
riparian_cover = c("RipCovBigTree", "RipCovConif", "RipCovNonWood", "RipCovUstory", "RipCovWood", "RipCovCanNone", 
                   "RipCovUstoryNone", "RipCovGrndNone")
large_wood = c("LWVol_Wet", "LWVol_WetSlow", "LWVol_WetFstTurb", "LWVol_WetFstNT", "LWFreq_Wet")
undercuts = c("Ucut_Area",  "UcutLgth_Pct", "UcutArea_Pct")
fish_cover = c("FishCovLW", "FishCovTVeg", "FishCovArt", "FishCovNone", "FishCovAqVeg", "FishCovTotal")
         
# start plotting
hab_p = ch_df %>%
  select(plot_cat, one_of(size)) %>%
  gather(variable, value, -plot_cat) %>%
  ggplot(aes(x = value, 
                   color = plot_cat,
                   fill = plot_cat)) +
  # geom_histogram() +
  geom_density(alpha = 0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) +
  facet_wrap(~ variable,
             scales = 'free')
hab_p

plot_list = list(size, pca, channel_units, complexity, side_channel, substrate, other, riparian_cover, large_wood, undercuts, fish_cover) %>%
  # rlang::set_names(nm = c('size', 'pca', 'channel_unit', 'complexity')) %>%
  map(.f = function(x) {
    ch_df %>%
      select(plot_cat, one_of(x)) %>%
      gather(variable, value, -plot_cat) %>%
      ggplot(aes(x = value, 
                 color = plot_cat,
                 fill = plot_cat)) +
      #geom_histogram(position = 'dodge') +
      geom_density(alpha = 0.3) +
      theme_classic() +
      theme(axis.text.x = element_text(color = 'black', size = 10),
            axis.text.y = element_text(color = 'black', size = 10)) +
      facet_wrap(~ variable,
                 scales = 'free')
  })

plot_list[[11]]
