# Author: Mike Ackerman & Kevin See
# Purpose: Examine abundance/densities of redds and explore preferred or target habitat conditions
# in identified high redd areas
#
# Created: 11/25/2019
# Last Modified: 11/25/2019
# Notes:

rm(list = ls())
#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(ggplot2)
library(janitor)

#-----------------------------------------------------------------
# load data
#-----------------------------------------------------------------
data("fh_redds_champ_2017") # paired redd/CHaMP habitat data through 2017

# The fish-habitat summer CHaMP data frame contains data for steelhead and steelhead. Let's just look at steelhead for now.
sthd_redd_df = fh_redds_champ_2017 %>%
  filter(Species == 'Steelhead')

# first, let's just examine fish abundance and density information
sthd_redd_n = sthd_redd_df %>%
  select(Watershed,
         Year = maxYr,
         ReddsPerKm = maxReddsPerKm) %>%
  mutate(logReddsPerKm = log(ReddsPerKm))

# quartiles of log(redds_per_km)
sthd_redd_qrtl     = quantile(sthd_redd_n$ReddsPerKm, probs = c(0, 0.25, 0.50, 0.75, 1))  
sthd_redd_qrtl_log = quantile(sthd_redd_n$logReddsPerKm, probs = c(0, 0.25, 0.50, 0.75, 1))  

# plot log transformed linear redd densities with quartiles
sthd_redd_p = sthd_redd_n %>%
  ggplot() +
  geom_histogram(aes(x = ReddsPerKm, fill = Watershed)) +
  geom_vline(xintercept = sthd_redd_qrtl, color = 'blue') +
  theme_bw() +
  labs(x = 'Redds per km',
       y = 'Frequency') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) 
sthd_redd_p

# plot log transformed linear redd densities with quartiles
sthd_redd_log_p = sthd_redd_n %>%
  ggplot() +
  geom_histogram(aes(x = logReddsPerKm, fill = Watershed)) +
  geom_vline(xintercept = sthd_redd_qrtl_log, color = 'blue') +
  theme_bw() +
  labs(x = 'log(redds per km)',
       y = 'Frequency') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) 
sthd_redd_log_p

# reduce sthd_redd_df in preparation for evaluation
sthd_redd_df = sthd_redd_df %>%
  select(Watershed, Stream, Channel_Type, Year = maxYr,              # site
         ReddsPerKm = maxReddsPerKm, LON_DD, LAT_DD, 
         CUMDRAINAG, MeanU, Q, Area_Wet, WetWdth_Int, WetBraid,      # size
         WetWdth_Avg, DpthThlwg_Avg, 
         NatPrin1, NatPrin2, DistPrin1,                              # PCA
         SlowWater_Freq, FstTurb_Freq, FstNT_Freq, FstTurb_Pct,      # channel units
         FstNT_Ct, FstNT_Pct, CU_Freq, 
         Grad, Sin, DetrendElev_SD, DpthThlwg_UF_CV, DpthWet_SD,     # complexity
         Sin_CL,
         WetWdth_CV, WetWDRat_CV, WetWDRat_Avg, PoolResidDpth, 
         WetSC_Pct, SCSm_Freq, SC_Area_Pct,                          # side channel
         SubEmbed_Avg, SubEmbed_SD, SubD16, SubD50, SubD84,          # substrate
         SubEstGrvl, SubEstSandFines, SubEstBldr, SubEstCbl, 
         Elev_M, Cond, avg_aug_temp,                                 # other
         RipCovBigTree, RipCovConif, RipCovGrnd, RipCovNonWood,      # riparian cover
         RipCovUstory, RipCovWood, RipCovCanNone, RipCovUstoryNone,
         RipCovGrndNone,
         LWVol_Wet, LWVol_WetSlow, LWVol_WetFstTurb, LWVol_WetFstNT, # large wood
         LWFreq_Wet, 
         Ucut_Area, UcutLgth_Pct, UcutArea_Pct,                       # undercuts
         FishCovLW, FishCovTVeg, FishCovArt, FishCovNone,             # fish cover
         FishCovAqVeg, FishCovTotal) %>%
  filter(ReddsPerKm > 0) %>%
  mutate(logReddsPerKm = log(ReddsPerKm)) %>%
  filter(!Watershed %in% c('John Day'),
         !Channel_Type %in% c('Confined', 'Meandering')) 

# next, let's parse the sthd_redd_df by Watershed, Channel_Type
# by watershed
wtr = unique(sthd_redd_df$Watershed)
for(w in wtr) {
  tmp = filter(sthd_redd_df, Watershed == as.character(w)) %>%
    mutate(qrtl = cut_number(logReddsPerKm, n = 4, labels = c('Q1','Q2','Q3','Q4'))) %>%
    mutate(qrtl = recode(qrtl,
                         `Q1` = 'Rest',
                         `Q2` = 'Rest',
                         `Q3` = 'Rest'))
  assign(paste('sthd_redd_', make_clean_names(w), sep = ''), tmp)
}

# by channel unit type
cht = unique(sthd_redd_df$Channel_Type)
cht = cht[!is.na(cht)]
for(c in cht) {
  tmp = filter(sthd_redd_df, Channel_Type == as.character(c)) %>%
    mutate(qrtl = cut_number(logReddsPerKm, n = 4, labels = c('Q1','Q2','Q3','Q4'))) %>%
    mutate(qrtl = recode(qrtl,
                         `Q1` = 'Rest',
                         `Q2` = 'Rest',
                         `Q3` = 'Rest'))
  assign(paste('sthd_redd_', make_clean_names(c), sep = ''), tmp)
}

# list of data frames
df_list = list(Asotin = sthd_redd_asotin,
               Entiat = sthd_redd_entiat, 
               Lemhi = sthd_redd_lemhi,
               Methow = sthd_redd_methow,
               UGR = sthd_redd_upper_grande_ronde,
               Tucannon = sthd_redd_tucannon,
               Wenatchee = sthd_redd_wenatchee,  # watersheds
               island_braided = sthd_redd_island_braided, 
               pool_riffle = sthd_redd_pool_riffle, 
               straight = sthd_redd_straight,
               plane_bed = sthd_redd_plane_bed,
               step_pool = sthd_redd_step_pool) # channel type

# find data frames that are too small for comparisons or plotting
min_samp_size = 20
small_dfs = names(which(sapply(df_list, nrow) > min_samp_size - 1, TRUE))
df_list = df_list[names(df_list) %in% small_dfs]; rm(small_dfs)

#-----------------------------------------------------------------
# lists of metric categories
#-----------------------------------------------------------------
size = c('CUMDRAINAG', 'MeanU', 'Q', 'Area_Wet', 'WetWdth_Int', 'WetBraid',
         'WetWdth_Avg', 'DpthThlwg_Avg') 
pca = c('NatPrin1', 'NatPrin2', 'DistPrin1')
channel_units = c('SlowWater_Freq', 'FstTurb_Freq', 'FstNT_Freq',
                  'FstTurb_Pct', 'FstNT_Ct', 'FstNT_Pct', 'CU_Freq') 
complexity = c('Grad', 'Sin', 'DetrendElev_SD', 'DpthThlwg_UF_CV', 'DpthWet_SD',
               'Sin_CL', 'WetWdth_CV', 'WetWDRat_CV', 'WetWDRat_Avg', 'PoolResidDpth') 
side_channel = c('WetSC_Pct', 'SCSm_Freq', 'SC_Area_Pct')
substrate = c('SubEmbed_Avg', 'SubEmbed_SD', 'SubD16', 'SubD50', 'SubD84',
              'SubEstGrvl', 'SubEstSandFines', 'SubEstBldr', 'SubEstCbl') 
other = c('Elev_M', 'Cond', 'avg_aug_temp')                                 
riparian_cover = c('RipCovBigTree', 'RipCovConif', 'RipCovGrnd', 'RipCovNonWood',
                   'RipCovUstory', 'RipCovWood', 'RipCovCanNone', 'RipCovUstoryNone',
                   'RipCovGrndNone')
large_wood = c('LWVol_Wet', 'LWVol_WetSlow', 'LWVol_WetFstTurb', 'LWVol_WetFstNT',
               'LWFreq_Wet')
undercuts = c('Ucut_Area', 'UcutLgth_Pct', 'UcutArea_Pct')
fish_cover = c('FishCovLW', 'FishCovTVeg', 'FishCovArt', 'FishCovNone',
               'FishCovAqVeg', 'FishCovTotal') 

# a list of lists
plot_list = list(size = size, 
                 pca = pca, 
                 channel_units = channel_units, 
                 complexity = complexity, 
                 side_channel = side_channel, 
                 substrate = substrate, 
                 other = other,
                 riparian_cover = riparian_cover, 
                 large_wood = large_wood, 
                 undercuts = undercuts, 
                 fish_cover = fish_cover)
# END LISTS

#-----------------------------------------------------------------
# Begin fish-habitat plots
#-----------------------------------------------------------------
# Mike's Way
fh_plot = function(data, metrics_list) {
  data %>%
    select(qrtl, one_of(metrics_list)) %>%
    gather(variable, value, -qrtl) %>%
    ggplot(aes(x = value,
               color = qrtl,
               fill = qrtl)) +
    geom_density(alpha = 0.3) +
    theme_bw() +
    labs(x = 'Value',
         y = 'Density',
         color = 'Category',
         fill = 'Category') +
    facet_wrap(~ variable,
               scales = 'free')
}
fh_plot(sthd_redd_lemhi, substrate)

# loop over data frames and metrics lists
for(d in 1:length(df_list)) {
  df = df_list[[d]]

  for(p in 1:length(plot_list)) {
    pl = plot_list[[p]]
    tmp_p = fh_plot(df, pl)
    ggsave(paste0('figures/sthd_redd_fh_plots/', names(df_list)[d], '_', names(plot_list)[p], '.png'))
  } # end plot metrics loop
} # end data frames loop

#-----------------------------------------------------------------
# Begin compare means by data frame and hab covariate
#-----------------------------------------------------------------
hab_list = unlist(unique(plot_list[1:length(plot_list)]))

# initiate blank df to store results
tst_results = setNames(data.frame(matrix(ncol = 3, nrow = length(df_list) * length(hab_list))),
                       c('data', 'hab_cov', 'p_value'))

# now loop over data frames and habitat covariates and perform 
# Wilcoxon rank sum test to compare means of Q4 vs. 'rest'
ctr = 1
for(d in 1:length(df_list)) {
  df = df_list[[d]]
  
  for(h in 1:length(hab_list)) {
    hc = hab_list[h]
    
    q4_data = pull(df[df$qrtl == 'Q4', hc])
    rest_data = pull(df[df$qrtl == 'Rest', hc])
    if(sum(is.na(q4_data)) == length(q4_data) |
       sum(is.na(rest_data)) == length(rest_data)) {
      tst_results[ctr, 1] = names(df_list)[[d]]
      tst_results[ctr, 2] = hc
      ctr = ctr + 1
      next
    }
    tst = try(wilcox.test(q4_data, rest_data))
    
    tst_results[ctr, 1] = names(df_list)[[d]]
    tst_results[ctr, 2] = hc
    tst_results[ctr, 3] = if_else(class(tst) == 'try-error',
                                  as.numeric(NA),
                                  tst$p.value)
    ctr = ctr + 1
    
  } # end habitat covariate loop
} # end data frame loop

# let's look at the # of significant comparisons by habitat covariate
sig_tst_results = tst_results %>%
  mutate(sig = ifelse(p_value <= 0.01, 1, 0)) %>%
  group_by(hab_cov) %>%
  summarise(sig_dfs = sum(sig, na.rm = T)) %>%
  ungroup() %>%
  filter(sig_dfs > 0) %>%
  #arrange(desc(sig_dfs)) %>%
  ggplot(aes(x = reorder(hab_cov, sig_dfs),
             y = sig_dfs)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(x = 'Habitat Covariate',
       y = '# of Significant Comparisons')
sig_tst_results
ggsave('figures/sthd_redd_significant_comps.png')

# let's plot 'raw' p-values by habitat covariate
raw_wcx_p = tst_results %>%
  ggplot(aes(x = p_value)) +
  geom_histogram(bins = 10) +
  theme_bw() +
  facet_wrap(~ hab_cov)
raw_wcx_p

# finally, let's get median and mean p-values
p_val_p = tst_results %>%
  group_by(hab_cov) %>%
  summarise(mn_p = mean(p_value, na.rm = T),
            med_p = median(p_value, na.rm = T)) %>%
  ungroup() %>%
  gather(key, value, mn_p:med_p) %>%
  ggplot(aes(x = reorder(hab_cov, -value),
             y = value)) +
  geom_col() +
  geom_hline(yintercept = 0.1, color = 'red') +
  theme_bw() +
  coord_flip() +
  labs(x = 'Habitat Covariate',
       y = 'p_value') +
  facet_wrap(~ key)
p_val_p
ggsave('figures/sthd_redd_p_values.png')

# End steelhead redds analysis
