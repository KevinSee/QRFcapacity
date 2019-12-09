# Author: Mike Ackerman & Kevin See
# Purpose: Examine abundance/densities for summer parr and explore preferred or target habitat 
# conditions in
# Created: 10/24/2019
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
data("fh_sum_champ_2017")   # summer juvenile parr paired fish/CHaMP habitat data through 2017
#data("fh_win_champ_2017")   # winter juvenile presmolts paired fish/CHaMP habitat data through 2017
#data("fh_redds_champ_2017") # paired redd/CHaMP habitat data through 2017

# The fish-habitat summer CHaMP data frame contains data for chinook and steelhead. Let's just look at Chinook for now.
chnk_sum_df = fh_sum_champ_2017 %>%
  filter(Species == 'Chinook')

# first, let's just examine fish abundance and density information
chnk_sum_n = chnk_sum_df %>%
  select(Year, Watershed, N, fish_dens, Area_Wet) %>% # fish_dens = linear fish densities
  filter(fish_dens > 0) %>% # only use sites where fish were observed
  mutate(fish_dens_m2 = N / Area_Wet, # areal fish density (fish/m2) 
         log_fish_dens = log(fish_dens + 0.005),
         log_fish_dens_m2 = log(fish_dens_m2 + 0.005))

# quartiles of linear and areal fish density estimates
chnk_sum_qrtl_lin  = quantile(chnk_sum_n$log_fish_dens, probs = c(0, 0.25, 0.50, 0.75, 1))
chnk_sum_qrtl_area = quantile(chnk_sum_n$log_fish_dens_m2, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = T) 

# plot log transformed fish densities with quartiles
chnk_sum_p = chnk_sum_n %>%
  ggplot() +
  geom_histogram(aes(x = log_fish_dens, fill = Watershed), bins = 50) +
  geom_vline(xintercept = chnk_sum_qrtl_lin, color = 'blue') +
  theme_bw() +
  labs(x = 'Juvenile Chinook Density (fish/m)',
       y = 'Frequency') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) 
chnk_sum_p

# reduce chnk_sum_df in preparation for evaluation
chnk_sum_df = chnk_sum_df %>%
  select(Watershed, Year, StreamName, Channel_Type, Lat, Lon,          # site
         N, fish_dens,                                                 # fish abundance/density
         FishSiteLength, FishWettedArea, CUMDRAINAG, MeanU, Q,         # size
         WetWdth_Int, WetBraid, WetWdth_Avg, DpthThlwg_Avg, Area_Wet,
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
         FishCovTotal) %>%
  mutate(fish_dens_m2 = N / Area_Wet, # areal fish density (fish/m2) 
         log_fish_dens = log(fish_dens + 0.005),
         log_fish_dens_m2 = log(fish_dens_m2 + 0.005)) %>%
  filter(fish_dens > 0)

# now consider parsing data by Watershed, Channel_Type, others?
# by watershed
wtr = unique(chnk_sum_df$Watershed)
for(w in wtr) {
tmp = filter(chnk_sum_df, Watershed == as.character(w)) %>%
  mutate(qrtl = cut_number(log_fish_dens, n = 4, labels = c('Q1','Q2','Q3','Q4'))) %>%
  mutate(qrtl = recode(qrtl,
                       `Q1` = 'Rest',
                       `Q2` = 'Rest',
                       `Q3` = 'Rest'))
assign(paste('chnk_sum_', make_clean_names(w), sep = ''), tmp)
}

# by channel unit type
cht = unique(chnk_sum_df$Channel_Type)
cht = cht[!is.na(cht)]
for(c in cht) {
tmp = filter(chnk_sum_df, Channel_Type == as.character(c)) %>%
  mutate(qrtl = cut_number(log_fish_dens, n = 4, labels = c('Q1','Q2','Q3','Q4'))) %>%
  mutate(qrtl = recode(qrtl,
                       `Q1` = 'Rest',
                       `Q2` = 'Rest',
                       `Q3` = 'Rest'))
assign(paste('chnk_sum_', make_clean_names(c), sep = ''), tmp)
}

# df_list = list(chnk_sum_entiat, chnk_sum_john_day, chnk_sum_lemhi, chnk_sum_minam, chnk_sum_south_fork_salmon,
#                chnk_sum_upper_grande_ronde, chnk_sum_wenatchee, # watersheds
#                chnk_sum_cascade, chnk_sum_confined, chnk_sum_island_braided, chnk_sum_meandering, chnk_sum_plane_bed,
#                chnk_sum_pool_riffle, chnk_sum_step_pool, chnk_sum_straight) # channel type

#-----------------------------------------------------------------
# lists of metric categories
#-----------------------------------------------------------------
# lists of metric categories
size = c("FishSiteLength", "FishWettedArea", "CUMDRAINAG", "MeanU", 
         "Q", "WetWdth_Int", "WetBraid", "WetWdth_Avg", "DpthThlwg_Avg")

pca  = c("DistPrin1", "NatPrin1", "NatPrin2")

channel_units = c("SlowWater_Pct", "SlowWater_Freq", "FstTurb_Pct", "FstTurb_Freq", "FstNT_Pct", "FstNT_Freq", "CU_Freq")

complexity = c("Grad", "Sin", "DetrendElev_SD", "DpthThlwg_UF_CV", "DpthWet_SD",  
               "WetWdth_CV", "WetWDRat_Avg", "PoolResidDpth")

side_channel = c("SC_Area_Pct", "WetSC_Pct", "SCSm_Freq")

substrate = c("SubD16", "SubD50", "SubD84", "SubEstGrvl", "SubEstSandFines", "SubEstBldr", "SubEstCbl")

other = c("Cond")

riparian_cover = c("RipCovBigTree", "RipCovConif", "RipCovNonWood", "RipCovUstory", 
                   "RipCovWood", "RipCovCanNone", "RipCovUstoryNone", "RipCovGrndNone")

large_wood = c("LWVol_Wet", "LWVol_WetSlow", "LWVol_WetFstTurb", "LWVol_WetFstNT", "LWFreq_Wet")

undercuts = c("Ucut_Area",  "UcutLgth_Pct", "UcutArea_Pct")

fish_cover = c("FishCovLW", "FishCovTVeg", "FishCovArt", "FishCovNone", "FishCovAqVeg", "FishCovTotal")

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
fh_plot(chnk_sum_lemhi, large_wood)

df_list = list(Entiat = chnk_sum_entiat, 
               JohnDay = chnk_sum_john_day, 
               Lemhi = chnk_sum_lemhi, 
               Minam = chnk_sum_minam, 
               SFS = chnk_sum_south_fork_salmon,
               UGR = chnk_sum_upper_grande_ronde, 
               Wenatchee = chnk_sum_wenatchee, # watersheds
               cascade = chnk_sum_cascade, 
               confined = chnk_sum_confined, 
               island_braided = chnk_sum_island_braided, 
               meandering = chnk_sum_meandering, 
               plane_bed = chnk_sum_plane_bed,
               pool_riffle = chnk_sum_pool_riffle, 
               step_pool = chnk_sum_step_pool, 
               straight = chnk_sum_straight) # channel type

# find data frames that are too small for comparisons or plotting
min_samp_size = 20
small_dfs = names(which(sapply(df_list, nrow) > min_samp_size - 1, TRUE))
df_list = df_list[names(df_list) %in% small_dfs]

# loop over data frames and metrics lists
# for(d in 1:length(df_list)) {
#   df = df_list[[d]] 
#   
#   for(p in 1:length(plot_list)) {
#     pl = plot_list[[p]]
#     tmp_p = fh_plot(df, pl)
#     ggsave(paste0('figures/chnk_sum_fh_plots/', names(df_list)[d], '_', names(plot_list)[p], '.png'))
#   } # end plot metrics loop
#   
# } # end data frames loop

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
ggsave('figures/chnk_sum_significant_comps.png')

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
ggsave('figures/chnk_sum_p_values.png')

# Kevin's cool way for making fh_plots
# plot_list = list(size, pca, channel_units, complexity, side_channel, substrate, 
#                  other, riparian_cover, large_wood, undercuts, fish_cover) %>%
#   map(.f = function(x) {
#     ch_df %>%
#       select(plot_cat, one_of(x)) %>%
#       gather(variable, value, -plot_cat) %>%
#       ggplot(aes(x = value, 
#                  color = plot_cat,
#                  fill = plot_cat)) +
#       #geom_histogram(position = 'dodge') +
#       geom_density(alpha = 0.3) +
#       theme_classic() +
#       theme(axis.text.x = element_text(color = 'black', size = 10),
#             axis.text.y = element_text(color = 'black', size = 10)) +
#       facet_wrap(~ variable,
#                  scales = 'free')
#   })
# plot_list[[1]]
