# Author: Mike Ackerman & Kevin See
# Purpose: Examine abundance/densities of steelhead winter presmolts and explore preferred or target habitat conditions
# in identified high density areas
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
data("fh_win_champ_2017") # paired redd/CHaMP habitat data through 2017

# The fish-habitat summer CHaMP data frame contains data for steelhead and steelhead. Let's just look at steelhead for now.
sthd_win_df = fh_win_champ_2017 %>%
  filter(Species == 'Steelhead')

# first, let's just examine fish abundance and density information
sthd_win_n = sthd_win_df %>%
  select(Watershed, Year, N, fish_dens) %>% # fish_dens = linear fish densities
  filter(fish_dens > 0) %>%
  mutate(log_fish_dens = log(fish_dens + 0.005))

# quartiles of linear and areal fish density estimates
sthd_win_qrtl = quantile(sthd_win_n$log_fish_dens, probs = c(0, 0.25, 0.50, 0.75, 1))

# plot log transformed fish densities with quartiles
sthd_win_p = sthd_win_n %>%
  ggplot() +
  geom_histogram(aes(x = log_fish_dens, fill = Watershed), bins = 50) +
  geom_vline(xintercept = sthd_win_qrtl, color = 'blue') +
  theme_bw() +
  labs(x = 'log(Presmolts/m)',
       y = 'Frequency') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10)) 
sthd_win_p

# reduce sthd_win_df in preparation for evaluation
sthd_win_df = sthd_win_df %>%
  select(Watershed, Year, Tier1_fish, Tier2_fish, Tier1, Tier2,
         fish_dens, AreaTotal,
         Dpth_Max, DpthThlwgExit, DpthResid,                            # depth
         FishCovLW, FishCovTVeg, FishCovArt, FishCovAqVeg, FishCovNone, # fish cover
         FishCovAll,
         SubEstBdrk, SubEstBldr, SubEstCbl, SubEstGrvl, SubEstSandFines,# substrate 
         SubEstCandBldr, SubD50, 
         n_UC, Ucut_Length, Ucut_Area, UcutArea_Pct,                    # undercut
         Sin, CU_Freq, Discharge, Temp, LWCount) %>% 
  filter(fish_dens > 0) %>%                                             # other
  mutate(log_fish_dens = log(fish_dens + 0.005),
         LWDens = LWCount / AreaTotal)

# next, let's parse the sthd_win_df by Watershed, Tier1
# by watershed
wtr = unique(sthd_win_df$Watershed)
for(w in wtr) {
  tmp = filter(sthd_win_df, Watershed == as.character(w)) %>%
    mutate(qrtl = cut_number(log_fish_dens, n = 4, labels = c('Q1','Q2','Q3','Q4'))) %>%
    mutate(qrtl = recode(qrtl,
                         `Q1` = 'Rest',
                         `Q2` = 'Rest',
                         `Q3` = 'Rest'))
  assign(paste('sthd_win_', make_clean_names(w), sep = ''), tmp)
}

# by tier 1 category
tr1 = unique(sthd_win_df$Tier1)
for(t in tr1) {
  tmp = filter(sthd_win_df, Tier1 == as.character(t)) %>%
    mutate(qrtl = cut_number(log_fish_dens, n = 4, labels = c('Q1','Q2','Q3','Q4'))) %>%
    mutate(qrtl = recode(qrtl,
                         `Q1` = 'Rest',
                         `Q2` = 'Rest',
                         `Q3` = 'Rest'))
  assign(paste('sthd_win_', make_clean_names(t), sep = ''), tmp)
}

# list of data frames
df_list = list(Entiat = sthd_win_entiat, 
               Lemhi = sthd_win_lemhi,
               Methow = sthd_win_methow,
               SFS = sthd_win_south_fork_salmon,
               UGR = sthd_win_upper_grande_ronde, 
               Wenatchee = sthd_win_wenatchee, 
               Yankee = sthd_win_yankee_fork, # watersheds
               pool = sthd_win_pool, 
               riffle = sthd_win_riffle,
               run = sthd_win_run,
               ssc = sthd_win_ssc) # Tier 1

# find data frames that are too small for comparisons or plotting
min_samp_size = 20
small_dfs = names(which(sapply(df_list, nrow) > min_samp_size - 1, TRUE))
df_list = df_list[names(df_list) %in% small_dfs]; rm(small_dfs)

#-----------------------------------------------------------------
# lists of metric categories
#-----------------------------------------------------------------
depth = c('Dpth_Max', 'DpthThlwgExit', 'DpthResid')
fish_cover = c('FishCovLW', 'FishCovTVeg', 'FishCovArt', 
               'FishCovAqVeg', 'FishCovNone', 'FishCovAll')
substrate = c('SubEstBdrk', 'SubEstBldr', 'SubEstCbl', 
              'SubEstGrvl', 'SubEstSandFines', 'SubEstCandBldr', 'SubD50')
undercut = c('n_UC', 'Ucut_Length', 'Ucut_Area', 'UcutArea_Pct')
other = c('Sin', 'CU_Freq', 'Discharge', 'Temp', 'LWCount', 'LWDens')

# a list of lists
plot_list = list(depth = depth,
                 fish_cover = fish_cover,
                 substrate = substrate,
                 undercut = undercut,
                 other = other)
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
fh_plot(sthd_win_lemhi, substrate)

# loop over data frames and metrics lists
# for(d in 1:length(df_list)) {
#   df = df_list[[d]]
# 
#   for(p in 1:length(plot_list)) {
#     pl = plot_list[[p]]
#     tmp_p = fh_plot(df, pl)
#     ggsave(paste0('figures/sthd_win_fh_plots/', names(df_list)[d], '_', names(plot_list)[p], '.png'))
#   } # end plot metrics loop
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
ggsave('figures/sthd_win_significant_comps.png')

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
ggsave('figures/sthd_win_p_values.png')

# End steelhead redds analysis