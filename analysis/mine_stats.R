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
data("fh_sum_champ_2017")
fish_hab = fh_sum_champ_2017 %>%
  mutate_at(vars(Watershed, Year),
            list(as.factor))



data("fh_redds_champ_2017")
fish_hab = fh_redds_champ_2017 %>%
  mutate_at(vars(Watershed),
            list(as.factor)) %>%
  rename(Lon = LON_DD,
         Lat = LAT_DD,
         N = maxReddsPerKm)


# and the appropriate habitat dictionrary to go with it
data("hab_dict_2017")
hab_dict = hab_dict_2017

#-----------------------------------------------------------------
# clip Chinook data to Chinook domain
data("chnk_domain")

# which sites were sampled for Chinook? 
chnk_samps = fish_hab %>%
  filter(Species == 'Chinook') %>%
  select(Site, Lon, Lat, N) %>%
  distinct() %>%
  st_as_sf(coords = c('Lon', 'Lat'),
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
  as('sf')

# only keep Chinook data from sites in Chinook domain
fish_hab %<>%
  filter(Species == 'Steelhead' |
           (Species == 'Chinook' & Site %in% chnk_sites$Site))

#-----------------------------------------------------------------
# # what are some possible habitat covariates?
poss_hab_mets = hab_dict %>%
  filter(MetricCategory != 'Categorical') %>%
  filter(ShortName %in% names(fish_hab)) %>%
  pull(ShortName) %>%
  unique()

mine_res = fish_hab %>%
  split(list(.$Species)) %>%
  map_df(.id = 'Species',
         .f = function(x) {
           x %>%
             mutate(fish_dens = log(fish_dens + 0.005)) %>%
             estimate_MIC(covars = poss_hab_mets,
                          response = 'fish_dens')
         }) %>%
  left_join(hab_dict %>%
              filter(MetricGroupName == 'Visit Metric') %>%
              select(Metric = ShortName,
                     MetricCategory,
                     Name),
            by = 'Metric') %>%
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

mine_plot_df = mine_res #%>%
# filter out some metrics with too many NAs or 0s
# filter((perc_NA < 0.2 & non_0 > 100) | MetricCategory == 'Temperature') #%>%
# # filter out metrics with very low variance
# filter(var < 0.1) %>% select(1:11)
# # filter out area and volume metrics
# filter(!grepl('Area', Metric),
#        !grepl('Vol', Metric),
#        Metric != 'Lgth_Wet')

# make a plot of MIC values for all species
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



#-----------------------------------------------------------------
# look at correlations between habitat metrics
#-----------------------------------------------------------------
library(corrr)
library(ggcorrplot)

sel_mets = poss_hab_mets
sel_mets = mine_plot_df %>%
  group_by(MetricCategory) %>%
  slice(1:5) %>%
  ungroup() %>%
  pull(Metric) %>%
  unique() %>%
  as.character()


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
  filter(x_category == y_category)
  filter(abs(r) > 0.5)

corr_p1 = corr_mat %>%
  # rearrange(absolute = F) %>%
  shave(upper = T) %>% 
  rplot(legend = T,
        print_cor = T)

corr_p2 = fish_hab %>%
  select(one_of(sel_mets)) %>%
  cor(use = 'pairwise') %>%
  ggcorrplot(method = 'square',
             hc.order = T,
             lab = T,
             type = "lower")
corr_p2

library(GGally)
fish_hab %>%
  select(one_of(sel_mets)) %>%
  ggcorr()
