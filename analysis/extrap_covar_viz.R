# Author: Kevin See
# Purpose: Look at correlations and patterns for GAAs
# Created: 12/17/2020
# Last Modified: 12/17/2021
# Notes: for master sample pts and 200m reaches

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
# master sample points
#-----------------------------------------------------------------
data("gaa")

gaa_all = gaa %>%
  # filter out a few areas
  filter(!HUC6NmNRCS %in% c('Upper Sacramento', 'Southern Oregon Coastal', 'Puget Sound', 'Northern California Coastal', 'Oregon Closed Basins')) %>%
  # don't use AEM sites in model
  filter(!grepl('^AEM', Site)) %>%
  # don't use non-GRTS sites
  filter(SiteID_alt != 'NonGRTSSite' | is.na(SiteID_alt)) %>%
  filter(!grepl('mega', Site, ignore.case = T)) %>%
  rename(Channel_Type = ChanlType) %>%
  rename(chnk_strmnet = chnk,
         sthd_strmnet = steel) %>%
  rename(sthd = sthd_range,
         chnk = chnk_range) %>%
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
data('champ_site_2011_17_avg')

range_comp = gaa_all %>%
  select(Site,
         any_of(extrap_num)) %>%
  mutate(Source = if_else(Site %in% unique(champ_site_2011_17_avg$Site),
                          "CHaMP Reaches",
                          "non-CHaMP Reaches")) %>%
  pivot_longer(any_of(extrap_num),
               names_to = "Metric",
               values_to = "value") %>%
  mutate(across(c(Source,
                  Metric),
                as_factor))


range_max = range_comp %>%
  group_by(Metric, Source) %>%
  summarise_at(vars(value),
               tibble::lst(min, max),
               na.rm = T) %>%
  filter(Source == 'CHaMP Reaches') %>%
  ungroup() %>%
  gather(type, value, -Metric, -Source)

range_max_all = range_max %>%
  bind_rows(range_comp %>%
              group_by(Metric, Source) %>%
              summarize(across(value,
                               tibble::lst(quantile),
                               probs = c(0.25),
                               na.rm = T)) %>%
              rename(lower_quant = value_quantile) %>%
              left_join(range_comp %>%
                          group_by(Metric, Source) %>%
                          summarize(across(value,
                                           tibble::lst(quantile),
                                           probs = c(0.75),
                                           na.rm = T)) %>%
                          rename(upper_quant = value_quantile)) %>%
              filter(Source == 'CHaMP Reaches') %>%
              ungroup() %>%
              pivot_longer(cols = ends_with('quant'),
                           names_to = "type",
                           values_to = "value")) %>%
  mutate(range_type = if_else(type %in% c('min', 'max'),
                              'Range',
                              'IQR'))

covar_range_p = range_comp %>%
  mutate(Source = str_remove(Source, " Reaches$")) %>%
  ggplot(aes(x = Source,
             y = value,
             fill = Source)) +
  geom_boxplot() +
  facet_wrap(~ Metric,
             scales = 'free') +
  geom_hline(data = range_max,
             aes(yintercept = value),
             lty = 2,
             color = 'darkgray') +
  theme_minimal() +
  theme(legend.position = "bottom")

# covar_range_p
# ggsave('output/figures/GAA_mastPts_comparison.pdf',
#        covar_range_p,
#        height = 7,
#        width = 10)

# another way to look at it
# break it down by HUC6
comp_df = gaa_all %>%
  mutate(Source = if_else(Site %in% unique(pred_hab_sites$Site),
                          "CHaMP Reaches",
                          "non-CHaMP Reaches")) %>%
  filter(SO_v1 <= 6,
         !GNIS_NA %in% c("Columbia River",
                           "Snake River")) %>%
  filter(sthd | chnk) %>%
  pivot_longer(cols = any_of(extrap_num),
               names_to = "Metric",
               values_to = "value") %>%
  left_join(range_max_all %>%
              select(-range_type) %>%
              pivot_wider(names_from = "type",
                          values_from = "value") %>%
              select(-Source)) %>%
  mutate(in_range = if_else(value >= min & value <= max,
                            T, F),
         in_iqr = if_else(value >= lower_quant & value <= upper_quant,
                          T, F)) %>%
  filter(!(Metric == "S2_02_11" & value < -9000)) %>%
  filter(!is.na(value))


# log these covariates for plotting purposes
log_mets = c('SrtCumDrn',
             'StrmPwr')

# 2 plots, one with logged covariates, one without
covar_comp_log = comp_df %>%
  filter(Metric %in% log_mets) %>%
  ggplot(aes(x = HUC6NmNRCS,
             y = value,
             fill = HUC6NmNRCS)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_hline(data = range_max_all %>%
               filter(Metric %in% log_mets),
             aes(yintercept = value,
                 linetype = range_type),
             color = 'darkblue') +
  scale_linetype(guide = 'none') +
  geom_boxplot(outlier.color = 'gray70') +
  facet_wrap(~ Metric,
             nrow = 2,
             scales = 'free') +
  scale_y_continuous(trans = "log",
                     breaks = scales::pretty_breaks()) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 6),
        axis.text.y = element_text(size = 6))

covar_comp_nolog = comp_df %>%
  filter(!Metric %in% log_mets) %>%
  ggplot(aes(x = HUC6NmNRCS,
             y = value,
             fill = HUC6NmNRCS)) +
  scale_fill_brewer(palette = "Spectral") +
  geom_hline(data = range_max_all %>%
               filter(!Metric %in% log_mets),
             aes(yintercept = value,
                 linetype = range_type),
             color = 'darkblue') +
  scale_linetype(guide = 'none') +
  geom_boxplot(outlier.color = 'gray70') +
  facet_wrap(~ Metric,
             nrow = 2,
             scales = 'free') +
  scale_y_continuous(trans = "identity",
                     breaks = scales::pretty_breaks()) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 6),
        axis.text.y = element_text(size = 6))

covar_comp_p = ggpubr::ggarrange(covar_comp_log,
                                 covar_comp_nolog,
                                 nrow = 1,
                                 widths = c(1, 3.5),
                                 common.legend = T,
                                 legend = "bottom")

# save the plot
# ggsave('output/figures/GAA_mastPts_comparison_huc6.pdf',
#        covar_comp_p,
#        height = 8,
#        width = 11)


# another way to summarize these results
comp_summ = comp_df %>%
  mutate(across(HUC6NmNRCS,
                fct_drop)) %>%
  group_by(Metric,
           HUC6NmNRCS,
           min,
           lower_quant, upper_quant,
           max) %>%
  summarise(n_rch = n_distinct(Site),
            n_champ = n_distinct(Site[Source == 'CHaMP Reaches']),
            n_in_range = n_distinct(Site[in_range]),
            n_out_range = n_distinct(Site[!in_range]),
            # n_in_range = n_distinct(Site[in_iqr]),
            # n_out_range = n_distinct(Site[!in_iqr]),
            pct_in_range = n_in_range / n_rch,
            max_huc = max(value, na.rm = T),
            min_huc = min(value, na.rm = T),
            .groups = "drop") %>%
  mutate(pct_max_over = if_else(max_huc > max,
                                (max_huc - max) / max,
                                NA_real_),
         pct_min_under = if_else(min_huc < min,
                                 (min_huc - min) / min,
                                 NA_real_))


comp_summ %>%
  # filter(!is.na(pct_min_under))
  # arrange(pct_in_range)
  arrange(desc(pct_max_over))

comp_summ %>%
  filter(HUC6NmNRCS == "Clearwater") %>%
  pull(pct_in_range)

comp_df %>%
  filter(HUC6NmNRCS == "Upper Columbia",
         !in_range) %>%
  group_by(GNIS_NA) %>%
  summarise(n_rch = n_distinct(Site),
            .groups = "drop") %>%
  arrange(desc(n_rch))


#-----------------------------------------------------------------
# 200m reaches
#-----------------------------------------------------------------
data("rch_200")
