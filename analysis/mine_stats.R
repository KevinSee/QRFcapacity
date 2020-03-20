# Author: Kevin See
# Purpose: Calculate MINE statistics on various fish/habitat datasets
# Created: 2/13/2020
# Last Modified: 3/20/2020
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
# summmer juveniles with CHaMP metrics
data("fh_sum_champ_2017")

# summer juveniles with DASH metrics
data("fh_sum_dash_2014_17")

# redds
data("fh_redds_champ_2017")

# winter juveniles
data("fh_win_champ_2017")

# combine all fish-habitat datasets into one list
fish_hab_list = list('Redds' = fh_redds_champ_2017 %>%
                       mutate_at(vars(Watershed),
                                 list(as.factor)) %>%
                       # what kind of redd density metric should we use?
                       mutate(fish_dens = maxReddsPerMsq),
                     'Winter' = fh_win_champ_2017 %>%
                       filter(!is.na(fish_dens)) %>%
                       mutate_at(vars(Watershed, Year, Tier1),
                                 list(as.factor)),
                     'Summer_CHaMP' = fh_sum_champ_2017 %>%
                       mutate_at(vars(Watershed, Year),
                                 list(as.factor)),
                     'Summer_DASH' = fh_sum_dash_2014_17 %>%
                       mutate_at(vars(Watershed, Year),
                                 list(as.factor)))

# alter a few metrics consistently across all datasets
fish_hab_list %<>%
  map(.f = function(x) {
    # scale some metrics by site length
    x %>%
      mutate_at(vars(starts_with('LWVol'),
                     ends_with('_Vol')),
                list(~ . / Lgth_Wet * 100))
    
    # add a metric showing "some" riparian canopy
    if("RipCovCanNone" %in% names(x)) {
      x %<>%
        mutate(RipCovCanSome = 100 - RipCovCanNone)
    }
    
    # add a metric showing "some" fish cover
    if("FishCovNone" %in% names(x)) {
      x %<>%
        mutate(FishCovSome = 100 - FishCovNone)
    }
    
    return(x)
  })


#-----------------------------------------------------------------
# and the appropriate habitat dictionrary to go with it
data("hab_dict_2017")
hab_dict = hab_dict_2017

# change some of the descriptions for large wood volume
hab_dict %<>%
  mutate(DescriptiveText = if_else(grepl("^LWVol", ShortName),
                                   paste0(str_remove(DescriptiveText, ".$"),
                                          ", scaled by site length."),
                                   DescriptiveText),
         UnitOfMeasure = if_else(grepl("^LWVol", ShortName),
                                 paste0(UnitOfMeasure,
                                        " per 100 meters"),
                                 UnitOfMeasure),
         UnitOfMeasureAbbrv = if_else(grepl("^LWVol", ShortName),
                                      paste0(UnitOfMeasureAbbrv,
                                             "/100m"),
                                      UnitOfMeasureAbbrv)) %>%
  # add description for some riparian canopy
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "RipCovCanNone") %>%
              mutate(ShortName = "RipCovCanSome",
                     Name = "Riparian Cover: Some Canopy",
                     DescriptiveText = "Percent of riparian canopy with some vegetation.")) %>%
  # add description for no riparian ground cover
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "RipCovGrnd") %>%
              mutate(ShortName = "RipCovGrndNone",
                     Name = "Riparian Cover: No Ground",
                     DescriptiveText = "Percent of groundcover with no vegetation.")) %>%
  # add description for some fish cover
  bind_rows(hab_dict_2017 %>%
              filter(ShortName == "FishCovNone") %>%
              mutate(ShortName = "FishCovSome",
                     Name = "Fish Cover: Some Cover",
                     DescriptiveText = "Percent of wetted area with some form of fish cover"))

#-----------------------------------------------------------------
# clip Chinook data to Chinook domain
data("rch_200")
data("champ_site_rch")

chnk_sites = champ_site_rch %>%
  inner_join(rch_200 %>%
               select(UniqueID, chnk)) %>%
  filter(chnk) %>%
  pull(Site) %>%
  as.character()

# add Big Springs and Little Springs sites in the Lemhi
chnk_sites = c(chnk_sites,
               fish_hab_list %>%
                 map_df(.id = 'dataset',
                        .f = function(x) {
                          x %>%
                            filter(grepl('Big0Springs', Site) | grepl('Little0Springs', Site)) %>%
                            select(Site) %>%
                            distinct()
                        }) %>%
                 pull(Site) %>%
                 unique()) %>%
  unique()


# only keep Chinook data from sites in Chinook domain (or with positive Chinook density)
fish_hab_list %<>%
  map(.f = function(x) {
    x %>%
      filter(Species == 'Steelhead' |
               (Species == 'Chinook' & (Site %in% chnk_sites | fish_dens > 0)))
  })


# #-------------------------------
# data("chnk_domain")
# 
# # which sites were sampled for Chinook? 
# chnk_samps = fish_hab_list %>%
#   map_df(.id = 'dataset',
#          .f = function(x) {
#            x %>%
#              filter(Species == 'Chinook') %>%
#              select(Site, LON_DD, LAT_DD, fish_dens) %>%
#              distinct()
#          }) %>%
#   filter(!is.na(LON_DD)) %>%
#   st_as_sf(coords = c('LON_DD', 'LAT_DD'),
#            crs = 4326) %>%
#   st_transform(st_crs(chnk_domain))
# 
# # chnk_samps %>%
# #   group_by(Site) %>%
# #   summarise(n_datasets = n_distinct(dataset)) %>%
# #   ungroup() %>%
# #   tabyl(n_datasets)
# 
# # set snap distance (in meters)
# st_crs(chnk_samps)
# snap_dist = 1000
# 
# # which of those sites are in Chinook domain?
# chnk_sites = chnk_samps %>%
#   as_Spatial() %>%
#   maptools::snapPointsToLines(chnk_domain %>%
#                                 mutate(id = 1:n()) %>%
#                                 select(id, MPG) %>%
#                                 as_Spatial(),
#                               maxDist = snap_dist,
#                               withAttrs = T,
#                               idField = 'id') %>%
#   as('sf') %>%
#   as_tibble() %>%
#   pull(Site) %>%
#   unique()
# 
# # only keep Chinook data from sites in Chinook domain
# fish_hab_list %<>%
#   map(.f = function(x) {
#     x %>%
#       filter(Species == 'Steelhead' |
#                (Species == 'Chinook' & Site %in% chnk_sites))
#   })

#-----------------------------------------------------------------
# what are some possible habitat covariates?
poss_hab_mets = fish_hab_list %>%
  map_df(.f = function(x) tibble(ShortName = names(x))) %>%
  distinct() %>%
  left_join(hab_dict %>%
              filter(! MetricGroupName %in% c('Tier 1 Summary', 'Tier 2 Summary')) %>%
              select(ShortName, MetricGroupName, Name, MetricCategory) %>%
              distinct()) %>%
  filter(is.na(MetricCategory) | MetricCategory != 'Categorical') %>%
  filter((!is.na(Name) |
            ShortName %in% c('Elev_M',
                             'CUMDRAINAG',
                             "DpthThlwg_Avg",
                             "SCSm_Area",
                             "SCSm_Freq",
                             "SCSm_Ct",
                             "SCSm_Vol",
                             "RipCovUstoryNone",
                             "RipCovGrndNone",
                             "SC_Area",
                             "SC_Area_Pct",
                             "ChnlUnitTotal_Ct",
                             "Discharge_fish",
                             "Temp",
                             "PercentIceCover",
                             "LWCount",
                             "SubEstBdrk",
                             "Ucut_Length",
                             "FishCovAll",
                             "SubEstCandBldr",
                             "UcutLgth",
                             "LWcnt_Wet"))) %>%
  mutate(Name = if_else(is.na(Name),
                        ShortName,
                        Name)) %>%
  filter(!ShortName %in% c('Tier1', 'Tier2')) %>%
  mutate(MetricCategory = if_else(grepl('SC', ShortName),
                                  'SideChannel',
                                  MetricCategory),
         MetricCategory = if_else(grepl('Sub', ShortName),
                                  'Substrate',
                                  MetricCategory),
         MetricCategory = if_else(grepl('^Rip', ShortName),
                                  'Riparian',
                                  MetricCategory),
         MetricCategory = if_else(grepl('^FishCov', ShortName) |
                                    grepl('Ucut', ShortName) |
                                    ShortName %in% c('PercentIceCover'),
                                  'Cover',
                                  MetricCategory),
         MetricCategory = if_else(grepl('^LW', ShortName),
                                  'Wood',
                                  MetricCategory),
         MetricCategory = if_else(grepl('Discharge', ShortName) |
                                    ShortName %in% c("DpthThlwg_Avg",
                                                     "Dpth_Max",
                                                     'DpthThlwgExit',
                                                     'DpthResid',
                                                     'TotalVol',
                                                     'CUMDRAINAG'),
                                  'Size',
                                  MetricCategory),
         MetricCategory = if_else(ShortName %in% c('Elev_M', 'Temp'),
                                  'Temperature',
                                  MetricCategory),
         MetricCategory = if_else(ShortName %in% c('ChnlUnitTotal_Ct'),
                                  'ChannelUnit',
                                  MetricCategory))


# poss_hab_mets %>%
#   mutate(MetricGroupName = if_else(is.na(MetricGroupName) &
#                                      (grepl('^SC', ShortName) |
#                                         grepl('^Rip', ShortName) |
#                                         ShortName %in% c('DpthThlwg_Avg')),
#                                    'Visit Metric',
#                                    MetricGroupName),
#          MetricGroupName = if_else(is.na(MetricGroupName) &
#                                      ShortName %in% c("Elev_M"),
#                                    'GAA',
#                                    MetricGroupName))
# poss_hab_mets %>%
#   filter(is.na(MetricGroupName))
#   tabyl(MetricGroupName)

#-----------------------------------------------------------------
# generate MINE statistics
#-----------------------------------------------------------------
mine_res = crossing(dataset = names(fish_hab_list),
                    species = unique(fish_hab_list$Summer_CHaMP$Species)) %>%
  mutate(fh_data = map2(dataset,
                        species,
                        .f = function(x, y) {
                          fish_hab_list[[x]] %>%
                            filter(Species == y)
                        }),
         metrics = map(fh_data,
                       .f = function(x) {
                         poss_hab_mets %>%
                           filter(ShortName %in% names(x)) %>%
                           pull(ShortName)
                       })) %>%
  mutate(mine_res = map2(fh_data,
                         metrics,
                         .f = function(x, y) {
                           if(sum(x$fish_dens == 0) == 0) {
                             try(x %>%
                                   mutate(fish_dens = log(fish_dens)) %>%
                                   estimate_MIC(covars = y,
                                                response = 'fish_dens'))
                           } else {
                             try(x %>%
                                   mutate(fish_dens = log(fish_dens + 0.005)) %>%
                                   estimate_MIC(covars = y,
                                                response = 'fish_dens'))
                           }
                         }))

# create database for plotting, potentially filtering out some metrics that we wouldn't want to use
mine_plot_list = mine_res %>%
  select(-fh_data, -metrics) %>%
  unnest(cols = mine_res) %>%
  left_join(poss_hab_mets,
            by = c('Metric' = 'ShortName')) %>%
  mutate_at(vars(MetricCategory),
            list(fct_explicit_na),
            na_level = 'Other') %>%
  mutate_at(vars(Name),
            list(as.character)) %>%
  mutate(Name = if_else(is.na(Name),
                        as.character(Metric),
                        Name)) %>%
  #split by dataset
  split(list(.$dataset)) %>%
  map(.f = function(x) {
    x %>%
  # put the metric names in descending order by MIC
  mutate_at(vars(Metric, Name),
            list(~ fct_reorder(., .x = MIC))) %>%
  select(species, MetricCategory, Metric, everything()) %>%
  arrange(species, MetricCategory, desc(MIC)) %>%
      # filter out some metrics with too many NAs or 0s
      filter((perc_NA < 0.5 & non_0 > 100)) %>%
      # filter out metrics with very low variance
      # filter(var < 0.1) %>%
      # filter(obsCV < 0.1)
      # janitor::tabyl(MetricCategory)
      # select(1:11)
      # filter out area and volume metrics
      filter(!grepl('Area$', Metric),
             !grepl('Vol$', Metric),
             !Metric %in% c('Lgth_Wet', 
                            'Lgth_BfChnl',
                            'Lgth_WetChnl',
                            'Area_Wet', 
                            'Area_Bf', 
                            'WetVol', 
                            'BfVol'))
    })


#-----------------------------------------------------
# make some plots of MIC values
#-----------------------------------------------------
mine_p = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      ggplot(aes(x = Name,
                 y = MIC,
                 fill = species)) +
      geom_col(position = position_dodge(1)) +
      coord_flip() +
      facet_wrap(~ MetricCategory,
                 scales = 'free_y',
                 ncol = 3) +
      scale_fill_brewer(palette = 'Set1',
                        guide = guide_legend(nrow = 1)) +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 5)) +
      labs(title = unique(x$dataset),
           fill = "Species")
  })

mine_p2 = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      ggplot(aes(x = Name,
                 y = MIC,
                 fill = species)) +
      geom_col(position = position_dodge(1)) +
      coord_flip() +
      scale_fill_brewer(palette = 'Set1',
                        guide = guide_legend(nrow = 1),
                        name = 'Species') +
      theme(legend.position = 'bottom',
            axis.text = element_text(size = 5)) +
      labs(title = unique(x$dataset))
  })

mine_chnk_p = mine_plot_list %>%
  map(.f = function(x) {
    x %>%
      filter(species == "Chinook") %>%
      # filter(MetricCategory != 'Other') %>%
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
            axis.text = element_text(size = 6)) +
      labs(title = unique(x$dataset),
           fill = 'Category')
  })

# save the plots to look at later
pdf('output/figures/MINE_Catg_All.pdf',
    width = 8,
    height = 9)
for(i in 1:length(mine_p)) {
  print(mine_p[[i]])
}
dev.off()

pdf('output/figures/MINE_All.pdf',
    width = 8,
    height = 9)
for(i in 1:length(mine_p2)) {
  print(mine_p2[[i]])
}
dev.off()

pdf('output/figures/MINE_Chnk.pdf',
    width = 8,
    height = 9)
for(i in 1:length(mine_chnk_p)) {
  print(mine_chnk_p[[i]])
}
dev.off()



#-----------------------------------------------------------------
# look at correlations between habitat metrics
#-----------------------------------------------------------------
library(corrr)
library(corrplot)

data("champ_cu")
data("champ_dash_avg")
data("champ_site_2011_17_avg")

hab_data_list = list('CHaMP' = champ_site_2011_17_avg,
                     'DASH' = champ_dash_avg,
                     'ChnlUnit' = champ_cu)

# transform a few metrics consistently across datasets
hab_data_list %<>%
  map(.f = function(x) {
    # scale some metrics by site length
    x %>%
      mutate_at(vars(starts_with('LWVol'),
                     ends_with('_Vol')),
                list(~ . / Lgth_Wet * 100))
    
    # add a metric showing "some" riparian canopy
    if("RipCovCanNone" %in% names(x)) {
      x %<>%
        mutate(RipCovCanSome = 100 - RipCovCanNone)
    }
    
    # add a metric showing "some" fish cover
    if("FishCovNone" %in% names(x)) {
      x %<>%
        mutate(FishCovSome = 100 - FishCovNone)
    }
    
    return(x)
  })

hab_corr = tibble(dataset = names(hab_data_list),
                  hab_data = hab_data_list) %>%
  mutate(metrics = map(hab_data,
                       .f = function(x) {
                         mets = poss_hab_mets %>%
                           filter(ShortName %in% names(x)) %>%
                           pull(ShortName)
                         x %>%
                           select(one_of(mets)) %>%
                           gather(Metric, value) %>%
                           group_by(Metric) %>%
                           summarise(n_tot = n(),
                                     n_NA = sum(is.na(value)),
                                     non_0 = sum(value != 0, na.rm = T)) %>%
                           mutate(perc_NA = n_NA / n_tot,
                                  perc_non_0 = non_0 / n_tot) %>%
                           filter(perc_NA < 0.5,
                                  non_0 > 100) %>%
                           filter(!grepl('Area$', Metric),
                                  !grepl('Vol$', Metric),
                                  !Metric %in% c('Lgth_Wet', 
                                                 'Lgth_BfChnl',
                                                 'Lgth_WetChnl',
                                                 'Area_Wet', 
                                                 'Area_Bf', 
                                                 'WetVol', 
                                                 'BfVol')) %>%
                           pull(Metric)
                       }),
         cor_mat = map2(hab_data,
                        metrics,
                        .f = function(x, y) {
                          x %>%
                            select(one_of(y)) %>%
                            cor(use = 'pairwise',
                                method = 'spearman')
                        }),
         corr_mat = map(cor_mat,
                        .f = as_cordf),
         catg_cor = map2(hab_data,
                         metrics,
                         .f = function(x, y) {
                           catg_list = try(poss_hab_mets %>%
                                             filter(ShortName %in% y) %>%
                                             split(list(.$MetricCategory)) %>%
                                             map(.f = function(z) {
                                               if(nrow(z) == 1) {
                                                 return(NULL)
                                               } else {
                                                 x %>%
                                                   select(one_of(z$ShortName)) %>%
                                                   cor(use = 'pairwise',
                                                       method = 'spearman')
                                               }
                                               }))
                           return(catg_list)
                                             }))


# pull out highly correlated metrics
corr_df = hab_corr$corr_mat %>%
  map_df(.id = 'dataset',
         .f = function(x) {
           x %>%
             rearrange(absolute = F) %>%
             shave(upper = T) %>% 
             stretch() %>%
             filter(!is.na(r)) %>%
             left_join(poss_hab_mets %>%
                         select(x = ShortName,
                                x_category = MetricCategory)) %>%
             left_join(poss_hab_mets %>%
                         select(y = ShortName,
                                y_category = MetricCategory)) %>%
             distinct() %>%
             # filter(x_category == y_category) %>%
             arrange(x_category, 
                     desc(abs(r)))
         })

corr_df %>%
  filter(abs(r) > 0.7) %>%
  filter(x_category == y_category) %>%
  group_by(dataset) %>%
  arrange(abs(r)) %>%
  slice(1:10) %>%
  as.data.frame()


# print correlation plots for all metrics
for(i in 1:nrow(hab_corr)) {
  pdf(paste0('output/figures/corr_plot_all_', hab_corr$dataset[i], '.pdf'),
      width = 10,
      height = 10)

  corrplot::corrplot.mixed(hab_corr$cor_mat[[i]],
                           upper = 'ellipse',
                           tl.pos = 'lt',
                           tl.cex = 0.5,
                           number.cex = 0.7,
                           order = 'FPC')
  
  dev.off()
}

# print correlation plots by metric category
for(i in 1:nrow(hab_corr)) {
  pdf(paste0('output/figures/corr_plot_catg_', hab_corr$dataset[i], '.pdf'),
      width = 8,
      height = 8)
  hab_corr$catg_cor[[i]][!sapply(hab_corr$catg_cor[[i]], is.null)] %>%
    map(.f = corrplot::corrplot.mixed,
        upper = 'ellipse',
        tl.pos = 'lt',
        order = 'FPC')
  dev.off()
}

