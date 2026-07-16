# Author: Kevin See
# Purpose: Create figures for carrying capacity review paper; Morgan Bond is lead author
# Created: 7/16/2026
# Last Modified: 7/16/2026
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(here)
library(ggpubr)

devtools::load_all()

# theme_set(theme_bw(base_family = "Helvetica",
#                    base_size = 14))
# theme_set(theme_pubr(base_family = "Helvetica"))
theme_set(theme_pubr(base_family = "serif",
                     base_size = 14))

#-----------------------------------------------------------------
# one figure showing quantile regression
#-----------------------------------------------------------------
# load data
load(here("data", 
          "fh_win_champ_2017.rda"))

# create plot showing quantile regression
fh_win_champ_2017 |> 
  filter(Tier1_fish == "Pool") |> 
  # filter(fish_dens < 2.5) |>
  filter(fish_dens > 0) |>
  # filter(Year == "2017-2018") |> 
  ggplot(aes(x = FishCovLW,
             y = fish_dens)) +
  geom_point() +
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(color = factor(after_stat(quantile))),
                linewidth = 1) +
  # geom_smooth(method = lm,
  #             color = "darkgreen",
  #             se = F) +
  scale_color_brewer(palette = "Set1",
                     name = "Quantile") +
  facet_wrap(~ Tier1_fish) +
  # scale_y_continuous(limits = c(NA, 3)) +
  coord_cartesian(ylim = c(NA, 3)) +
  labs(x = "Fish Cover - Large Wood",
       y = expression("Chinook Pre-smolt Winter Density (fish / " * m^2 * ")")) +
  theme(legend.position = 'bottom')
  # theme(legend.position = "bottom",
  #       strip.text = element_text(size = 9),
  #       axis.text = element_text(size = 8))

ggsave(filename = "O:/Documents/ResearchPapers/2015_HabitatCapacityReview/Update2025/quant_reg.tiff",
       width = 174,
       height = 174,
       units = "mm")


#-----------------------------------------------------------------
# one figure showing partial dependence plots from a 
# quantile random forest model
#-----------------------------------------------------------------
# pull data and model from published QRF paper
data_path <- 
  file.path("C:",
            "Users",
            "seek1477",
            "Documents",
            "Git",
            "MyProjects",
            "QRFpaper",
            "analysis",
            "data",
            "derived_data")

# load some data
qrf_data = read_rds(file.path(data_path,
                              "qrf_data.rds"))
dens_offset = read_rds(file.path(data_path,
                                 "dens_offset.rds"))
sel_hab_mets = read_rds(file.path(data_path,
                                  "sel_hab_mets.rds"))
qrf_mod = read_rds(file.path(data_path,
                             "qrf_mod.rds"))
hab_dict = read_rds(file.path(data_path,
                              "hab_dict.rds"))

# change some of the names of metrics for plotting purposes
sel_hab_mets <-
  sel_hab_mets %>%
  left_join(hab_dict %>%
              select(Metric = ShortName,
                     Name) %>%
              distinct()) %>%
  mutate(Name = recode(Name,
                       'Wetted Width To Depth Ratio Avg' = 'Width:Depth Ratio',
                       'Wetted Depth SD' = 'Depth Complexity',
                       'Wetted Channel Braidedness' = 'Braidedness',
                       'Fish Cover: Total' = 'Fish Cover',
                       'Riparian Cover: Some Canopy' = 'Riparian Canopy',
                       'Wetted Width Integrated' = 'Wetted Width',
                       'Substrate < 6mm' = 'Fines',
                       # 'Substrate: D16',
                       'Channel Unit Frequency' = 'Channel Unit\nFrequency',
                       'Avg. August Temperature' = 'Avg. August Temp.',
                       'Large Wood Volume: Wetted Slow Water' = 'Large Wood\nFreq. in Pools'))


# which habitat metrics should be plotted?
plot_metrics <-
  c("avg_aug_temp",
    "FishCovTotal",
    "CU_Freq",
    "RipCovCanSome")

# plot partial dependence plots
plot_partial_dependence(rf_mod = qrf_mod,
                        data = qrf_data,
                        plot_covars = plot_metrics,
                        data_dict = hab_dict %>%
                          select(-Name) %>%
                          inner_join(sel_hab_mets %>%
                                       select(ShortName = Metric,
                                              Name)) |>
                          filter(ShortName %in% plot_metrics),
                        type = 'quantile',
                        pred_quantile = 0.9,
                        log_transform = T,
                        log_offset = dens_offset,
                        scales = 'free') +
  labs(y = 'Capacity Prediction (per m)') +
  scale_color_grey(start = 0.5, end = 0.5) +
  theme_pubr(base_family = "serif",
             base_size = 14) +
  theme(legend.position = 'none')

ggsave(filename = "O:/Documents/ResearchPapers/2015_HabitatCapacityReview/Update2025/qrf_pdp.tiff",
       width = 174,
       height = 174,
       units = "mm")


