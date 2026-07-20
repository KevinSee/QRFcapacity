# Author: Kevin See
# Purpose: Create figures for carrying capacity review paper; Morgan Bond is lead author
# Created: 7/16/2026
# Last Modified: 7/17/2026
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
theme_set(theme_pubr(base_family = "Arial",
                     base_size = 12))


theme_set(theme_pubr(base_family = "sans",
                     base_size = 12))

#-----------------------------------------------------------------
# one figure showing quantile regression
#-----------------------------------------------------------------
# load data
load(here("data", 
          "fh_win_champ_2017.rda"))

# create plot showing quantile regression
fh_win_champ_2017 |> 
  filter(Tier1_fish == "Pool") |>
  filter(fish_dens < 3) |>
  # filter(fish_dens > 0) |>
  # filter(Year == "2017-2018") |> 
  ggplot(aes(x = FishCovLW,
             y = fish_dens)) +
  geom_point() +
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(color = factor(after_stat(quantile))),
                linewidth = 1) +
  geom_smooth(method = lm,
              color = "gray70",
              se = F,
              linetype = 2) +
  scale_color_brewer(palette = "Set1",
                     name = "Quantile") +
  # facet_wrap(~ Tier1_fish) +
  # scale_y_continuous(limits = c(NA, 3)) +
  coord_cartesian(ylim = c(NA, 3)) +
  labs(x = "Fish Cover - Large Wood",
       y = expression("Chinook Pre-smolt Winter Density (fish / " * m^2 * ")")) +
  theme(legend.position = 'bottom')
  # theme(legend.position = "bottom",
  #       strip.text = element_text(size = 9),
  #       axis.text = element_text(size = 8))

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig4.tiff"),
       width = 150,
       height = 160,
       units = "mm")

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig4.png"),
       width = 150,
       height = 160,
       units = "mm")



# code to try a few different covariates
library(quantreg)

mod_df <-
  fh_win_champ_2017 |> 
  filter(Tier1_fish == "Pool")

taus <- c(seq(0.05, 0.95, by = 0.05), 0.99)

names(mod_df)
my_covar = "FishCovLW"


qmod <-
  rq(formula(paste("fish_dens ~", my_covar)),
     tau = taus,
     data = mod_df)

summ_list <-
  summary(qmod)

for(i in seq_along(taus)) {
  
  res <- tibble(tau = summ_list[[i]]$tau,
                coef_tab = list(summ_list[[i]]$coefficients)) |> 
    mutate(across(coef_tab,
                  ~ map(.,
                        .f = function(x) {
                          as_tibble(x, rownames = "coef")
                        })))
  
  if(i == 1) {
    res_df <- res
  } else {
    res_df <-
      bind_rows(
        res_df,
        res)
  }
  rm(res)
}

p1 <-
  res_df |> 
  unnest(coef_tab) |> 
  clean_names() |> 
  mutate(across(lower_bd,
                ~ case_when(. < -2e-2 ~ NA_real_,
                            .default = .))) |>
  mutate(across(coef,
                ~ case_when(. == "(Intercept)" ~ "B0",
                            .default = "B1")),
         ci_width = upper_bd - lower_bd) |> 
  ggplot(aes(x = tau,
             y = coefficients)) +
  geom_ribbon(aes(ymin = lower_bd,
                  ymax = upper_bd),
              color = NA,
              fill = "gray80") +
  geom_line(aes(y = lower_bd),
            color = "red",
            linetype = 2) +
  geom_line(aes(y = upper_bd),
            color = "red",
            linetype = 2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = 3) +
  facet_wrap(~ coef,
             scales = "free_y") +
  labs(x = "Quantile",
       y = "Coefficient Value",
       title = my_covar)

p2 <-
  res_df |> 
  unnest(coef_tab) |> 
  clean_names() |> 
  mutate(across(lower_bd,
                ~ case_when(. < -2e-2 ~ NA_real_,
                            .default = .))) |>
  filter(!is.na(lower_bd)) |> 
  mutate(across(coef,
                ~ case_when(. == "(Intercept)" ~ "B0",
                            .default = "B1")),
         ci_width = upper_bd - lower_bd,
         select_tau = if_else(between(tau, 0.89, 0.91), T, F)) |> 
  ggplot(aes(x = tau,
             y = ci_width)) +
  geom_line() +
  geom_point(aes(color = select_tau,
                 size = select_tau)) +
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"),
                     guide = "none") +
  scale_size_manual(values = c("TRUE" = 5,
                               "FALSE" = 3),
                     guide = "none") +
  facet_wrap(~ coef,
             scales = "free_y") +
  labs(x = "Quantile",
       y = "CI Width",
       title = my_covar)

ggarrange(p1,
          p2,
          nrow = 2,
          ncol = 1)


mod_df |> 
  rename(mod_covar = any_of(my_covar)) |> 
  ggplot(aes(x = mod_covar,
             y = fish_dens)) +
  geom_point() +
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(color = factor(after_stat(quantile))),
                linewidth = 1) +
  geom_smooth(method = lm,
              color = "gray70",
              se = F,
              linetype = 2) +
  scale_color_brewer(palette = "Set1",
                     name = "Quantile") +
  facet_wrap(~ Tier1_fish) +
  # scale_y_continuous(limits = c(NA, 3)) +
  coord_cartesian(ylim = c(NA, 3)) +
  labs(x = my_covar,
       y = expression("Chinook Pre-smolt Winter Density (fish / " * m^2 * ")")) +
  theme(legend.position = 'bottom')


#-----------------------------------------------------------------
# recreate Figure 2 from Cade and Noon 2003
#-----------------------------------------------------------------
n_pts = 100
sig = 4
b0 = 5
b1 = 100
b2 = 10
b3 = 70
b4 = 40

set.seed(4)
x_fac <- runif(n_pts)
y_ideal <- b0 + b1*x_fac + rnorm(length(x_fac), 0, sig)
while(sum(y_ideal < 0) > 0){
  y_ideal[which(y_ideal < 0)] <- b0 + b1*x_fac[which(y_ideal < 0)] + rnorm(sum(y_ideal < 0), 0, sig)
}


y2_pts <- sample(1:n_pts, size = 10)
y_2 <- y_ideal
y_2[y2_pts] <- b0 + b2*x_fac[y2_pts] + rnorm(length(y2_pts), 0, sig)
while(sum(y_2 < 0) > 0){
  y_2[which(y_2 < 0)] <- b0 + b1*x_fac[which(y_2 < 0)] + rnorm(sum(y_2 < 0), 0, sig)
}

set.seed(9)
y3_pts <- sample(c(1:n_pts)[-y2_pts], size = 10)
y_3 <- y_2
y_3[y3_pts] <- b0 + b3*x_fac[y3_pts] + rnorm(length(y3_pts), 0, sig)
while(sum(y_3 < 0) > 0){
  y_3[which(y_3 < 0)] <- b0 + b3*x_fac[which(y_3 < 0)] + rnorm(sum(y_3 < 0), 0, sig)
}


y4_pts <- sample(c(1:n_pts)[-c(y2_pts, y3_pts)], size = 60)
y_4 <- y_3
y_4[y4_pts] <- b0 + b4*x_fac[y4_pts] + rnorm(length(y4_pts), 0, sig*2)
while(sum(y_4 < 0) > 0){
  y_4[which(y_4 < 0)] <- b0 + b4*x_fac[which(y_4 < 0)] + rnorm(sum(y_4 < 0), 0, sig*2)
}

pt_color = "darkgreen"
ideal_color = "firebrick"

p1 <-
  tibble(x_fac,
       y_ideal) |> 
  ggplot(aes(x = x_fac,
             y = y_ideal)) +
  geom_point(color = pt_color,
             size = 3) +
  geom_smooth(method = lm,
              color = ideal_color,
              se = F,
              lwd = 3) +
  labs(x = "Measured Factor",
       y = "Carrying Capacity")

p2 <-
  tibble(x_fac,
       y_ideal,
       y_2) |> 
  ggplot(aes(x = x_fac)) +
  geom_point(aes(y = y_2),
             color = pt_color,
             size = 3) +
  geom_smooth(aes(y = y_ideal),
              method = lm,
              color = ideal_color,
              se = F,
              lwd = 3) +
  labs(x = "Measured Factor",
       y = "Carrying Capacity")

p3 <-
  tibble(x_fac,
       y_ideal,
       y_3) |> 
  ggplot(aes(x = x_fac)) +
  geom_point(aes(y = y_3),
             color = pt_color,
             size = 3) +
  geom_smooth(aes(y = y_ideal),
              method = lm,
              color = ideal_color,
              se = F,
              lwd = 3) +
  labs(x = "Measured Factor",
       y = "Carrying Capacity")

p4 <-
  tibble(x_fac,
         y_ideal,
         y_4) |> 
  ggplot(aes(x = x_fac)) +
  geom_point(aes(y = y_4),
             color = pt_color,
             size = 3) +
  geom_smooth(aes(y = y_ideal),
              method = lm,
              color = ideal_color,
              se = F,
              lwd = 3) +
  labs(x = "Measured Factor",
       y = "Carrying Capacity")

p5 <-
  p4 + 
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(y = y_4,
                    color = factor(after_stat(quantile))),
                linewidth = 1) +
  # scale_color_brewer(palette = "Set2",
  #                    name = "Quantile") +
  scale_color_grey(name = "Quantile",
                   guide = "none") +
  theme(legend.position = "bottom")

p_left <-
  ggarrange(p1 +
              theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()),
            p2 +
              theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()),
            p3 +
              theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()),
            p4 +
              theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()),
            p5,
            ncol = 1,
            align = "v",
            heights = c(rep(1, 4), 1.3))



# create text and arrows to be included in figure
# Define the text for each row
texts <- c(
  "Ideal: only the measured\nfactor is limiting",
  "1 unmeasured factor is\nlimiting at some sites",
  "2 unmeasured factors are\nlimiting at some sites",
  "Unmeasured factors are\nlimiting at many sites",
  "Quantile regression\nfits to panel above"
)

# Data frame for the text positions
df_text <- data.frame(
  x = 0,                     # Align text to the left of this plot
  y = c(4.7, 3.7, 2.7, 1.7, 0.7), # Centers of the 4 "rows"
  label = texts
)

# Data frame for the arrow positions (between the rows)
df_arrows <- data.frame(
  x = 0.1,                     # Align arrows to the left
  y = c(4.5, 3.5, 2.5, 1.5),      # Start of the arrows
  yend = c(4, 3, 2, 1)    # End of the arrows
)

# Build the right-hand panel
p_right <- ggplot() +
  # Add the text
  geom_text(data = df_text, 
            aes(x = x, 
                y = y, 
                label = label), 
            hjust = 0, 
            vjust = 0.5, 
            size = 4, 
            lineheight = 0.9) +
  # Add the arrows
  geom_segment(data = df_arrows, 
               aes(x = x, 
                   y = y, 
                   xend = x, 
                   yend = yend),
               arrow = arrow(length = unit(0.15, "inches"), 
                             type = "open"),
               linewidth = 2, 
               linejoin = "mitre",
               lineend = "round") +
  # Lock the y-axis to a 0-5 scale to easily align with your 5 stacked plots
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  # Give a little x-axis room for the text to print
  scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(clip = "off") +   # FIX 2: Prevents ANY clipping at the plot edges
  # Remove all background, axes, and gridlines
  theme_void()

ggarrange(p_left,
          p_right,
          ncol = 2,
          align = "h",
          widths = c(2, 1))


ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig4b.tiff"),
       width = 160,
       height = 230,
       units = "mm")

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig4b.png"),
       width = 160,
       height = 230,
       units = "mm")

# shrink to only 3 panels
texts <- c(
  "Ideal: only the measured\nfactor is limiting",
  "Unmeasured factors are\nlimiting at many sites",
  "Quantile regression\nfits to panel above"
)

# Data frame for the text positions
df_text <- data.frame(
  x = 0,                     # Align text to the left of this plot
  y = c(2.7, 1.7, 0.7), # Centers of the 3 "rows"
  label = texts
)

# Data frame for the arrow positions (between the rows)
df_arrows <- data.frame(
  x = 0.1,                     # Align arrows to the left
  y = c(2.5, 1.5),      # Start of the arrows
  yend = c(2, 1)    # End of the arrows
)

# Build the right-hand panel
p_right <- ggplot() +
  # Add the text
  geom_text(data = df_text, 
            aes(x = x, 
                y = y, 
                label = label), 
            hjust = 0, 
            vjust = 0.5, 
            size = 4, 
            lineheight = 0.9) +
  # Add the arrows
  geom_segment(data = df_arrows, 
               aes(x = x, 
                   y = y, 
                   xend = x, 
                   yend = yend),
               arrow = arrow(length = unit(0.15, "inches"), 
                             type = "open"),
               linewidth = 2, 
               linejoin = "mitre",
               lineend = "round") +
  # Lock the y-axis to a 0-3 scale to easily align with the 3 stacked plots
  scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
  # Give a little x-axis room for the text to print
  scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(clip = "off") +   # FIX 2: Prevents ANY clipping at the plot edges
  # Remove all background, axes, and gridlines
  theme_void()

p_right

p_left <-
  ggarrange(p1 +
              theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()),
            p4 +
              theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()),
            p5,
            ncol = 1,
            align = "v",
            heights = c(rep(1, 2), 1.3))

ggarrange(p_left,
          p_right,
          ncol = 2,
          align = "h",
          widths = c(2, 1))

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig4b.tiff"),
       width = 160,
       height = 230,
       units = "mm")

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig4b.png"),
       width = 160,
       height = 230,
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
  mutate(across(Name,
                ~ replace_values(.,
                                 'Wetted Width To Depth Ratio Avg' ~ 'Width:Depth Ratio',
                                 'Wetted Depth SD' ~ 'Depth Complexity',
                                 'Wetted Channel Braidedness' ~ 'Braidedness',
                                 'Fish Cover: Total' ~ 'Fish Cover (%)',
                                 'Riparian Cover: Some Canopy' ~ 'Riparian Canopy (%)',
                                 'Wetted Width Integrated' ~ 'Wetted Width',
                                 'Substrate < 6mm' ~ 'Fines',
                                 # 'Substrate: D16',
                                 'Channel Unit Frequency' ~ 'Channel Unit\nFrequency (Units / 100 m)',
                                 'Avg. August Temperature' ~ 'Avg. August Temp. (C)',
                                 'Large Wood Volume: Wetted Slow Water' ~ 'Large Wood\nFreq. in Pools')))

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
                        pred_quantile = c(0.9),
                        log_transform = T,
                        log_offset = dens_offset,
                        scales = 'free') +
  labs(y = 'Capacity Prediction (Fish per m)') +
  scale_color_grey(start = 0.5, end = 0.5) +
  theme_pubr(base_family = "Arial",
             base_size = 12) +
  theme(legend.position = 'none')

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig5.tiff"),
       width = 150,
       height = 160,
       units = "mm")

ggsave(filename = file.path("O:",
                            "Documents",
                            "ResearchPapers",
                            "2015_HabitatCapacityReview",
                            "Update2025",
                            "Fig5.png"),
       width = 150,
       height = 160,
       units = "mm")


sel_hab_mets

my_covar = sel_hab_mets$Metric[11]


lm(as.formula(paste("log(fish_dens + dens_offset) ~", my_covar)),
   data = qrf_data) |> 
  summary()

library(quantreg)
qmod <-
  rq(as.formula(paste("log(fish_dens + dens_offset) ~", my_covar)),
     tau = c(0.1, 0.25, 0.5, 0.75, 0.9),
     data = qrf_data)

qmod <-
  rq(as.formula(paste("fish_dens ~", my_covar)),
     tau = c(0.1, 0.25, 0.5, 0.75, 0.9),
     data = qrf_data)


summary(qmod, se = "iid")[[3]]$coeff
  

preds <-
  predict(qmod) |> 
  as_tibble() |> 
  bind_cols(qrf_data |> 
              filter(fish_dens > 0) |> 
              select(Site:Year,
                     one_of(my_covar))) |> 
  mutate(across(starts_with("V"),
                ~ exp(.) - dens_offset)) |> 
  pivot_longer(starts_with("V"),
               names_to = "quantile",
               values_to = "pred") |> 
  mutate(across(quantile,
                ~ recode_values(.,
                                "V1" ~ "0.1",
                                "V2" ~ "0.25",
                                "V3" ~ "0.5",
                                "V4" ~ "0.75",
                                "V5" ~ "0.9")))


qrf_data |> 
  # filter(fish_dens > 0) |> 
  rename(test_covar = all_of(my_covar)) |> 
  ggplot(aes(x = test_covar,
             y = fish_dens)) +
  geom_point() +
  # geom_line(data = preds |> 
  #             rename(test_covar = all_of(my_covar)),
  #           aes(y = pred,
  #               color = factor(quantile))) +
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(color = factor(after_stat(quantile))),
                linewidth = 1) +
  # geom_smooth(method = lm,
  #             formula = log(y) ~ x,
  #             color = "black",
  #             se = F,
  #             linetype = 2) +
  scale_color_brewer(palette = "Set1",
                     name = "Quantile") +
  # scale_y_continuous(limits = c(NA, 55)) +
  # scale_y_continuous(trans = "log",
  #                    breaks = scales::breaks_pretty()) +
  labs(x = sel_hab_mets$Name[sel_hab_mets$Metric == my_covar],
       y = "Chinook Parr Summer Density (fish / m)") +
  theme(legend.position = 'bottom')



# create plot showing quantile regression
sel_hab_mets

my_covar = sel_hab_mets$Metric[1]

qrf_data |> 
  rename(test_covar = all_of(my_covar)) |> 
  filter(test_covar < 100) |> 
  filter(fish_dens > 0) |>
  # filter(Year == "2017-2018") |> 
  ggplot(aes(x = test_covar,
             y = log(fish_dens))) +
  geom_point() +
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(color = factor(after_stat(quantile))),
                linewidth = 1) +
  geom_smooth(method = lm,
              color = "black",
              se = F,
              linetype = 2) +
  scale_color_brewer(palette = "Set2",
                     name = "Quantile") +
  scale_y_continuous(trans = "exp",
                     breaks = scales::breaks_pretty()) +
  labs(x = sel_hab_mets$Name[sel_hab_mets$Metric == my_covar],
       y = "Chinook Parr Summer Density (fish / m)") +
  theme(legend.position = 'bottom')
# theme(legend.position = "bottom",
#       strip.text = element_text(size = 9),
#       axis.text = element_text(size = 8))


qrf_data |>
  pivot_longer(any_of(sel_hab_mets$Metric),
               names_to = "covar",
               values_to = "value") |> 
  filter(covar %in% c("DpthWet_SD",
                      "Q",
                      "WetWdth_Int",
                      "WetWDRat_Avg")) |>
  left_join(sel_hab_mets,
            by = join_by(covar == Metric)) |> 
  filter(fish_dens > 0) |>
  ggplot(aes(x = value,
             y = fish_dens)) +
  geom_point() +
  geom_quantile(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                aes(color = factor(after_stat(quantile))),
                linewidth = 1) +
  geom_smooth(method = lm,
              color = "black",
              se = F,
              linetype = 2) +
  scale_color_brewer(palette = "Set1",
                     name = "Quantile") +
  facet_wrap(~ Name,
             scales = "free") +
  labs(x = "Covariate Value",
       y = "Chinook Parr Summer Density (fish / m)") +
  theme(legend.position = 'bottom')
