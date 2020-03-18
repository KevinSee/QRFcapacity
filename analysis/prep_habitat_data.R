# Author: Kevin See
# Purpose: Prep habitat data
# Created: 5/14/2019
# Last Modified: 2/25/2020
# Notes: some data downloaded from CHaMP webpage on 9/16/2015
# final champ dataset downloaded on 3/8/2018
# Richie joined the NorWeST temperature data to the master sample points

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(WriteXLS)
library(readxl)
library(QRFcapacity)
library(sf)
library(usethis)

#-----------------------------------------------------------------
# get globally available attributes from all master sample points
#-----------------------------------------------------------------
gaa = read_csv('data/raw/master_sample_pts/IC_Sites_withMetrics_20151016.csv') %>%
  rename(Site = Site_ID, 
         Lon = LON_DD,
         Lat = LAT_DD) %>%
  # fix one issue with Beechie classification (hardly any "braided" channels)
  mutate(ChanlType = recode(ChanlType,
                            'braided' = 'island_braided')) %>%
  # mark some crazy values as NAs
  mutate_at(vars(GDD, MAVELV, Elev_M, TRange, Precip), 
            list(~ if_else(. < 0, as.numeric(NA), .))) %>%
  mutate_at(vars(Slp_NHD_v1), 
            list(~ if_else(. > 2, as.numeric(NA), .))) %>%
  mutate_at(vars(Site, CHaMPsheds),
            list(as.factor)) %>%
  # bring in some temperature data from NorWeST
  left_join(read_csv('data/prepped/mast_samp_norw.csv') %>%
              select(-c(X1:Field1, FID_2, COMID)) %>%
              filter(Distance < 100) %>%
              rename(FLow_Aug = Flow_Ag,
                     S2_02_11 = S2_02_1,
                     S30_2040D = S30_204,
                     S32_2080D = S32_208))

# compare some of the GAAs that come from the GAA file and the NorWeST file
comp_df = gaa %>%
  filter(steel == 'Yes' | chnk == 'Yes') %>%
  select(Site,
         CHaMPsheds,
         ELEV, 
         SLOPE, 
         PRECIP,
         CUMDRAI) %>%
  gather(variable, NorWeST, -Site, -CHaMPsheds) %>%
  left_join(gaa %>%
              filter(steel == 'Yes' | chnk == 'Yes') %>%
              select(Site, 
                     CHaMPsheds,
                     ELEV = Elev_M, 
                     SLOPE = Slp_NHD_v1, 
                     PRECIP = Precip,
                     CUMDRAI = SrtCumDrn) %>%
              mutate(CUMDRAI = CUMDRAI^2) %>%
              gather(variable, MSS, -Site, -CHaMPsheds))

comp_df %>%
  filter(CHaMPsheds == 'Lemhi') %>%
  ggplot(aes(x = MSS,
             y = NorWeST)) +
  geom_point() +
  geom_abline(color = 'red') +
  facet_wrap(~ variable,
             scales = 'free') +
  theme_bw()

# comp_df %>%
#   filter(CHaMPsheds == 'Lemhi') %>%
#   left_join(gaa %>%
#               select(Site, Lon, Lat)) %>%
#   st_as_sf(coords = c('Lon', 'Lat'),
#            crs = 4326) %>%
#   st_transform(crs = 5070) %>%
#   gather(source, value, NorWeST, MSS) %>%
#   filter(variable == 'ELEV') %>%
#   ggplot() +
#   geom_sf(aes(color = value)) +
#   facet_wrap(~ source) +
#   theme(axis.text = element_blank())

#------------------------------------
# determine which sites are in Chinook and steelhead domains
#------------------------------------
data("chnk_domain")
data("sthd_domain")

gaa_sf = gaa %>%
  select(Site, Lon, Lat) %>%
  distinct() %>%
  st_as_sf(coords = c('Lon', 'Lat'),
           crs = 4326) %>%
  st_transform(st_crs(chnk_domain))

# build a buffer around species domain, as polygon
# buffer the species domain by some distance, create a polygon
buff_dist = 500

chnk_buff = chnk_domain %>%
  st_combine() %>%
  st_buffer(dist = buff_dist)

chnk_buff %<>%
  st_buffer(dist = 0)

chnk_pts = gaa_sf %>%
  st_intersection(chnk_buff)

use_data(chnk_pts,
         version = 2,
         overwrite = T)

rm(chnk_buff, chnk_pts)

# sthd_buff = sthd_domain %>%
#   st_union() %>%
#   st_buffer(dist = buff_dist)

sthd_buff = sthd_domain %>%
  st_combine() %>%
  st_buffer(dist = buff_dist)

sthd_buff %<>%
  st_buffer(dist = 0)

sthd_pts = gaa_sf %>%
  st_intersection(sthd_buff)

use_data(sthd_pts,
         version = 2,
         overwrite = T)

rm(sthd_buff, sthd_pts)


# add to gaa
gaa %<>%
  mutate(chnk_range = if_else(Site %in% chnk_pts$Site, T, F),
         sthd_range = if_else(Site %in% sthd_pts$Site, T, F))

# make available like a package, by calling "data()"
use_data(gaa,
         version = 2,
         overwrite = T)

#------------------------------------
# pull out lats/longs for use below
#------------------------------------
data("gaa")

gaa_locs = gaa %>%
  select(Site, Lon, Lat)

gaa_meta = readxl::read_excel('data/raw/master_sample_pts/GAA_Metadata_20150506.xlsx') %>%
  select(ShortName = GlossaryTermName,
         Name = DisplayName,
         DescriptiveText = Description,
         FieldName,
         UnitOfMeasureAbbrv = Units) %>%
  mutate(ShortName = str_remove(ShortName, '_TB$'))

#-----------------------------------------------------------------
# read in HUC 6 polygons and attach NatPrin1, NatPrin2 & DistPrin1 to each
#-----------------------------------------------------------------
huc12 = st_read('data/raw/watershed_boundaries/WBDHU12.shp') %>%
  st_transform(st_crs(chnk_domain))

huc_pca = read_excel('data/raw/habitat/Final_HUC6_classes_with_PCA_20110704.xlsx',
                     sheet = 'Eastside') %>%
  rename(HUC12 = subwat) %>%
  mutate_at(vars(HUC12),
            list(~ as.factor(as.character(.)))) %>%
  select(-west_nat_feat_class,
         -mountains_nat_feat_class,
         # -LAT, -LON,
         -SUBWAT)


huc12 %>%
  inner_join(huc_pca) %>%
  mutate(huc_pre = str_sub(HUC12, 1, 4)) %>%
  janitor::tabyl(huc_pre)

huc12 %>%
  anti_join(huc_pca) %>%
  mutate(huc_pre = str_sub(HUC12, 1, 4)) %>%
  # janitor::tabyl(huc_pre)
  select(huc_pre) %>%
  plot()

huc_pca %>%
  rowwise() %>%
  mutate(inHUC_bdry = if_else(HUC12 %in% huc12$HUC12, T, F)) %>%
  ungroup() %>%
  st_as_sf(coords = c('LON', 'LAT'),
           crs = 4326) %>%
  ggplot() +
  geom_sf(aes(color = inHUC_bdry))

#-----------------------------------------------------------------
# get CHaMP data from 2011 - 2014
#-----------------------------------------------------------------
champ_site_2011_14 = read_csv('data/raw/habitat/CHaMP_ProgramMetrics_20150916/MetricAndCovariates.csv') %>%
  inner_join(read_csv('data/raw/habitat/CHaMP_ProgramMetrics_20150916/MetricVisitInformation.csv')) %>%
  left_join(read_csv('data/raw/habitat/CHaMP_ProgramMetrics_20150916/StreamTempSummer7dAM.csv')) %>%
  mutate_at(vars(SiteName, WatershedName), 
            list(as.factor)) %>%
  arrange(SiteName, VisitYear, SampleDate) %>%
  mutate_at(vars(PoolToTurbulentAreaRatio, BraidChannelRatio), 
            list(as.numeric)) %>%
  # add one more metric related to cnt and freq of channel units
  mutate(CU_Ct = SlowWater_Ct + FstTurb_Ct + FstNT_Ct,
         CU_Freq = CU_Ct / (SiteLength / 100)) %>%
  # add some temperature metrics generated by Kris McNyset's temperature modeling
  left_join(read_csv('data/raw/temperature/kris_temp_pts_mn_mxmn.csv') %>%
              select(SiteName,
                     WatershedName = WatershedN,
                     VisitYear = year,
                     MeanTemp = Mean,
                     MaxMeanTemp = MaxMean))

# get any missing lat/longs from GAA data
champ_site_2011_14 %<>%
  left_join(gaa_locs %>%
              rename(SiteName = Site)) %>%
  mutate(LON_DD = if_else(is.na(LON_DD), Lon, LON_DD),
         LAT_DD = if_else(is.na(LAT_DD), Lat, LAT_DD)) %>%
  select(-Lon, -Lat)


# average metrics across years for sites visited multiple times, keep metrics that don't change
champ_site_2011_14_avg = champ_site_2011_14 %>%
  filter(`Primary Visit` == 'Yes') %>%
  select(-matches('FileName'),
         -matches('GCD')) %>%
  group_by(SiteName) %>%
  summarise_at(vars(SiteLength:WidthCategory,
                    Grad:MaxMeanTemp),
               list(median),
               na.rm = T) %>%
  left_join(champ_site_2011_14 %>%
              filter(`Primary Visit` == 'Yes') %>%
              select(ProgramSiteID:WatershedName,
                     x_albers:Elev_M,
                     OwnerType:MeanU) %>%
              gather(metric, value, -SiteName) %>%
              filter(!is.na(value)) %>%
              distinct() %>%
              spread(metric, value, fill = NA) %>%
              distinct() %>%
              mutate_at(vars(CUMDRAINAG:DistPrin1,
                             Elev_M:LAT_DD,
                             LON_DD:MeanU,
                             NatPrin1:NatPrin2,
                             Ppt:Strah,
                             WatershedID,
                             x_albers:y_albers,
                             CEC_L1, CEC_L2),
                        list(as.numeric))) %>%
  select(one_of(names(champ_site_2011_14)))

# read in dictionary of habitat metrics
hab_dict_2014 = read_csv(paste0('data/raw/habitat/CHaMP_ProgramMetrics_20150916/Definitions.csv'))

# put metrics in categories
hab_catg = filter(hab_dict_2014,
                  MetricGroupName %in% c('Visit Metric', 'Stream Temp Summer 7dAM'),
                  !grepl('^GCD', ShortName),
                  !grepl('GeoDatabase$', ShortName),
                  !grepl('Img$', ShortName),
                  !grepl('^HydraModel', ShortName),
                  !grepl('RBT Outputs', ShortName),
                  !grepl('ResultsXML', ShortName),
                  !grepl('LogFile', ShortName)) %>%
  select(ShortName, Name, DescriptiveText, UnitOfMeasure, UnitOfMeasureAbbrv) %>%
  mutate(MetricCategory = ifelse(grepl('^Sub', ShortName), 'Substrate',
                                 ifelse(grepl('^LW', ShortName), 'Wood',
                                        ifelse((grepl('^FishCov', ShortName) |
                                                  grepl('Ucut', ShortName)), 'Cover',
                                               ifelse(grepl('^RipCov', ShortName), 'Riparian',
                                                      ifelse((grepl('SlowWater', ShortName) |
                                                                grepl('FstTurb', ShortName) |
                                                                grepl('FstNT', ShortName) |
                                                                grepl('PoolToTurbulentAreaRatio', ShortName)), 'ChannelUnit',
                                                             ifelse((grepl('Island', ShortName) |
                                                                       grepl('Sin', ShortName) |
                                                                       grepl('_CV$', ShortName) |
                                                                       grepl('DpthWet_SD', ShortName) |
                                                                       grepl('DetrendElev_SD', ShortName) |
                                                                       grepl('Braid', ShortName) |
                                                                       grepl('Side Channel', Name) |
                                                                       grepl('Lgth_ThlwgCLRat', ShortName) |
                                                                       grepl('DRat', ShortName)), 'Complexity',
                                                                    ifelse((grepl('temperature', DescriptiveText) |
                                                                              grepl('SolarSummr_Avg', ShortName) |
                                                                              grepl('7dAM', ShortName)), 'Temperature',
                                                                           ifelse((grepl('Cond', ShortName) |
                                                                                     grepl('Alk', ShortName) |
                                                                                     grepl('DriftBioMass', ShortName)), 'WaterQuality', 'Size'))))))))) %>%
  mutate(ShortName = as.factor(ShortName),
         MetricCategory = as.factor(MetricCategory))

hab_dict_2014 %<>%
  left_join(hab_catg) %>%
  # add a couple other metrics
  bind_rows(tibble(ShortName = c('DistPrin1', 'NatPrin1', 'NatPrin2', 'mean_JulAug_temp', 'CUMDRAINAG', 'BraidChannelRatio', 'PoolToTurbulentAreaRatio'),
                   Name = c('Disturbance Index', 'Natural PC 1', 'Natural PC 2', 'Mean Summer Temperature', 'Cummulative Drainage Area', 'Braid to Channel Ratio', 'Pool To Turbulent Area Ratio'),
                   DescriptiveText = c('Disturbance index that includes measures of % urban, % agricultural, % impervious surface and road density (Whittier et al. 2011).',
                                       'A natural index that describes watershed slope, precipitation, growing season (growing degree day), and low-gradient streams (Whittier et al. 2011).',
                                       NA,
                                       'Average of all hourly temperature measurements collected July 15 - August 31.',
                                       'The cumulative land area that drains a given location/site.',
                                       NA, NA),
                   UnitOfMeasure = NA,
                   UnitOfMeasureAbbrv = NA,
                   MetricGroupName = c(rep('GAA', 3), 'Visit Metric', 'GAA', rep('Visit Metric', 2)),
                   MetricCategory = c(rep('Land Classification', 3), 'Temperature', 'Size', 'Complexity', 'ChannelUnit'))) %>%
  bind_rows(tibble(ShortName = c('VisitYear', 'ValleyClass', 'ChannelType', 'Ppt', 'MeanU', 'SiteLength', 'AverageBFWidth'),
                   Name = c('Year', 'Valley Class', 'Beechie Channel Type', 'Precipitation', 'Mean Annual Discharge', 'Site Length', 'Average Bankfull Width'),
                   DescriptiveText = NA,
                   UnitOfMeasure = NA,
                   UnitOfMeasureAbbrv = NA,
                   MetricGroupName = c('Visit Metric', rep('GAA', 6)),
                   MetricCategory = c(rep('Categorical', 3), rep('Size', 4)))) %>%
  bind_rows(tibble(ShortName = c('CU_Ct', 'CU_Freq', 'MeanTemp', 'MaxMeanTemp'),
                   Name = c('Channel Unit Count', 'Channel Unit Frequency', 'Mean Summer Temperature', 'Max Mean Weekly Summer Temperature'),
                   UnitOfMeasure = c('Count', 'count per 100 meter', 'Degree (Celsius)', 'Degree (Celsius)'),
                   DescriptiveText = c('Number of channel units.',
                                       'Number of channel units per 100 meters.',
                                       rep(NA, 2)),
                   MetricGroupName = 'Visit Metric',
                   MetricCategory = rep(c('ChannelUnit', 'Temperature'), each = 2))) %>%
  # filter(!is.na(MetricCategory))
  select(ShortName, Name, MetricEngineName, MetricGroupName, DescriptiveText, UnitOfMeasure, UnitOfMeasureAbbrv, MetricCategory) %>%
  distinct()

hab_dict_2014 = hab_dict_2014 %>%
  filter(!ShortName %in% gaa_meta$ShortName) %>%
  bind_rows(hab_dict_2014 %>%
              filter(is.na(MetricEngineName)) %>%
              select(ShortName, MetricCategory) %>%
              inner_join(gaa_meta))

#-----------------------------------------------------------------
# save prepped data
#-----------------------------------------------------------------
list('CHaMP' = champ_site_2011_14,
     'Avg CHaMP' = champ_site_2011_14_avg,
     'Metric Dictionary' = hab_dict_2014) %>%
  WriteXLS('data/Prepped/CHaMP_2011_2014_Data.xlsx',
           AdjWidth = T,
           FreezeRow = 1,
           BoldHeaderRow = T)

# make available like a package, by calling "data()"
use_data(champ_site_2011_14, champ_site_2011_14_avg, hab_dict_2014,
         version = 2,
         overwrite = T)


#-----------------------------------------------------------------
# make use of final CHaMP data - downloaded 3/8/2018
#-----------------------------------------------------------------
load('data/raw/habitat/CHaMP_FinalMetrics_20180308/CHaMPdata.rda')

champ_site_2011_17 = siteData %<>%
  # add one more metric related to cnt and freq of channel units
  mutate(CU_Ct = SlowWater_Ct + FstTurb_Ct + FstNT_Ct,
         CU_Freq = CU_Ct / (Lgth_Wet / 100)) %>%
  # add Sin_CL
  mutate(Sin_CL = Sin) %>%
  # get any missing lat/longs from GAA data
  left_join(gaa_locs) %>%
  mutate(LON_DD = if_else(is.na(LON_DD), Lon, LON_DD),
         LAT_DD = if_else(is.na(LAT_DD), Lat, LAT_DD)) %>%
  select(-Lon, -Lat)

# get channel unit metrics
champ_cu = chunitDf

# the channel unit numbers do not necessarily line up with the same channel unit each year. Every survey, the CHaMP crew re-delineated the channel units, so DO NOT assume that channel unit #4 in 2015 is the same channel unit #4 in 2016. So no averaging of channel unit metrics.

# calculate average habitat data across all years
champ_site_2011_17_avg = champ_site_2011_17 %>%
  filter(VisitObjective == 'Primary Visit',
         VisitStatus == 'Released to Public') %>%
  select(-(`Metric #`:GenerationDate),
         -RS_name,
         -Geo_Cond) %>%
  group_by(Watershed, Site) %>%
  summarise_at(vars(LAT_DD,
                    LON_DD,
                    SlowWater_Area:CU_Freq,
                    Sin_CL),
               list(median),
               na.rm = T) %>%
  ungroup() %>%
  left_join(champ_site_2011_17 %>%
              filter(VisitObjective == 'Primary Visit',
                     VisitStatus == 'Released to Public') %>%
              select(Watershed, Site,
                     # Category:SideChannel,
                     x_albers:y_albers,
                     Stream:MeanU,
                     Geo_Cond) %>%
              distinct() %>%
              arrange(Watershed, Site) %>%
              gather(metric, value, -Watershed, -Site) %>%
              filter(!is.na(value)) %>%
              distinct() %>%
              spread(metric, value, fill = NA) %>%
              distinct() %>%
              mutate_at(vars(CEC_L1, CEC_L2,
                             CUMDRAINAG, DistPrin1,
                             Elev_M, 
                             HUC4:LEVEL3_NM,
                             MeanU,
                             NatPrin1:NatPrin2,
                             Ppt:Strah,
                             x_albers:y_albers),
                        list(as.numeric))) %>%
  mutate(VisitObjective = 'Primary Visit',
         VisitStatus = 'Released to Public',
         VisitYear = 'All') %>%
  select(one_of(names(champ_site_2011_17))) %>%
  mutate_at(vars(Site, Watershed),
            list(fct_explicit_na))

# updated habitat dictionary
hab_dict_2017 = read_csv('data/raw/habitat/CHaMP_FinalMetrics_20180308/Definitions.csv') %>%
  select(ShortName, Name, MetricEngineName, MetricGroupName, DescriptiveText, UnitOfMeasure, UnitOfMeasureAbbrv) %>%
  distinct() %>%
  left_join(hab_catg %>%
              select(ShortName, MetricCategory)) %>%
  bind_rows(hab_dict_2014 %>%
              filter(is.na(MetricEngineName))) %>%
  # add some temperature metrics from NorWeST
  bind_rows(tibble(ShortName = c('aug_temp',
                                 'avg_aug_temp'),
                   Name = c('August Temperature',
                            'Avg. August Temperature'),
                   MetricEngineName = 'NorWeST',
                   MetricGroupName = 'Visit Metric',
                   DescriptiveText = c('Average predicted daily August temperature from NorWest for a particular year.',
                                       'Average predicted daily August temperature from NorWest, averaged across the years 2002-2011.'),
                   UnitOfMeasure = 'Degree (Celsius)',
                   UnitOfMeasureAbbrv = "°C",
                   MetricCategory = 'Temperature'))

use_data(champ_site_2011_17, champ_site_2011_17_avg, champ_cu, hab_dict_2017,
         overwrite = T,
         version = 2)

#-----------------------------------------------------------------
# DASH metrics at CHaMP sites
# covers 2014-2017 (after CHaMP protocol was stabalized)
# Richie re-calculated these from CHaMP measurements, following the DASH methods
#-----------------------------------------------------------------
data("champ_site_2011_17")

champ_dash = read_csv('data/raw/habitat/CHaMP_DASH/ChaMP_Dash_Met.csv') %>%
  select(-starts_with('X')) %>%
  rename(Ucut_Area = UcutArea,
         Sin_CL = Sin) %>%
  # add some info from CHaMP
  left_join(champ_site_2011_17 %>%
              select(VisitID, 
                     Panel,
                     Protocol,
                     Crew,
                     Stream,
                     VisitObjective,
                     VisitStatus,
                     Elev_M,
                     OwnerType:HUC6,
                     Ppt,
                     NatClass,
                     DistClass,
                     FstNT_Area,
                     Geo_Cond)) %>%
  select(one_of(names(champ_site_2011_17)),
         everything()) %>%
  select(Watershed, Site, SampleDate, 
         VisitID, Panel, VisitYear,
         everything())
  
# filter out some sites with no DASH metrics attached to them
# data.frame with how many DASH metrics were computed
n_dash_mets = champ_dash %>%
  select(Site, Watershed, VisitID, VisitYear) %>%
  bind_cols(champ_dash %>%
              select(SlowWater_Area:LWcnt_Wet,
                     -Geo_Cond) %>%
              transmute(n_NA = rowSums(is.na(.)))) %>%
  arrange(desc(n_NA))

xtabs(~ Watershed + VisitYear + (n_NA > 50), 
      n_dash_mets) %>%
  addmargins()

# champ_dash %<>%
#   anti_join(n_dash_mets %>%
#               filter(n_dash_mets < 50))

names(champ_site_2011_17)[!names(champ_site_2011_17) %in% names(champ_dash)] %>%
  sort()

# average metrics across years for each site
champ_dash_avg = champ_dash %>%
  filter(VisitObjective == 'Primary Visit',
         VisitStatus == 'Released to Public') %>%
  select(-Geo_Cond) %>%
  group_by(Watershed, Site) %>%
  summarise_at(vars(LAT_DD,
                    LON_DD,
                    SlowWater_Area:CU_Freq,
                    Sin_CL:LWcnt_Wet),
               list(median),
               na.rm = T) %>%
  ungroup() %>%
  left_join(champ_dash %>%
              filter(VisitObjective == 'Primary Visit',
                     VisitStatus == 'Released to Public') %>%
              select(Watershed, Site,
                     Stream:MeanU,
                     Geo_Cond) %>%
              distinct() %>%
              arrange(Watershed, Site) %>%
              gather(metric, value, -Watershed, -Site) %>%
              filter(!is.na(value)) %>%
              distinct() %>%
              spread(metric, value, fill = NA) %>%
              distinct() %>%
              mutate_at(vars(CUMDRAINAG, DistPrin1,
                             Elev_M, 
                             HUC4:HUC6,
                             MeanU,
                             NatPrin1:NatPrin2,
                             Ppt:Strah),
                        list(as.numeric))) %>%
  mutate(VisitObjective = 'Primary Visit',
         VisitStatus = 'Released to Public',
         VisitYear = 'All') %>%
  select(one_of(names(champ_dash))) %>%
  mutate_at(vars(Site, Watershed),
            list(fct_explicit_na))

n_dash_mets %>%
  anti_join(champ_dash_avg %>%
              select(-VisitYear)) %>%
  arrange(n_NA)


use_data(champ_dash, champ_dash_avg,
         overwrite = T,
         version = 2)

#-----------------------------------------------------------------
# associate each CHaMP site with a 200 - meter reach
#-----------------------------------------------------------------
data("champ_site_2011_17")
data("rch_200")


champ_sf = champ_site_2011_17 %>%
  filter(! Watershed %in% c('Big-Navarro-Garcia (CA)',
                            # 'Region 17',
                            'CHaMP Training')) %>%
  select(Site, Watershed, StreamName, LON_DD, LAT_DD) %>%
  mutate_at(vars(Watershed, StreamName),
            list(str_trim)) %>%
  mutate_at(vars(StreamName),
            list(fct_explicit_na)) %>%
  distinct() %>%
  group_by(Site, Watershed, StreamName) %>%
  summarise_at(vars(LON_DD, LAT_DD),
               list(mean),
               na.rm = T) %>%
  ungroup() %>%
  filter(!is.na(LON_DD)) %>%
  arrange(Site, Watershed, StreamName) %>%
  mutate_at(vars(Watershed),
            list(fct_drop)) %>%
  st_as_sf(coords = c("LON_DD", "LAT_DD"),
           crs = 4326) %>%
  st_transform(st_crs(rch_200))

# save to be processed by QGIS
st_write(champ_sf,
         dsn = 'data/prepped/200m_reaches/CHaMP_Site_locs.gpkg')

# used the NNjoin plugin with QGIS to join each point to it's closest 200 m reach
champ_rch = st_read('data/prepped/200m_reaches/CHaMP_site_reach.gpkg') %>%
  rename(UniqueID = join_UniqueID,
         GNIS_Name = join_GNIS_Name) %>%
  rename(geometry = geom)

identical(n_distinct(champ_sf$Site),
          n_distinct(champ_rch$Site))

# which sites seemed to get snapped a bit far?
champ_rch %>%
  filter(distance > 200) %>%
  xtabs(~ Watershed, .,
        drop.unused.levels = T)

# which sites may have been snapped to the wrong place?
# Stream names don't match, or they do but distance seems far
# maximum acceptable distance to have point moved
max_dist = 100
# max_dist = 50
snap_issues = champ_rch %>%
  mutate_at(vars(StreamName, GNIS_Name),
            list(as.character)) %>%
  filter((StreamName != GNIS_Name & StreamName != '(Missing)') |
           (StreamName == GNIS_Name & distance > max_dist) |
           (StreamName == "(Missing)" & distance > max_dist) ) %>%
  arrange(desc(distance)) %>%
  mutate(issue = if_else(StreamName != GNIS_Name & StreamName != '(Missing)',
                         "Mismatch StreamName",
                         if_else(StreamName == GNIS_Name & distance > max_dist,
                                 "Large Snap Distance",
                                 if_else(StreamName == "(Missing)" & distance > max_dist,
                                         "Missing StreamName",
                                         as.character(NA)))))

# save to send to Richie
snap_issues %>%
  st_write('data/prepped/200m_reaches/SnapIssues.gpkg')
# # save as shapefile
# snap_issues %>%
#   st_write('data/prepped/200m_reaches/SnapIssues.shp')

# Richie sent this file back, with some points fixed (new UniqueID, called UniqueID_2)
snap_issues_fix = st_read('data/prepped/200m_reaches/SnapIssues_Fixed/SnapIssues_Fixed_RC.shp') %>%
  mutate(Use = if_else(is.na(Use), 0, Use)) %>%
  mutate_at(vars(Use),
            list(as.factor))

xtabs(~ Use + (UniqueID_2 == 0), snap_issues_fix)

snap_issues_fix %>%
  filter(Use == 3)




# 
champ_rch_2 = champ_rch %>%
  anti_join(snap_issues %>%
              st_drop_geometry(),
            by = "Site") %>%
  rbind(rch_200 %>%
          select(one_of(names(champ_rch)), geometry) %>%
          inner_join(snap_issues_fix %>%
                       st_drop_geometry() %>%
                       filter(Use %in% c(1, 3)) %>%
                       mutate(UniqueID = if_else(Use == 1,
                                                 UniqueID_2,
                                                 UniqueID)) %>%
                       select(Site, Watershed, StreamName, 
                              UniqueID)) %>%
          mutate(distance = NA) %>%
          select(one_of(names(champ_rch))))

# which sites got dropped? Should they all have been dropped?
champ_rch %>%
  anti_join(champ_rch_2 %>%
              st_drop_geometry(),
            by = "Site") %>%
  left_join(snap_issues_fix %>%
              st_drop_geometry() %>%
              select(Site, UniqueID_2, Notes, Action, Use)) %>%
  xtabs(~ Use, .)

# create a dataframe for use in the package
champ_site_rch = champ_rch_2 %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(Site, Watershed, UniqueID)

# save to use as data object in package
use_data(champ_site_rch,
         overwrite = T,
         version = 2)

#-----------------------------------------------------------------
# prep CHaMP frame
#-----------------------------------------------------------------
# read in entire CHaMP frame (Secesh was modified by Jean Olson to correct extent of spring Chinook)
champ_frame = st_read('data/raw/habitat/CHaMP_Frames_All_20151019_CHNKSu_Secesh/CHaMP_Frames_All_20151019_CHNKSu_Secesh.shp') %>%
  st_zm()

# save as CSV file for later use, and to make it faster to run (reading in the shapefile takes a LOOOONG time)
champ_frame %>%
  as_tibble() %>%
  write_csv('data/prepped/champ_frame_data.csv')
