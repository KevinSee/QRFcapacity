# Author: Kevin See
# Purpose: Process species ranges
# Created: 5/14/2019
# Last Modified: 3/31/2020
# Notes: 
# From StreamNet: http://www.streamnet.org/data/interactive-maps-and-gis-data/
# Bureau of Rec provided updated extents in the Upper Salmon watersheds
# Ted Sedell at ODFW provided updated extents in the Upper Grande Ronde
# downloaded new ranges from StreamNet on June 19, 2019 (compiled in Jan, 2019)
# Morgan Bond provided the 200 m reaches, and the updated species extents (I believe from StreamNet)

#-----------------------------------------------------------------
# load needed libraries
library(usethis)
library(tidyverse)
library(sf)
library(magrittr)

#-----------------------------------------------------------------
# set projection we'd like to use consistently
myCRS = 5070

#-----------------------------------------------------------------
# read in stream network
#-----------------------------------------------------------------
# original 200 m reaches from Morgan Bond
rch_200_org = st_read('data/raw/stream_network/crb_streams_v2_master.shp') %>%
  st_transform(crs = myCRS) %>%
  st_zm()

# this includes NorWeST temperature data and a few GAA's joined from the master sample points, all put together by Richie Carmichael
# load each file separately
for(load_file in list.files('data/prepped/200m_reaches/full_join')) {
  load(paste0('data/prepped/200m_reaches/full_join/', load_file))
}
rm(load_file)


rch_200_fj = clearwater_full_join %>%
  st_transform(crs = myCRS) %>%
  select(one_of(names(rch_200_org)),
         S2_02_11 = S2_02_11.x,
         S30_2040D = S30_2040D.x,
         S32_2080D = S32_2080D.x,
         DistPrin1,
         NatPrin1,
         NatPrin2,
         geometry) %>%
  mutate(region = 'Clearwater') %>%
  rbind(midcolumbia_full_join %>%
          st_transform(crs = myCRS) %>%
          select(one_of(names(rch_200_org)),
                 S2_02_11 = S2_02.11,
                 S30_2040D = S30_2040D.x,
                 S32_2080D = S32_2080D.x,
                 DistPrin1,
                 NatPrin1,
                 NatPrin2,
                 geometry) %>%
          mutate(region = 'MidColumbia')) %>%
  rbind(middlesnake_full_join %>%
          st_transform(crs = myCRS) %>%
          select(one_of(names(rch_200_org)),
                 S2_02_11 = S2_02_11.x,
                 S30_2040D = S30_2040D.x,
                 S32_2080D = S32_2080D.x,
                 DistPrin1,
                 NatPrin1,
                 NatPrin2,
                 geometry) %>%
          mutate(region = 'MidSnake')) %>%
  rbind(salmon_full_join %>%
          st_transform(crs = myCRS) %>%
          select(one_of(names(rch_200_org)),
                 S2_02_11 = S2_02_11.x,
                 S30_2040D = S30_2040D.x,
                 S32_2080D = S32_2080D.x,
                 DistPrin1,
                 NatPrin1,
                 NatPrin2,
                 geometry) %>%
          mutate(region = 'Salmon')) %>%
  rbind(UpperColumbiaYakima_full_join %>%
          st_transform(crs = myCRS) %>%
          select(one_of(names(rch_200_org)),
                 S2_02_11 = S2_02_11.x,
                 S30_2040D = S30_2040D.x,
                 S32_2080D = S32_2080D.x,
                 DistPrin1,
                 NatPrin1,
                 NatPrin2,
                 geometry) %>%
          mutate(region = 'UC_Yakima'))

# a few reaches showed up in multiple regions (right on the border). These are the calls from Richie as to which version to keep
rch_200_fj %<>%
  filter(!(UniqueID == 452782 & region == 'UC_Yakima'),
         !(UniqueID %in% c(797627, 1429595, 1724718) & region == 'MidColumbia'),
         !(UniqueID %in% c(347660, 379870) & region == 'Clearwater'))

nrow(rch_200_fj)
sum(duplicated(rch_200_fj$UniqueID))
rch_200_fj %>%
  st_drop_geometry() %>%
  filter(UniqueID %in% UniqueID[duplicated(UniqueID)]) %>%
  arrange(UniqueID, region) %>%
  select(UniqueID, GNIS_Name, S2_02_11:region)

# data('rch_200')
# rch_200 %>%
#   mutate(included = if_else(UniqueID %in% rch_200_fj$UniqueID, T, F)) %>%
#   # xtabs(~ HUC6_name + included, .,
#   #       drop.unused.levels = T)
#   xtabs(~ HUC6_name + included + (chnk | sthd), .,
#         drop.unused.levels = T)

rch_200 = rch_200_org %>%
  left_join(rch_200_fj %>%
              st_drop_geometry() %>%
              select(UniqueID, S2_02_11:region))

sum(duplicated(rch_200$UniqueID))

rm(clearwater_full_join,
   midcolumbia_full_join,
   middlesnake_full_join,
   salmon_full_join,
   UpperColumbiaYakima_full_join,
   rch_200_fj)

#-----------------------------------------------------------------
# Add species' population information
#-----------------------------------------------------------------
### THIS TOOK TOO LONG, ENDED UP RUNNING IN QGIS ###

# # read in population boundaries (polygons)
# chnk_pops = st_read('data/raw/domain/CHNK_SPSU_All.shp') %>%
#   st_transform(crs = myCRS)
# sthd_pops = st_read('data/raw/domain/STHD_SUWI_All.shp') %>%
#   st_transform(crs = myCRS) %>%
#   filter(grepl('summer', RUN_TIMING))

# chnk_mpg = chnk_pops %>%
#   group_by(ESU_DPS, MPG) %>%
#   summarise_at(vars(SHAPE_AREA),
#                list(sum)) %>%
#   ungroup()
# 
# chnk_esu = chnk_pops %>%
#   group_by(ESU_DPS) %>%
#   summarise_at(vars(SHAPE_AREA),
#                list(sum)) %>%
#   ungroup()
# 
# chnk_esu %>%
#   ggplot() + 
#   geom_sf(aes(fill = ESU_DPS)) +
#   theme_bw() +
#   theme(axis.text = element_blank())

# # join populations to reach segments
# rch_chnk_pop = rch_200 %>%
#   select(UniqueID) %>%
#   st_join(chnk_pops) %>%
#   mutate(lngth = st_length(.)) %>%
#   group_by(UniqueID) %>%
#   summarise(chnk_esu = ESU_DPS[which.max(lngth)],
#             chnk_mpg = MPG[which.max(lngth)],
#             chnk_pop_id = NWR_POPID[which.max(lngth)],
#             chnk_pop = NWR_NAME[which.max(lngth)],
#             lngth = max(lngth)) %>%
#   ungroup() %>%
#   as_tibble()
# 
# sum(duplicated(rch_chnk_pop$UniqueID))  
# identical(nrow(rch_chnk_pop), nrow(rch_200))
# identical(n_distinct(rch_chnk_pop$UniqueID), n_distinct(rch_200$UniqueID))
# 
# rch_sthd_pop = rch_200 %>%
#   select(UniqueID) %>%
#   st_join(sthd_pops) %>%
#   mutate(lngth = st_length(.)) %>%
#   group_by(UniqueID) %>%
#   summarise(sthd_dps = ESU_DPS[which.max(lngth)],
#             sthd_mpg = MPG[which.max(lngth)],
#             sthd_pop_id = NWR_POPID[which.max(lngth)],
#             sthd_pop = NWR_NAME[which.max(lngth)],
#             lngth = max(lngth)) %>%
#   ungroup() %>%
#   as_tibble()
# 
# 
# rch_200 %<>%
#   left_join(rch_chnk_pop %>%
#               rename(chnk_length = lngth)) %>%
#   left_join(rch_sthd_pop %>%
#               rename(sthd_length = lngth))

#-----------------------------------------------------------------
# read in version with species' populations
#-----------------------------------------------------------------
# Chinook pops did not keep every UniqueID, but steelhead one did
rch_pops_chnk = st_read('data/raw/stream_network/crb_streams_v2_chnk_pop.gpkg') %>%
  st_transform(crs = myCRS)

rch_pops_sthd = st_read('data/raw/stream_network/crb_streams_v2_sthd_pop.gpkg') %>%
  st_transform(crs = myCRS)

rch_200$UniqueID[!rch_200$UniqueID %in% rch_pops_sthd$UniqueID]

# add species' population information
rch_200 %<>%
  left_join(rch_pops_chnk %>%
              st_drop_geometry() %>%
              as_tibble() %>%
              select(UniqueID, starts_with('chnk'))) %>%
  left_join(rch_pops_sthd %>%
              st_drop_geometry() %>%
              as_tibble() %>%
              select(UniqueID, starts_with('sthd')))

xtabs(~ is.na(chnk_NWR_NAME) + is.na(sthd_NWR_NAME), rch_200)

#-----------------------------------------------------------------
# get species' ranges
#-----------------------------------------------------------------
spr_chnk = st_read('data/raw/domain/crb_v2_spr_chnk.gpkg') %>%
  st_transform(crs = myCRS) %>%
  st_zm()

sum_chnk = st_read('data/raw/domain/crb_v2_sum_chnk.gpkg') %>%
  st_transform(crs = myCRS) %>%
  st_zm()

sum_sthd = st_read('data/raw/domain/crb_v2_sum_sthd.gpkg') %>%
  st_transform(crs = myCRS) %>%
  st_zm()

# compare summer and spring chinook domains
spr_dom = spr_chnk %>%
  filter(chin_sprng == 1) %>%
  select(HUC6_code,
         BranchID,
         ReachID,
         UniqueID) %>%
  as_tibble() %>%
  select(-geom)

sum_dom = sum_chnk %>%
  filter(chin_summr == 1) %>%
  select(HUC6_code,
         BranchID,
         ReachID, 
         UniqueID) %>%
  as_tibble() %>%
  select(-geom)

# anti_join(sum_dom,
#           spr_dom)
# 
# anti_join(spr_dom,
#           sum_dom)
# 
# sum_chnk %>%
#   filter(chin_summr == 1) %>%
#   anti_join(spr_dom) %>%
#   mutate_at(vars(HUC6_name),
#             list(fct_drop)) %>%
#   select(HUC6_name) %>%
#   plot()

# data frame containing all the spring/summer Chinook unique IDs
chnk_dom = spr_chnk %>%
  filter(chin_sprng == 1) %>%
  select(UniqueID,
         Species,
         Run,
         UseType) %>%
  rbind(sum_chnk %>%
          filter(chin_summr == 1) %>%
          anti_join(spr_dom) %>%
          select(UniqueID,
                 Species,
                 Run,
                 UseType)) %>%
  as_tibble() %>%
  mutate_at(vars(Species, Run, UseType),
            list(fct_drop)) %>%
  select(-geom)
  
# add species range to 200 m reaches
rch_200 %<>%
  full_join(chnk_dom %>%
              select(UniqueID,
                     chnk_run = Run,
                     chnk_use = UseType) %>%
              mutate(chnk = T)) %>%
  full_join(sum_sthd %>%
              filter(steel_summ == 1) %>%
              as_tibble() %>%
              select(UniqueID,
                     sthd_run = Run,
                     sthd_use = UseType) %>%
              mutate(sthd = T)) %>%
  mutate_at(vars(chnk, sthd),
            list(~ if_else(is.na(.), F, .)))

xtabs(~ is.na(chnk_NWR_NAME) + chnk, rch_200)

# where are reaches in the Chinook domain, but don't have population information attached (because those pops aren't listed)?
rch_200 %>%
  filter(chnk,
         is.na(chnk_NWR_NAME)) %>%
  ggplot() +
  geom_sf(aes(color = ecoregion)) +
  theme(axis.text = element_blank())

# subset of just anadromous areas
rch_200_and = rch_200 %>%
  filter((chnk | sthd))

# make available like a package, by calling "data()"
use_data(rch_200,
         version = 2,
         overwrite = T)


#----------------------------------------------------------------
# pull out species domains and join to population polygons
chnk_domain = rch_200 %>%
  filter(chnk) %>%
  mutate(Source = 'StreamNet') %>%
  st_join(chnk_pops %>%
            select(ESU_DPS:NWR_NAME),
          largest = T) %>%
  mutate_at(vars(ESU_DPS:NWR_NAME),
            list(fct_drop)) %>%
  select(-ET_ID, -starts_with('sthd')) %>%
  select(Source, everything())

sthd_domain = rch_200 %>%
  filter(sthd) %>%
  mutate(Source = 'StreamNet') %>%
  st_join(sthd_pops %>%
            select(ESU_DPS:NWR_NAME),
          largest = T) %>%
  mutate_at(vars(ESU_DPS:NWR_NAME),
            list(fct_drop)) %>%
  select(-ET_ID, -starts_with('chnk')) %>%
  select(Source, everything())


# #-----------------------------------------------------------------
# # get species' ranges
# #-----------------------------------------------------------------
# # read in StreamNet shapefile
# StmNt = st_read('data/raw/domain/FishDist_AllSpecies.shp') %>%
# # StmNt = st_read('data/raw/domain/StreamNet_FishDist.shp') %>%
#   st_transform(crs = myCRS)
# 
# # read in population boundaries (polygons)
# chnk_pops = st_read('data/raw/domain/CHNK_SPSU_All.shp') %>%
#   st_transform(crs = myCRS)
# sthd_pops = st_read('data/raw/domain/STHD_SUWI_All.shp') %>%
#   st_transform(crs = myCRS) %>%
#   filter(grepl('summer', RUN_TIMING))
# 
# 
# # read in watershed boundaries
# # entire interior columbia river basin (to clip StreamNet domain)
# crb = st_read('/Users/kevin/Dropbox/ISEMP/Data/DesignDocs/WatershedBoundaryLines/Shape/WBDHU4.shp') %>%
#   st_transform(myCRS) %>%
#   filter(!NAME %in% c('Kootenai-Pend Oreille-Spokane',
#                       'Puget Sound',
#                       'Oregon-Washington Coastal',
#                       'Oregon Closed Basins',
#                       # 'Upper Snake',
#                       # 'Middle Snake', 
#                       'Willamette')) %>%
#   st_union(by_feature = F) %>%
#   st_sf(tibble(NAME = 'Interior CRB'),
#         geom = .)
# 
# # pull out species domains, clip to crb polygon, and join to population polygons
# chnk_domain = StmNt %>%
#   filter(Species == 'Chinook salmon' &
#            Run %in% c('Spring', 'Summer')) %>%
#   st_zm() %>%
#   st_intersection(crb) %>%
#   mutate(Source = 'StreamNet') %>%
#   st_join(chnk_pops %>%
#             select(ESU_DPS:NWR_NAME),
#           largest = T) %>%
#   mutate_at(vars(ESU_DPS:NWR_NAME),
#             list(fct_drop)) %>%
#   select(Source, StreamName, Species, SciName, UseType, ESU_DPS:NWR_NAME)
# 
# sthd_domain = StmNt %>%
#   filter(Species == 'Steelhead' &
#            Run == 'Summer') %>%
#   st_zm() %>%
#   mutate(Source = 'StreamNet') %>%
#   st_intersection(crb) %>%
#   st_join(sthd_pops %>%
#             select(ESU_DPS:NWR_NAME),
#           largest = T) %>%
#   mutate_at(vars(ESU_DPS:NWR_NAME),
#             list(fct_drop)) %>%
#   select(Source, StreamName, Species, SciName, UseType, ESU_DPS:NWR_NAME)



#-----------------------------------------------------------------
# upper Salmon extents, based on BoR designations
#-----------------------------------------------------------------
# Chinook
uppSalmChnk = st_read('data/raw/domain/UP_Salmon_ChinookExtents_All.shp') %>%
  st_transform(myCRS) %>%
  st_zm() %>%
  # all NAs for Basin are in North Fork Salmon
  mutate(Basin = fct_explicit_na(Basin, na_level = 'North Fork Salmon')) %>%
  mutate(Basin = recode(Basin,
                        'East Fork Salmon' = 'East Fork Salmon River',
                        'Lemhi' = 'Lemhi River',
                        'North Fork Salmon' = 'North Fork Salmon River',
                        'Pahsimeroi' = 'Pahsimeroi River',
                        'Upper Salmon' = 'Salmon River Upper Mainstem above Redfish Lake')) %>%
  mutate(HUC8_code = str_sub(ReachCode, 1, 8))

xtabs(~ Basin + HUC8_code, uppSalmChnk)

uppSalmChnk_poly = uppSalmChnk %>%
  group_by(Basin) %>%
  summarise(geometry = st_combine(geometry),
            geometry = st_convex_hull(geometry)) %>%
  ungroup() %>%
  st_cast(to = "MULTIPOLYGON") %>%
  st_buffer(dist = 1000)

uppSalmChnk_poly %>%
  ggplot() +
  geom_sf(aes(fill = Basin))


uppSalm_StmNt = rch_200 %>%
  filter(chnk_MPG == 'Upper Salmon River') %>%
  filter(HUC8_code %in% unique(uppSalmChnk$HUC8_code)) %>%
  mutate(Basin = str_remove(chnk_NWR_NAME, 'Chinook Salmon \\(Snake River Spring/Summer-run ESU\\) \\- ')) %>%
  filter(Basin %in% unique(uppSalmChnk$Basin),
         (Basin == 'Salmon River Upper Mainstem above Redfish Lake' | is.na(GNIS_Name) | GNIS_Name != 'Salmon River'),
         !(Basin == 'Lemhi River' & grepl('203$', HUC8_code)),
         !(Basin == 'North Fork Salmon River' & GNIS_Name %in% c('Pine Creek',
                                                                 'Spring Creek',
                                                                 'Squaw Creek',
                                                                 'Indian Creek',
                                                                 'Dump Creek',
                                                                 'Moose Creek',
                                                                 'Boulder Creek')))

uppSalmChnk_poly_v2 = uppSalm_StmNt %>%
  filter(chnk) %>%
  group_by(Basin) %>%
  summarise(geometry = st_combine(geometry),
            geometry = st_convex_hull(geometry)) %>%
  ungroup() %>%
  st_cast(to = "MULTIPOLYGON")



# uppSalmChnk %>%
uppSalm_StmNt %>%
  filter(chnk) %>%
  ggplot() +
  geom_sf(aes(color = Basin)) +
  geom_sf(data = uppSalmChnk_poly_v2,
          aes(fill = Basin),
          alpha = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank())

rch_200 %>%
  filter(chnk_MPG == 'Upper Salmon River') %>%
  filter(!UniqueID %in% uppSalm_StmNt$UniqueID) %>%
  filter(chnk) %>%
  ggplot() +
  geom_sf(color = 'black',
          lwd = 1) +
  # geom_sf(data = uppSalm_StmNt,
  #         aes(color = Basin)) +
  geom_sf(data = uppSalmChnk,
          aes(color = Basin)) +
  theme_bw() +
  theme(axis.text = element_blank())


p1 = uppSalm_StmNt %>%
  filter(chnk) %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'StreamNet')

p2 = uppSalmChnk %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'BoR')

ggpubr::ggarrange(plotlist = list(p1, p2),
                  nrow = 1, 
                  ncol = 2,
                  common.legend = T,
                  legend = 'bottom')

# drop areas of Upper Salmon MPG from StreamNet and replace with BoR ranges
uppSalmChnk_buff = uppSalmChnk %>%
  select(Basin,
         GNIS_Name,
         ReachCode,
         OBJECTID) %>%
  st_buffer(dist = 200,
            endCapStyle = 'SQUARE')

# create table of unique IDs that are within that buffer (pick unique ID with longest length in each buffer polygon)
uppSalmChnk_tab = uppSalm_StmNt %>%
  st_intersection(uppSalmChnk_buff) %>%
  mutate_at(vars(starts_with("GNIS_Name")),
            list(as.character)) %>%
  filter(GNIS_Name == GNIS_Name.1) %>%
  mutate(lngth = st_length(.)) %>%
  group_by(UniqueID) %>%
  summarise(OBJECTID = OBJECTID[which.max(lngth)],
            ReachCode = ReachCode[which.max(lngth)],
            lngth = max(lngth)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  filter(as.numeric(lngth) >= 100)

uppSalmChnk_tab %>%
  select(UniqueID) %>%
  inner_join(rch_200) %>%
  left_join(uppSalm_StmNt %>%
              st_drop_geometry() %>%
              select(UniqueID, Basin)) %>%
  xtabs(~ Basin + chnk, .) %>%
  prop.table(margin = 1) %>% round(2)
  # addmargins()

table(uppSalm_StmNt$UniqueID %in% uppSalmChnk_tab$UniqueID)

rch_200 %>%
  inner_join(uppSalmChnk_tab %>%
  select(UniqueID)) %>%
  left_join(uppSalm_StmNt %>%
              st_drop_geometry() %>%
              select(UniqueID, Basin)) %>%
  ggplot() + 
  geom_sf(aes(color = chnk))

p3 = uppSalm_StmNt %>%
  left_join(uppSalmChnk_tab %>%
              select(UniqueID) %>%
              mutate(chnk_v2 = T)) %>%
  mutate_at(vars(chnk_v2),
            list(~ if_else(is.na(.), F, .))) %>%
  gather(version, chnk, chnk, chnk_v2) %>%
  mutate(version = recode(version,
                          'chnk' = 'Old',
                          'chnk_v2' = 'New')) %>%
  filter(chnk) %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  facet_wrap(~ version)

p12 = ggpubr::ggarrange(plotlist = list(p1, p2),
                  nrow = 1, 
                  ncol = 2,
                  common.legend = T,
                  legend = 'bottom')

ggpubr::ggarrange(plotlist = list(p12, p3),
                  nrow = 2, 
                  ncol = 1,
                  heights = c(1.5, 1),
                  common.legend = T,
                  legend = 'bottom')

rch_200_v2 = rch_200 %>%
  filter(!UniqueID %in% uppSalm_StmNt$UniqueID) %>%
  rbind(uppSalm_StmNt %>%
          select(-chnk) %>%
          left_join(uppSalmChnk_tab %>%
                      select(UniqueID) %>%
                      mutate(chnk = T)) %>%
          select(one_of(names(rch_200))))
  
p4 = rch_200 %>%
  filter(chnk_MPG == 'Upper Salmon River') %>%
  filter(chnk) %>%
  mutate(Basin = str_remove(chnk_NWR_NAME, 'Chinook Salmon \\(Snake River Spring/Summer-run ESU\\) \\- ')) %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'Old')

p5 = rch_200_v2 %>%
  filter(chnk_MPG == 'Upper Salmon River') %>%
  filter(chnk) %>%
  mutate(Basin = str_remove(chnk_NWR_NAME, 'Chinook Salmon \\(Snake River Spring/Summer-run ESU\\) \\- ')) %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'New')

ggpubr::ggarrange(plotlist = list(p4, p5),
                  nrow = 1, 
                  ncol = 2,
                  common.legend = T,
                  legend = 'bottom')



# Steelhead
uppSalmSthd = st_read('data/raw/domain/UPsalmon_SteelheadExtent_All.shp') %>%
  st_transform(myCRS) %>%
  st_zm() %>%
  # mutate(StreamName = GNIS_Name) %>%
  select(-Basin) %>%
  st_join(sthd_pops %>%
            filter(MPG == 'Salmon River') %>%
            mutate(Basin = str_remove(NWR_NAME, 'Steelhead \\(Snake River Basin DPS\\) \\- ')) %>%
            select(Basin))

uppSalm_StmNt_sthd = rch_200 %>%
  filter(sthd_MPG == 'Salmon River') %>%
  filter(HUC8_code %in% unique(uppSalmSthd$HUC8_code)) %>%
  mutate(Basin = str_remove(NWR_NAME, 'Steelhead \\(Snake River Basin DPS\\) \\- ')) %>%
  filter(Basin %in% unique(uppSalmSthd$Basin))


p1 = sthd_domain %>%
  filter(MPG == 'Salmon River') %>% #,
         # StreamName != 'Salmon River') %>%
  mutate(Basin = str_remove(NWR_NAME, 'Steelhead \\(Snake River Basin DPS\\) \\- ')) %>%
  filter(Basin %in% unique(uppSalmSthd$Basin)) %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'StreamNet')

p2 = uppSalmSthd %>%
  ggplot(aes(color = Basin)) +
  geom_sf() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'BoR')

ggpubr::ggarrange(plotlist = list(p1, p2),
                  nrow = 1, 
                  ncol = 2,
                  common.legend = T,
                  legend = 'bottom')


# drop areas of Upper Salmon MPG from StreamNet and replace with BoR ranges
uppSalmSthd_buff = uppSalmSthd %>%
  select(Basin,
         GNIS_Name,
         ReachCode,
         OBJECTID) %>%
  mutate(HUC8_code = str_sub(ReachCode, 1, 8)) %>%
  st_buffer(dist = 200,
            endCapStyle = 'SQUARE')

# create table of unique IDs that are within that buffer (pick unique ID with longest length in each buffer polygon)
uppSalmSthd_tab = rch_200 %>%
  filter(HUC8_code %in% unique(uppSalmSthd_buff$HUC8_code)) %>%
  st_intersection(uppSalmSthd_buff) %>%
  mutate_at(vars(starts_with("GNIS_Name")),
            list(as.character)) %>%
  filter(GNIS_Name == GNIS_Name.1) %>%
  mutate(lngth = st_length(.)) %>%
  group_by(UniqueID) %>%
  summarise(OBJECTID = OBJECTID[which.max(lngth)],
            ReachCode = ReachCode[which.max(lngth)],
            lngth = max(lngth)) %>%
  ungroup() %>%
  as_tibble() %>%
  select(-geometry) %>%
  filter(as.numeric(lngth) >= 100)


rch_200_upsa = rch_200 %>%
  filter(HUC8_code %in% c(unique(uppSalmChnk_buff$HUC8_code),
                          unique(uppSalmSthd_buff$HUC8_code))) %>%
  filter(GNIS_Name != 'Salmon River',
         strm_order <= 6) %>%
  mutate(chnk = if_else(UniqueID %in% uppSalmChnk_tab$UniqueID,
                        T, F),
         sthd = if_else(UniqueID %in% uppSalmSthd_tab$UniqueID,
                        T, F))

rch_200 %<>%
  filter(!UniqueID %in% rch_200_upsa$UniqueID) %>%
  rbind(rch_200_upsa)

# # drop areas of Upper Salmon DPS from StreamNet and replace with BoR ranges
# sthd_domain %>%
#   filter(!NWR_NAME %in% paste('Steelhead (Snake River Basin DPS) -', unique(uppSalmSthd$Basin))) %>%
#   rbind(uppSalmSthd %>%
#           mutate(NWR_NAME = paste('Steelhead (Snake River Basin DPS) -', Basin)) %>%
#           left_join(sthd_domain %>%
#                       filter(NWR_NAME %in% paste('Steelhead (Snake River Basin DPS) -', unique(uppSalmSthd$Basin))) %>%
#                       as_tibble %>%
#                       select(Species, SciName, ESU_DPS:NWR_NAME) %>%
#                       distinct) %>%
#           mutate(UseType = 'Spawning, rearing and migration',
#                  Source = 'BoR') %>%
#           select(one_of(names(sthd_domain)))) -> sthd_domain

#-----------------------------------------------------------------
# Upper Grande Ronde extents, based on ODFW designations
#-----------------------------------------------------------------
# Chinook
ugrChnk = st_read('data/raw/domain/CRITFC_Chinook_Target_Frame_April2012Copy.shp') %>%
  st_transform(myCRS) %>%
  mutate(Basin = recode(Basin,
                        'Upper Grande Ronde River' = 'Grande Ronde River Upper Mainstem'),
         NWR_NAME = paste('Chinook Salmon (Snake River Spring/Summer-run ESU) -', Basin),
         Source = 'ODFW') %>%
  select(Source, GNIS_Name = FEAT_NAME,
         chnk_use = critfc_use,
         NWR_NAME) %>%
  mutate(chnk_use = recode(chnk_use,
                           'current rearing' = 'Rearing and migration',
                           'current spawning' = 'Spawning and rearing'))


# drop areas of Upper Salmon MPG from StreamNet and replace with BoR ranges
ugrChnk_buff = ugrChnk %>%
  st_buffer(dist = 200,
            endCapStyle = 'SQUARE')

# create table of unique IDs that are within that buffer (pick unique ID with longest length in each buffer polygon)
ugrChnk_tab = rch_200 %>%
  filter(HUC8_code == '17060104') %>%
  # select(chnk_use) %>% plot()
  st_intersection(ugrChnk_buff) %>%
  mutate_at(vars(starts_with("GNIS_Name")),
            list(as.character)) %>%
  filter(GNIS_Name == GNIS_Name.1) %>%
  mutate(lngth = st_length(.)) %>%
  group_by(UniqueID) %>%
  summarise(chnk = chnk[which.max(lngth)],
            chnk_use = chnk_use.1[which.max(lngth)],
            ReachID = ReachID[which.max(lngth)],
            lngth = max(lngth)) %>%
  ungroup() %>%
  as_tibble() %>%
  select(-geometry) %>%
  filter(as.numeric(lngth) >= 100)


ugrChnk_tab %>%
  filter(!chnk) %>%
  xtabs(~ chnk_use, .)
  select(UniqueID) %>%
  inner_join(rch_200) %>%
  as.data.frame() %>%
  head()



chnk_domain %>%
  filter(! NWR_NAME %in% unique(ugrChnk$NWR_NAME)) %>%
  rbind(ugrChnk) -> chnk_domain

# steelhead
ugrSthd = st_read('data/raw/domain/StS_Grum_Spawning_Universe.shp') %>%
  st_transform(myCRS) %>%
  rename(StreamName = FEAT_NAME) %>%
  mutate(NWR_NAME = 'Steelhead (Snake River Basin DPS) - Grande Ronde River Upper Mainstem',
         Source = 'ODFW') %>%
  left_join(sthd_domain %>%
              as_tibble %>%
              filter(NWR_NAME %in% .$NWR_NAME) %>%
              select(Species, SciName, UseType, ESU_DPS:NWR_NAME) %>%
              distinct) %>%
  mutate(UseType = 'Spawning, rearing and migration') %>%
  select(one_of(names(sthd_domain)))

sthd_domain %>%
  filter(! NWR_NAME %in% unique(ugrSthd$NWR_NAME)) %>%
  rbind(ugrSthd) -> sthd_domain

#-----------------------------------------------------------------
# John Day extent for Chinook, based on ODFW
#-----------------------------------------------------------------
jd_chnk = st_read("data/raw/domain/ODFW_ChS_Distribution.shp") %>%
  st_transform(myCRS) %>%
  st_intersection(st_read("/Users/seek/OneDrive - Merck Sharp & Dohme, Corp/Data/WatershedBoundaries/WBDHU6.shp") %>%
                    filter(NAME == 'John Day') %>%
                    st_transform(myCRS)) %>%
  mutate(Source = 'ODFW',
         ESU_DPS = 'Middle Columbia River Chinook Salmon ESU',
         MPG = NA,
         NWR_POPID = NA,
         NWR_NAME = "Chinook Salmon (Middle Columbia River Chinook Salmon ESU) - John Day") %>%
  unite(SciName, fhdGenus, fhdSp, sep = " ", remove = F)

chnk_domain %>%
  rbind(jd_chnk %>%
          select(Source = fhdOEnt,
                 StreamName = fhdStNm,
                 Species = fhdSpNm,
                 SciName,
                 UseType = fhdUseTy,
                 ESU_DPS,
                 MPG,
                 NWR_POPID,
                 NWR_NAME)) -> chnk_domain

#-----------------------------------------------------------------
# Save species extents
#-----------------------------------------------------------------
# make available like in a package, by calling "data()"
use_data(chnk_domain, sthd_domain,
         version = 2,
         overwrite = T)

# save as shape files
st_write(chnk_domain,
         dsn = 'data/prepped/chnk_domain.shp',
         driver = 'ESRI Shapefile',
         delete_layer = T)

st_write(sthd_domain,
         dsn = 'data/prepped/sthd_domain.shp',
         driver = 'ESRI Shapefile',
         delete_layer = T)

# save as GPKG
st_write(chnk_domain,
         dsn = 'data/prepped/chnk_domain_GPKG.gpkg',
         driver = "GPKG",
         delete_layer = T)

# save as GPKG
st_write(sthd_domain,
         dsn = 'data/prepped/sthd_domain_GPKG.gpkg',
         driver = "GPKG",
         delete_layer = T)
