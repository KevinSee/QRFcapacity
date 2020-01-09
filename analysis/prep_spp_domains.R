# Author: Kevin See
# Purpose: Process species ranges
# Created: 5/14/2019
# Last Modified: 9/17/19
# Notes: 
# From StreamNet: http://www.streamnet.org/data/interactive-maps-and-gis-data/
# Bureau of Rec provided updated extents in the Upper Salmon watersheds
# Ted Sedell at ODFW provided updated extents in the Upper Grande Ronde
# downloaded new ranges from StreamNet on June 19, 2019 (compiled in Jan, 2019)

#-----------------------------------------------------------------
# load needed libraries
library(usethis)
library(tidyverse)
library(sf)

#-----------------------------------------------------------------
# set projection we'd like to use consistently
myCRS = 5070

#-----------------------------------------------------------------
# get species' ranges
#-----------------------------------------------------------------
# read in StreamNet shapefile
StmNt = st_read('data/raw/domain/FishDist_AllSpecies.shp') %>%
# StmNt = st_read('data/raw/domain/StreamNet_FishDist.shp') %>%
  st_transform(crs = myCRS)


# read in population boundaries (polygons)
chnk_pops = st_read('data/raw/domain/CHNK_SPSU_All.shp') %>%
  st_transform(crs = myCRS)
sthd_pops = st_read('data/raw/domain/STHD_SUWI_All.shp') %>%
  st_transform(crs = myCRS) %>%
  filter(grepl('summer', RUN_TIMING))


# read in watershed boundaries
# entire interior columbia river basin (to clip StreamNet domain)
crb = st_read('/Users/kevin/Dropbox/ISEMP/Data/DesignDocs/WatershedBoundaryLines/Shape/WBDHU4.shp') %>%
  st_transform(myCRS) %>%
  filter(!NAME %in% c('Kootenai-Pend Oreille-Spokane',
                      'Puget Sound',
                      'Oregon-Washington Coastal',
                      'Oregon Closed Basins',
                      # 'Upper Snake',
                      # 'Middle Snake', 
                      'Willamette')) %>%
  st_union(by_feature = F) %>%
  st_sf(tibble(NAME = 'Interior CRB'),
        geom = .)

# pull out species domains, clip to crb polygon, and join to population polygons
chnk_domain = StmNt %>%
  filter(Species == 'Chinook salmon' &
           Run %in% c('Spring', 'Summer')) %>%
  st_zm() %>%
  st_intersection(crb) %>%
  mutate(Source = 'StreamNet') %>%
  st_join(chnk_pops %>%
            select(ESU_DPS:NWR_NAME),
          largest = T) %>%
  mutate_at(vars(ESU_DPS:NWR_NAME),
            list(fct_drop)) %>%
  select(Source, StreamName, Species, SciName, UseType, ESU_DPS:NWR_NAME)

sthd_domain = StmNt %>%
  filter(Species == 'Steelhead' &
           Run == 'Summer') %>%
  st_zm() %>%
  mutate(Source = 'StreamNet') %>%
  st_intersection(crb) %>%
  st_join(sthd_pops %>%
            select(ESU_DPS:NWR_NAME),
          largest = T) %>%
  mutate_at(vars(ESU_DPS:NWR_NAME),
            list(fct_drop)) %>%
  select(Source, StreamName, Species, SciName, UseType, ESU_DPS:NWR_NAME)



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
  rename(StreamName = GNIS_Name)

p1 = chnk_domain %>%
  filter(MPG == 'Upper Salmon River',
         StreamName != 'Salmon River') %>%
  mutate(Basin = str_remove(NWR_NAME, 'Chinook Salmon \\(Snake River Spring/Summer-run ESU\\) \\- ')) %>%
  filter(Basin %in% unique(uppSalmChnk$Basin)) %>%
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
chnk_domain %>%
  filter(!(MPG == 'Upper Salmon River' & StreamName != 'Salmon River')) %>%
  rbind(uppSalmChnk %>%
          mutate(NWR_NAME = paste('Chinook Salmon (Snake River Spring/Summer-run ESU) -', Basin)) %>%
          left_join(chnk_domain %>%
                      filter(MPG == 'Upper Salmon River',
                             StreamName != 'Salmon River') %>%
                      as_tibble %>%
                      # select(ESU_DPS:NWR_NAME) %>%
                      select(Species, SciName, ESU_DPS:NWR_NAME) %>%
                      distinct) %>%
          mutate(UseType = 'Spawning, rearing and migration',
                 Source = 'BoR') %>%
          select(one_of(names(chnk_domain)))) -> chnk_domain


# Steelhead
uppSalmSthd = st_read('data/raw/domain/UPsalmon_SteelheadExtent_All.shp') %>%
  st_transform(myCRS) %>%
  st_zm() %>%
  mutate(StreamName = GNIS_Name) %>%
  select(-Basin) %>%
  st_join(sthd_pops %>%
            filter(MPG == 'Salmon River') %>%
            mutate(Basin = str_remove(NWR_NAME, 'Steelhead \\(Snake River Basin DPS\\) \\- ')) %>%
            select(Basin))


p1 = sthd_domain %>%
  filter(MPG == 'Salmon River',
         StreamName != 'Salmon River') %>%
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

# drop areas of Upper Salmon DPS from StreamNet and replace with BoR ranges
sthd_domain %>%
  filter(!NWR_NAME %in% paste('Steelhead (Snake River Basin DPS) -', unique(uppSalmSthd$Basin))) %>%
  rbind(uppSalmSthd %>%
          mutate(NWR_NAME = paste('Steelhead (Snake River Basin DPS) -', Basin)) %>%
          left_join(sthd_domain %>%
                      filter(NWR_NAME %in% paste('Steelhead (Snake River Basin DPS) -', unique(uppSalmSthd$Basin))) %>%
                      as_tibble %>%
                      select(Species, SciName, ESU_DPS:NWR_NAME) %>%
                      distinct) %>%
          mutate(UseType = 'Spawning, rearing and migration',
                 Source = 'BoR') %>%
          select(one_of(names(sthd_domain)))) -> sthd_domain

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
  select(Source, StreamName = FEAT_NAME,
         UseType = critfc_use,
         NWR_NAME) %>%
  left_join(chnk_domain %>%
              as_tibble %>%
              filter(NWR_NAME %in% .$NWR_NAME) %>%
              select(Species, SciName, ESU_DPS:NWR_NAME) %>%
              distinct) %>%
  select(one_of(names(chnk_domain)))

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
