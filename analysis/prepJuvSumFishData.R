# Author: Kevin See
# Purpose: Prep summer juvenile fish data
# Created: 5/14/2019
# Last Modified: 6/13/19
# Notes: need info for site name, fish crew, sample data, site length, watershed AND
# number marks, captures, recaptures OR
# 1st, 2nd, 3rd pass depletion OR
# snorkel counts / abundance estimate


#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(readxl)
library(dbplyr)

#-----------------------------------------------------------------
# Entiat
#-----------------------------------------------------------------
# all data from 2006-2016, compiled by Terraqua
ent = read_csv('data/raw/fish/summer/EntiatFishData_2006-2016_20170329.csv') %>%
  rename(Watershed = Subbasin) %>%
  select(Year, SiteName, Watershed, Lat = LAT_DD, Lon = LON_DD,
         Season, SampleDate, FishCrew, Method, Species,
         FishSiteLength = Site.Length.Fish,
         FishWettedArea = Site.Wetted.Area,
         matches('^Pass'), 
         N = N.hat,
         SE = N.hat.SE,
         Nmethod = N_method) %>%
  mutate_at(vars(matches('^Pass')),
            list(as.integer)) %>%
  mutate(Method = recode(Method,
                         'Combo' = 'CU Depletion')) %>%
  # fix the year of the SampleDate for one entry
  mutate(SampleDate = if_else(Year != year(SampleDate),
                              # (Year == 2016 & SiteName == 'ENT00001-2A1'),
                              as.POSIXct(ymd(paste(Year, month(SampleDate), day(SampleDate)))),
                              SampleDate)) %>%
  # # fix one date, based on a different file
  mutate(SampleDate = if_else(Year == 2012 & SiteName == 'CBW05583-489131' & Species == 'Steelhead',
                              as.POSIXct(ymd(20120725)),
                              SampleDate)) %>%
  # add 2017 data
  bind_rows(excel_sheets('data/raw/fish/summer/Entiat2017_SummaryAbundance.xlsx') %>%
              as.list() %>%
              map_df(.f = function(x) {
                if(grepl('^Sheet', x[1])) return(NULL)
                
                read_excel('data/raw/fish/summer/Entiat2017_SummaryAbundance.xlsx',
                           x[1]) %>%
                  rename(SiteName = Site.ID,
                         Watershed = Subbasin,
                         FishSiteLength = Site.Length.Fish) %>%
                  # select(one_of(names(.))) %>%
                  mutate_at(vars(matches('^Pass')),
                            list(as.integer))
              })) %>%
  mutate_at(vars(SampleDate),
            list(floor_date),
            unit = 'days') %>%
  mutate(FishCrew = recode(FishCrew,
                           'TerraQ' = 'Terraqua')) %>%
  select(Year:Nmethod)
  

#-----------------------------------------------------------------
# Asotin
#-----------------------------------------------------------------
aso = read_excel('data/raw/fish/summer/AsotinCHaMP_juvSteelheadMarkRecaps_2011-2018.xlsx') %>%
  rename(FishSiteName = SiteName,
         SiteName = CHaMPSiteID,
         Year = Yr,
         Lat = SiteLatitudeDD,
         Lon = SiteLongitudeDD,
         Pass1.M = CountOfMarks,
         Pass2.C = CountOfCapture,
         Pass3.R = CountOfRecaptures) %>%
  mutate(Method = 'Mark Recapture',
         FishCrew = 'ELR',
         Species = 'Steelhead',
         FishWettedArea = FishSite_WettedWidth * FishSiteLength,
         N = NA,
         SE = NA,
         Nmethod = NA) %>%
  # assign a sample date
  mutate(SampleDate = if_else(Season == 'Summer',
                              ymd(paste0(Year, '0715')),
                              if_else(Season == 'Fall',
                                      ymd(paste0(Year, '1001')),
                                      as.Date(NA)))) %>%
  mutate_at(vars(SampleDate),
            list(as.POSIXct)) %>%
  select(FishSiteName, Stream = StreamName, one_of(names(ent)))

# data for each fish site is duplicated if multiple CHaMP sites within that fish site
aso %>%
  select(-SiteName) %>%
  distinct() %>%
  nrow()
nrow(aso)

asoSites = aso %>%
  select(Year, FishSiteName, SiteName) %>%
  distinct %>%
  arrange(FishSiteName, SiteName, Year)

# look at all CHaMP sites in Asotin, and match them with corresponding Stream / FishSiteName
data(siteData)
siteData %>% 
  # filter(grepl('ASW', SiteName)) %>%
  filter(Watershed == 'Asotin') %>%
  select(Site,
         Year = VisitYear) %>%
  distinct() %>%
  mutate(FishSiteName = str_split(Site, ' ', simplify = T)[,1],
         FishSiteName = str_remove(FishSiteName, 'ASW00001-')) %>%
  mutate(champ = T) %>%
  full_join(asoSites %>%
              rename(Site = SiteName) %>%
              mutate(aso = T)) %>%
  mutate_at(vars(champ, aso),
            list(~ if_else(is.na(.), F, .))) %>%
  # filter(Year < 2015) %>%
  # filter(!champ)
  xtabs(~ champ + aso, .)

# drop rows that have duplicated CHaMP site name, keep first CHaMP site
aso %<>%
  group_by(FishSiteName, Year, Season) %>%
  slice(1) %>%
  distinct() %>%
  ungroup()
  
  
#-----------------------------------------------------------------
# Upper Grande Ronde
#-----------------------------------------------------------------
ugr = read_excel('data/raw/fish/summer/ODFW_Export_Output4Kevin.xlsx') %>%
  rename(Watershed = WatershedName,
         FishSiteLength = SiteLength,
         Lon = Long,
         N = `Abundance (from calibration)`) %>%
  mutate_at(vars(Year, starts_with('Pass')),
            list(as.integer)) %>%
  mutate(Nmethod = 'Prev. Calc.') %>%
  select(one_of(names(ent)))

#-----------------------------------------------------------------
# John Day
#-----------------------------------------------------------------
# use the fish database provided by Nick Weber for fish data from ELR
myConn = DBI::dbConnect(RSQLite::SQLite(),
                        '/Users/kevin/Dropbox/ISEMP/Data/Fish/John Day/JohnDayRovingFish_MASTER.db')
src_dbi(myConn)

dce = tbl(myConn,
          'fld_DataCollectionEvent') %>%
  mutate(FishWettedArea = SiteLength * WettedWidth) %>%
  select(FishSiteName = SiteName,
         Site = GRTSSiteID,
         FishSiteLength = SiteLength,
         FishWettedArea,
         Method = dceType,
         anayMeth = ELRanalysisStatus,
         SampleDate = SurveyDateTime,
         Stream = StreamName,
         dceGroup,
         dceID:dceSubType,
         SampledStatus, SurveyType,
         dceName) %>%
  filter(Method != 'Adult Trap') %>%
  collect() %>%
  mutate_at(vars(Site),
            list(~if_else(. == 'NA', as.character(NA), .))) %>%
  mutate_at(vars(SampleDate),
            list(ymd_hms)) %>%
  mutate_at(vars(Stream),
            list(str_to_title)) %>%
  mutate(Stream = recode(Stream,
                         'East Fork Beach Creek' = 'East Fork Beech Creek',
                         'Brl' = 'Bridge Creek')) %>%
  mutate(Season = str_remove(dceSubType, '^Late'),
         Season = str_remove(Season, '^Mid'),
         Season = str_split(Season, ' ', simplify = T)[,1],
         Year = str_sub(dceSubType, -4),
         Year = as.numeric(Year)) %>%
  mutate(Year = if_else(is.na(Year),
                        year(SampleDate),
                        Year)) %>%
  # fix 2 dceGroups that are duplicates
  mutate(dceGroup = if_else(dceID %in% c(2470, 2471),
                            as.integer(max(dceGroup, na.rm = T) + 1),
                            dceGroup),
         dceGroup = if_else(dceID %in% c(2478, 2479),
                            as.integer(max(dceGroup, na.rm = T) + 1),
                            dceGroup)) %>%
  # fix two sample dates for one site that seem to be mixed up
  mutate(SampleDate = if_else(dceID == 151,
                              ymd_hms('20111016 13:38:00'),
                              SampleDate),
         SampleDate = if_else(dceID == 161,
                              ymd_hms('20111017 13:19:00'),
                              SampleDate)) %>%
  arrange(Year, FishSiteName, dceGroup, SampleDate)

fishPass = tbl(myConn, 'fld_FishCapturePass') %>%
  select(dceID, passKey, CaptureMethod) %>%
  collect() 

fishObs = tbl(myConn, 'fld_FishObservation') %>%
  filter(Species %in% c('Steelhead', 'Chinook')) %>%
  filter(FishLifeStage != 'Adult') %>%
  # filter for certain size class
  filter(FishForkLength >= 35,
         FishLifeStage != 'Fry') %>%
  filter(FishForkLength <= 250) %>%
  # fix one fish count number
  mutate(FishCount = if_else(dceID == 2266 & FishCount == 0,
                             1, FishCount)) %>%
  # fix one PitTagCaptureType (appears to not be a recapture, but a mark)
  mutate(PitTagCaptureType = if_else(dceID == 2340 & PitTagID == '3DD.0077597CA8',
                                     'New Tag',
                                     PitTagCaptureType)) %>%
  collect() %>%
  mutate_at(vars(PitTagCaptureType),
            list(~if_else(. %in% c('', 'NA'), as.character(NA), .))) %>%
  mutate_at(vars(PitTagCaptureType),
            list(str_to_title)) %>%
  mutate(PitTagCaptureType = recode(PitTagCaptureType,
                                    'Efficiency Recapture' = 'Non-Efficiency Recapture'))

# depletion data
elrDepl = dce %>%
  left_join(fishPass) %>%
  left_join(fishObs %>%
              group_by(Species, dceID, passKey) %>%
              summarise(nFish = sum(FishCount))) %>%
  filter(Method == 'Depletion') %>%
  filter(DcePassNumber != -99 | is.na(DcePassNumber)) %>%
  mutate(CaptureMethod = if_else(dceName %in% c('SFM-396146-20120624-1202',
                                                'SFM-292210-20120626-0838'),
                                 'Electro Fishing',
                                 CaptureMethod)) %>%
  arrange(SampleDate, dceID, Species) %>%
  spread(passKey, nFish,
         fill = 0) %>%
  # mutate(id = paste(dceName, Species, sep = '_')) %>%
  # filter(id %in% id[duplicated(id)]) %>% as.data.frame()
  select(-dceGroup, -DcePassNumber, -dceSubType, -SampledStatus, -SurveyType) %>%
  rename(Pass1.M = `1`,
         Pass2.C = `2`,
         Pass3.R = `3`,
         Pass4 = `4`,
         SiteName = Site) %>%
  # any 4th pass 0's become NAs (to be analyzed like 3-pass depletions). I don't trust those 0's on the 4th pass
  mutate_at(vars(Pass4),
            list(~ if_else(. == 0, as.numeric(NA), .))) %>%
  mutate(Year = year(SampleDate),
         Watershed = 'John Day',
         FishCrew = 'ELR') %>%
  select(one_of(names(aso)), everything()) %>%
  arrange(Year, FishSiteName, SampleDate)

# mark-recapture data
elrMR = dce %>%
  filter(Method %in% c('Mark', 'Recapture')) %>%
  left_join(fishObs %>%
              group_by(Species, dceID, PitTagCaptureType) %>%
              summarise(nFish = sum(FishCount))) %>%
  filter(!is.na(dceGroup)) %>%
  filter(DcePassNumber < 3) %>%
  arrange(dceGroup, Species, Method) %>%
  group_by(FishSiteName, dceGroup, Species) %>%
  summarise(Pass1.M = sum(nFish[Method == 'Mark'], na.rm = T),
            Pass2.C = sum(nFish[Method == 'Recapture'], na.rm = T),
            Pass3.R = sum(nFish[Method == 'Recapture' & PitTagCaptureType == 'Non-Efficiency Recapture'], na.rm = T)) %>%
  left_join(dce %>%
              group_by(dceGroup) %>%
              filter(SampleDate == min(SampleDate)) %>%
              slice(1) %>%
              ungroup()) %>%
  mutate(Method = 'Mark Recapture',
         Watershed = 'John Day',
         FishCrew = 'ELR') %>%
  rename(SiteName = Site) %>%
  select(one_of(names(aso)), everything(),
         -SampledStatus, -SurveyType) %>%
  arrange(Year, FishSiteName, SampleDate)

# count data (including Method == 'Mark' with no dceGroup -> no recapture pass)
elrCnt = dce %>%
  filter(Method %in% c('Count') | (Method == 'Mark' & is.na(dceGroup))) %>%
  left_join(fishObs %>%
              group_by(Species, dceID) %>%
              summarise(nFish = sum(FishCount))) %>%
  mutate(Watershed = 'John Day',
         FishCrew = 'ELR',
         Method = 'Single Pass',
         Pass2.C = as.numeric(NA),
         Pass3.R = as.numeric(NA)) %>%
  rename(SiteName = Site,
         Pass1.M = nFish) %>%
  select(one_of(names(aso)), everything()) %>%
  arrange(Year, FishSiteName, SampleDate)

# which DCE's are still missing?
dce %>%
  select(FishSiteName, Year, Season, Method, anayMeth, SampledStatus) %>%
  distinct() %>%
  anti_join(elrDepl %>%
              bind_rows(elrMR) %>%
              bind_rows(elrCnt) %>%
              select(FishSiteName, Year, Season, dceID) %>%
              distinct()) %>%
  # xtabs(~ Method + is.na(SampledStatus), .) %>%
  xtabs(~ Method + SampledStatus, .) %>%
  addmargins()
# all the missing depletions were not sampled

jdELR = elrDepl %>%
  bind_rows(elrMR) %>%
  bind_rows(elrCnt) %>%
  arrange(Year, FishSiteName, SampleDate, Species)

# xtabs(~ Season + Year + Species, jdELR)

# add some GRTS sites to rows with missing values, based on other years where that fish site has a GRTS site associated with it
jdELR %<>%
  left_join(jdELR %>%
              filter(!is.na(SiteName)) %>%
              select(FishSiteName, Site = SiteName, Stream) %>%
              distinct()) %>%
  mutate(SiteName = if_else(is.na(SiteName),
                            Site,
                            SiteName)) %>%
  select(-Site) %>%
  select(one_of(names(aso)), starts_with('Pass'))

# # which fish sites never have a GRTS site associated with them?
# xtabs(~ is.na(SiteName) + Year, jdELR)
# 
# jdELR %>%
#   filter(is.na(SiteName)) %>%
#   select(FishSiteName, Stream) %>%
#   distinct() %>%
#   xtabs(~ Stream, .) %>% 
#   sort(decreasing = T) %>% addmargins()
# 
# n_distinct(jdELR$FishSiteName)

#----------------------
# ODFW
#----------------------
# uses all fish at least 35mm in length
jdODFW = read_csv('data/raw/fish/summer/ODFW_JohnDay_SiteSummary_35mmandUp.csv') %>%
  rename(Lat = US.LatDD, Lon = US.LongDD,
         SampleDate = Date,
         Species = SpeciesSampled,
         FishSiteLength = ReachLength,
         Stream = StreamName,
         Pass1.M = Pass1Cap,
         Pass2.C = Pass2Cap,
         Pass3.R = Recap) %>%
  mutate(SiteName = str_split(DataCollectionEvent, ' ', simplify = T)[,1],
         Year = year(SampleDate),
         Method = if_else(NumPass == 1,
                          'Single Pass',
                          'Mark Recapture')) %>%
  mutate(Watershed = 'John Day',
         FishCrew = 'ODFW') %>%
  mutate(Species = recode(Species,
                          `O'mykiss` = 'Steelhead')) %>%
  mutate_at(vars(Year, starts_with('Pass')),
            list(as.integer)) %>%
  mutate_at(vars(SampleDate),
            list(as.POSIXct)) %>%
  select(DataCollectionEvent, one_of(names(aso)))

# # this re-creates the summary table, based on the raw fish data, but without some details like sampling data and method.
# lngthCutOff = 35
# jdODFW = read_csv('data/raw/fish/summer/ODFW_JohnDay_FishSummary.csv') %>%
#   filter(!is.na(DataCollectionEvent)) %>%
#   filter(Length < 0 | Length >= lngthCutOff) %>%
#   filter(!(Length > 200 & SpeciesCode == '11W')) %>%
#   select(-1) %>%
#   mutate(SiteName = str_split(DataCollectionEvent, ' ', simplify = T)[,1],
#          Year = str_split(DataCollectionEvent, ' ', simplify = T)[,2]) %>%
#   group_by(DataCollectionEvent, SiteName, Year, SpeciesCode, `Pass Number`) %>%
#   summarise_at(vars(Mark, Recap),
#                list(sum),
#                na.rm = T) %>%
#   gather(pass, nFish, Mark, Recap) %>%
#   filter((`Pass Number` == 1 & pass == 'Mark') |
#            (`Pass Number` == 2 & pass == 'Recap')) %>%
#   select(-`Pass Number`) %>%
#   spread(pass, nFish) %>%
#   full_join(read_csv('data/raw/fish/summer/ODFW_JohnDay_FishSummary.csv') %>%
#               filter(!is.na(DataCollectionEvent)) %>%
#               filter(Length < 0 | Length >= lngthCutOff) %>%
#               filter(!(Length > 200 & SpeciesCode == '11W')) %>%
#               select(-1) %>%
#               mutate(SiteName = str_split(DataCollectionEvent, ' ', simplify = T)[,1],
#                      Year = str_split(DataCollectionEvent, ' ', simplify = T)[,2]) %>%
#               group_by(DataCollectionEvent, SiteName, Year, SpeciesCode, `Pass Number`) %>%
#               summarise(nFish = n()) %>%
#               mutate(`Pass Number` = recode(`Pass Number`,
#                                             '1' = 'Pass1.M',
#                                             '2' = 'Pass2.C',
#                                             '0' = 'Unknown')) %>%
#               spread(`Pass Number`, nFish) %>%
#               filter(is.na(Unknown)) %>%
#               select(-Unknown)) %>%
#   ungroup() %>%
#   mutate(Species = if_else(grepl('^32', SpeciesCode),
#                            'Steelhead',
#                            if_else(grepl('^1', SpeciesCode),
#                                    'Chinook',
#                                    as.character(NA)))) %>%
#   mutate_at(vars(Year, starts_with('Pass')),
#             list(as.integer)) %>%
#   select(DataCollectionEvent:SpeciesCode, Species, everything())

# uses all fish at least 35mm in length
jdODFW = read_csv('data/raw/fish/summer/ODFW_JohnDay_SiteSummary_35mmandUp.csv') %>%
  rename(Lat = US.LatDD, Lon = US.LongDD,
         SampleDate = Date,
         Species = SpeciesSampled,
         FishSiteLength = ReachLength,
         Stream = StreamName,
         Pass1.M = Pass1Cap,
         Pass2.C = Pass2Cap,
         Pass3.R = Recap) %>%
  mutate(SiteName = str_split(DataCollectionEvent, ' ', simplify = T)[,1],
         Year = year(SampleDate),
         Method = if_else(NumPass == 1,
                          'Single Pass',
                          'Mark Recapture')) %>%
  mutate(Watershed = 'John Day',
         FishCrew = 'ODFW') %>%
  mutate(Species = recode(Species,
                          `O'mykiss` = 'Steelhead')) %>%
  mutate_at(vars(Year, starts_with('Pass')),
            list(as.integer)) %>%
  mutate_at(vars(SampleDate),
            list(as.POSIXct)) %>%
  select(DataCollectionEvent, one_of(names(aso)))

#-----------------------------------------------------------------
# Wenatchee
#-----------------------------------------------------------------
# start with 2011 data
tq = read_excel('data/raw/fish/summer/Wenatchee/Wen-Ent 2011 S-T channel units zero fish (120312).xlsx',
                'Site Scale Abundances') %>%
  bind_rows(read_excel('data/raw/fish/summer/Wenatchee/Wen-Ent 2011 S-T channel units zero fish (120312).xlsx',
                       'Site Scale Zero Fish')) %>%
  rename(SiteName = GRTSSiteID,
         Stream = StreamName,
         Species = SpeciesName,
         SampleDate = SurveyDateTime) %>%
  group_by(SiteName, Watershed, Species, SampleDate) %>%
  summarise_at(vars(N = Abundance),
               list(sum)) %>%
  mutate(FishCrew = 'Terraqua',
         Year = 2011,
         Method = 'CU Depletion',
         Season = 'Summer',
         SE = NA,
         Nmethod = 'Prev. Calc.') %>%
  select(one_of(names(aso))) %>%
  # add 2012 data
  bind_rows(read_csv('data/raw/fish/summer/Wenatchee/2012_MarkRecap.csv') %>%
              select(SiteName, 
                     Watershed = SubBasin, 
                     Stream = StreamName,
                     Lat, Lon = Long,
                     SampleDate = SurveyDateTime,
                     `Sthd M`:`Chin R`,
                     -`Sthd N`, -`Chin N`) %>%
              mutate(SiteName = recode(SiteName,
                                       '2A8' = 'ENT00001-2A8')) %>%
              mutate_at(vars(SampleDate),
                        list(mdy)) %>%
              gather(Species, fish, `Sthd M`:`Chin R`) %>%
              mutate(pass = if_else(grepl('M$', Species), 'Pass1.M',
                                    if_else(grepl('C$', Species), 'Pass2.C',
                                            if_else(grepl('R$', Species), 'Pass3.R', as.character(NA)))),
                     Species = str_sub(Species, 1, 4),
                     Species = recode(Species,
                                      'Sthd' = 'Steelhead',
                                      'Chin' = 'Chinook')) %>%
              spread(pass, fish) %>%
              mutate(Method = 'Mark Recapture') %>%
              bind_rows(read_csv('data/raw/fish/summer/Wenatchee/2012_SinglePass.csv') %>%
                          select(SiteName, 
                                 Watershed = SubBasin, 
                                 Stream = StreamName,
                                 Lat, Lon = Long,
                                 SampleDate = SurveyDateTime,
                                 Chinook = `Chin Total`,
                                 Steelhead = `Sthd Total`) %>%
                          gather(Species, Pass1.M, Chinook, Steelhead) %>%
                          mutate(Method = 'Single Pass') %>%
                          mutate_at(vars(SampleDate),
                                    list(mdy))) %>%
              mutate(Year = 2012,
                     Season = 'Summer',
                     FishCrew = 'Terraqua') %>%
              mutate_at(vars(SampleDate),
                        list(as.POSIXct))) %>%
  select(one_of(names(ent))) %>%
  # add 2013 data
  bind_rows(read_excel('data/raw/fish/summer/Wenatchee/2013 STM Summary Data 20131002.xlsx',
                       'Mark Recap Counts') %>%
              select(SiteName = `Site ID`,
                     Watershed = Subbasin,
                     Stream,
                     SampleDate = `Sample Date`,
                     `Sthd M`:`Sthd R`,
                     `Ch M`:`Ch R`) %>%
              gather(Species, fish, `Sthd M`:`Ch R`) %>%
              mutate(pass = if_else(grepl('M$', Species), 'Pass1.M',
                                    if_else(grepl('C$', Species), 'Pass2.C',
                                            if_else(grepl('R$', Species), 'Pass3.R', as.character(NA)))),
                     Species = str_split(Species, '\\ ', simplify = T)[,1],
                     Species = recode(Species,
                                      'Sthd' = 'Steelhead',
                                      'Ch' = 'Chinook')) %>%
              spread(pass, fish) %>%
              mutate(Method = 'Mark Recapture') %>%
              select(one_of(names(aso))) %>%
              bind_rows(read_excel('data/raw/fish/summer/Wenatchee/2013 STM Summary Data 20131002.xlsx',
                                   'Depletion Counts') %>%
                          select(SiteName = `Site ID`,
                                 Watershed = Subbasin,
                                 Stream,
                                 SampleDate = `Sample Date`,
                                 `Sthd 1P`:`Sthd 3P`,
                                 `Ch 1P`:`Ch 3P`) %>%
                          gather(Species, fish, `Sthd 1P`:`Ch 3P`) %>%
                          mutate(pass = if_else(grepl('1P$', Species), 'Pass1.M',
                                                if_else(grepl('2P$', Species), 'Pass2.C',
                                                        if_else(grepl('3P$', Species), 'Pass3.R', as.character(NA)))),
                                 Species = str_split(Species, '\\ ', simplify = T)[,1],
                                 Species = recode(Species,
                                                  'Sthd' = 'Steelhead',
                                                  'Ch' = 'Chinook')) %>%
                          spread(pass, fish) %>%
                          mutate_at(vars(starts_with('Pass')),
                                    list(as.integer)) %>%
                          mutate(Method = if_else(is.na(Pass2.C), 
                                                  'Single Pass', 
                                                  'Depletion'))) %>%
              mutate(Year = 2013,
                     Season = 'Summer',
                     FishCrew = 'Terraqua') %>%
              select(one_of(names(aso)))) %>%
  ungroup()

# do data from the Entiat match in tq and ent data.frames?
ent %>%
  filter(Year %in% unique(tq$Year),
         FishCrew == 'Terraqua') %>%
  select(Year, SiteName, Method, Species, SampleDate, starts_with('Pass')) %>%
  mutate(source = 'ent') %>%
  gather(pass, fish, starts_with('Pass')) %>%
  bind_rows(tq %>%
              filter(Watershed == 'Entiat') %>%
              select(Year, SiteName, Method, Species, SampleDate, starts_with('Pass')) %>%
              mutate(source = 'tq') %>%
              gather(pass, fish, starts_with('Pass'))) %>%
  spread(source, fish) %>%
  filter(ent != tq |
           (is.na(ent) & !is.na(tq)) |
           (is.na(tq) & !is.na(ent)) )
# tq seems to have slightly higher numbers on a few single pass surveys, so we'll use tq as default

#-----------------------------------------------------------------
# Lemhi
#-----------------------------------------------------------------
qci = read_csv('data/raw/fish/summer/QCI_GRTS_2009-2018.csv',
               col_types = paste0(paste(rep('?', 14), collapse = ''), 'd', 'd')) %>%
  rename(Watershed = WatershedName,
         FishSiteLength = SiteLength,
         Lon = Long,
         p = P_Capture,
         pSE = SE_P_Capture) %>%
  mutate_at(vars(Species),
            list(str_to_title)) %>%
  mutate_at(vars(starts_with('Pass')),
            list(as.integer)) %>%
  mutate_at(vars(SampleDate),
            list(mdy)) %>%
  mutate_at(vars(SampleDate),
            list(as.POSIXct)) %>%
  mutate(Method = recode(Method,
                         'Count' = 'Single Pass')) %>%
  select(one_of(names(aso)), starts_with('p'))

# add row for each species when species says "Zero Targets"
qci %>%
  filter(Species != 'Zero Targets') %>%
  bind_rows(qci %>%
              filter(Species == 'Zero Targets') %>%
              select(-Species) %>%
              crossing(Species = c('Chinook', 'Steelhead')) %>%
              mutate(Pass1.M = 0)) %>%
  mutate_at(vars(Species, Method),
            list(fct_drop)) -> qci

#-----------------------------------------------------------------
# Put it all together
#-----------------------------------------------------------------
fishSumDf = aso %>%
  bind_rows(ent %>%
              anti_join(tq %>%
                          select(SiteName, Year, Species, FishCrew, SampleDate))) %>%
  bind_rows(tq) %>%
  bind_rows(ugr) %>%
  bind_rows(jdELR %>%
              filter(!is.na(SiteName))) %>%
  bind_rows(jdODFW %>%
              select(-DataCollectionEvent)) %>%
  bind_rows(qci) %>%
  filter(Year >= 2011) %>%
  filter(Season != 'Winter' | is.na(Season)) %>%
  mutate_at(vars(Watershed, Season, FishCrew, Method, Species),
            list(fct_drop)) %>%
  select(Year, Site = SiteName, FishSite = FishSiteName, everything()) %>%
  mutate_at(vars(N, SE),
            list(~ if_else(Method %in% c('Depletion', 'Mark Recapture', 'Single Pass') & !is.na(Pass1.M),
                           as.numeric(NA),
                           .))) %>%
  mutate_at(vars(FishSite),
            list(~ if_else(is.na(.),
                           Site,
                           .))) %>%
  # filter out one survey where the number of recaptures was greater than the number of marks
  filter(!(Method == 'Mark Recapture' & Pass3.R > Pass1.M)) %>%
  select(Year:FishWettedArea, Pass1.M, Pass2.C, Pass3.R, Pass4, Nmethod, everything())

# add some missing lat/long from the CHaMP data
data(siteData)

fishSumDf %<>%
  left_join(siteData %>%
              select(Site, Year = VisitYear,
                     LON_DD, LAT_DD) %>%
              distinct() %>%
              filter(!is.na(LON_DD))) %>%
  mutate(Lat = if_else(is.na(Lat),
                       LAT_DD,
                       Lat),
         Lon = if_else(is.na(Lon),
                       LON_DD,
                       Lon)) %>%
  select(-LON_DD, -LAT_DD)

# save as csv file
write_csv(fishSumDf,
          'data/prepped/QRFsummerDataPrepped.csv')

# save to use like in a package
use_data(fishSumDf,
         overwrite = T)


#-----------------------------------------------------------------
# Issues
#-----------------------------------------------------------------
# surveys with more recaptures than marks
fishSumDf %>%
  filter(Method == 'Mark Recapture',
         Pass3.R > Pass1.M) %>%
  select(Watershed, FishSite, Year, SampleDate, FishCrew, Species, Pass1.M:pSE)
# drop the ODFW data point - raw data doesn't make sense

fishSumDf %>%
  filter(Method == 'Continuous',
         is.na(p)) %>%
  # filter(Pass1.M > 0) %>%
  # as.data.frame()
  xtabs(~ Pass1.M == 0, .)


