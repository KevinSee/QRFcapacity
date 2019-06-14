# Author: Kevin See
# Purpose: Prep the data collected by various outfits
# Created: 5/16/2019
# Last Modified: 5/16/2019
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(lubridate)
library(readxl)
library(stringr)
library(tidyverse)
library(magrittr)
library(measurements)


#-----------------------------------------------------------------
# Bring in some CHaMP habitat data at the channel unit scale, to fill in blank Tier 1 classifications.
data(chunitDf)
champCU = chunitDf %>%
  select(VisitID, SiteName = Site, ChUnitNumber, Tier1) %>%
  mutate(Tier1 = recode(Tier1,
                        'Fast-NonTurbulent/Glide' = 'Run',
                        'Fast-Turbulent' = 'Riffle',
                        'Slow/Pool' = 'Pool',
                        'Small Side Channel' = 'SC'))

#-----------------------------------------------------------------
# Read in fish data from ODFW, QCI and WDFW.

#-----------------------------------------------------------------
# ODFW
#-----------------------------------------------------------------
# snorkel
odfwSnork = read_excel('data/raw/fish/winter/ODFW - Basin Data for Kevin.xlsx',
                       'SnorkelCount') %>%
  rename(SiteUnit = `Site_ID_+Unit`,
         SiteName = `Site ID`,
         ChUnitNumber = `Unit No.`,
         Tier1 = `Unit Type`,
         Temp = `H20 Temp`) %>%
  mutate(Tier1 = recode(Tier1,
                        'FNT' = 'Run')) %>%
  mutate(Date = if_else(!is.na(Time),
                        ymd_hms(paste(date(Date), hour(Time), minute(Time), second(Time))),
                        Date)) %>%
  select(-(Tot_Count:ncol(.)), -Time) %>%
  gather(species, count, `BT counted`:`CH counted`) %>%
  mutate(species = str_replace(species, ' counted', '')) %>%
  mutate_at(vars(SiteUnit, SiteName),
            list(str_to_upper))

# mark recapture
odfwMR = read_excel('data/raw/fish/winter/ODFW - Basin Data for Kevin.xlsx',
                    'MarkRecap') %>%
  rename(SiteName = `Site ID`,
         ChUnitNumber = `Unit No.`,
         Tier1 = `Unit Type`,
         SampleMethod = Method) %>%
  mutate(Tier1 = recode(Tier1,
                        'FNT' = 'Run')) %>%
  select(-`N-hat(chapman)`, -Time) %>%
  mutate_at(vars(SiteName),
            list(str_to_upper)) %>%
  gather(sample, value, `BT Marked`:`CH New`) %>%
  mutate(SiteUnit = paste(SiteName, ChUnitNumber, sep = '_'),
         species = str_sub(sample, 1, 2),
         sample = str_sub(sample, 4),
         sample = factor(sample,
                         levels = c('Marked', 'New', 'Recap'))) %>%
  mutate(SampleMethod = str_replace(SampleMethod, 'Efish', 'efish'),
         Method = 'MarkRecap') %>%
  select(-SampleMethod) %>%
  spread(sample, value) %>%
  mutate(Capture = New + Recap) %>%
  select(SiteName:Method, 
         Pass1_M = Marked, 
         Pass2_C = Capture, 
         Pass3_R = Recap)

# depletions
odfwDeplOrg = read_excel('data/raw/fish/winter/ODFW - Basin Data for Kevin.xlsx',
                         'Depletions') %>%
  rename(SiteName = `Site ID`,
         ChUnitNumber = `Unit No.`,
         Tier1 = `Unit Type`) %>%
  mutate(Tier1 = recode(Tier1,
                        'FNT' = 'Run')) %>%
  mutate_at(vars(SiteName),
            list(str_to_upper)) %>%
  mutate(Date = if_else(!is.na(Time),
                        ymd_hms(paste(date(Date), hour(Time), minute(Time), second(Time))),
                        Date)) %>%
  mutate(Method = 'Depletion',
         SiteUnit = paste(SiteName, ChUnitNumber, sep = '_')) %>%
  select(-matches('tot$'), -matches('Nhat'), -X, -Y, -Time) %>%
  gather(variable, value, matches('Pass')) %>%
  mutate(Pass = str_sub(variable, 1, 5),
         Pass = str_replace(Pass, 'Pass', '')) %>%
  mutate(variable = ifelse(grepl('Effort', variable),
                           'Effort',
                           variable),
         species = ifelse(variable != 'Effort',
                          str_sub(variable, -2),
                          NA)) %>%
  mutate(value = as.numeric(value))

# don't worry about effort, not enough data to incorporate that as variable
odfwDepl = odfwDeplOrg %>%
  filter(variable != 'Effort') %>%
  select(-variable) %>%
  spread(Pass, value) %>%
  rename(Pass1_M = `1`,
         Pass2_C = `2`,
         Pass3_R = `3`)


# snorkel counts
odfwSnorkOnly = read_excel('data/raw/fish/winter/qry_Count_Salmonids_By_Unit_winter.xlsx',
                           'Combined_Kevins_Format') %>%
  select(Stream:pass3_R) %>%
  rename(Pass1_M = pass1_M,
         Pass2_C = pass2_C,
         Pass3_R = pass3_R,
         Tier1 = Tier_1) %>%
  mutate(Tier1 = recode(Tier1,
                        'FNT' = 'Run')) %>%
  # use scaled count (the raw fish count divided by % of channel unit sampled.  Units were sometimes partially sampled due to ice or extremely shallow water.)
  select(-count) %>%
  rename(count = `scaled_count*`) %>%
  mutate(SurveyType = recode(SurveyType,
                             'Snorkel' = 'Snorkeling')) %>%
  mutate_at(vars(matches('^Pass')),
            list(as.numeric))


odfwData = odfwSnork %>%
  select(-Method) %>%
  full_join(odfwMR %>%
              bind_rows(odfwDepl) %>%
              select(-Date, -Temp)) %>%
  mutate(Discharge = NA) %>%
  mutate(SurveyType = 'Calibration') %>%
  rename(DCEtype = Method) %>%
  select(SiteUnit:Date,
         Crew, DCEtype, SurveyType, Temp, Discharge,
         everything()) %>%
  bind_rows(odfwSnorkOnly) %>%
  mutate(Tier1 = recode(Tier1,
                        'Fast-NonTurbulent' = 'Run',
                        'Fast-Turbulent' = 'Riffle',
                        'Slow/Pool' = 'Pool',
                        'SmSideChnnl' = 'Small Side Channel'))

# what's missing?
odfwData %>%
  summarise_at(vars(everything()),
               list(~sum(is.na(.)) / length(.))) %>%
  gather(variable, percNA)

#-----------------------------------------------------------------
# QCI
#-----------------------------------------------------------------
qciData = read_excel('data/raw/fish/winter/QCI_winterQRF_capture.xlsx') %>%
  rename(ChUnitNumber = ChUnitNum,
         Stream = StreamName,
         Date = SurveyDateTime,
         Temp = `Temp(°C)`,
         Discharge = `Discharge(m3/s)`,
         species = Species,
         count = SnorkelCount,
         SurveyType = Method,
         Pass2_C = C,
         Pass3_R = R) %>%
  mutate(SiteUnit = paste(SiteName, ChUnitNumber, sep = '_'),
         Discharge = as.numeric(Discharge),
         Crew = 'QCI',
         DCEtype = recode(DCEtype,
                          'MR' = 'MarkRecap'),
         species = recode(species,
                          'Chinook' = 'CH',
                          'Steelhead' = 'OM'),
         Tier1 = ifelse(Tier2 == 'Fast Non-turbulent',
                        'Run',
                        ifelse(Tier2 == 'Riffle',
                               'Riffle',
                               ifelse(Tier2 %in% c('Pool/Off Channel', 'Side Channel'),
                                      'Small Side Channel',
                                      'Pool')))) %>%
  select(one_of(names(odfwData)), PercentIceCover, Tier2) %>%
  select(SiteUnit:Discharge, PercentIceCover, Tier2, everything())

# for surveys with missing discharge data, use discharge from other surveys at the same site
qciDischarge = qciData %>%
  select(Date, Stream, SiteName, SurveyType, Discharge) %>%
  mutate(Date = floor_date(Date, unit = 'day')) %>%
  distinct()

qciData %<>%
  left_join(qciDischarge %>%
              group_by(Stream, SiteName) %>%
              summarise(nTot = n_distinct(Date),
                        nNonNA = n_distinct(Date[!is.na(Discharge)])) %>%
              left_join(qciDischarge %>%
                          filter(!is.na(Discharge)) %>%
                          group_by(SiteName) %>%
                          summarise_at(vars(Discharge),
                                       list(min = min, 
                                            median = median, 
                                            mean = mean, 
                                            max = max))) %>%
              ungroup() %>%
              filter(nTot > nNonNA,
                     nNonNA > 0) %>%
              select(SiteName, meanDis = mean)) %>%
  mutate(Discharge = if_else(is.na(Discharge),
                             meanDis,
                             Discharge)) %>%
  select(-meanDis)

# # what's missing?
# qciData %>%
#   summarise_at(vars(everything()),
#                list(~sum(is.na(.)) / length(.))) %>%
#   gather(variable, percNA)
# 
# xtabs(~ DCEtype + SurveyType + is.na(count), qciData)

#-----------------------------------------------------------------
# WDFW
#-----------------------------------------------------------------
# Flow data
wdfwFlow = read_excel('data/raw/fish/winter/WDFW Night Snorkel Data.xlsx',
                      'Flow',
                      range = 'O4:Q39') %>%
  rename(Stream = 1,
         VisitID = `Site ID`,
         FlowCFS = `Flow CFS`) %>%
  filter(!grepl('_2$', VisitID)) %>%
  mutate(Discharge = conv_unit(FlowCFS,
                               'ft3_per_sec',
                               'm3_per_sec'),
         VisitID = as.integer(str_replace(VisitID, '_1$', ''))) %>%
  select(-FlowCFS) %>%
  full_join(read_excel('data/raw/fish/winter/WDFW Night Snorkel Data.xlsx',
                       'Flow',
                       range = 'A1:D632') %>%
              rename(VisitID = `Stream ID`) %>%
              distinct() %>%
              mutate(Date = if_else(!is.na(Time),
                                    ymd_hms(paste(date(Date), hour(Time), minute(Time), second(Time))),
                                    Date)) %>%
              select(-Time) %>%
              filter(!grepl('_2$', VisitID)) %>%
              mutate(VisitID = as.integer(str_replace(VisitID, '_1$', '')))) %>%
  select(FlowDate = Date, everything())


# snorkel
wdfwSnork = read_excel('data/raw/fish/winter/WDFW Night Snorkel Data.xlsx',
                       'Raw Snorkel Data') %>%
  select(Date,
         Time = `Start Time`,
         Temp = `Water Temp C`,
         Snorkelers:Stream,
         Watershed = Basin,
         VisitID = `Visit ID`,
         SiteID = `Site ID`,
         SiteLength = `Site L (m)`,
         NumSnork = `# Snrk`,
         ChUnit = `Unit #`,
         Tier2 = Type,
         PercSnork = `% Snrk`,
         Visibility = `Vis (0-3)`,
         ChUnitNotes = `Channel Unit Notes`,
         SiteNotes = `Site Notes`,
         `BT <80`:`Sculpin`) %>%
  mutate(Date = if_else(!is.na(Time),
                        ymd_hms(paste(date(Date), hour(Time), minute(Time), second(Time))),
                        Date)) %>%
  select(-matches('Total'), -matches('^WH'), -Time) %>%
  mutate(ChUnitNumber = str_replace(ChUnit, '[[:alpha:]]', '')) %>%
  mutate_at(vars(Temp),
            list(as.numeric)) %>%
  filter(PercSnork > 0,
         NumSnork > 0) %>%
  # correct a few dates
  mutate(Date = if_else(VisitID == 4806,
                        min(Date[VisitID == 4806]),
                        Date)) %>%
  left_join(wdfwFlow) %>%
  mutate(diff = abs(as.numeric(difftime(Date, FlowDate, units = 'days'))),
         Discharge = ifelse(diff > 7,
                            NA,
                            Discharge)) %>%
  select(-diff, -`Lamprey J`, -Sculpin) %>%
  gather(spp_size, count, `BT <80`:`CH >100`) %>%
  mutate(species = str_extract(spp_size, '[:alpha:]+'),
         species = factor(species,
                          levels = c('CH', 'OM', 'BT')),
         sizeCls = str_replace(spp_size, '[:alpha:]+', ''),
         sizeCls = str_trim(sizeCls),
         sizeCls = factor(sizeCls,
                          levels = c('<100', '100+', '<80', '80-129', '130-199', '200+'))) %>%
  select(-spp_size) %>%
  mutate(count = count / (PercSnork / 100)) %>%
  select(Date:SiteNotes, ChUnitNumber, Discharge, species, sizeCls, count) %>%
  mutate_at(vars(count),
            list(~ as.integer(round(.)))) %>%
  mutate(count = ifelse(is.na(count),
                        0, count))

# WDFW mark-recapture / snorkel calibration surveys

# by size class
wdfwMRsizeCls = read_excel('data/raw/fish/winter/WDFW Night Snorkel Data.xlsx',
                           'Correction Factor',
                           range = 'S40:BP60') %>%
  rename(Stream = 1,
         VisitID = 2,
         Tier2 = 3) %>%
  select(-starts_with('Total'), -15, -27, -39) %>%
  mutate(id = 1:n()) %>%
  gather(variable, value, -(Stream:Tier2), -id) %>%
  mutate(sizeCls = str_remove(variable, '\\.\\.[:digit:]+'),
         index = str_split(variable, '\\.\\.', simplify = T)[,2],
         index = as.integer(index),
         spp = ifelse(sizeCls %in% c('<100', '100+'),
                      'CH',
                      ifelse(index %in% c(6:9, 18:21, 30:33, 42:45), 
                             'OM',
                             'BT')),
         pass = ifelse(spp == 'CH',
                       ifelse(index %in% c(4,5),
                              'count',
                              ifelse(index %in% c(16, 17), 
                                     'Pass1_M',
                                     ifelse(index %in% c(28,29),
                                            'Pass3_R',
                                            ifelse(index %in% c(40,41),
                                                   'Pass2_unmarked',
                                                   NA)))),
                       NA),
         pass = ifelse(spp == 'OM',
                       ifelse(index %in% c(6:9),
                              'count',
                              ifelse(index %in% c(18:21), 
                                     'Pass1_M',
                                     ifelse(index %in% c(30:33),
                                            'Pass3_R',
                                            ifelse(index %in% c(42:45),
                                                   'Pass2_unmarked',
                                                   pass)))),
                       pass),
         pass = ifelse(spp == 'BT',
                       ifelse(index %in% c(10:13),
                              'count',
                              ifelse(index %in% c(22:25), 
                                     'Pass1_M',
                                     ifelse(index %in% c(34:37),
                                            'Pass3_R',
                                            ifelse(index %in% c(46:49),
                                                   'Pass2_unmarked',
                                                   pass)))),
                       pass)) %>%
  mutate(pass = factor(pass),
         pass = fct_relevel(pass, 'count'),
         spp = factor(spp,
                      levels = c('CH', 'OM', 'BT')),
         sizeCls = factor(sizeCls,
                          levels = c('<100', '100+', '<80', '80-129', '130-199', '200+'))) %>%
  select(-variable, -index) %>%
  arrange(id, Stream, VisitID, spp, pass, sizeCls) %>%
  spread(pass, value) %>%
  mutate(Pass2_C = Pass2_unmarked + Pass3_R) %>%
  select(Stream:Tier2, id, species = spp, sizeCls, count, Pass1_M, Pass2_C, Pass3_R) %>%
  arrange(id, VisitID, species, sizeCls)

# lump across size classes
wdfwMR = wdfwMRsizeCls %>%
  group_by(Stream, VisitID, Tier2, id, species) %>%
  summarise_at(vars(count:Pass3_R),
               list(sum)) %>%
  ungroup() %>%
  mutate(VisitID = as.integer(str_sub(VisitID, 1, 4))) %>%
  mutate(Tier2 = recode(Tier2,
                        'Fast Non Turb' = 'Run')) %>%
  select(-id) %>%
  left_join(read_excel('data/raw/fish/winter/WDFW Night Snorkel Data.xlsx',
                       'Correction Factor',
                       range = 'A1:Q392') %>%
              select(Date,
                     Time = `Start Time`,
                     Temp = `Water Temp C`,
                     Stream,
                     Watershed = Basin,
                     VisitID = `Site ID`,
                     ChUnitNumber = `Unit #`,
                     Tier2 = Habitat,
                     species = Species) %>%
              mutate(Date = if_else(!is.na(Time),
                                    ymd_hms(paste(date(Date), hour(Time), minute(Time), second(Time))),
                                    Date)) %>%
              mutate(Date = if_else(year(Date) > 2018 & month(Date) > 7,
                                    ymd_hms(paste('2017', month(Date), mday(Date), hour(Date), minute(Date), second(Date))),
                                    Date)) %>%
              select(-Time) %>%
              group_by(Date, Watershed, Stream, VisitID, ChUnitNumber, Tier2, Temp, species) %>%
              summarise(Pass1_M = n()) %>%
              ungroup()) %>%
  select(Watershed, Stream, VisitID, ChUnitNumber, Tier2, Date, Temp, everything()) %>%
  arrange(VisitID, ChUnitNumber, species) %>%
  filter(!is.na(Watershed))


wdfwData = wdfwSnork %>%
  group_by(Watershed, Stream, VisitID, ChUnitNumber, Date, Temp, Discharge, species) %>%
  summarise_at(vars(count),
               list(sum),
               na.rm = T) %>%
  ungroup() %>%
  mutate(SurveyType = 'Snorkeling',
         DCEtype = 'Count') %>%
  left_join(wdfwSnork %>%
              mutate(Tier2 = recode(Tier2,
                                    'OC' = 'Off Channel',
                                    'PP' = 'Plunge Pool',
                                    'SP' = 'Scour Pool',
                                    'RA' = 'Rapid',
                                    'RI' = 'Riffle',
                                    'SSC' = 'Small Side Channel')) %>%
              select(Stream, VisitID, ChUnitNumber, Tier2) %>%
              group_by(Stream, VisitID, ChUnitNumber) %>%
              slice(1) %>%
              ungroup()) %>%
  bind_rows(wdfwMR %>%
              left_join(wdfwFlow) %>%
              mutate(diff = abs(as.numeric(difftime(Date, FlowDate, units = 'days'))),
                     Discharge = ifelse(diff > 7,
                                        NA,
                                        Discharge)) %>%
              select(-diff, -FlowDate) %>%
              mutate(DCEtype = 'MarkRecap',
                     SurveyType = 'Calibration')) %>%
  left_join(champCU %>%
              mutate_at(vars(ChUnitNumber),
                        list(as.character)) %>%
              bind_rows(champCU %>%
                          filter(VisitID == 4816,
                                 ChUnitNumber %in% c(7,8)) %>%
                          select(VisitID, SiteName, Tier1) %>%
                          distinct() %>%
                          mutate(ChUnitNumber = '7&8'))) %>%
  mutate(SiteUnit = paste(SiteName, ChUnitNumber, sep = '_'),
         # Tier1 = ifelse(Tier2 == 'Run',
         #                'Run',
         #                ifelse(Tier2 %in% c('Riffle', 'RA', 'RI'),
         #                       'Riffle',
         #                       ifelse(grepl('Pool', Tier2),
         #                              'Pool',
         #                              Tier2))),
         Crew = 'WDFW') %>%
  select(one_of(names(qciData))) %>%
  arrange(Stream, SiteName, ChUnitNumber, species)

#------------------------------------------------------
# Combine everything and save as a .csv file.
fishWinDf = odfwData %>%
  mutate(Crew = 'ODFW') %>%
  bind_rows(qciData) %>%
  mutate_at(vars(ChUnitNumber),
            list(as.character)) %>%
  bind_rows(wdfwData %>%
              mutate(PercentIceCover = NA)) %>%
  select(Stream, SiteName, SiteUnit, ChUnitNumber, 
         Crew, SurveyType, DCEtype,
         Date,
         Tier1, Tier2, Discharge, Temp, PercentIceCover,
         everything()) %>%
  mutate(Tier2 = recode(Tier2,
                        'Fast Non-turbulent' = 'Run',
                        'Fast-NonTurbulent' = 'Run',
                        'SmSideChnnl' = 'Small Side Channel'),
         Tier1 = recode(Tier1,
                        'SC' = 'SSC',
                        'Small Side Channel' = 'SSC')) %>%
  rename(Species = species) %>%
  mutate(Species = recode(Species,
                          'CH' = 'Chinook',
                          'OM' = 'Steelhead',
                          'BT' = 'BrookTrout')) %>%
  mutate_at(vars(Stream, Crew, Tier1, Tier2, Species),
            list(as.factor))

# save as csv file
write_csv(fishWinDf,
          'data/prepped/QRFwinterDataPrepped.csv')

# save to use as data
use_data(fishWinDf,
         overwrite = T)

#------------------------------------------------------
# anything missing?
fishWinDf %>%
  summarise_at(vars(everything()),
               list(~sum(is.na(.)) / length(.))) %>%
  gather(variable, percNA) %>%
  arrange(desc(percNA))

xtabs(~ is.na(Pass2_C) + is.na(Pass1_M), fishWinDf)

fishWinDf %>%
  filter(is.na(Pass1_M)) %>%
  xtabs(~ SurveyType, .)

fishWinDf %>%
  filter(is.na(count)) %>%
  xtabs(~ SurveyType, .)
