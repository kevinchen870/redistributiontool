#Load global packages
library(sf)
library(raster)
library(dplyr)
library(leaflet)
library(openxlsx)
library(stringr)
library(shiny)
library(DT)
library(strayr)
library(shinyscreenshot)

#Read in data
SA1 = readRDS('files/SA1.rds')

SA2 = readRDS('files/SA2.rds')

CED =  readRDS('files/CED.rds')

Popdata = read.xlsx("files/Victoria-SA1.xlsx") %>%
  filter(Division != 'VIC TOTAL') %>%
  mutate(sa1_code_2021 = 
           as.character(`Statistical.Area.Level.1.(SA1).(2021.SA1s)`)) %>%
  rename(tot_project = Projected.enrolment.Monday.17.April.2028)

SA1 = SA1 %>%
  subset(state_code_2021 == '2') %>%
  left_join(Popdata, by = 'sa1_code_2021')

#Create summaries
Pop_data_summary_SA2 = Popdata %>%
  group_by(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`,
           `Statistical.Area.Level.2.(SA2).Name`,
           Division) %>%
  summarise(tot_current = sum(Actual.enrolment.Wednesday.9.August.2023),
            tot_project = sum(tot_project)) %>%
  mutate(Division = str_to_lower(Division),
         SA2_CODE21 = 
           as.character(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`)) %>%
  setNames(c('SA2.Code','SA2.Name','Division','tot_current', 
             'tot_project','SA2_CODE21'))


SA2 = SA2 %>%
  subset(state_code_2021 == '2') %>%
  left_join(Pop_data_summary_SA2, by = c('sa2_code_2021' = 'SA2_CODE21'))


Pop_data_summary_CED = Pop_data_summary_SA2 %>%
  ungroup() %>%
  group_by(Division) %>%
  summarise(tot_current = sum(tot_current),
            tot_project = sum(tot_project)) %>%
  mutate(Division = str_to_lower(Division),
         within_projection = case_when(tot_project>131691 ~ 'Over Projection',
                                       tot_project <122785 ~ 'Under Projection',
                                       TRUE ~ 'Within Projection '),
         diff_from_quota = tot_project-127238,
         deviation_2028 = round(diff_from_quota/127238 * 100,2))

shp_CED = CED %>%
  subset(state_code_2021 == '2') %>%
  mutate(CED_NAME21 = tolower(ced_name_2021)) %>%
  left_join(Pop_data_summary_CED, by = c('CED_NAME21' = 'Division'))

#Create colour palette
pal <- colorBin(
  palette = "Spectral",
  domain = Pop_data_summary_CED$deviation_2028,
  bins = 5)
