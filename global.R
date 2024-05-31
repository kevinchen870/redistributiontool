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
library(mapedit)
library(leaflet.extras)
sf::sf_use_s2(FALSE)
#Read in data
SA1 = readRDS('files/SA1.rds')

SA2 = readRDS('files/SA2.rds')

CED =  readRDS('files/CED.rds')

New_Divisions_May = readRDS('files/NewCED.rds') 

lga = read_absmap('lga2022')

Popdata = read.xlsx("files/Victoria-SA1 revised.xlsx") %>%
  filter(Division != 'VIC TOTAL') %>%
  mutate(sa1_code_2021 = 
           as.character(`Statistical.Area.Level.1.(SA1).(2021.SA1s)`),
         `Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)` = 
           as.character(`Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)`),
  ) %>%
  rename(tot_project = Projected.enrolment.Monday.17.April.2028)

#New data from May
popdata.May = read.xlsx("~/redistributiontool/files/Vic May 2024-proposed-electoral-divisions-SA1-and-SA2.xlsx") %>%
  filter(!is.na(`SA1.Code.(2021.SA1s)`)) %>%
  mutate(sa1_code_2021 = 
           as.character(`SA1.Code.(2021.SA1s)`)) %>%
  rename(tot_project = `Projected.Enrolment.17/04/28`)


Popdata = Popdata %>%
  select(sa1_code_2021,`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`, `Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)`,`Statistical.Area.Level.1.(SA1).(2021.SA1s)`) %>%
  left_join(popdata.May, by = c('Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)' = 'sa1_code_2021' ))

SA1 = SA1 %>%
  subset(state_code_2021 == '2') %>%
  left_join(Popdata, by = 'sa1_code_2021') %>%
  rename(sa1_7code_2021 = 
           `Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)`,
         Current.Division = `Current.Divison.as.of.26/07/21`)

#Create summaries
Pop_data_summary_SA2 = Popdata %>%
  group_by(`SA2.Name.(2021.SA1s)`,
           `Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`,
           Proposed.Division,
           `Current.Divison.as.of.26/07/21`) %>%
  summarise(tot_current = sum(`Actual.Enrolment.09/08/23`),
            tot_project = sum(tot_project)) %>%
  mutate(Division = str_to_lower(`Current.Divison.as.of.26/07/21`),
         SA2_CODE21 = 
           as.character(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`)) %>%
  setNames(c('SA2.Name','SA2.Code','Proposed Division','Current Division','tot_current', 
             'tot_project','Division','SA2_CODE21'))


Pop_data_summary_CED = Pop_data_summary_SA2 %>%
  ungroup() %>%
  group_by(`Division`) %>%
  summarise(tot_current = sum(tot_current),
            tot_project = sum(tot_project)) %>%
  mutate(`Division` = str_to_lower(`Division`),
         within_projection = case_when(tot_project>131691 ~ 'Over Projection',
                                       tot_project <122785 ~ 'Under Projection',
                                       TRUE ~ 'Within Projection '),
         diff_from_quota = tot_project-127238,
         deviation_2028 = round(diff_from_quota/127238 * 100,2))

shp_CED = CED %>%
  subset(state_code_2021 == '2') %>%
  filter(!is.na(cent_lat)) %>%
  mutate(CED_NAME21 = tolower(ced_name_2021)) %>%
  left_join(Pop_data_summary_CED, by = c('CED_NAME21' = 'Division'))

#Create colour palette
pal <- colorBin(
  palette = "Spectral",
  domain = Pop_data_summary_CED$deviation_2028,
  bins = 5)
