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


popdata.May.totals = read.xlsx("files/Vic May 2024-proposed-electoral-divisions-SA1-and-SA2.xlsx") %>%
  filter(!is.na(`SA1.Code.(2021.SA1s)`)) %>%
  mutate(sa1_code_2021 = 
           as.character(substr(`SA1.Code.(2021.SA1s)`,1,7))) %>%
  rename(tot_project = `Projected.Enrolment.17/04/28`) %>%
  group_by(sa1_code_2021) %>%
  summarise(tot_project = sum(tot_project))

popdata.May = read.xlsx("files/Vic May 2024-proposed-electoral-divisions-SA1-and-SA2.xlsx") %>%
  filter(!is.na(`SA1.Code.(2021.SA1s)`)) %>%
  mutate(sa1_code_2021 = 
           as.character(substr(`SA1.Code.(2021.SA1s)`,1,7))) %>%
  arrange(sa1_code_2021,desc(`Projected.Enrolment.17/04/28`)) %>%
  distinct(sa1_code_2021, .keep_all = TRUE) %>%
  left_join(popdata.May.totals, by = 'sa1_code_2021')

user_upload = SA1 %>%
  filter(!is.na(cent_lat)) %>%
  mutate( `User Divsion Name` = '') %>%
  select(sa1_7_digit_code = sa1_7code_2021, sa1_code_2021, `Current Division` = Current.Division,
         `Proposed Division` = Proposed.Division, `User Divsion Name`) %>%
  distinct() %>%
  sf::st_drop_geometry(data_all)

write.csv(user_upload, 'files/blank SA1 template')


#merge with SA1 to get geomeasures

SA1_merge = user_upload %>%
  left_join(readRDS('files/SA1.rds'),by = 'sa1_code_2021') 
