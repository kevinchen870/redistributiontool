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
library(htmlwidgets)

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
  mutate( `Your Divsion Name` = '') %>%
  select(sa1_7_digit_code = sa1_7code_2021, sa1_code_2021, `Current Division` = Current.Division,
         `Proposed Division` = Proposed.Division, `Your Divsion Name`) %>%
  distinct() %>%
  sf::st_drop_geometry(data_all)

write.csv(user_upload, 'files/blank SA1 template.csv')

read.csv('files/blank SA1 template.csv')


SA1_merge = user_upload %>%
  left_join(readRDS('files/SA1.rds'),by = 'sa1_code_2021') 


test = user_upload %>%
  filter(!`Your Divsion Name` =='')

#UI
ui = fluidPage(
  tags$div(
    "Please allow a minute for the map to load up.",
    tags$br(),
    "Download template to enter your divisions mapped to SA1s"),
  downloadLink("downloadTemplate", "Download Template"),
  tags$div(tags$br()),
  tags$div('Upload completed template'),
  fileInput("userSA1upload", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  tags$div(tags$br()),
  leafletOutput("SA1mapping"),
  tags$div(tags$br()),
  actionButton("savemap", "Save map as html"),
  tags$div(tags$br())
)

server <- function(input, output) {
  #template
  output$downloadTemplate <- downloadHandler(
    filename = 'SA1 to Division mapping template.csv',
    content = function(file) {
      file.copy('files/blank SA1 template.csv', file, row.names=FALSE)
    })
  
  
  basemap <- leaflet() %>%
      addTiles() %>%
      addPolygons(data = lga %>%filter(state_code_2021 == 2),
                  fillColor = "white",
                  fillOpacity = 0,
                  color = "yellow",
                  stroke = TRUE,
                  weight = 2,
                  group = "LGA",
                  label = ~lga_name_2022) %>%
      addPolygons(data = SA2 %>%filter(state_code_2021 == 2,
                                       !is.na(cent_lat)),
                  fillColor = "white",
                  fillOpacity = 0,
                  color = "green",
                  stroke = TRUE,
                  weight = 1,
                  group = "SA2",
                  label = SA2$SA2_name_2021) %>%
      addPolygons(data = New_Divisions_May%>% sf::st_zm(),
                  fillColor = 'white',
                  fillOpacity = 0,
                  color = "blue",
                  stroke = TRUE,
                  weight = 2,
                  group = "Proposed Divisions",
                  label = lapply(paste0("Proposed Division: ",
                                        str_to_title(New_Divisions_May$Sortname),'<br>',
                                        "Projected Population: ",
                                        New_Divisions_May$Projected),
                                 htmltools::HTML)) %>%
      addPolygons(data= shp_CED, 
                  fillOpacity =0.0,
                  weight = 2, stroke = TRUE, 
                  color = 'red',
                  group = 'Current Divisions', 
                  label = lapply(paste0("Current Division: ",
                                        str_to_title(shp_CED$CED_NAME21),'<br>',
                                        "Projected Population: ",
                                        shp_CED$tot_project),
                                 htmltools::HTML)) %>%
      addLayersControl(
        overlayGroups = c("Current Divisions", "SA2", 'LGA',"Proposed Divisions"),
        options = layersControlOptions(collapsed = TRUE))
  
  output$SA1mapping <- renderLeaflet("basemap")
  
  observe({
    req(input$file)
    SA1_user <- read.csv(input$userSA1upload$datapath) %>%
      filter(!is.na(`Your Division Name`) | !`Your Divsion Name` =='') %>%
      left_join(readRDS(SA1.rds), by = sa1_code_2021) %>%
      group_by(`Your Division Name`) %>%
      summarise(Projected = sum(tot_project))
      st_make_valid() %>% 
      st_union()
    
    proxy <- leafletProxy("SA1mapping") 
    proxy %>% 
      addPolygons(data = SA1_user%>% sf::st_zm(),
                  fillColor = 'white',
                  fillOpacity = 0,
                  color = "black",
                  stroke = TRUE,
                  weight = 3,
                  group = "Your Divisions",
                  label = lapply(paste0("Your Division: ",
                                        str_to_title(SA1_user$`User Division Name`),'<br>',
                                        "Projected Population: ",
                                        SA1_user$Projected),
                                 htmltools::HTML)) %>%
      addLayersControl(
        overlayGroups = c("Current Divisions", "SA2", 'LGA',"Proposed Divisions",'Your Divisions'),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Current Divisions") %>%
      hideGroup("LGA") %>%
      hideGroup("Proposed Divisions") %>%
      hideGroup("SA2")
    
    output$savemap <- downloadHandler(
      filename = "Your Proposed Divisions.html",
      content = function(file){
        saveWidget(
          widget = SA1mapping()
          , file = file
        )
      }
    )
    })
}

shinyApp(ui, server)
