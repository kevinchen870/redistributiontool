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

lga = read_absmap('lga2022')

Popdata = read.xlsx("files/Victoria-SA1 revised.xlsx") %>%
  filter(Division != 'VIC TOTAL') %>%
  mutate(sa1_code_2021 = 
           as.character(`Statistical.Area.Level.1.(SA1).(2021.SA1s)`)) %>%
  rename(tot_project = Projected.enrolment.Monday.17.April.2028)

SA1 = SA1 %>%
  subset(state_code_2021 == '2') %>%
  left_join(Popdata, by = 'sa1_code_2021') %>%
  rename(sa1_7code_2021 = 
           `Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)`)

#Create summaries
Pop_data_summary_SA2 = Popdata %>%
  group_by(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`,
           `Statistical.Area.Level.2.(SA2).Name.(2021.SA2s)`,
           Division) %>%
  summarise(tot_current = sum(Actual.enrolment.Wednesday.9.August.2023),
            tot_project = sum(tot_project)) %>%
  mutate(Division = str_to_lower(Division),
         SA2_CODE21 = 
           as.character(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`)) %>%
  setNames(c('SA2.Code','SA2.Name','Division','tot_current', 
             'tot_project','SA2_CODE21'))


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


# selected = editMap(leaflet() %>%
#                      addTiles() %>%
#                      #Add divisions with deviation palette
#                      addPolygons(data= shp_CED,
#                                  fillOpacity =0.1, fillColor = ~pal(shp_CED$deviation_2028),
#                                  weight = 5, stroke = TRUE, color = 'white',
#                                  group = 'Current Divisions',
#                                  label = lapply(paste0("Division: ",
#                                                        str_to_title(shp_CED$CED_NAME21),'<br>',
#                                                        "Projected Population: ",
#                                                        shp_CED$tot_project,
#                                                        '<br>',
#                                                        "Deviation: ",shp_CED$deviation_2028),
#                                                 htmltools::HTML),
#                                  highlightOptions = highlightOptions(color = "white",
#                                                                      weight = 2,
#                                                                      bringToFront = FALSE)) %>%
#                      #Add SA1 base layer
#                      addPolygons(data = SA1,
#                                  fillColor = "white",
#                                  fillOpacity = 0.3,
#                                  color = "black",
#                                  stroke = TRUE,
#                                  weight = 1,
#                                  layerId = ~sa1_code_2021,
#                                  group = "SA1",
#                                  label = ~sa1_code_2021) %>%
#                      #Add LGA base layer
#                      addPolygons(data = lga %>%filter(state_code_2021 == 2),
#                                  fillColor = "white",
#                                  fillOpacity = 0.3,
#                                  color = "blue",
#                                  stroke = TRUE,
#                                  weight = 4,
#                                  layerId = ~lga_code_2022,
#                                  group = "LGA",
#                                  label = ~lga_name_2022) %>%
#   # Layers control
#   addLayersControl(
#     overlayGroups = c("Current Divisions", "SA1",'LGA'),
#     options = layersControlOptions(collapsed = TRUE)) %>%
#   #Add deviation legend
#   addLegend(values = shp_CED$deviation_2028,position = 'bottomright',
#             title = 'deviation from projected (%)',
#             colors = c("#D53E4F", "#FC8D59", "#FEE08B", 
#                        "#E6F598", "#99D594", "#3288BD"),
#             labels = c('-8% - -6%',
#                        '-6%- -4%',
#                        '-4% - -2%',
#                        '-2% - 0%',
#                        '0% - 2%',
#                        '2% - 4%')))
# 
# st_is_valid(selected$finished)
# st_make_valid(selected$finished)
# Goldstein= st_intersection(SA1,st_make_valid(selected$finished))
# plot(Goldstein['Division'])
#Define UI of app
ui = fluidPage(
  
  titlePanel('Redistribution Tool v0.2'),
  tags$div(
  "Please allow a minute for the map to load up.",
  tags$br(),
  "Click on the polygon to build from SA1. Press finished on the polygon, and then display data to see the table and projected total.",
  tags$br(),
  "To edit the points, click on the writing tool and move the points of the box.", 
  tags$br(),
  "Before beginning a new electorate or area, Please clear the current shape using the rubbish icon, 'Clear All'."),
  
  # selectizeInput(inputId = "Division",
  #                label = "Select Division",
  #                choices = sort(unique(Popdata$Division)), 
  #                selected = NULL,
  #                multiple = TRUE,
  #                width = '80%'),
  tags$div(tags$br()),
  #leafletOutput("map"),
  editModUI('editor'),
  tags$div(tags$br()),
  actionButton("save", "Display current shape"),
  # actionButton("screenshot", "Screenshot current map view"),
  tags$div(tags$br()),
  # selectizeInput(inputId = "selected_locations",
  #                label = "Selected SA1s:",
  #                choices = SA1$SA1_CODE21,
  #                selected = NULL,
  #                multiple = TRUE,
  #                width = '80%'),
  # tags$div(tags$br()),
  # actionButton("ClearSelection", "Clear all"),
  # tags$div(tags$br()),
  
  'List of SA1s being used',
  
  dataTableOutput("SA1.table"),
  'Total projected population of SA1s selected',
  tags$div(tags$br()),
  dataTableOutput("Total.table"),
  
  tags$footer(
    class = "footer",
    div(
      style = "max-width: 600px; display: inline-block; text-align: left",
      "Created by Kevin Chen. Please send feedback to kevinchen870@gmail.com"
    ),
    align = "center"
  )
)

#Server functions

server <- function(input, output, session){
  
  #create empty vector to hold all click ids
  selected_ids <- reactiveValues(ids = vector())
  
  
  #initial map output
  edits <- callModule(editMod, "editor",
      leaflet() %>%
      addTiles() %>%
      #Add divisions with deviation palette
      addPolygons(data= shp_CED, 
                  fillOpacity =0.2, fillColor = ~pal(shp_CED$deviation_2028),
                  weight = 5, stroke = TRUE, color = 'white',
                  group = 'Current Divisions', 
                  label = lapply(paste0("Division: ",
                                        str_to_title(shp_CED$CED_NAME21),'<br>',
                                        "Projected Population: ",
                                        shp_CED$tot_project,
                                        '<br>',
                                        "Deviation: ",shp_CED$deviation_2028),
                                 htmltools::HTML),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = FALSE)) %>%
      #Add SA2 base layer
      addPolygons(data = SA1,
                  fillColor = "white",
                  fillOpacity = 0.3,
                  color = "black",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~sa1_code_2021,
                  group = "SA1",
                  label = ~sa1_code_2021) %>%
        #Add clickable SA2s as a group 
        addPolygons(data = SA1,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~sa1_7code_2021,
                    group = ~sa1_code_2021) %>%
        #Add LGA base layer
        addPolygons(data = lga %>%filter(state_code_2021 == 2),
                    fillColor = "white",
                    fillOpacity = 0.3,
                    color = "blue",
                    stroke = TRUE,
                    weight = 4,
                    layerId = ~lga_code_2022,
                    group = "LGA",
                    label = ~lga_name_2022) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Current Divisions", "SA1", 'LGA'),
        options = layersControlOptions(collapsed = TRUE)) %>%
        #Hide clickable groups
        hideGroup(group = SA1$sa1_code_2021)  %>%
      #Add deviation legend
      addLegend(values = shp_CED$deviation_2028,position = 'bottomright',
                title = 'deviation from projected (%)',
                colors = c("#D53E4F", "#FC8D59", "#FEE08B", 
                           "#E6F598", "#99D594", "#3288BD"),
                labels = c('-8% - -6%',
                           '-6%- -4%',
                           '-4% - -2%',
                           '-2% - 0%',
                           '0% - 2%',
                           '2% - 4%')),
      editor = 'leaflet.extras',
      editorOptions =list(polylineOptions = FALSE,
                          circleOptions = FALSE,
                          rectangleOptions = FALSE,
                          markerOptions = FALSE,
                          circleMarkerOptions = FALSE))
  #define leaflet proxy for second level map
  proxy <- leafletProxy("editor-map")
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  
  observeEvent(input$save,{
    if(is.null(edits()$finished)){
      proxy %>% hideGroup(group = selected$groups)
      
    }
    req(edits()$finished)
    proxy %>% hideGroup(group = selected$groups)
    
    SA1_selected <- st_intersection(SA1,st_make_valid(edits()$finished))
    
    selected$groups = c(SA1_selected$sa1_code_2021)
    proxy %>% showGroup(group =  selected$groups)
    #Render SA1 table
  output$SA1.table = renderDataTable({
    SA1 %>% 
      filter(sa1_code_2021 %in% SA1_selected$sa1_code_2021) %>%
      select('SA1 Code' = sa1_code_2021,
             #'SA2 Name' = sa2_name_2021,
             'Current Division' = Division,
             'Projected Population' = tot_project) %>%
      st_drop_geometry(data_all) %>%
      datatable(style="bootstrap",
                selection = "multiple",
                rownames = F,
                extensions = 'Buttons',
                escape=FALSE,
                options = list(lengthChange = F,
                               scrollX = "auto",
                               dom = 'Bfrtip',
                               buttons = list( 
                                 list(extend = 'csv',   
                                      filename =  'SA2 selected'),
                                 list(extend = 'copy',text = 'Copy'))))
  }, server = FALSE)
  
  # observeEvent(input$screenshot, {
  #   screenshot(selector = "#editor-map",filename = 'CurrentMapView')
  # })

    output$Total.table = renderDataTable({
    SA1 %>% 
      filter(sa1_code_2021 %in% SA1_selected$sa1_code_2021) %>%
      mutate(Division = str_to_title(Division)) %>%
      st_drop_geometry(data_all) %>%
      summarise(tot_project = sum(tot_project)) %>%
      mutate(deviation = paste0(round((tot_project-127238)/127238 * 100,2),
                                '%')) %>%
      rename('Total selected projected population' =tot_project) %>%
      datatable(style="bootstrap",
                selection = "multiple",
                rownames = F,
                extensions = 'Buttons',
                escape=FALSE,
                options = list(lengthChange = F,
                               scrollX = "auto",
                               dom = 'Bfrtip',
                               buttons = c('copy')))
  })
  })
}
  

shinyApp(
  ui = ui,
  server <- server)
    
    
    
