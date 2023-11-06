library(sf)
library(raster)
library(dplyr)
library(leaflet)
library(openxlsx)
library(stringr)
library(shiny)
library(DT)
library(janitor)
install.packages('rlang')
devtools::install_github("mattcowgill/strayr")


Popdata = read.xlsx("Victoria-SA1.xlsx") %>%
  filter(Division != 'VIC TOTAL') %>%
  mutate(SA1_CODE21 = as.character(`Statistical.Area.Level.1.(SA1).(2021.SA1s)`)) %>%
  rename(tot_project = Projected.enrolment.Monday.17.April.2028)

SA1 = read_sf("shapefile/SA1_2021_AUST_GDA2020.shp") %>%
  subset(STE_CODE21 == '2') %>%
  left_join(Popdata, by = 'SA1_CODE21') %>%
  rename(SA1_7CODE21 =`Statistical.Area.Level.1.(SA1).Code.(7-digit).(2021.SA1s)` )




Pop_data_summary_SA2 = Popdata %>%
  group_by(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`,
           `Statistical.Area.Level.2.(SA2).Name`,
           Division) %>%
  summarise(tot_current = sum(Actual.enrolment.Wednesday.9.August.2023),
            tot_project = sum(tot_project)) %>%
  mutate(Division = str_to_lower(Division),
         SA2_CODE21 = as.character(`Statistical.Area.Level.2.(SA2).Code.(2021.SA2s)`)) %>%
  setNames(c('SA2.Code','SA2.Name','Division','tot_current', 'tot_project','SA2_CODE21'))


SA2 = read_sf("shapefile/SA2_2021_AUST_GDA2020.shp") %>%
  subset(STE_CODE21 == '2') %>%
  left_join(Pop_data_summary_SA2, by = c('SA2_CODE21' = 'SA2_CODE21'))


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

shp_CED = read_sf("shapefile/CED_2021_AUST_GDA2020.shp") %>%
  subset(STE_CODE21 == '2') %>%
  mutate(CED_NAME21 = tolower(CED_NAME21)) %>%
  left_join(Pop_data_summary_CED, by = c('CED_NAME21' = 'Division')) %>%
  filter(AREASQKM21 >0)

pal <- colorBin(
  palette = "Spectral",
  domain = Pop_data_summary_CED$deviation_2028,
  bins = 5)



shinyApp(
  ui = fluidPage(
    
    titlePanel('Redistribution Tool v0.1'),
    "Click on map to build from SA2",
    leafletOutput("map"),
    "Select SA2s from list",
    selectizeInput(inputId = "selected_locations",
                   label = "Selected:",
                   choices = SA1$SA1_CODE21,
                   selected = NULL,
                   multiple = TRUE),
    
    'List of SA2s being used',
    dataTableOutput("SA2.table"),
    'Total projected population of SA2s selected',
    dataTableOutput("Total.table"),
    
    tags$footer(
      class = "footer",
      div(
        style = "max-width: 600px; display: inline-block; text-align: left",
        "Created by Kevin Chen"
      ),
      align = "center"
    )
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data= shp_CED, 
                    fillOpacity =0.8, fillColor = ~pal(shp_CED$deviation_2028),
                    weight = 5, stroke = TRUE, color = 'white',
                    group = 'Current Divisions', 
                    popup = paste0("Division: ",shp_CED$CED_NAME21,'<br>',
                                   "Projected Population: ",shp_CED$tot_project,'<br>',
                                   "Deviation: ",shp_CED$deviation_2028,'<br>',
                                   "Projection: ",shp_CED$within_projection),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = FALSE)) %>%
        addPolygons(data = SA1,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~SA1_CODE21,
                    group = "SA1",
                    label = ~SA1_CODE21) %>%
        addPolygons(data = SA1,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~SA1_7CODE21,
                    group = ~SA1_CODE21) %>%
        # Layers control
        addLayersControl(
          overlayGroups = c("Current Divisions", "SA1"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(group = SA1$SA1_CODE21) # nc$CNTY_ID
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "SA1"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy %>% showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           choices = SA1$SA1_CODE21,
                           selected = selected$groups,
                           server = TRUE)
    })
    
    observeEvent(input$selected_locations, {
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }, 
    
    ignoreNULL = FALSE)
    output$SA2.table = renderDataTable({
      SA1 %>% 
        filter(SA1_CODE21 %in% selected$groups) %>%
        mutate(Division = str_to_title(Division)) %>%
        select('SA1 Code' = SA1_CODE21, 'SA2 Code' = SA2_CODE21,
               Division,
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
                                 buttons = c('csv', 'copy')))
    })
    
    output$Total.table = renderDataTable({
      SA1 %>%
        filter(SA1_CODE21 %in% selected$groups) %>%
        mutate(Division = str_to_title(Division)) %>%
        st_drop_geometry(data_all) %>%
        summarise(tot_project = sum(tot_project)) %>%
        mutate(deviation = paste0(round((tot_project-127238)/127238 * 100,2),'%')) %>%
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
