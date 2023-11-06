#Server functions

server <- function(input, output, session){
  
  #create empty vector to hold all click ids
  selected_ids <- reactiveValues(ids = vector())
  
  #initial map output
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #Add divisions with deviation palette
      addPolygons(data= shp_CED, 
                  fillOpacity =0.8, fillColor = ~pal(shp_CED$deviation_2028),
                  weight = 5, stroke = TRUE, color = 'white',
                  group = 'Current Divisions', 
                  label = lapply(paste0("Division: ",shp_CED$CED_NAME21,'<br>',
                                 "Projected Population: ",shp_CED$tot_project,
                                 '<br>',
                                 "Deviation: ",shp_CED$deviation_2028),
                                 htmltools::HTML),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = FALSE)) %>%
      #Add SA2 base layer
      addPolygons(data = SA2,
                  fillColor = "white",
                  fillOpacity = 0.5,
                  color = "black",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~sa2_name_2021,
                  group = "SA2",
                  label = ~sa2_name_2021) %>%
      #Add clickable SA2s as a group 
      addPolygons(data = SA2,
                  fillColor = "red",
                  fillOpacity = 0.5,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~sa2_code_2021,
                  group = ~sa2_name_2021) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Current Divisions", "SA2"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      #Hide clickable groups
      hideGroup(group = SA2$sa2_name_2021)  %>%
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
                           '2% - 4%'))
  }) #END RENDER LEAFLET
  
  #define leaflet proxy for second level map
  proxy <- leafletProxy("map")
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  #Observe clicked event
  observeEvent(input$map_shape_click, {
    #If clicked on SA2 layer
    if(input$map_shape_click$group == "SA2"){
      selected$groups <- c(selected$groups, input$map_shape_click$id)
      proxy %>% showGroup(group = input$map_shape_click$id)
    } 
    #If base division layer then do nothing
    else if (input$map_shape_click$group =='Current Divisions') {
      NULL
      } 
    #see if any difference
    else {
      selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
      proxy %>% hideGroup(group = input$map_shape_click$group)
    }
    #update selected inputs
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         choices = SA2$sa2_name_2021,
                         selected = selected$groups,
                         server = TRUE)
  })
  #observe screenshot events
  observeEvent(input$screenshot, {
    screenshot(id = 'map',
               filename = 'map screenshot')
  })
  
  #observe clear selection event
  observeEvent(input$ClearSelection, {
    #update selected inputs
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         choices = SA2$sa2_name_2021,
                         selected = NULL,
                         server = TRUE)
  })
  
  
  #observe selector events
  observeEvent(input$selected_locations, {
    removed_via_selectInput <- setdiff(selected$groups, 
                                       input$selected_locations)
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
  
  #Data tables of list of SA2s
  output$SA2.table = renderDataTable({
    SA2 %>% 
      filter(sa2_name_2021 %in% selected$groups) %>%
      mutate(Division = str_to_title(Division)) %>%
      select('SA2 Code' = sa2_code_2021, 'SA2 Name' = sa2_name_2021,
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
  
  #Data table of total
  output$Total.table = renderDataTable({
    SA2 %>% 
      filter(sa2_name_2021 %in% selected$groups) %>%
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
  
}
