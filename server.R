#Server functions

server <- function(input, output) {
  #template
  output$downloadTemplate <- downloadHandler(
    filename = 'SA1 to Division mapping template.csv',
    content = function(file) {
      file.copy('files/blank SA1 template.csv', file)
    })
  
  
  basemap <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = lga %>% filter(state_code_2021 == 2),
                fillColor = "white",
                fillOpacity = 0,
                color = "yellow",
                stroke = TRUE,
                weight = 2,
                group = "LGA",
                layerId = ~lga$lga_code_2022,
                label = ~lga_name_2022) %>%
    addPolygons(data = SA2 %>% filter(state_code_2021 == 2,
                                      !is.na(cent_lat)),
                fillColor = "white",
                fillOpacity = 0,
                color = "green",
                stroke = TRUE,
                weight = 1,
                group = "SA2",
                layerId = ~SA2$sa2_code_2021,
                label = SA2$SA2_code_2021) %>%
    addPolygons(data = New_Divisions_May %>% sf::st_zm(),
                fillColor = 'white',
                fillOpacity = 0,
                color = "black",
                stroke = TRUE,
                weight = 2,
                group = "Proposed Divisions",
                layerId = ~New_Divisions_May$E_div_numb,
                label = lapply(paste0("Division: ",
                                      str_to_title(New_Divisions_May$Sortname),'<br>',
                                      "Projected Population: ",
                                      New_Divisions_May$Projected,'<br>',
                                      "Deviation: ",
                                      round((New_Divisions_May$Projected-127238)/127238 * 100,2),
                                      '%'),
                               htmltools::HTML))  %>%
    addPolygons(data= shp_CED, 
                fillOpacity =0.0,
                weight = 2, stroke = TRUE, 
                color = 'red',
                group = 'Current Divisions', 
                layerId = ~shp_CED$ced_code_2021,
                label = lapply(paste0("Division: ",
                                      str_to_title(shp_CED$ced_name_2021),'<br>',
                                      "Projected Population: ",
                                      shp_CED$tot_project),
                               htmltools::HTML)) %>%
    addLayersControl(
      overlayGroups = c("Current Divisions", 'LGA',"Proposed Divisions", 'SA2'),
      options = layersControlOptions(collapsed = TRUE)) %>%
    addFullscreenControl()
  
  output$SA1mapping <- renderLeaflet({basemap})
  
  observe({
    req(input$userSA1upload)
    SA1_user <- read.csv(input$userSA1upload$datapath,
                         colClasses = c(rep('character',5))) %>%
      setNames(c('sa1_7_digit_code','sa1_code_2021',
                 'Current Division','Proposed Division', 'Your Division Name')) %>%
      filter(!is.na(`Your Division Name`) & trimws(`Your Division Name`) !="") %>%
      left_join(SA1, by = 'sa1_code_2021') %>%
      group_by(`Your Division Name`) %>%
      summarise(projected = sum(tot_project),
                geometry = st_union(geometry)) %>%
      st_as_sf()
    
    proxy <- leafletProxy("SA1mapping") 
    
    proxy %>% 
      addPolygons(data = SA1_user,
                  fillColor = 'white',
                  fillOpacity = 0,
                  color = "blue",
                  stroke = TRUE,
                  weight = 4,
                  group = "Your Divisions",
                  layerId =  ~SA1_user$`Your Division Name`,
                  label = lapply(paste0("Your Division: ",
                                        str_to_title(SA1_user$`Your Division Name`),'<br>',
                                        "Projected Population: ",
                                        SA1_user$projected,'<br>',
                                        "Deviation: ",
                                        round((SA1_user$projected-127238)/127238 * 100,2),
                                        '%'),
                                 htmltools::HTML)) %>%
      addLayersControl(
        overlayGroups = c("Current Divisions", 'LGA',"Proposed Divisions",'SA2', 'Your Divisions'),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Current Divisions") %>%
      hideGroup("LGA") %>%
      hideGroup("Proposed Divisions") %>%
      hideGroup('SA2')
  })
  
  
  
  #create empty vector to hold all click ids
  selected_ids <- reactiveValues(ids = vector())
  
  
  #initial map output
  edits <- callModule(editMod, "editor",
                      
                      
                      leaflet() %>%
                        addTiles() %>%
                        #Add divisions with deviation palette
                        addPolygons(data= shp_CED, 
                                    fillOpacity =0.0, fillColor = ~pal(shp_CED$deviation_2028),
                                    weight = 3, stroke = TRUE, color = 'red',
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
                                    fillOpacity = 0,
                                    color = "green",
                                    stroke = TRUE,
                                    weight = 2,
                                    #layerId = ~lga_code_2022,
                                    group = "LGA",
                                    label = ~lga_name_2022) %>%
                        addPolygons(data = New_Divisions_May%>% sf::st_zm(),
                                    fillColor = 'white',
                                    fillOpacity = 0,
                                    color = "blue",
                                    stroke = TRUE,
                                    weight = 3,
                                    #layerId = ~Sortname,
                                    group = "Proposed Divisions",
                                    label = lapply(paste0("Division: ",
                                                          str_to_title(New_Divisions_May$Sortname),'<br>',
                                                          "Projected Population: ",
                                                          New_Divisions_May$Projected),
                                                   htmltools::HTML)) %>%
                        # Layers control
                        addLayersControl(
                          overlayGroups = c("Current Divisions", "SA1", 'LGA',"Proposed Divisions"),
                          options = layersControlOptions(collapsed = TRUE)) %>%
                        addFullscreenControl() %>%
                        #Hide clickable groups
                        hideGroup(group = SA1$sa1_code_2021), #%>%
                      #Add deviation legend
                      # addLegend(values = shp_CED$deviation_2028,position = 'bottomright',
                      #           title = 'deviation from projected (%)',
                      #           colors = c("#D53E4F", "#FC8D59", "#FEE08B", 
                      #                      "#E6F598", "#99D594", "#3288BD"),
                      #           labels = c('-8% - -6%',
                      #                      '-6%- -4%',
                      #                      '-4% - -2%',
                      #                      '-2% - 0%',
                      #                      '0% - 2%',
                      #                      '2% - 4%')),
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
               'Current Division' = Current.Division,
               'Proposed Division' = Proposed.Division,
               'Projected Population' = tot_project) %>%
        st_drop_geometry(data_all) %>%
        datatable(style="bootstrap",
                  selection = "multiple",
                  rownames = F,
                  extensions = 'Buttons',
                  escape=FALSE,
                  options = list(lengthChange = F,
                                 scrollY = "true",
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
        mutate(Division = str_to_title(Current.Division)) %>%
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
                                 scrollY = "true",
                                 dom = 'Bfrtip',
                                 buttons = c('copy')))
    })
  })
}