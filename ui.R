#Define UI of app
ui = fluidPage(
  
  titlePanel('Redistribution Tool v0.1'),
  "Click on map to build from SA2",
  
  leafletOutput("map"),
  tags$div(tags$br()),
  actionButton("screenshot", "Save current map view"),
  tags$div(tags$br()),
  selectizeInput(inputId = "selected_locations",
                 label = "Selected SA2s:",
                 choices = SA2$SA2_NAME21,
                 selected = NULL,
                 multiple = TRUE,
                 width = '80%'),
  tags$div(tags$br()),
  actionButton("ClearSelection", "Clear all"),
  tags$div(tags$br()),
  
  'List of SA2s being used',
  
  dataTableOutput("SA2.table"),
  'Total projected population of SA2s selected',
  tags$div(tags$br()),
  dataTableOutput("Total.table"),
  
  tags$footer(
    class = "footer",
    div(
      style = "max-width: 600px; display: inline-block; text-align: left",
      "Created by Kevin Chen November 2023"
    ),
    align = "center"
  )
)