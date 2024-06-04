#Define UI of app

ui = navbarPageWithLogo(
  title = "Redistribution Tool",
  inverse = T,
  position = "fixed-top",
  tabPanel(
    "Create own Division Map",
    tags$head(includeCSS("styles.css")),
    tags$br(),
    tags$div(
      "Please allow a minute for the map to load up.",
      tags$br()),
    sidebarPanel(
      div("Download template to enter your divisions mapped to SA1s"),
      tags$br(),
      downloadLink("downloadTemplate", "Download Template"),
      tags$br(),
      tags$div('Upload completed template'),
      fileInput("userSA1upload", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$div(tags$br())),
    
    mainPanel(
      tags$br(),
      tags$div('Please allow a minute for the map to load'),
      tags$br(),
      tags$style(type = "text/css", "#SA1mapping {width:100; height: calc(100vh - 240px) !important;}"),
      fillPage(leafletOutput("SA1mapping"))
    )
  ),
  
  tabPanel(
    title = 'Build From SA1',
    tags$head(includeCSS("styles.css")),
    style = "height: 90vh; overflow-y: auto;",
    tags$br(),
    tags$br(),
    tags$br(),
    tags$div(
      "Please allow a minute for the map to load up.",
      tags$br(),
      "You can toggle the SA1, council boundary and current divisions on and off.",
      tags$br(),
      "Click on the polygon to build from SA1. Press finished on the polygon, and then display data to see the table and projected total.",
      tags$br(),
      "To edit the points, click on the edit layers tool and move the points of the box.", 
      tags$br(),
      "Before beginning a new electorate or area, Please clear the current shape by clicking 'Clear All' in the delete layers icon."),
    tags$div(tags$br()),
    editModUI('editor'),
    tags$div(tags$br()),
    actionButton("save", "Display current shape"),
    # actionButton("screenshot", "Screenshot current map view"),
    tags$div(tags$br()),
    
    'List of SA1s being used',
    dataTableOutput("SA1.table"),
    'Total projected population of SA1s selected',
    tags$div(tags$br()),
    dataTableOutput("Total.table")
  ),
  tags$footer(
    class = "footer",
    div(
      "Created by Kevin Chen. Please send feedback to kevinchen870@gmail.com"
    ),
    align = "center"
  )
)
