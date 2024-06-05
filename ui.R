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
  )),
  tabPanel(
    title = 'User Guide',
    tags$head(includeCSS("styles.css")),
           style = "height: 90vh; overflow-y: auto;text-align: center; overflow-x: auto;",
    div(tags$br(),tags$br(),
        h3("Create your own map"),
        'Please download the template from the link in the sidepanel'),
        tags$img(src="Download template.png"),
        tags$br(),
        div('Then put your division names in the corresponding column.'),
        tags$br(),
        HTML('<b>There is no need to change any other part of the CSV.</b>'),
        tags$br(),
        HTML('<b>Please ensure that all your SA1s border each other as the app will dissolve all internal boundaries.</b>'),
        tags$br(),
        tags$img(src = 'Example CSV upload.png'),
        tags$br(),
        div('Finally upload your file to the application. Your boundaries should automatically appear in blue. You can then include other boundaries if required'),
        tags$br(),
        tags$img(src='Your division final.png'),
        tags$br(),
        h3("Building from SA1"),
    "Click on the polygon to build from SA1.",
    tags$br(),
    tags$img(src = 'draw polygon.png' ),
    tags$br(),
    'Create points to cover the SA1s your division will be. Press finish once happy.',
    tags$br(),
    tags$img(src = 'Completed Polygon.png'),
    tags$br(),
    "Then press display current shape to see the shape highlighted as well as the SA1s used and projected total.",
    tags$br(),
    tags$img(src = 'Display Shape.png'),
    tags$br(),
    "To edit the points, click on the edit layers tool and move the points of the shape. Once happy, save and press display shape to update the highlighted area", 
    tags$br(),
    tags$img(src = 'edit shape.png'),
    tags$br(),
    "Before beginning a new electorate, please clear the current shape by clicking 'Clear All' in the delete layers icon.",
    tags$img(src = 'clear all.png')),
  tags$footer(
    class = "footer",
    div(
      "Created by Kevin Chen. Please send feedback to kevinchen870@gmail.com"
    ),
    align = "center"
  ))