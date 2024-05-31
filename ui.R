#Define UI of app
ui = fluidPage(
  
  titlePanel('Redistribution Tool v0.2'),
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
