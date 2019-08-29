library(rglwidget)
library(shinyjs)
library(GPoM.FDLyapu)
source("uiWolf.R", local = TRUE)
source("uiGrond.R", local = TRUE)
source("ui3DWolf.R", local = TRUE)
source("ui3DGrond.R", local = TRUE)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("FDLyapu"),
  fluidRow(column(6, fileInput(
    "file", 'Input filename'
  )),
  column(6, fluidRow(
    column(6, NULL),
    column(
      3,
      textInput("outFileName", label = "Output filename"),
      downloadButton("downloadData", "Save results")
    )
  ))),
  
  
  fluidRow(tabsetPanel(
    uiWolf,
    uiGrond,
    ui3DWolf,
    ui3DGrond,
    
    tabPanel("Equations",
             sidebarLayout(
               sidebarPanel(
                 h4("Equations"),
                 verbatimTextOutput("equations"),
                 radioButtons(
                   "substitButton",
                   "Display mode",
                   c(
                     "x, y,..." = "1",
                     "X1, X2,..." = "0",
                     "X_1, X_2, ..." = "2"
                   ),
                   inline = TRUE
                 ),
                 radioButtons(
                   "precisionButton",
                   "Precision",
                   c("Default" = "Default",
                     "Custom" = "Custom"),
                   inline = TRUE
                 ),
                 conditionalPanel(
                   condition = "input.precisionButton == \"Custom\"",
                   sliderInput("approxSlider", "Additional digits", 2, 10, 2)
                   
                 )
               ),
               mainPanel()
             ))
    
  ))
))
