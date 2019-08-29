ui3DWolf <- tabPanel(
  "3DWolf",
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4, textInput(
          "Axe13DW", label = "Axis 1", value = "1"
        )),
        column(4, textInput(
          "Axe23DW", label = "Axis 2", value = "2"
        )),
        column(4, textInput(
          "Axe33DW", label = "Axis 3", value = "3"
        ))
      ),
      fluidRow(column(4, NULL),
               column(4, column(
                 2,
                 actionButton("display3DW", label = "Display", align = "center")
               ))),
      textOutput("text3DW"),
      
      hr(),
      fluidRow(
        column(
          3,
          numericInput(
            "undersamp3DW",
            label = "Undersampling ratio",
            value = 1,
            min = 1
          )
        ),
        column(
          3,
          textInput("LyapInd3DW", label = "Lyapunov exponent index", value = "1")
        ),
        column(3, h4("Min"), verbatimTextOutput("minExpW")),
        column(3, h4("Max"), verbatimTextOutput("maxExpW"))
      ),
      h4("3D palette definition"),
      
      checkboxInput("autoScale3DW", "Automatic scale", value = TRUE),
      conditionalPanel(
        condition = "input.autoScale3DW == false",
        fluidRow(
          column(4, numericInput("colour1W", label = "Minimum", 0)),
          column(4, numericInput("colour2W", label = "Maximum", 10)),
          column(4, numericInput("nbColoursW", label = "Number of colours", 20))
        ),
        radioButtons(
          "paletteButtonW",
          "PaletteW",
          c(
            "Rainbow" = "rainbow",
            "Terrain" = "terrain",
            "Heat" = "heat",
            "Topo" = "topo"
          ),
          inline = TRUE
        )
      )
      
      ,
      
      
      
      
      plotOutput("paletteW")
    ),
    
    mainPanel(
      fluidRow( 
        column(6, rglwidgetOutput("plot3DWolf1")),
        column(6, 
               br(),
               br(),
               sliderInput("xRotationW", "X rotation",-90, 90, 0),
               sliderInput("yRotationW", "Y rotation",-90, 90, 0),
               sliderInput("zRotationW", "Z rotation",-90, 90, 0)),
               fluidRow(column(4, br()),
                        column(4, actionButton("reset3DW", "Reset")),
                        column(4, br())
               )
      )
      


    )
  )
)