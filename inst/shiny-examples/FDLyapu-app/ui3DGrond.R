ui3DGrond <- tabPanel(
  "3DGrond",
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4, textInput(
          "Axe13DG", label = "Axis 1", value = "1"
        )),
        column(4, textInput(
          "Axe23DG", label = "Axis 2", value = "2"
        )),
        column(4, textInput(
          "Axe33DG", label = "Axis 3", value = "3"
        ))
      ),
      fluidRow(column(4, NULL),
               column(4, column(
                 2,
                 actionButton("display3DG", label = "Display", align = "center")
               ))),
      textOutput("text3DG"),
      
      hr(),
      fluidRow(
        column(
          3,
          numericInput(
            "undersamp3DG",
            label = "Undersampling ratio",
            value = 1,
            min = 1
          )
        ),
        column(
          3,
          textInput("LyapInd3DG", label = "Lyapunov exponent index", value = "1")
        ),
        column(3, h4("Min"), verbatimTextOutput("minExpG")),
        column(3, h4("Max"), verbatimTextOutput("maxExpG"))
      ),
      h4("3D palette definition"),
      
      checkboxInput("autoScale3DG", "Automatic scale", value = TRUE),
      conditionalPanel(
        condition = "input.autoScale3DG == false",
        fluidRow(
          column(4, numericInput("colour1G", label = "Minimum", 0)),
          column(4, numericInput("colour2G", label = "Maximum", 10)),
          column(4, numericInput("nbColoursG", label = "Number of colours", 20))
        ),
        radioButtons(
          "paletteButtonG",
          "PaletteG",
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
      
      
      
      
      plotOutput("paletteG")
    ),
    
    mainPanel(
      fluidRow( 
        column(6, rglwidgetOutput("plot3DGrond1")),
        column(6, 
               br(),
               br(),
               sliderInput("xRotationG", "X rotation",-90, 90, 0),
               sliderInput("yRotationG", "Y rotation",-90, 90, 0),
               sliderInput("zRotationG", "Z rotation",-90, 90, 0)),
               fluidRow(column(4, br()),
                        column(4, actionButton("reset3DG", "Reset")),
                        column(4, br())
               )
      )
      


    )
  )
)