
source(file="checkboxGroupInputW.R")
uiWolf <- tabPanel("Wolf",
         sidebarLayout(
           sidebarPanel(
             h4("Processing"),
             fluidRow(
               column(6,textInput("endTimeW", 'Integration Time', ""), align = "center"),
               column(6,textInput('timeStepW', 'Time Step', ""), align = "center")
             ),


             fluidRow(
               column(4,actionButton("runW", label = "Run"), align = "center"),
               column(4,actionButton("stopW", label = "Stop"), align = "center"),
               column(4,actionButton("resetW", label = "Reset"), align = "center")
             ),

             hr(),
             h4("Plot"),
             radioButtons("plotTypeW", label=NULL,
                          c("Lyapunov exponents"="exp", "Local Dky"="dky"), inline = TRUE),
             fluidRow(
               column(4,numericInput("printIterW", label="Refresh frequency", 50), align = "center")
               )



             ,

             hr(),
             
             fluidRow(
               column(4, h4("Statistics")),
               column(4,numericInput("lastIterW", label="Window width", 100), align = "center")
             ),
             h5("Window (min/max iterations)"),
             verbatimTextOutput("minMaxIterW"),
             
             h5("Mean values"),
             verbatimTextOutput("meanW"),
             h5("Standard deviations"),
             verbatimTextOutput("sdW")

           )

           ,
           conditionalPanel(condition = "input.runW > 0",
                            mainPanel(
                              checkboxExponentsWolf,
                              plotOutput("plotWolf1"),

                              fluidRow(
                                column(3, checkboxInput("autoScaleX1W", "Automatic X scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleX1W == false",
                                         actionButton("scaleApplyX1W", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleX1W == false",
                                  textInput("xMax1W", label="Max.", 0),
                                  textInput("xMin1W", label="Min.", 0)
                                )),
                                column(3, checkboxInput("autoScaleY1W", "Automatic Y scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleY1W == false",
                                         actionButton("scaleApplyY1W", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleY1W == false",
                                  textInput("yMax1W", label="Max.", 1),
                                  textInput("yMin1W", label="Min.", 0)

                                ))
                              ),

                              plotOutput("plotWolf2"),
                              fluidRow(
                                column(3, checkboxInput("autoScaleX2W", "Automatic X scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleX2W == false",
                                         actionButton("scaleApplyX2W", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleX2W == false",
                                  textInput("xMax2W", label="Max.", 0),
                                  textInput("xMin2W", label="Min.", 0)
                                )),
                                column(3, checkboxInput("autoScaleY2W", "Automatic Y scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleY2W == false",
                                         actionButton("scaleApplyY2W", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleY2W == false",
                                  textInput("yMax2W", label="Max.", 1),
                                  textInput("yMin2W", label="Min.", 0)

                                ))
                              ),

                              # third graph

                              plotOutput("plotWolf3"),
                              fluidRow(
                                column(3, checkboxInput("autoScaleX3W", "Automatic X scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleX3W == false",
                                         actionButton("scaleApplyX3W", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleX3W == false",
                                  textInput("xMax3W", label="Max.", 0),
                                  textInput("xMin3W", label="Min.", 0)
                                )),
                                column(3, checkboxInput("autoScaleY3W", "Automatic Y scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleY3W == false",
                                         actionButton("scaleApplyY3W", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleY3W == false",
                                  textInput("yMax3W", label="Max.", 1),
                                  textInput("yMin3W", label="Min.", 0)

                                ))
                              )




                            )
           )
         )
)

