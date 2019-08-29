
source(file="checkboxGroupInputG.R")
uiGrond <- tabPanel("Grond",
         sidebarLayout(
           sidebarPanel(
             h4("Processing"),
             fluidRow(
               column(6,textInput("endTimeG", 'Integration Time', ""), align = "center"),
               column(6,textInput('timeStepG', 'Time Step', ""), align = "center")
             ),


             fluidRow(
               column(4,actionButton("runG", label = "Run"), align = "center"),
               column(4,actionButton("stopG", label = "Stop"), align = "center"),
               column(4,actionButton("resetG", label = "Reset"), align = "center")
             ),

             hr(),
             h4("Plot"),
             radioButtons("plotTypeG", label=NULL,
                          c("Lyapunov exponents"="exp", "Local Dky"="dky"), inline = TRUE),
             fluidRow(
               column(4,numericInput("printIterG", label="Refresh frequency", 50), align = "center")
               )



             ,

             hr(),
             h4("Statistics"),
             
             h5("Mean values"),
             verbatimTextOutput("meanG"),
             h5("Standard deviations"),
             verbatimTextOutput("sdG"),
             h5("Window (min/max iterations)"),
             verbatimTextOutput("minMaxIterG"),
             fluidRow(
               column(4,numericInput("lastIterG", label="Window width", 100), align = "center")
             )

           )

           ,
           conditionalPanel(condition = "input.runG > 0",
                            mainPanel(
                              checkboxExponentsGrond,
                              plotOutput("plotGrond1"),

                              fluidRow(
                                column(3, checkboxInput("autoScaleX1G", "Automatic X scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleX1G == false",
                                         actionButton("scaleApplyX1G", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleX1G == false",
                                  textInput("xMax1G", label="Max.", 0),
                                  textInput("xMin1G", label="Min.", 0)
                                )),
                                column(3, checkboxInput("autoScaleY1G", "Automatic Y scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleY1G == false",
                                         actionButton("scaleApplyY1G", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleY1G == false",
                                  textInput("yMax1G", label="Max.", 1),
                                  textInput("yMin1G", label="Min.", 0)

                                ))
                              ),

                              plotOutput("plotGrond2"),
                              fluidRow(
                                column(3, checkboxInput("autoScaleX2G", "Automatic X scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleX2G == false",
                                         actionButton("scaleApplyX2G", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleX2G == false",
                                  textInput("xMax2G", label="Max.", 0),
                                  textInput("xMin2G", label="Min.", 0)
                                )),
                                column(3, checkboxInput("autoScaleY2G", "Automatic Y scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleY2G == false",
                                         actionButton("scaleApplyY2G", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleY2G == false",
                                  textInput("yMax2G", label="Max.", 1),
                                  textInput("yMin2G", label="Min.", 0)

                                ))
                              ),

                              # third graph

                              plotOutput("plotGrond3"),
                              fluidRow(
                                column(3, checkboxInput("autoScaleX3G", "Automatic X scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleX3G == false",
                                         actionButton("scaleApplyX3G", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleX3G == false",
                                  textInput("xMax3G", label="Max.", 0),
                                  textInput("xMin3G", label="Min.", 0)
                                )),
                                column(3, checkboxInput("autoScaleY3G", "Automatic Y scale", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.autoScaleY3G == false",
                                         actionButton("scaleApplyY3G", label = "Apply scale")
                                       )
                                ),
                                column(3, conditionalPanel(
                                  condition = "input.autoScaleY3G == false",
                                  textInput("yMax3G", label="Max.", 1),
                                  textInput("yMin3G", label="Min.", 0)

                                ))
                              )




                            )
           )
         )
)

