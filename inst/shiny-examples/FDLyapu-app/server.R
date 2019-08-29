# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(rgl)
library(GPoM)
library(GPoM.FDLyapu)

shinyServer(function(input, output, session) {


  outL <<- list()
  
  outL$Wolf <<- NULL
  calcTimeW <<- reactiveValues(start = 0, end = 0, stop = 0)
  yDkyW <<- reactiveValues(min=NULL, max=NULL)
  yLyapW1 <<- reactiveValues(min=NULL, max=NULL)
  yLyapW2 <<- reactiveValues(min=NULL, max=NULL)
  yLyapW3 <<- reactiveValues(min=NULL, max=NULL)
  tStartW <<- 0.

  
  outL$Grond <<- NULL
  calcTimeG <<- reactiveValues(start = 0, end = 0, stop = 0)
  yDkyG <<- reactiveValues(min=NULL, max=NULL)
  yLyapG1 <<- reactiveValues(min=NULL, max=NULL)
  yLyapG2 <<- reactiveValues(min=NULL, max=NULL)
  yLyapG3 <<- reactiveValues(min=NULL, max=NULL)
  tStartG <<- 0.
  
  source("load.R", local = TRUE)

  source("serverGrond.R", local = TRUE)
  source("serverWolf.R", local = TRUE)
  
  source("server3DWolf.R", local = TRUE)
  source("server3DGrond.R", local = TRUE)
  
  output$equations <- renderPrint({
    if (!is.null(input$file)) {
      substit <- switch(input$substitButton,
                     "0" = 0,
                     "1" = 1,
                     "2" = 2)
      visuEq(KL, substit = substit, approx=input$approxSlider)      
    }



  })
  ############################################ OUTPUTS #################################################################
  
  # output save
  output$downloadData<- downloadHandler(
    filename = function() {
      input$outFileName
    },
    content = function(file) {
      save(outL, file = file)
    }
  )
})

