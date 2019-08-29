

expListNameG = c(
  "exp1G",
  "exp2G",
  "exp3G",
  "exp4G",
  "exp5G",
  "exp6G",
  "exp7G",
  "exp8G",
  "exp9G",
  "exp10G"
)


expLG <- reactiveValues(x = NULL)
observe({
  input$runG
  expLG$x = c(
    input$exp1G,
    input$exp2G,
    input$exp3G,
    input$exp4G,
    input$exp5G,
    input$exp6G,
    input$exp7G,
    input$exp8G,
    input$exp9G,
    input$exp10G
  )
  
})


observe({
  loadedFile = !is.null(input$file) && input$file != ""
  shinyjs::toggleState("runG", loadedFile)
  shinyjs::toggleState("stopG", loadedFile)
  shinyjs::toggleState("resetG", loadedFile)
  if (loadedFile) {
    for (i in seq(1, nVar)) {
      shinyjs::enable(expListNameG[i])
      updateCheckboxInput(session, expListNameG[i], value = TRUE)
    }
    for (i in seq(nVar + 1, length(expListNameG))) {
      updateCheckboxInput(session, expListNameG[i], value = FALSE)
    }
    
    isolate({
      outL$Grond <<- NULL
      calcTimeG$start = 0
      calcTimeG$end = 0
      calcTimeG$stop = 1
      yDkyG$min = NULL
      yDkyG$max = NULL
      yLyapG1$min = NULL
      yLyapG1$max = NULL
      yLyapG2$min = NULL
      yLyapG2$max = NULL
      yLyapG3$min = NULL
      yLyapG3$max = NULL
      tStartG <<- 0.
      
    })
    
  }
})


observe({
  shinyjs::toggleState("display3DG", condition = (input$runG > 0))
})


#  DISABLES THE CHECKBOXINPUTS FOR THE EXPONENTS TO DISPLAY FOR INDICES > nVar
observeEvent(input$file, {
  if (!is.null(input$file) && input$file != "") {
    for (i in seq(nVar + 1, length(expListNameG))) {
      shinyjs::disable(expListNameG[i])
      updateCheckboxInput(session, expListNameG[i], value = FALSE)
    }
  }
})


# ENABLES/DISABLES THE CHECKBOXINPUTS FOR THE EXPONENTS TO DISPLAY (ACCORDING TO THE NUMBER OF VARIABLES)
observeEvent(input$plotTypeG, {
  if (!is.null(input$file) && input$file != "") {
    if (input$plotTypeG == "exp") {
      for (i in seq(1, nVar)) {
        shinyjs::enable(expListNameG[i])
      }
    }
    
    
    else {
      for (i in seq(1, nVar)) {
        shinyjs::disable(expListNameG[i])
      }
    }
  }
})


# RESET VALUES
observeEvent(input$resetG, {
  isolate({
    outL$Grond <<- NULL
    calcTimeG$start = 0
    calcTimeG$end = 0
    calcTimeG$stop = 1
    yDkyG$min = NULL
    yDkyG$max = NULL
    yLyapG1$min = NULL
    yLyapG1$max = NULL
    yLyapG2$min = NULL
    yLyapG2$max = NULL
    yLyapG3$min = NULL
    yLyapG3$max = NULL
    tStartG <<- 0.
    shinyjs::toggleState("display3DG")
  })
})


# calcul Grond
observe({
  tFinal <- pEndTimeGrond()
  
  deltat <- isolate(input$printIterG) * pTimeStepGrond()
  isolate({
    if (calcTimeG$start == 0) {
      calcTimeG$start <- tStartG
    }
    calcTimeG$end <- calcTimeG$start + deltat
    
    outL$Grond <<- lyapFDGrond(
      outL$Grond,
      nVar = nVar,
      dMax = dMax,
      KL,
      intgrMthod = intgrMthod,
      tDeb = calcTimeG$start,
      dt = pTimeStepGrond(),
      tFin = calcTimeG$end,
      yDeb = yDeb,
      nIterMin = nIterMin,
      nIterStats = xylim2$last
    )
    calcTimeG$start <- calcTimeG$end
  })
  if ((isolate(calcTimeG$start) < pEndTimeGrond()) &
      (isolate(calcTimeG$stop) == 0)) {
    invalidateLater(0, session)
  }
  
})

output$plotGrond1 <- renderPlot({
  if (calcTimeG$start > 0) {
    plotLocalExponents(
      nVar,
      outL$Grond,
      xlim = xylim1$x,
      ylim = xylim1$y,
      plotType = input$plotTypeG,
      dt = pTimeStepGrond(),
      expList = expLG$x,
      legend = input$legendG
    )
    
    
  }
  
})

# parametres calcul Grond
pTimeStepGrond <- eventReactive(input$runG, {
  calcTimeG$stop = 0
  as.numeric(input$timeStepG)
})
pEndTimeGrond <- eventReactive(input$runG, {
  calcTimeG$stop = 0
  as.numeric(input$endTimeG)
})

observeEvent(input$stopG, {
  calcTimeG$stop = 1
})

observeEvent(input$runG, {
  calcTimeG$stop = 0
})


             
             
               
               observeEvent(input$scaleApplyY1G, {
                 if (input$plotTypeG == "exp") {
                   yLyapG1$min = input$yMin1G
                   yLyapG1$max = input$yMax1G
                 }
                 else{
                   yDkyG$min = input$yMin1G
                   yDkyG$max = input$yMax1G
                 }
               })
               
               observeEvent(input$scaleApplyY2G, {
                 if (input$plotTypeG == "exp") {
                   yLyapG2$min = input$yMin2G
                   yLyapG2$max = input$yMax2G
                 }
                 else{
                   yDkyG$min = input$yMin2G
                   yDkyG$max = input$yMax2G
                 }
               })
               
               observeEvent(input$scaleApplyY3G, {
                 if (input$plotTypeG == "exp") {
                   yLyapG3$min = input$yMin3G
                   yLyapG3$max = input$yMax3G
                 }
                 else{
                   yDkyG$min = input$yMin3G
                   yDkyG$max = input$yMax3G
                 }
               })
               
               ##################################### plot 1 Grond ############################################################
               # horizontal scale plot 1
               xMin1G <- eventReactive(input$scaleApplyX1G, {
                 xmin <- max(0., as.numeric(input$xMin1G))
               })
               xMax1G <- eventReactive(input$scaleApplyX1G, {
                 xmax <- as.numeric(input$xMax1G)
               })
               
               # vertical scale plot 1
               yMin1G <- eventReactive((input$scaleApplyY1G), {
                 as.numeric(input$yMin1G)
               })
               yMax1G <- eventReactive(input$scaleApplyY1G, {
                 as.numeric(input$yMax1G)
               })
               
               
               
               observe({
                 if ((input$autoScaleX1G == FALSE) &
                     (input$runG == 1) & (input$scaleApplyX1G == 0)) {
                   updateTextInput(session, "xMax1G", value = signif(max(outL$Grond$t[, 1]), 3))
                   
                 }
               })
               
               observe({
                 if ((input$autoScaleY1G == FALSE) &  (input$runG > 0)) {
                   # exposants de Lyapunov
                   if (input$plotTypeG == "exp") {
                     if (is.null(yLyapG1$min)) {
                       updateTextInput(session, "yMin1G", value = floor(min(outL$Grond$lyapExpLoc)))
                       
                       
                     }
                     else {
                       updateTextInput(session, "yMin1G", value = yLyapG1$min)
                     }
                     # updateTextInput(session,"yMax1G",value=max(outL$Grond$lyapExpLoc))
                     if (is.null(yLyapG1$max)) {
                       updateTextInput(session, "yMax1G", value = ceiling(max(outL$Grond$lyapExpLoc)))
                     }
                     else {
                       updateTextInput(session, "yMax1G", value = yLyapG1$max)
                     }
                     
                   }
                   # dimension KY
                   else {
                     D <- DkyCalc("Grond", nVar, outL$Grond$lyapExpLoc)
                     validD <- D[!is.nan(D)]
                     ylim = c(min(validD), max(validD))
                     if (is.null(yDkyG$min)) {
                       updateTextInput(session, "yMin1G", value = floor(min(validD)))
                     }
                     else {
                       updateTextInput(session, "yMin1G", value = yDkyG$min)
                     }
                     
                     if (is.null(yDkyG$max)) {
                       updateTextInput(session, "yMax1G", value = ceiling(max(validD)))
                     }
                     else {
                       updateTextInput(session, "yMax1G", value = yDkyG$max)
                     }
                     
                     
                   }
                 }
               })
               
               xylim1 <- reactiveValues(x = 0, y = 0)
               observe({
                 input$plotTypeG
                 if ((input$autoScaleX1G == FALSE) &
                     (input$scaleApplyX1G > 0)) {
                   xmin = xMin1G() / as.numeric(input$timeStepG)
                   xmax = xMax1G() / as.numeric(input$timeStepG)
                   xylim1$x = c(as.integer(xmin), as.integer(xmax))
                 }
                 else {
                   xylim1$x = NULL
                 }
                 
                 if ((input$autoScaleY1G == FALSE) &
                     (input$scaleApplyY1G > 0)) {
                   xylim1$y = c(yMin1G(), yMax1G())
                 }
                 else {
                   xylim1$y = NULL
                 }
                 
               })
               
               ##################################### plot 2 Grond ############################################################
               # horizontal scale plot 2
               xMin2G <- eventReactive(input$scaleApplyX2G, {
                 xmin <- max(0., as.numeric(input$xMin2G))
               })
               xMax2G <- eventReactive(input$scaleApplyX2G, {
                 xmax <- as.numeric(input$xMax2G)
               })
               
               # vertical scale plot 2
               yMin2G <- eventReactive((input$scaleApplyY2G), {
                 as.numeric(input$yMin2G)
               })
               yMax2G <- eventReactive(input$scaleApplyY2G, {
                 as.numeric(input$yMax2G)
               })
               
               
               
               observe({
                 if ((input$autoScaleX2G == FALSE) &
                     (input$runG == 1) & (input$scaleApplyX2G == 0)) {
                   updateTextInput(session, "xMax2G", value = signif(max(outL$Grond$t[, 1]) / as.numeric(input$timeStepG) - 1, 3))
                   
                 }
               })
               
               observeEvent(input$plotTypeG, {
                 if (input$scaleApplyY2G > 0) {
                   if (input$plotTypeG == "exp") {
                     updateTextInput(session, "yMin2G", value = yLyapG2$min)
                     updateTextInput(session, "yMax2G", value = yLyapG2$max)
                   }
                   else{
                     updateTextInput(session, "yMin2G", value = yDkyG$min)
                     updateTextInput(session, "yMax2G", value = yDkyG$max)
                   }
                 }
                 
               })
               
               
               
               
               observe({
                 lastIndex = length(outL$Grond$lyapExp[, 1])
                 nIter = xylim2$last
                 xlim = c(max(0, (lastIndex - (2 * nIter + 1))), lastIndex - 1)
                 
                 if ((input$autoScaleY2G == FALSE) &  (input$runG == 1)) {
                   # exposants de Lyapunov
                   if (input$plotTypeG == "exp") {
                     serie <- outL$Grond$lyapExp[xlim[1]:xlim[2],]
                     
                     
                     if (is.null(yLyapG2$min)) {
                       updateTextInput(session, "yMin2G", value = floor(min(serie)))
                       
                       
                     }
                     else {
                       updateTextInput(session, "yMin2G", value = yLyapG2$min)
                     }
                     # updateTextInput(session,"yMax2G",value=max(outL$Grond$lyapExpLoc))
                     if (is.null(yLyapG2$max)) {
                       updateTextInput(session, "yMax2G", value = ceiling(max(serie)))
                     }
                     else {
                       updateTextInput(session, "yMax2G", value = yLyapG2$max)
                     }
                     
                   }
                   # dimension KY
                   else {
                     serie <- outL$Grond$lyapExpLoc[xlim[1]:xlim[2],]
                     D <- DkyCalc("Grond", nVar, serie)
                     validD <- D[!is.nan(D)]
                     ylim = c(min(validD), max(validD))
                     if (is.null(yDkyG$min)) {
                       updateTextInput(session, "yMin2G", value = floor(min(validD)))
                     }
                     else {
                       updateTextInput(session, "yMin2G", value = yDkyG$min)
                     }
                     
                     if (is.null(yDkyG$max)) {
                       updateTextInput(session, "yMax2G", value = ceiling(max(validD)))
                     }
                     else {
                       updateTextInput(session, "yMax2G", value = yDkyG$max)
                     }
                     
                     
                   }
                 }
               })
               
               xylim2 <- reactiveValues(x = 0, y = 0, last = 0)
               observe({
                 input$plotTypeG
                 if ((input$autoScaleX2G == FALSE) &
                     (input$scaleApplyX2G > 0)) {
                   xmin = xMin2G()
                   xmax = xMax2G()
                   xylim2$x = c(xMin2G(), xMax2G()) #c(as.integer(xmin), as.integer(xmax))
                   xylim2$last = as.integer(xmax) - (as.integer(xmax) - as.integer(xmin)) / 2
                 }
                 else {
                   xylim2$x = NULL
                   xylim2$last = input$lastIterG
                 }
                 
                 if ((input$autoScaleY2G == FALSE) &
                     (input$scaleApplyY2G > 0)) {
                   xylim2$y = c(yMin2G(), yMax2G())
                 }
                 else {
                   xylim2$y = NULL
                 }
                 
               })
               
               
               
               
               
               
               
               
               # PLOT MEAN GRAPH
               output$plotGrond2 <- renderPlot({
                 if (calcTimeG$start > 0) {
                   plotMeanExponents(
                     nVar,
                     outL$Grond,
                     xylim2$last,
                     xlim = xylim2$x,
                     ylim = xylim2$y,
                     plotType = input$plotTypeG,
                     expList = expLG$x,
                     legend = input$legendG
                   )
                 }
               })
               
               
               
               ###############################################  PLOT 3 Grond METHOD ########################################################
               
               
               # horizontal scale plot 3
               xMin3G <- eventReactive(input$scaleApplyX3G, {
                 xmin <- max(0., as.numeric(input$xMin3G))
               })
               xMax3G <- eventReactive(input$scaleApplyX3G, {
                 xmax <- as.numeric(input$xMax3G)
               })
               
               # vertical scale plot 3
               yMin3G <- eventReactive((input$scaleApplyY3G), {
                 as.numeric(input$yMin3G)
               })
               yMax3G <- eventReactive(input$scaleApplyY3G, {
                 as.numeric(input$yMax3G)
               })
               
               
               
               observe({
                 if ((input$autoScaleX3G == FALSE) &
                     (input$runG == 1) & (input$scaleApplyX3G == 0)) {

                   updateTextInput(session, "xMin3G", value = 1)
                   updateTextInput(session, "xMax3G", value = dim(outL$Grond$stdExp)[1])                   
                 }
               })
               
               observeEvent(input$plotTypeG, {
                 if (input$scaleApplyY3G > 0) {
                   if (input$plotTypeG == "exp") {
                     updateTextInput(session, "yMin3G", value = yLyapG3$min)
                     updateTextInput(session, "yMax3G", value = yLyapG3$max)
                   }
                   else{
                     updateTextInput(session, "yMin3G", value = yDkyG$min)
                     updateTextInput(session, "yMax3G", value = yDkyG$max)
                   }
                 }
                 
               })
               
               
               
               
               observe({
                 if ((input$autoScaleY3G == FALSE) &  (input$runG > 0)) {
                   # exposants de Lyapunov
                   if (input$plotTypeG == "exp") {
                     if (is.null(yLyapG3$min)) {
                       updateTextInput(session, "yMin3G", value = floor(min(outL$Grond$stdExp)))
                     }
                     else {
                       updateTextInput(session, "yMin3G", value = yLyapG3$min)
                     }
                     if (is.null(yLyapG3$max)) {
                       updateTextInput(session, "yMax3G", value = ceiling(max(outL$Grond$stdExp)))
                     }
                     else {
                       updateTextInput(session, "yMax3G", value = yLyapG3$max)
                     }
                     
                   }
                   # dimension KY
                   else {
                     # D <- DkyCalc("Grond", nVar, outL$Grond$stdExp)
                     # validD <- D[!is.nan(D)]
                     # ylim = c(min(validD), max(validD))
                     if (is.null(yDkyG$min)) {
                       updateTextInput(session, "yMin3G", value = floor(min(outL$Grond$stdDky)))
                     }
                     else {
                       updateTextInput(session, "yMin3G", value = yDkyG$min)
                     }
                     
                     if (is.null(yDkyG$max)) {
                       updateTextInput(session, "yMax3G", value = ceiling(max(outL$Grond$stdDky)))
                     }
                     else {
                       updateTextInput(session, "yMax3G", value = yDkyG$max)
                     }
                     
                     
                   }
                 }
               })
               
               
               
               
               
               
               xylim3 <- reactiveValues(x = 0, y = 0)
               observe({
                 input$plotTypeG
                 if ((input$autoScaleX3G == FALSE) &
                     (input$scaleApplyX3G > 0)) {
                   xmin = xMin3G()
                   xmax = xMax3G()
                   xylim3$x = c(xMin3G(), xMax3G())
                 }
                 else {
                   xylim3$x = NULL
                 }
                 
                 if ((input$autoScaleY3G == FALSE) &
                     (input$scaleApplyY3G > 0)) {
                   xylim3$y = c(yMin3G(), yMax3G())
                 }
                 else {
                   xylim3$y = NULL
                 }
                 
               })
               
               
               
               
               
               
               
               
               # PLOT CONVERGENCE GRAPH
               output$plotGrond3 <- renderPlot({
                 if (calcTimeG$start > 0) {
                   plotConvergence(
                     nVar,
                     outL$Grond,
                     xlim = xylim3$x,
                     ylim = xylim3$y,
                     lastIter = input$lastIterG,
                     plotType = input$plotTypeG,
                     expList = expLG$x,
                     legend = input$legendG
                   )
                   
                   
                 }
               })
               
               ###################################### STATISTICS #################################################################
               
               # STATISTICS MEAN OUTPUT
               output$meanG <- renderPrint({
                 if (calcTimeG$start > 0) {
                   Lyap <- outL$Grond
                   lastIndex = length(Lyap$lyapExp[, 1])
                   lastIndices = seq(max(1, (lastIndex - input$lastIterG)), lastIndex, 1)
                   m <- NULL
                   if (input$plotTypeG == "exp") {
                     for (ind in 1:nVar) {
                       lastIndex = length(Lyap$meanExp[, 1])
                       m[ind] <- Lyap$meanExp[lastIndex, ind]
                     }
                   }
                   else {
                     #D <- DkyCalc("Grond", nVar, Lyap$lyapExp)
                     #m <- mean(D[lastIndices])
                     lastIndex = length(Lyap$meanDky)
                     m <- Lyap$meanDky[lastIndex]
                   }
                   m
                   
                   
                 }
               })
               
               # STATISTICS STANDARD DEVIATION OUTPUT
               
               output$sdW <- renderPrint({              
               if (calcTimeG$start > 0) {
                 Lyap <- outL$Grond
                 lastIndex = length(Lyap$lyapExp[, 1])
                 lastIndices = seq(max(1, (lastIndex - input$lastIterG)), lastIndex, 1)
                 s <- NULL
                 if (input$plotTypeG == "exp") {
                   for (ind in 1:nVar) {
                     lastIndex = length(Lyap$stdExp[, 1])
                     s[ind] <- Lyap$stdExp[lastIndex, ind]
                   }
                 }
                 else {
                   #         D <- DkyCalc("Grond", nVar, Lyap$lyapExp)
                   #         s <- sd(D[lastIndices])
                   lastIndex = length(Lyap$stdDky)
                   s <- Lyap$stdDky[lastIndex]
                   
                 }
                 
                 s
               }
               })
               
               

output$minMaxIterG <- renderPrint({
  if (calcTimeG$start > 0) {
    Lyap <- outL$Grond
    lastIndex = length(Lyap$lyapExp[, 1])
    
    minmax = c(lastIndex - input$lastIterG, lastIndex)
    
    minmax
  }
})




