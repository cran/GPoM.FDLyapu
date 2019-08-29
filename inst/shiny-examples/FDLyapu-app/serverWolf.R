

expListNameW = c(
  "exp1W",
  "exp2W",
  "exp3W",
  "exp4W",
  "exp5W",
  "exp6W",
  "exp7W",
  "exp8W",
  "exp9W",
  "exp10W"
)


expLW <- reactiveValues(x = NULL)
observe({
  input$runW
  expLW$x = c(
    input$exp1W,
    input$exp2W,
    input$exp3W,
    input$exp4W,
    input$exp5W,
    input$exp6W,
    input$exp7W,
    input$exp8W,
    input$exp9W,
    input$exp10W
  )
  
})


observe({
  loadedFile = !is.null(input$file) && input$file != ""
  shinyjs::toggleState("runW", loadedFile)
  shinyjs::toggleState("stopW", loadedFile)
  shinyjs::toggleState("resetW", loadedFile)
  if (loadedFile) {
    for (i in seq(1, nVar)) {
      shinyjs::enable(expListNameW[i])
      updateCheckboxInput(session, expListNameW[i], value = TRUE)
    }
    for (i in seq(nVar + 1, length(expListNameW))) {
      updateCheckboxInput(session, expListNameW[i], value = FALSE)
    }
    
    isolate({
      outL$Wolf <<- NULL
      calcTimeW$start = 0
      calcTimeW$end = 0
      calcTimeW$stop = 1
      yDkyW$min = NULL
      yDkyW$max = NULL
      yLyapW1$min = NULL
      yLyapW1$max = NULL
      yLyapW2$min = NULL
      yLyapW2$max = NULL
      yLyapW3$min = NULL
      yLyapW3$max = NULL
      tStartW <<- 0.
      
    })
    
  }
})


observe({
  shinyjs::toggleState("display3DW", condition = (input$runW > 0))
})


#  DISABLES THE CHECKBOXINPUTS FOR THE EXPONENTS TO DISPLAY FOR INDICES > nVar
observeEvent(input$file, {
  if (!is.null(input$file) && input$file != "") {
    for (i in seq(nVar + 1, length(expListNameW))) {
      shinyjs::disable(expListNameW[i])
      updateCheckboxInput(session, expListNameW[i], value = FALSE)
      
    }
  }
})


# ENABLES/DISABLES THE CHECKBOXINPUTS FOR THE EXPONENTS TO DISPLAY (ACCORDING TO THE NUMBER OF VARIABLES)
observeEvent(input$plotTypeW, {
  if (!is.null(input$file) && input$file != "") {
    if (input$plotTypeW == "exp") {
      for (i in seq(1, nVar)) {
        shinyjs::enable(expListNameW[i])
      }
    }
    else {
      for (i in seq(1, nVar)) {
        shinyjs::disable(expListNameW[i])
      }
    }
  }
})


# RESET VALUES
observeEvent(input$resetW, {
  isolate({
    outL$Wolf <<- NULL
    calcTimeW$start = 0
    calcTimeW$end = 0
    calcTimeW$stop = 1
    yDkyW$min = NULL
    yDkyW$max = NULL
    yLyapW1$min = NULL
    yLyapW1$max = NULL
    yLyapW2$min = NULL
    yLyapW2$max = NULL
    yLyapW3$min = NULL
    yLyapW3$max = NULL
    tStartW <<- 0.
    shinyjs::toggleState("display3DW")
  })
})


# calcul Wolf
observe({
  tFinal <- pEndTimeWolf()
  
  deltat <- isolate(input$printIterW) * pTimeStepWolf()
  isolate({
    if (calcTimeW$start == 0) {
      calcTimeW$start <- tStartW
    }
    calcTimeW$end <- calcTimeW$start + deltat
    
    outL$Wolf <<- lyapFDWolf(
      outL$Wolf,
      nVar = nVar,
      dMax = dMax,
      KL,
      intgrMthod = intgrMthod,
      tDeb = calcTimeW$start,
      dt = pTimeStepWolf(),
      tFin = calcTimeW$end,
      yDeb = yDeb,
      nIterMin = nIterMin,
      nIterStats = xylim2$last
    )
    calcTimeW$start <- calcTimeW$end
  })
  if ((isolate(calcTimeW$start) < pEndTimeWolf()) &
      (isolate(calcTimeW$stop) == 0)) {
    invalidateLater(0, session)
  }
  
})

output$plotWolf1 <- renderPlot({
  if (calcTimeW$start > 0) {
    plotLocalExponents(
      nVar,
      outL$Wolf,
      xlim = xylim1$x,
      ylim = xylim1$y,
      plotType = input$plotTypeW,
      dt = pTimeStepWolf(),
      expList = expLW$x,
      legend = input$legendW
    )
    
    
  }
  
})

# parametres calcul Wolf
pTimeStepWolf <- eventReactive(input$runW, {
  calcTimeW$stop = 0
  as.numeric(input$timeStepW)
})
pEndTimeWolf <- eventReactive(input$runW, {
  calcTimeW$stop = 0
  as.numeric(input$endTimeW)
})

observeEvent(input$stopW, {
  calcTimeW$stop = 1
})

observeEvent(input$runW, {
  calcTimeW$stop = 0
})

observeEvent(input$scaleApplyY1W, {
  if (input$plotTypeW == "exp") {
    yLyapW1$min = input$yMin1W
    yLyapW1$max = input$yMax1W
  }
  else{
    yDkyW$min = input$yMin1W
    yDkyW$max = input$yMax1W
  }
})

observeEvent(input$scaleApplyY2W, {
  if (input$plotTypeW == "exp") {
    yLyapW2$min = input$yMin2W
    yLyapW2$max = input$yMax2W
  }
  else{
    yDkyW$min = input$yMin2W
    yDkyW$max = input$yMax2W
  }
})

observeEvent(input$scaleApplyY3W, {
  if (input$plotTypeW == "exp") {
    yLyapW3$min = input$yMin3W
    yLyapW3$max = input$yMax3W
  }
  else{
    yDkyW$min = input$yMin3W
    yDkyW$max = input$yMax3W
  }
})
##################################### plot 1 Wolf ############################################################
# horizontal scale plot 1
xMin1W <- eventReactive(input$scaleApplyX1W, {
  xmin <- max(0., as.numeric(input$xMin1W))
})
xMax1W <- eventReactive(input$scaleApplyX1W, {
  xmax <- as.numeric(input$xMax1W)
})

# vertical scale plot 1
yMin1W <- eventReactive((input$scaleApplyY1W), {
  as.numeric(input$yMin1W)
})
yMax1W <- eventReactive(input$scaleApplyY1W, {
  as.numeric(input$yMax1W)
})



observe({
  if ((input$autoScaleX1W == FALSE) &
      (input$runW == 1) & (input$scaleApplyX1W == 0)) {
    updateTextInput(session, "xMax1W", value = signif(max(outL$Wolf$t[, 1]), 3))
    
  }
})

observe({
  if ((input$autoScaleY1W == FALSE) &  (input$runW > 0)) {
    # exposants de Lyapunov
    if (input$plotTypeW == "exp") {
      if (is.null(yLyapW1$min)) {
        updateTextInput(session, "yMin1W", value = floor(min(outL$Wolf$lyapExpLoc)))
      }
      else {
        updateTextInput(session, "yMin1W", value = yLyapW1$min)
      }
      if (is.null(yLyapW1$max)) {
        updateTextInput(session, "yMax1W", value = ceiling(max(outL$Wolf$lyapExpLoc)))
      }
      else {
        updateTextInput(session, "yMax1W", value = yLyapW1$max)
      }
      
    }
    # dimension KY
    else {
      D <- DkyCalc("Wolf", nVar, outL$Wolf$lyapExpLoc)
      validD <- D[!is.nan(D)]
      ylim = c(min(validD), max(validD))
      if (is.null(yDkyW$min)) {
        updateTextInput(session, "yMin1W", value = floor(min(validD)))
      }
      else {
        updateTextInput(session, "yMin1W", value = yDkyW$min)
      }
      
      if (is.null(yDkyW$max)) {
        updateTextInput(session, "yMax1W", value = ceiling(max(validD)))
      }
      else {
        updateTextInput(session, "yMax1W", value = yDkyW$max)
      }
      
      
    }
  }
})

xylim1 <- reactiveValues(x = 0, y = 0)
observe({
  input$plotTypeW
  if ((input$autoScaleX1W == FALSE) &  (input$scaleApplyX1W > 0)) {
    xmin = xMin1W() / as.numeric(input$timeStepW)
    xmax = xMax1W() / as.numeric(input$timeStepW)
    xylim1$x = c(as.integer(xmin), as.integer(xmax))
  }
  else {
    xylim1$x = NULL
  }
  
  if ((input$autoScaleY1W == FALSE) &  (input$scaleApplyY1W > 0)) {
    xylim1$y = c(yMin1W(), yMax1W())
  }
  else {
    xylim1$y = NULL
  }
  
})

##################################### plot 2 Wolf ############################################################
# horizontal scale plot 2
xMin2W <- eventReactive(input$scaleApplyX2W, {
  xmin <- max(0., as.numeric(input$xMin2W))
})
xMax2W <- eventReactive(input$scaleApplyX2W, {
  xmax <- as.numeric(input$xMax2W)
})

# vertical scale plot 2
yMin2W <- eventReactive((input$scaleApplyY2W), {
  as.numeric(input$yMin2W)
})
yMax2W <- eventReactive(input$scaleApplyY2W, {
  as.numeric(input$yMax2W)
})



observe({
  if ((input$autoScaleX2W == FALSE) &
      (input$runW == 1) & (input$scaleApplyX2W == 0)) {
    updateTextInput(session, "xMax2W", value = signif(max(outL$Wolf$t[, 1]) / as.numeric(input$timeStepW) - 1, 3))
    
  }
})

observeEvent(input$plotTypeW, {
  if (input$scaleApplyY2W > 0) {
    if (input$plotTypeW == "exp") {
      updateTextInput(session, "yMin2W", value = yLyapW2$min)
      updateTextInput(session, "yMax2W", value = yLyapW2$max)
    }
    else{
      updateTextInput(session, "yMin2W", value = yDkyW$min)
      updateTextInput(session, "yMax2W", value = yDkyW$max)
    }
  }
  
})




observe({
  lastIndex = length(outL$Wolf$lyapExp[, 1])
  nIter = xylim2$last
  xlim = c(max(0, (lastIndex - (2 * nIter + 1))), lastIndex - 1)
  
  if ((input$autoScaleY2W == FALSE) &  (input$runW == 1)) {
    # exposants de Lyapunov
    if (input$plotTypeW == "exp") {
      serie <- outL$Wolf$lyapExp[xlim[1]:xlim[2],]
      
      
      if (is.null(yLyapW2$min)) {
        updateTextInput(session, "yMin2W", value = floor(min(serie)))
        
        
      }
      else {
        updateTextInput(session, "yMin2W", value = yLyapW2$min)
      }
      # updateTextInput(session,"yMax2W",value=max(outL$Wolf$lyapExpLoc))
      if (is.null(yLyapW2$max)) {
        updateTextInput(session, "yMax2W", value = ceiling(max(serie)))
      }
      else {
        updateTextInput(session, "yMax2W", value = yLyapW2$max)
      }
      
    }
    # dimension KY
    else {
      serie <- outL$Wolf$lyapExpLoc[xlim[1]:xlim[2],]
      D <- DkyCalc("Wolf", nVar, serie)
      validD <- D[!is.nan(D)]
      ylim = c(min(validD), max(validD))
      if (is.null(yDkyW$min)) {
        updateTextInput(session, "yMin2W", value = floor(min(validD)))
      }
      else {
        updateTextInput(session, "yMin2W", value = yDkyW$min)
      }
      
      if (is.null(yDkyW$max)) {
        updateTextInput(session, "yMax2W", value = ceiling(max(validD)))
      }
      else {
        updateTextInput(session, "yMax2W", value = yDkyW$max)
      }
      
      
    }
  }
})

xylim2 <- reactiveValues(x = 0, y = 0, last = 0)
observe({
  input$plotTypeW
  if ((input$autoScaleX2W == FALSE) &  (input$scaleApplyX2W > 0)) {
    xmin = xMin2W()
    xmax = xMax2W()
    xylim2$x = c(xMin2W(), xMax2W())
    xylim2$last = as.integer(xmax) - (as.integer(xmax) - as.integer(xmin)) / 2
  }
  else {
    xylim2$x = NULL
    xylim2$last = input$lastIterW
  }
  
  if ((input$autoScaleY2W == FALSE) &  (input$scaleApplyY2W > 0)) {
    xylim2$y = c(yMin2W(), yMax2W())
  }
  else {
    xylim2$y = NULL
  }
  
})








# PLOT MEAN GRAPH
output$plotWolf2 <- renderPlot({
  if (calcTimeW$start > 0) {
    plotMeanExponents(
      nVar,
      outL$Wolf,
      xylim2$last,
      xlim = xylim2$x,
      ylim = xylim2$y,
      plotType = input$plotTypeW,
      expList = expLW$x,
      legend = input$legendW
    )
  }
})



###############################################  PLOT 3 Wolf METHOD ########################################################


# horizontal scale plot 3
xMin3W <- eventReactive(input$scaleApplyX3W, {
  xmin <- max(0., as.numeric(input$xMin3W))
})
xMax3W <- eventReactive(input$scaleApplyX3W, {
  xmax <- as.numeric(input$xMax3W)
})

# vertical scale plot 3
yMin3W <- eventReactive((input$scaleApplyY3W), {
  as.numeric(input$yMin3W)
})
yMax3W <- eventReactive(input$scaleApplyY3W, {
  as.numeric(input$yMax3W)
})



observe({
  if ((input$autoScaleX3W == FALSE) &
      (input$runW == 1) & (input$scaleApplyX3W == 0)) {
    updateTextInput(session, "xMin3W", value = 1)
    updateTextInput(session, "xMax3W", value = dim(outL$Wolf$stdExp)[1])
    
  }
})

observeEvent(input$plotTypeW, {
  if (input$scaleApplyY3W > 0) {
    if (input$plotTypeW == "exp") {
      updateTextInput(session, "yMin3W", value = yLyapW3$min)
      updateTextInput(session, "yMax3W", value = yLyapW3$max)
    }
    else{
      updateTextInput(session, "yMin3W", value = yDkyW$min)
      updateTextInput(session, "yMax3W", value = yDkyW$max)
    }
  }
  
})




observe({
  if ((input$autoScaleY3W == FALSE) &  (input$runW > 0)) {
    # exposants de Lyapunov
    if (input$plotTypeW == "exp") {
      if (is.null(yLyapW3$min)) {
        updateTextInput(session, "yMin3W", value = floor(min(outL$Wolf$stdExp)))
      }
      else {
        updateTextInput(session, "yMin3W", value = yLyapW3$min)
      }
      if (is.null(yLyapW3$max)) {
        updateTextInput(session, "yMax3W", value = ceiling(max(outL$Wolf$stdExp)))
      }
      else {
        updateTextInput(session, "yMax3W", value = yLyapW3$max)
      }
      
    }
    # dimension KY
    else {
      if (is.null(yDkyW$min)) {
        updateTextInput(session, "yMin3W", value = floor(min(outL$Wolf$stdDky)))
      }
      else {
        updateTextInput(session, "yMin3W", value = yDkyW$min)
      }
      
      if (is.null(yDkyW$max)) {
        updateTextInput(session, "yMax3W", value = ceiling(max(outL$Wolf$stdDky)))
      }
      else {
        updateTextInput(session, "yMax3W", value = yDkyW$max)
      }
      
      
    }
  }
})






xylim3 <- reactiveValues(x = 0, y = 0)
observe({
  input$plotTypeW
  if ((input$autoScaleX3W == FALSE) &  (input$scaleApplyX3W > 0)) {
    xmin = xMin3W()
    xmax = xMax3W()
    xylim3$x = c(xMin3W(), xMax3W())
  }
  else {
    xylim3$x = NULL
  }
  
  if ((input$autoScaleY3W == FALSE) &  (input$scaleApplyY3W > 0)) {
    xylim3$y = c(yMin3W(), yMax3W())
  }
  else {
    xylim3$y = NULL
  }
  
})








# PLOT CONVERGENCE GRAPH
output$plotWolf3 <- renderPlot({
  if (calcTimeW$start > 0) {
    plotConvergence(
      nVar,
      outL$Wolf,
      xlim = xylim3$x,
      ylim = xylim3$y,
      lastIter = input$lastIterW,
      plotType = input$plotTypeW,
      expList = expLW$x,
      legend = input$legendW
    )
    
    
  }
})

###################################### STATISTICS #################################################################

# STATISTICS MEAN OUTPUT
output$meanW <- renderPrint({
  if (calcTimeW$start > 0) {
    Lyap <- outL$Wolf
    lastIndex = length(Lyap$lyapExp[, 1])
    lastIndices = seq(max(1, (lastIndex - input$lastIterW)), lastIndex, 1)
    m <- NULL
    if (input$plotTypeW == "exp") {
      for (ind in 1:nVar) {
        lastIndex = length(Lyap$meanExp[, 1])
        m[ind] <- Lyap$meanExp[lastIndex, ind]
      }
    }
    else {
      lastIndex = length(Lyap$meanDky)
      m <- Lyap$meanDky[lastIndex]
    }
    m
    
    
  }
})


# STATISTICS STANDARD DEVIATION OUTPUT

output$sdW <- renderPrint({
  if (calcTimeW$start > 0) {
    Lyap <- outL$Wolf
    lastIndex = length(Lyap$lyapExp[, 1])
    lastIndices = seq(max(1, (lastIndex - input$lastIterW)), lastIndex, 1)
    s <- NULL
    if (input$plotTypeW == "exp") {
      for (ind in 1:nVar) {
        lastIndex = length(Lyap$stdExp[, 1])
        s[ind] <- Lyap$stdExp[lastIndex, ind]
      }
    }
    else {
      lastIndex = length(Lyap$stdDky)
      s <- Lyap$stdDky[lastIndex]
      
    }
    
    s
  }
})

# STATISTICS WINDOW
output$minMaxIterW <- renderPrint({
  if (calcTimeW$start > 0) {
    Lyap <- outL$Wolf
    lastIndex = length(Lyap$lyapExp[, 1])
    
    minmax = c(lastIndex - input$lastIterW, lastIndex)
    
    minmax
  }
})




