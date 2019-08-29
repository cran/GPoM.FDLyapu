# defines the axes used for the plot
axes <- reactiveValues(x1 = 1, x2 = 2, x3 = 3)
observeEvent(input$display3DG, {
  
  axes$x1 = as.integer(input$Axe13DG)
  axes$x2 = as.integer(input$Axe23DG)
  axes$x3 = as.integer(input$Axe33DG)
  
})
# prints an error message if axis values are incorrect (either out bounds values or two equal axis)
output$text3DG <- renderText({
  input$display3DG
  validate(
    need((axes$x1 <= nVar), 'Incorrect value for axis 1'),
    need((axes$x2 <= nVar), 'Incorrect value for axis 2'),
    need((axes$x3 <= nVar), 'Incorrect value for axis 3'),
    need(((axes$x1 != axes$x2) & (axes$x1 != axes$x3) & (axes$x2 != axes$x3)), 'Axis must be different')
  )
  ""
})

# defines the sequence used for the plot according to the undersampling ratio
undersamp <- reactiveValues(sequence = NULL)
observeEvent (input$display3DG, {
  undersamp$sequence <- seq(1, dim(outL$Grond$y)[1], as.integer(input$undersamp3DG))
})

# Print the minimum value observed for the chosen exponent
output$minExpG <- renderPrint({
  if (calcTimeG$start > 0) {
    min(outL$Grond$lyapExp[,as.integer(input$LyapInd3DG)])
  }
})

# Print the maximum value observed for the chosen exponent
output$maxExpG <- renderPrint({
  if (calcTimeG$start > 0) {
    max(outL$Grond$lyapExp[,as.integer(input$LyapInd3DG)])
  }
})

# defines the range of values used for the palette
colours <- reactiveValues(min= NULL, max = NULL, pp = NULL)
observe ({
  if ((input$autoScale3DG) & (input$runG)){
        colours$min = min(outL$Grond$lyapExp[,as.integer(input$LyapInd3DG)])
        colours$max = max(outL$Grond$lyapExp[,as.integer(input$LyapInd3DG)])
 }
  else {
      colours$min = input$colour1G
      colours$max = input$colour2G      
  }
})

# sets the colour of each point of the graph
cols <- eventReactive( (input$display3DG), {
  Lyap <- outL$Grond
  val <- Lyap$lyapExpLoc[undersamp$sequence, as.integer(input$LyapInd3DG)]
  colour <- (val - colours$min) / (colours$max - colours$min)
  
  colour[((colour<0))*(1:length(Lyap$t[undersamp$sequence]))] <- (1/input$nbColoursG)
  colour[((colour>1))*(1:length(Lyap$t[undersamp$sequence]))] <- 1
  colour <- round(colour * input$nbColoursG)
 
  nb <- input$nbColoursG
  pal <- switch(input$paletteButtonG,
                rainbow = rainbow(nb+1, end=0.7),
                terrain = terrain.colors(nb+1),
                heat = heat.colors(nb+1),
                topo = topo.colors(nb+1))
  cols = pal[colour]
  cols[is.nan(val)] = "#000000"
  cols
})

# displays the colour palette
output$paletteG <- renderPlot({
  nb <- input$nbColoursG
  col1 <- colours$min 
  col2 <- colours$max

  labels = signif(seq(col1, col2,(col2-col1)/nb),4)


  pal <- switch(input$paletteButtonG,
                rainbow = rainbow(nb+1, end=0.7),
                terrain = terrain.colors(nb+1),
                heat = heat.colors(nb+1),
                topo = topo.colors(nb+1))

  pie(rep(1,nb), labels=labels, col = pal, radius = 0.8)
})

# plots the 3D plot with the axes labels
output$plot3DGrond1 <- renderRglwidget({
  if ((calcTimeG$start > 0) & (input$display3DG >0)){
    Lyap <- outL$Grond
    y <- Lyap$y[undersamp$sequence, ]
    x1 <- y[, axes$x1]
    x1lab = paste0('X', axes$x1)
    x2 <- y[, axes$x2]
    x2lab = paste0('X', axes$x2)
    x3 <- y[, axes$x3]
    x3lab = paste0('X', axes$x3)
    
    open3d(useNULL = T)
    id <- plot3d(x1, x2, x3, col=cols(), xlab = x1lab, ylab = x2lab, zlab = x3lab)
    
    par3d(userMatrix = rotationMatrix(input$xRotationG*pi/180, 1, 0, 0) 
          %*% rotationMatrix(input$yRotationG*pi/180, 0, 1, 0)
          %*% rotationMatrix(input$zRotationG*pi/180, 0, 0, 1),
          mouseMode = "none")
    
    rglwidget()
    
    
  }
  
})

# # defines the rotation of the the plot
observeEvent( input$reset3DG,{
  
  updateSliderInput(session, "xRotationG", value=0)
  updateSliderInput(session, "yRotationG", value=0)  
  updateSliderInput(session, "zRotationG", value=0)
  
})
