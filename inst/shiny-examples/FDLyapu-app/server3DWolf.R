# defines the axes used for the plot
axes <- reactiveValues(x1 = 1, x2 = 2, x3 = 3)
observeEvent(input$display3DW, {
  
  axes$x1 = as.integer(input$Axe13DW)
  axes$x2 = as.integer(input$Axe23DW)
  axes$x3 = as.integer(input$Axe33DW)
  
})
# prints an error message if axis values are incorrect (either out bounds values or two equal axis)
output$text3DW <- renderText({
  input$display3DW
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
observeEvent (input$display3DW, {
  undersamp$sequence <- seq(1, dim(outL$Wolf$y)[1], as.integer(input$undersamp3DW))
})

# Print the minimum value observed for the chosen exponent
output$minExpW <- renderPrint({
  if (calcTimeW$start > 0) {
    min(outL$Wolf$lyapExp[,as.integer(input$LyapInd3DW)])
  }
})

# Print the maximum value observed for the chosen exponent
output$maxExpW <- renderPrint({
  if (calcTimeW$start > 0) {
    max(outL$Wolf$lyapExp[,as.integer(input$LyapInd3DW)])
  }
})

# defines the range of values used for the palette
colours <- reactiveValues(min= NULL, max = NULL, pp = NULL)
observe ({
  if ((input$autoScale3DW) & (input$runW)){
        colours$min = min(outL$Wolf$lyapExp[,as.integer(input$LyapInd3DW)])
        colours$max = max(outL$Wolf$lyapExp[,as.integer(input$LyapInd3DW)])
 }
  else {
      colours$min = input$colour1W
      colours$max = input$colour2W      
  }
})

# sets the colour of each point of the graph
cols <- eventReactive( (input$display3DW), {
  Lyap <- outL$Wolf
  val <- Lyap$lyapExpLoc[undersamp$sequence, as.integer(input$LyapInd3DW)]
  colour <- (val - colours$min) / (colours$max - colours$min)
  
  colour[((colour<0))*(1:length(Lyap$t[undersamp$sequence]))] <- (1/input$nbColoursW)
  colour[((colour>1))*(1:length(Lyap$t[undersamp$sequence]))] <- 1
  colour <- round(colour * input$nbColoursW)
 
  nb <- input$nbColoursW
  pal <- switch(input$paletteButtonW,
                rainbow = rainbow(nb+1, end=0.7),
                terrain = terrain.colors(nb+1),
                heat = heat.colors(nb+1),
                topo = topo.colors(nb+1))
  cols = pal[colour]
  cols[is.nan(val)] = "#000000"
  cols
})

# displays the colour palette
output$paletteW <- renderPlot({
  nb <- input$nbColoursW
  col1 <- colours$min 
  col2 <- colours$max

  labels = signif(seq(col1, col2,(col2-col1)/nb),4)


  pal <- switch(input$paletteButtonW,
                rainbow = rainbow(nb+1, end=0.7),
                terrain = terrain.colors(nb+1),
                heat = heat.colors(nb+1),
                topo = topo.colors(nb+1))

  pie(rep(1,nb), labels=labels, col = pal, radius = 0.8)
})

# plots the 3D plot with the axes labels
output$plot3DWolf1 <- renderRglwidget({
  if ((calcTimeW$start > 0) & (input$display3DW >0)){
    Lyap <- outL$Wolf
    y <- Lyap$y[undersamp$sequence, ]
    x1 <- y[, axes$x1]
    x1lab = paste0('X', axes$x1)
    x2 <- y[, axes$x2]
    x2lab = paste0('X', axes$x2)
    x3 <- y[, axes$x3]
    x3lab = paste0('X', axes$x3)
    
    open3d(useNULL = T)
    id <- plot3d(x1, x2, x3, col=cols(), xlab = x1lab, ylab = x2lab, zlab = x3lab)
    
    par3d(userMatrix = rotationMatrix(input$xRotationW*pi/180, 1, 0, 0) 
          %*% rotationMatrix(input$yRotationW*pi/180, 0, 1, 0)
          %*% rotationMatrix(input$zRotationW*pi/180, 0, 0, 1),
          mouseMode = "none")
    
    rglwidget()
    
    
  }
  
})

# # defines the rotation of the the plot
observeEvent( input$reset3DW,{
  
  updateSliderInput(session, "xRotationW", value=0)
  updateSliderInput(session, "yRotationW", value=0)  
  updateSliderInput(session, "zRotationW", value=0)
  
})
