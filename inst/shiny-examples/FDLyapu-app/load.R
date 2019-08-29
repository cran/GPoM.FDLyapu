observeEvent(input$file,{
  source(input$file$datapath)
  if (!is.null(tEnd)) updateTextInput(session, "endTimeW", value=tEnd)
  if (!is.null(timeStep)) updateTextInput(session, "timeStepW", value=timeStep)
  if (!is.null(printIter)) updateTextInput(session, "printIterW", value=printIter)
  if (!is.null(lastIter)) updateTextInput(session, "lastIterW", value=lastIter)
  if (!is.null(tEnd)) updateTextInput(session, "endTimeG", value=tEnd)
  if (!is.null(timeStep)) updateTextInput(session, "timeStepG", value=timeStep)
  if (!is.null(printIter)) updateTextInput(session, "printIterG", value=printIter)
  if (!is.null(lastIter)) updateTextInput(session, "lastIterG", value=lastIter)
  
  # in the case of a global reset (when runs have already occurred) the 3D button are disabled
  if(input$runW > 0){
    shinyjs::toggleState("display3DW")
  }
  if(input$runW > 0){
    shinyjs::toggleState("display3DG")
  }
})
