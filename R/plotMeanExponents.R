#' @title plotMeanExponents
#' @description Plots the averaged Lyapunov exponents or the averaged Kaplan-Yorke dimension
#'
#' @inheritParams lyapFDWolf
#' @inheritParams plotLocalExponents
#' @export
#' @importFrom graphics plot lines
plotMeanExponents <- function(nVar, outL, nIterStats= NULL, plotType="exp", xlim=NULL, ylim=NULL, expList=NULL, legend=FALSE) {
  colorExp=c("blue", "red", "green", "black", "purple", "magenta", "yellow", "maroon", "forestgreen")

  

  
  # Exponents that have to be plotted
  if (is.null(expList)) {
    expList = rep(TRUE, nVar)
  }
  
    lastIndex = length(outL$lyapExp[,1])

    
    if (is.null(xlim)) {
      if (is.null(nIterStats)) {
          xlim = c(0, lastIndex)
      }
      else {
          xlim=c(max(0, (lastIndex-(2*nIterStats+1))), lastIndex)
      }
    }

    if (plotType == "exp") {
      
      if (is.null(expList) | expList[1]) {
        colFirst = colorExp[1]
      }
      else {
        colFirst = "lightgray"
      }
      

      if (is.null(ylim)) {
        if (is.null(xlim)) {
          ylim = c(min(outL$lyapExp[, expList[1:nVar]]), max(outL$lyapExp[, expList[1:nVar]]))
        }
        else {
          ylim = c(min(outL$lyapExp[xlim[1]:xlim[2], expList[1:nVar]]), max(outL$lyapExp[xlim[1]:xlim[2],expList[1:nVar]]))

        }
      }
      plot(x = outL$lyapExp[, 1], y = NULL, 'l', xlim = xlim, ylim=ylim, xlab = 'Iteration',
           ylab = expression("<" ~ lambda ~ ">"),
           main = 'Mean Lyapunov exponents', col=colFirst)
      for (iExp in 2:nVar) {
        if (is.null(expList) | expList[iExp]) {
          lines(x= outL$lyapExp[,iExp], y= NULL, 'l', col=colorExp[iExp])
        }
        else {
          lines(x= outL$lyapExp[,iExp], y= NULL, 'l', col = 'lightgray')
        }
      }
      l <- NULL
      for (i in 1:length(which(expList))) {
        l[i] = as.expression(bquote("<"~lambda[.(which(expList)[i])]~">"))
      }
      horizontalBarX = c(0, 2*lastIndex)
      horizontalBarY = c(0, 0)
      lines(horizontalBarX, horizontalBarY, 'l', col='grey')
      if (legend) {
        legend(xlim[1], ylim[1] + (ylim[2]-ylim[1])*0.8, l, colorExp[which(expList)], cex = 0.8)
      }

    }
    else {
      D <- outL$Dky 
      if (is.null(ylim)) {
        ylim = c(min(D), max(D))
      }
      plot(x = D, y = NULL,  'l', xlim=xlim, ylim=ylim, xlab = 'Iteration', ylab = '<D>', main = 'Mean Dky', col=colorExp[1])
      if (legend) {
        legend(xlim[1], ylim[1] + (ylim[2]-ylim[1])*0.8, '<Dky>', colorExp[1], cex = 0.8)
      }
    }



    if (!is.null(nIterStats)) {
        verticalBarX = c(lastIndex-(nIterStats+1), lastIndex-(nIterStats+1))
        verticalBarY = c(ylim[1], ylim[2])
        lines(verticalBarX, verticalBarY, 'l', col='black')
    }

}
