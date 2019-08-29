#' @title plotConvergence
#' @description Plots the evolution of the standard deviations of either the Lyapunov exponents or the Kaplan-Yorke dimension
#'
#' @inheritParams lyapFDWolf
#' @inheritParams plotLocalExponents
#' 
#' @export
#' @importFrom graphics plot lines
plotConvergence <- function(nVar, outL, plotType="exp", xlim=NULL, ylim=NULL,
                           lastIter= NULL, dt=NULL, expList = NULL, legend = FALSE) {

  colorExp=c("blue", "red", "green", "black", "purple",
             "magenta", "yellow", "maroon", "forestgreen")

  # Exponents that have to be plotted
  if (is.null(expList)) {
    expList = rep(TRUE, nVar)
  }
  
  # Horizontal limits (common for exponents and Dky)
  if (is.null(xlim)) {
    xlim = c(1, dim(outL$stdExp)[1])
  }

  # Vertical limits
  # for the exponents case
  if (plotType == "exp") {
    if (is.null(ylim)) {
      ylim = c(min(outL$stdExp[, expList[1:nVar]]), max(outL$stdExp[, expList[1:nVar]]))
    }
    if (is.null(expList) | expList[1]) {
      colFirst = colorExp[1]
    }
    else {
      colFirst = "lightgray"
    }
    
    plot(outL$stdExp[,1], type = "l", xlim=xlim, ylim=ylim, xlab = 'Macro-iteration', ylab = expression(sigma),
         main = 'Standard deviation', col=colFirst)
    for (iExp in 2:nVar) {
      if (is.null(expList) | expList[iExp]) {
        lines(outL$stdExp[,iExp], col=colorExp[iExp])
      }
      else {
        lines(outL$stdExp[,iExp], col="lightgray")
      }

    }
    l <- NULL



     for (i in 1:length(which(expList))) {
       l[i] = as.expression( bquote(sigma[.(which(expList)[i])]))
     }
    if (legend) {
      legend(xlim[1], ylim[2]*0.8, l, colorExp[which(expList)], cex = 0.8)
    }
  }
  # Vertical limits
  # for the Kaplan-Yorke dimension
  else {
    # Vertical limits
    D <- outL$stdDky #DkyCalc(outL$methodName, nVar, outL$lyapExpLoc)
    if (is.null(ylim)) {
      validD <- D[!is.nan(D)]
      if (is.null(xlim)) {
        ylim = c(min(validD), max(validD))
      }
      else {
        ylim = c(min(validD[xlim[1]:xlim[2]]), max(validD[xlim[1]:xlim[2]]))
      }
    }
    plot(D, type="l", xlim=xlim, ylim=ylim, xlab = 'Macro-iteration', ylab = expression(sigma), 
         main = 'Convergence', col=colorExp[1])
    if (legend) {
      legend(xlim[1], ylim[2]*0.8, expression(sigma), colorExp[1], cex = 0.8)
    }
  }
}
