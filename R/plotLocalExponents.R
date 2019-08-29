#' @title plotLocalExponents 
#' @description Plots the local Lyapunov exponents or the local Kaplan-Yorke dimension
#'
#' @inheritParams lyapFDWolf
#' 
#' @param outL An input list issued from the FDLyapu algorithm updated at each call, with
#' outL$t the time vector,
#' outL$LyapExp the mean Lyapunov exponents 
#' outL$LyapExpLoc the local Lyapunov exponents,
#' outL$DkyLoc the mean Kaplan-Yorke dimension.
#' outL$DkyLoc the local Kaplan-Yorke dimension.
#' 
#' 
#' @param plotType If equals to "exp" the Lyapunov exponents are plotted. Otherwise the Kaplan-Yorke dimension is plotted
#' @param xlim The limits used for the x-axis. If NULL it is automatically adjusted to the minimum and maximum data values 
#' @param ylim The limits used for the u-axis. If NULL it is automatically adjusted to the minimum and maximum data values 
#' @param lastIter If not NULL indicates the last iterations used for the plot.
#' @param expList Indicates which exponents have to be plotted. If NULL all are plotted otherwise must be a vector of booleans
#' @param legend Indicates if the legend has to appear (TRUE) or not (FALSE)
#' @export
#' @importFrom graphics plot lines
plotLocalExponents <- function(nVar, outL, plotType="exp", xlim=NULL, ylim=NULL,
                               lastIter= NULL, dt=NULL, expList = NULL, legend = FALSE) {
  
  colorExp=c("blue", "red", "green", "black", "purple",
             "magenta", "yellow", "maroon", "forestgreen")
  
  # Exponents that have to be plotted
  if (is.null(expList)) {
    expList = rep(TRUE, nVar)
  }
  
  # Horizontal limits (common for exponents and Dky)
  if (is.null(xlim)) {
    xlimT = c(min(outL$t[,1]), max(outL$t[,1]))
  }
  else {
    xlimT = c(xlim[1]*dt, xlim[2]*dt)
    xlimR = c(min(outL$t[,1]), max(outL$t[,1]))
  }
  
  lastIndex = length(outL$lyapExp[,1])
  if (!is.null(lastIter)) {
    xlimT <- c((lastIndex-(lastIter+1))*dt, (lastIndex*dt))
  }
  
  
  # Vertical limits
  # for the exponents case
  if (plotType == "exp") {
    if (is.null(ylim)) {
      ylim = c(min(outL$lyapExpLoc[, expList[1:nVar]]), max(outL$lyapExpLoc[, expList[1:nVar]]))
    }
    
    if (is.null(expList) | expList[1]) {
      colFirst = colorExp[1]
    }
    else {
      colFirst = "lightgray"
    }
    
    plot(outL$t[,1], outL$lyapExpLoc[,1], 'l', xlim=xlimT, ylim=ylim, xlab = 'Time', ylab = expression(lambda(t)),
         main = 'Local Lyapunov exponents', col = colFirst)
    for (iExp in 2:nVar) {
      if (is.null(expList) | expList[iExp]) {
        lines(outL$t[,1], outL$lyapExpLoc[,iExp], 'l', col=colorExp[iExp])
      }
      else {
        lines(outL$t[,1], outL$lyapExpLoc[,iExp], 'l', col="lightgray")
      }
      
    }
    l <- NULL
    
    
    
    for (i in 1:length(which(expList))) {
      l[i] = as.expression( bquote(lambda[.(which(expList)[i])](t)))
    }
    if (legend) {
      legend(xlimT[1], ylim[1]+(ylim[2]-ylim[1])*0.8, l, colorExp[which(expList)], cex = 0.8)
    }
  }
  # Vertical limits
  # for the Kaplan-Yorke dimension case 
  else {
    # Vertical limits
    D <- outL$DkyLoc #DkyCalc(outL$methodName, nVar, outL$lyapExpLoc)
    if (is.null(ylim)) {
      validD <- D[!is.nan(D)]
      if (is.null(xlim)) {
        ylim = c(min(validD), max(validD))
      }
      else {
        ylim = c(min(validD[xlim[1]:xlim[2]]), max(validD[xlim[1]:xlim[2]]))
      }
    }
    plot(outL$t[,1], D, 'l', xlim=xlimT, ylim=ylim, xlab = 'Time', main = 'Local Dky', col=colorExp[1])
    lines(outL$t[,1], D%%1, col = 'red')
    if (legend) {
      legend(xlimT[1], ylim[1]+(ylim[2]-ylim[1])*0.8, c('Dky(t)', '{Dky(t)}'), col=c(colorExp[1], 'red'), cex = 0.8, lty=1)
    }
  }
}
