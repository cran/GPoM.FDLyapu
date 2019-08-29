#' @title DkyCalc : computes the Kaplan-Yorke dimension
#' @description Computes the Kaplan-Yorke dimension
#' from the Lyapunov exponents (Kaplan and Yorke 1979).
#'
#' @param methodName The method that was used to compute the lyapunov exponents
#' @param nVar The model dimension (which corresponds to the number of exponents)
#' @param lyapExp Time series of the local Lyapunov exponents spectrum (one column
#' for each exponent)
#'
#' @references
#' Kaplan, J. & Yorke, J., Chaotic behavior of multidimensional difference equations.
#' In: Peitgen H. O. and Walther H. O., "Functional Differential Equations and
#' the Approximation of Fixed Points", Lecture Notes in Mathematics. 730.
#' Berlin: Springer. p. 204-227, 1979.
#'
#' @examples
#' #' Load the global model (here for Ebola Virus Diesease)
#' data(Ebola)
#' nVar = dim(Ebola$KL)[2]
#' pMax = dim(Ebola$KL)[1]
#' dMax = p2dMax(nVar, pMax)
#' #' Compute the time series of Lyapunov exponents
#' outLyapFD <- NULL
#' outLyapFD$Wolf <- lyapFDWolf(outLyapFD$Wolf, nVar= nVar, dMax = dMax,
#'                              coeffF = Ebola$KL,
#'                              tDeb = 0, dt = 0.01, tFin = 2,
#'                              yDeb = Ebola$yDeb)
#' #' estimate the Kaplan-Yorke dimension
#' DkyCalc(methodName = "Wolf", nVar= 3, lyapExp = outLyapFD$Wolf$lyapExpLoc)
#'
#' @export
DkyCalc <- function(methodName, nVar, lyapExp) {
  if (methodName == "Wolf") iStart <- 1
  if (methodName == "Grond") iStart <- 2

  k <-array(dim=length(lyapExp[,1]))
  num <-array(dim=length(lyapExp[,1]))
  denom <- array(dim=length(lyapExp[,1]))
  num[] <- denom[] <- k[] <- 0.
  lExpO <- lyapExp * 0
  for (j in 1:dim(lyapExp)[1]) {
    lExpO[j,] <- sort(lyapExp[j,], decreasing = TRUE)
  }
  for (i in iStart:nVar) {
    lesposit <- which((num + lExpO[,i]) >0.)
    num[lesposit] <- num[lesposit] + lExpO[lesposit,i]
    k[lesposit] <- k[lesposit] + 1
    denom[lExpO[,i]<0.] <- denom[lExpO[,i]<0.] + abs(lExpO[(lExpO[,i]<0.),i])
  }

  D <- array(dim=length(lyapExp[,1]))
  D[] <- NaN
  cond <- (denom > 0.)
  D[cond] <- k[cond] + (num[cond]/denom[cond])
  D
}
