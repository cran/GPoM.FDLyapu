#' @title lyapFDGrond : Computes the Lyapunov spectrum (with
#' compelled flow direction)
#' @description Computes all the Lyapunov exponents based
#' on Gram-Schmidt procedure with zero-Lyapunov exponent compelled
#' to the flow direction (Grond et al. 1985).
#' The Jacobian matrix is computed from the original model
#' by semi-Formal Derivation.
#'
#' @inheritParams lyapFDWolf
#' @return List of output data
#' @references
#' F. Grond, H. H. Diebner, S. Sahle, A. Mathias, S. Fischer,
#' O. E. Rossler, A robust, locally interpretable algorithm for
#' Lyapunov exponents, Chaos, Solitons \& Fractals, 16, 841-852 (2003).
#'
#' F. Grond \& H. H. Diebner: Local Lyapunov exponents for
#' dissipative continuous systems. Chaos, Solitons \& Fractals, 23,
#' 1809-1817 (2005).
#'
#' @examples
#' data(Ebola)
#' nVar = dim(Ebola$KL)[2]
#' pMax = dim(Ebola$KL)[1]
#' dMax = p2dMax(nVar, pMax)
#' outLyapFD <- NULL
#' outLyapFD$Grond <- lyapFDGrond(outLyapFD$Grond, nVar= nVar, dMax = dMax, coeffF = Ebola$KL, 
#'                                tDeb = 0, dt = 0.01, tFin = 2, yDeb = Ebola$yDeb)
#'
#' @export
#' @import deSolve
#' @importFrom stats sd
lyapFDGrond <- function(outLyapFD = NULL, nVar, dMax, coeffF, intgrMthod = 'rk4',
                        tDeb = 0, dt, tFin, yDeb, Ddeb=NULL,
                        nIterMin = 1, nIterStats=50) {
  
  if (nVar != dim(coeffF)[2]) {
    stop('nVar = ', nVar,
         ' not compatible with model dimension corresponding to nVar = ', dim(coeffF)[2])
  }
  if (d2pMax(nVar, dMax) != dim(coeffF)[1]) {
    stop('dMax = ', dMax,
         ' not compatible with model formulation corresponding to dMax = ',
         p2dMax(nVar, dim(coeffF)[1]))
  }
  
  if (!is.null(outLyapFD)) {
    prev_yOut <- outLyapFD$y
    prev_D <- outLyapFD$D
    prev_tOut <- outLyapFD$t
    prev_lyapOut <- outLyapFD$lyapExp
    prev_lyapLocOut <- outLyapFD$lyapExpLoc
    SumLogNorm <- outLyapFD$SumLogNorm
    tDebInit <- outLyapFD$tDebInit
    prev_stdExp <- outLyapFD$stdExp
    prev_meanExp <- outLyapFD$meanExp
    prev_Dky <- outLyapFD$Dky
    prev_DkyLoc <- outLyapFD$DkyLoc
    prev_stdDky <- outLyapFD$stdDky
    prev_meanDky <- outLyapFD$meanDky
  }
  # Nb of iterations
  nIter = round((tFin-tDeb)/dt)
  
  # Compute the Jacobian
  jaco <- jacobi(nVar,dMax,coeffF)
  
  # Initial matrix
  if (is.null(Ddeb)) {
    Ddeb <- matrix(diag(1,nVar,nVar),nrow = nVar*nVar, ncol = 1)
  }
  
  # Initial conditions
  t<-tDeb
  if (is.null(outLyapFD)) {
    tDebInit <- tDeb
    y <- yDeb
    D <- Ddeb
    SumLogNorm <- matrix(0, nrow=nVar, ncol=1)
  }
  else {
    nbLig = dim(outLyapFD$y)[1]
    y <- outLyapFD$y[nbLig,]
    D <- outLyapFD$D
  }
  
  lyapOut <- matrix(0, nrow=nVar, ncol=0)
  lyapLocOut <- matrix(0, nrow=nVar, ncol=0)
  tOut <- matrix(0, nrow=1, ncol=0)
  yOut <- matrix(0, nrow=nVar, ncol=0)
  
  for(Iter in 1:nIter){
    # concatenates the input of the ODE function
    yD <- rbind(as.matrix(y),D)
    
    # Integrates ODE equations and computes the Jacobian matrix
    yDNew <- ode(yD, (0:11)*0.1*dt, derivODEwithJ, list(coeffF,jaco), method = intgrMthod)
    
    # deconcatenates the output of the ODE function
    t <- t + dt
    y <- yDNew[dim(yDNew)[1]-1,2:(nVar+1)]
    D <- yDNew[dim(yDNew)[1]-1,(nVar+2):(nVar*(nVar+1)+1)]
    # computed the flow direction
    V <- yDNew[dim(yDNew)[1],2:(nVar+1)] - yDNew[dim(yDNew)[1]-1,2:(nVar+1)]
    NormV <- sqrt(t(V) %*% V)
    V <- V / as.vector(NormV)
    
    # Decompostion on a orthonormal basis through a Gram-Schmidt procedure
    pParam <- list(phi=matrix(0,nVar,nVar),Y=matrix(0,nVar,nVar))
    D0 <- matrix(D, nrow=nVar, ncol=nVar)
    pParam$Y <- D0
    pParam$Y[,1] <- as.vector(t(V) %*% D0[,1]) * V
    pParam$Y[,2] <- pParam$Y[,2] + D0[,1] - pParam$Y[,1]
    Norm <- D0[1,] * 0
    #     Norm[1] <- sqrt(t(D0[,1]) %*% D0[,1])
    #     D0[,1] <- D0[,1] / Norm[1]
    
    for (k in 1:(nVar)) {
      ortho <- GSproc(pParam,k)
      Norm[k] <- sqrt(t(ortho) %*% ortho)
      ortho <- ortho  / Norm[k]
      pParam$phi[,k] = ortho
    }
    
    
    if ((Iter > nIterMin) || (!is.null(outLyapFD))) {
      SumLogNorm <- SumLogNorm + log(Norm)
      lyap <- SumLogNorm / (t-tDebInit-nIterMin*dt)
      lyapLoc <- log(Norm)/dt
      #print(cbind(t,t(lyap)))
      
      tOut <- cbind(tOut,t)
      yOut <- cbind(yOut,y)
      lyapOut <- cbind(lyapOut,lyap)
      lyapLocOut <- cbind(lyapLocOut,lyapLoc)
    }
    # Update
    #y <- yDNew[dim(yDNew)[1],2:(nVar+1)]
    D <- matrix(pParam$phi,nrow=nVar*nVar,ncol=1)
    
    
  }
  
  
  
  
  
  
  Dky <- as.matrix(DkyCalc("Grond", nVar, t(lyapOut)))
  DkyLoc <- as.matrix(DkyCalc("Grond", nVar, t(lyapLocOut)))



  if (!is.null(outLyapFD)) {
    outLyapFD$y <- rbind(prev_yOut, t(yOut))
    outLyapFD$D <- D
    outLyapFD$t <- rbind(prev_tOut, t(tOut))
    outLyapFD$lyapExp <- rbind(prev_lyapOut, t(lyapOut))
    outLyapFD$lyapExpLoc <- rbind(prev_lyapLocOut, t(lyapLocOut))
    outLyapFD$SumLogNorm <- SumLogNorm
    outLyapFD$Dky <- rbind(prev_Dky, Dky)
    outLyapFD$DkyLoc <- rbind(prev_DkyLoc, DkyLoc)
  }
  else {
    outLyapFD <- list()
    outLyapFD$y <- t(yOut)
    outLyapFD$D <- D
    outLyapFD$t <- t(tOut)
    outLyapFD$lyapExp <-  t(lyapOut)
    outLyapFD$lyapExpLoc <- t(lyapLocOut)
    outLyapFD$SumLogNorm <- SumLogNorm
    outLyapFD$tDebInit <- tDeb
    outLyapFD$Dky <- Dky
    outLyapFD$DkyLoc <- DkyLoc
  }
  
  
  
  tl = outLyapFD$lyapExp
  lastIndex = length(tl[,1])
  lastIndices = seq(max(1, (lastIndex-nIterStats)),lastIndex,1)
  stdExp <- NULL
  meanExp <- NULL
  for (ind in 1:nVar) {
    stdExp[ind] <- sd(tl[lastIndices,ind])
    meanExp[ind] <- mean(tl[lastIndices,ind])
  }
  stdDky <- sd(outLyapFD$Dky[lastIndices])
  meanDky <- mean(outLyapFD$Dky[lastIndices])
  
  if (!is.null(outLyapFD$stdExp)) {
    outLyapFD$stdExp <- rbind(prev_stdExp, t(stdExp))
    outLyapFD$meanExp <- rbind(prev_meanExp, t(meanExp))
    outLyapFD$stdDky <- rbind(prev_stdDky, t(stdDky))
    outLyapFD$meanDky <- rbind(prev_meanDky, t(meanDky))
  }  
  else {
    outLyapFD$stdExp <- t(stdExp)
    outLyapFD$meanExp <- t(meanExp)
    outLyapFD$stdDky <- t(stdDky)
    outLyapFD$meanDky <- t(meanDky)    
  }
  outLyapFD$methodName <- "Grond"
  outLyapFD
}
