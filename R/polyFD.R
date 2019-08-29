polyFD <- function(nVar,dMax,toDeriv,coeff,xwhat) {

  pMax <- choose(dMax+nVar,nVar)
  labels <- vector("character",length(toDeriv))
  if (is.vector(toDeriv)) {
      toDeriv <- t(t(toDeriv))
  }
  # derivation
  newcoeff <- toDeriv[xwhat,]
  Deriv <- toDeriv
  Deriv[xwhat,] <- toDeriv[xwhat,]-1

  for (i in 1:dim(toDeriv)[2]) {
      if (Deriv[xwhat,i]==-1) {
            Deriv[,i] <- 0
        }
    }
    newcoeff <- newcoeff * coeff

    struct <- regOrd(nVar,dMax)
    coeffOut <- matrix(0, ncol = dim(struct)[2], nrow=1)
    polyOut  <- matrix(0, ncol = dim(struct)[2], nrow=1)
  for (i in 1:dim(Deriv)[2]) {
      ok <- FALSE
      k <- 1
      while (!ok) {
             test <- (Deriv[,i]==struct[,k])
             test <- test %*% test
             if (test == nVar) {
               ok <- TRUE
               coeffOut[k] <- coeffOut[k] + newcoeff[i]
               polyOut[k] <- 1
           }
           k <- k + 1
      }
  }
  #print(coeffOut)
    #print(labelPoly(nVar,dMax,polyOut*1:dim(struct)[2]))
    if (coeffOut[1] == 0) {
        polyOut[1] <- 0
    }
    DrvedPoly <- list()
  DrvedPoly$coeff <- coeffOut
  DrvedPoly$poly <- polyOut

  DrvedPoly
}
