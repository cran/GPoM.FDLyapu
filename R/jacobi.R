jacobi <- function(nVar,dMax,coeffF,strF=NULL) {

   strRef <- regOrd(nVar,dMax)
   pMax <- choose(dMax+nVar,nVar)

   # structure of Lorenz model
   if (is.null(strF)) {
       strF <- coeffF * 0 + 1
       strF[coeffF == 0] <- 0
   }
   #labelPoly(nVar,dMax,strF[,1]*1:pMax)
   #labelPoly(nVar,dMax,strF[,2]*1:pMax)
   #labelPoly(nVar,dMax,strF[,3]*1:pMax)
   # coefficients
   #print(coeffF)

   # Compute Jacob
   J <- matrix(0, ncol = pMax, nrow=0)
   lc <- matrix(0, ncol = 2, nrow=0)
   for (i in 1:nVar) {
        for (j in 1:nVar) {
             toBeDerived <- strRef[,strF[,i]*1:pMax]
             l <- (1:dim(coeffF)[1])[coeffF[,i]!=0]
             dpoly <- polyFD(nVar,dMax,toBeDerived,coeffF[l,i],j)
             #print(c(i,j))
             #print(dpoly$coeff)
             # and terms:
             #print(labelPoly(nVar,dMax, dpoly$poly*(1:pMax)))
             J <- rbind(J,dpoly$coeff)
             lc <- rbind(lc,c(i,j))
        }
   }
   Jac <- list()
   Jac$J <- J
   Jac$lc <- lc
   Jac
}
