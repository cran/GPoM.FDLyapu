data("Reit98")
KL <<- Reit98
nVar <<- 9
dMax <<- 2
yDeb <<- etat0 <- c(3.105053, 3.335297, -2.367497,
                    3.276356, 0.2985118, -2.195064,
                    -35.06073, -20.59253, -18.33939)
intgrMthod <<- "rk4"
nIterMin <<- 1
tEnd <<- 0.5
timeStep <<- 0.001
printIter <<- 10
lastIter <<- 50
