#########
# Rossler 79 #
#########

# parameters:
a <- 0.25
b <- 3
c <- 0.5
d <- 0.05
# model dimension
nVar = 4
# polynomial max degree
dMax = 2
# build model matrix
K <- matrix(0,nrow=15,ncol=4)
K[4,1] <- K[7,1] <-  -1
K[2,2] <- K[11,2] <- K[13,3] <- 1
K[7,2] <- a
K[1,3] <- b
K[4,4] <- -c
K[2,4] <- d
# initial conditions
etat0 <- c(-10,-6,0,10)

KL <<- K
nVar <<- nVar
dMax <<- dMax
yDeb <<- etat0
intgrMthod <<- "rk4"
nIterMin <<- 1
tEnd <<- 100.
timeStep <<- 0.02
printIter <<- 100
lastIter <<- 50
