## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##"
)

## ---- eval=TRUE----------------------------------------------------------
# parameters
a = 0.52
b = 2
c = 4
# equations
Eq1 <- c(0,-1, 0,-1, 0, 0, 0, 0, 0, 0)
Eq2 <- c(0, 0, 0, a, 0, 0, 1, 0, 0, 0)
Eq3 <- c(b,-c, 0, 0, 0, 0, 0, 1, 0, 0)
K = cbind(Eq1, Eq2, Eq3)

## ---- eval=TRUE----------------------------------------------------------
visuEq(nVar = 3, dMax = 2, K = K, substit = c("x", "y", "z"))

## ---- eval=TRUE----------------------------------------------------------
# The dynamical system equations (just defined by matrix K)
#
# The number of variables (it can be deduced from matrix K)
nVar = dim(K)[2]
# The maximum polynomial degree of the formulation (also deduced from K)
pMax = dim(K)[1]
dMax = p2dMax(nVar, pMax)
# The initial conditions
inicond <- c(-0.04298734, 1.025536, 0.09057987)
# The integration time step
timeStep <- 0.01
# The initial and ending integration time
tDeb <- 0
tFin <- 10

## ---- eval=FALSE---------------------------------------------------------
#  # Prepare the output file
#  outLyapFD <- NULL
#  # Method 1
#  outLyapFD$Wolf <- lyapFDWolf(outLyapFD$Wolf, nVar= nVar, dMax = dMax,
#                               coeffF = K,
#                               tDeb = tDeb, dt = timeStep, tFin = tFin,
#                               yDeb = inicond)
#  # Method 2
#  outLyapFD$Grond <- lyapFDGrond(outLyapFD$Grond, nVar= nVar, dMax = dMax,
#                               coeffF = K,
#                               tDeb = tDeb, dt = timeStep, tFin = tFin,
#                               yDeb = inicond)

## ---- echo = FALSE, eval=TRUE--------------------------------------------
# To avoid a long time computing, the results are directly loaded
load("../data/outLyapFD.rda")
outLyapFD <- outLyapFD$Ro76

## ---- eval=TRUE----------------------------------------------------------
names(outLyapFD$Wolf)

## ---- eval=TRUE,  fig.align = 'center', fig.width = 4, fig.height = 4----
plot(outLyapFD$Wolf$y[,1], outLyapFD$Wolf$y[,2], type ='l', xlab = 'x', ylab = 'y')

## ---- eval=TRUE----------------------------------------------------------
# For method 1 (Wolf et al. 1985)
outLyapFD$Wolf$meanExp
#
# For method 2 (Grond et al. 2003)
outLyapFD$Grond$meanExp

## ---- eval=TRUE----------------------------------------------------------
# For method 1 (Wolf et al. 1985)
outLyapFD$Wolf$stdExp
#
# For method 2 (Grond et al. 2003)
outLyapFD$Grond$stdExp

## ---- eval=TRUE,  fig.align = 'center'-----------------------------------
plotMeanExponents(nVar, outLyapFD$Wolf, nIterStats = 400, xlim = c(1,9999), legend=TRUE)

## ---- eval=TRUE,  fig.align = 'center'-----------------------------------
plotMeanExponents(nVar, outLyapFD$Wolf, nIterStats = 1000, expList = c(TRUE, FALSE, FALSE), legend=TRUE)

## ---- eval=TRUE,  fig.align = 'center'-----------------------------------
plotMeanExponents(nVar, outLyapFD$Grond, nIterStats = 1000, expList = c(TRUE, TRUE, FALSE), legend=TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  # For method 1 (Wolf et al. 1985)
#  outLyapFD$Wolf$meanDky
#  #
#  # For method 2 (Grond et al. 2003)
#  outLyapFD$Grond$meanDky

## ---- eval=FALSE---------------------------------------------------------
#  # For method 1 (Wolf et al. 1985)
#  outLyapFD$Wolf$stdDky
#  #
#  # For method 2 (Grond et al. 2003)
#  outLyapFD$Grond$stdDky

## ---- eval=TRUE,  fig.align = 'center'-----------------------------------
plotLocalExponents(nVar, outLyapFD$Grond, legend=TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  shiny::runApp('../inst/shiny-examples/FDLyapu-app')

## ---- echo = FALSE, eval=TRUE,  fig.align = 'center'---------------------
  # load the model
  data("allMod_nVar3_dMax2")
  K <- allMod_nVar3_dMax2$L63
  # Edit the equations
  visuEq(nVar = 3, dMax = 2, K = allMod_nVar3_dMax2$L63, substit = 1, approx = 3)

## ---- echo = FALSE, eval=TRUE,  fig.align = 'center'---------------------
  # load the model
  source("../inst/shiny-examples/FDLyapu-app/examples/ex4D_Rossler_1979.R")
  visuEq(nVar = 4, dMax = 2, K = K, substit = 1)

## ---- eval=FALSE---------------------------------------------------------
#  # Prepare the output file
#  outLyapFD <- NULL
#  # Method 2
#  outLyapFD$Grond <- lyapFDGrond(outLyapFD$Grond, nVar= 4, dMax = 2, coeffF = K,
#                                 tDeb = 0, dt = timeStep, tFin = 250, yDeb = c(-10,-6,0,10))

## ---- echo = FALSE, eval=TRUE--------------------------------------------
# To avoid a long time computing, the results are directly loaded
load("../data/outLyapFD.rda")
outLyapFD <- outLyapFD$Ro79

## ---- eval=TRUE,  fig.align = 'center'-----------------------------------
# Plot the results
plotMeanExponents(nVar, outLyapFD$Grond, nIterStats = 1000, expList = c(TRUE, TRUE, TRUE, FALSE), legend=TRUE)

## ---- eval = TRUE--------------------------------------------------------
outLyapFD$Grond$meanExp
outLyapFD$Grond$stdExp

## ---- eval = TRUE--------------------------------------------------------
outLyapFD$Grond$meanDky
outLyapFD$Grond$stdDky

## ---- echo = TRUE, eval=TRUE,  fig.align = 'center'----------------------
  # load the model
  data(Plague)
  # Model 0 (10-term)
  # tuning
  KL0 <- Plague$models$model93
  KL0[7,1] <- KL0[7,1]*0.598
  visuEq(nVar = 3, dMax = 2, KL0, approx = 2, substit = 1)
  
  # Model 1 (11-term) directly chaotic
   KL1 <- Plague$models$model129
   visuEq(nVar = 3, dMax = 2, KL1, approx = 2, substit = 1)

## ---- echo = FALSE, eval=TRUE,  fig.align = 'center'---------------------
  # load the model
  source("../inst/shiny-examples/FDLyapu-app/examples/ex4D_EbolaModel_2016.R")
  visuEq(nVar = 4, dMax = 2, K = KL, substit = c("I", "D1", "D2", "D3"), approx = 2)

## ---- echo = FALSE, eval=TRUE,  fig.align = 'center'---------------------
  # load the model
  source("../inst/shiny-examples/FDLyapu-app/examples/ex5D_Dynamo_2015.R")
  visuEq(nVar = 5, dMax = 2, K = KL, approx = 3)

## ---- echo = FALSE, eval=TRUE,  fig.align = 'center'---------------------
  # load the model
  source("../inst/shiny-examples/FDLyapu-app/examples/ex9D_RayleighBenard_1998.R")
  visuEq(nVar = 9, dMax = 2, K = KL, approx = 2,
         substit = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"))

