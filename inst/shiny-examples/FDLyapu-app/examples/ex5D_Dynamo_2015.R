  nVar <<- 5
  dMax <<- 2
#  poLabs(nVar, dMax)
  # model formulation
  # coeff
  a = 1
  b = 2
  c = 0.7
  p = 1.1
  q = 0.1
  # equations
  Eq1 <- c(0, q, 0, -p, 0, 0, 0, 0, 0, 0,  0, 0, 0, 1, 0, -a, 0, 0, 0,  0, 0)
  Eq2 <- c(0, q, 0, -p, 0, 0, 0, 0, 0, 0, -a, 0, 0, 0, 0, -b, 0, 0, 1,  0, 0)
  Eq3 <- c(1, 0, 0,  0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, -1, 0)
  Eq4 <- c(0, 0, 0,  0, 0, 0, 0, 0, 0, 0,  c, 0, 0, 0, 0,  0, 0, 0, 0,  0, 0)
  Eq5 <- c(0, 0, 0,  q, 0, 0, 0, 0, 0, 0,  q, 0, 0, 0, 0,  q, 0, 0, 0,  0, 0)
  KL <- cbind(Eq1, Eq2, Eq3, Eq4, Eq5)
#  visuEq(nVar = nVar, dMax = dMax, K = KL, substit = c("x1", "x2", "x3", "x4", "x5"))
  #
  # numerical integration
  yDeb <- c(0.8, 0.2, 0.4, 0.2, 0.1)
#  outNumi <- numicano(nVar = nVar, dMax = dMax, Istep = 100000, onestep = 1/100, KL = KL, v0 = yDeb, method = 'rk4')
#  plot(outNumi$reconstr[,1], outNumi$reconstr[,2], type = 'l')
  intgrMthod <<- "ode45"
  nIterMin <<- 1
  tEnd <<- 20.
  printIter <<- 100
  lastIter <<- 500
  timeStep <<- 0.01
  