#' Lorenz-1963 system
#'
#' @name Lorenz63
#' @docType data
#'
#' @description
#' The Lorenz-1963 model is a three-dimensional model obtained
#' by Edouard N. Lorenz in 1963. The system reads
#' \eqn{dX/dt = -\sigma Y -\sigma Z}
#' \eqn{dY/dt = X (\rho - Z) - Y}
#' \eqn{dZ/dt = X Y - \beta Z}.
#' For \eqn{(\sigma, \rho, \beta) = (10,28,8/3)}, it produces a chaotic
#' behavior. The chaotic attractor reached at the convergence
#' has become paradigmatic of chaos.
#' Its formulation can be visualized using
#' visuEq(nVar = 4, dMax = 2, K = Lrz63$KL, approx = 2)
#'
#' @author Sylvain Mangiarotti \email{sylvain.mangiarotti@cesbio.cnes.fr}
#' @references Lorenz, Edward Norton (1963). "Deterministic nonperiodic flow".
#' Journal of the Atmospheric Sciences. 20 (2): 130–141.
#'
#' @keywords data
"Lorenz63"
