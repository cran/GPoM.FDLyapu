#' Ebola model for the epidemic of Ebola Virus Disease
#'
#' @name Ebola
#' @docType data
#'
#' @description
#' The Ebola model is a four-dimensional model obtained
#' by the global modelling technique from a set of two
#' observational variables. The model reads
#' \eqn{dX1/dt = a1 Y1 Y3 + a2 Y2 - a3 X1 Y1}
#' \eqn{dY1/dt = Y2}
#' \eqn{dY2/dt = Y3}
#' \eqn{dY3/dt = b1 + b2 Y3 + b3 Y3^2 - b4 Y2 - b5 Y2^2 + b6 Y1
#'          -b7 Y1 Y3 + b8 Y1 Y2 - b9Y2 - b10 X1
#'          -b11 X1 Y3 - b12 X1 Y2 + b13 X1 Y1 + b14X1^2}.
#' It produces a chaotic behavior and its attractor has
#' a fractal dimension Dky = 3.03.
#' Its formulation can be visualized using
#' visuEq(nVar = 4, dMax = 2, K = Ebola$KL, approx = 2)
#' (the number of digit is truncated)
#'
#' @author Sylvain Mangiarotti \email{sylvain.mangiarotti@cesbio.cnes.fr}
#' @references
#' [1] Sylvain Mangiarotti, Marisa Peyre, and Mireille Huc,
#' A chaotic model for the epidemic of Ebola virus disease
#' in West Africa (2013-2016), Chaos, 26, 113112, 2016.
#'
#' @keywords data
"Ebola"
