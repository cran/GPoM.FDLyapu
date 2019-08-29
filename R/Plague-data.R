#' eco-epidemiology Plague model for the epidemic-epizootic of plague in Bombay (1896-1911)
#'
#' @name Plague
#' @docType data
#'
#' @description
#' The eco-epidemiolgy plague model is a three-dimensional model
#' obtained using the global modelling technique from a set of three
#' observational variables:
#' - X the number of human cases
#' - Y the number of contaminated brown rats
#' - Z the number of contaminated black rats
#' \eqn{dX/dt = fx(X,Y,Z)}
#' \eqn{dY/dt = fy(X,Y,Z)}
#' \eqn{dZ/dt = fz(X,Y,Z)}.
#' It can produce a chaotic behavior by tuning its parameterization.
#' Its formulation can be visualized using
#' visuEq(K = GP_DRDT_1110_dM2$models$model93, approx = 2, substit = 1)
#' (the number of digit is truncated)
#'
#' @author Sylvain Mangiarotti \email{sylvain.mangiarotti@cesbio.cnes.fr}
#' @references
#' [1] Sylvain Mangiarotti,
#' 	Low dimensional chaotic models for the plague epidemic
#' 	in Bombay (1896-1911), Chaos Solitons \& Fractals, 26, 113112, 2016.
#'
#' @keywords data
"Plague"
