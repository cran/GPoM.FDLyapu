#' @title GSproc : Gram-Schmidt procedure
#'
#' @description Computes regressors coefficients
#' using the Gram-Schmidt procedure.
#'
#' @param polyK One list including $Y and $phy with:
#' $Y a matrix for which the ith column will be used
#' to add one orthogonal vector to the (i-1)th vectors of the
#' current orthogonal base;
#' and $phy such as the current orthogonal base is
#' given by the (i-1)th first columns of matrix polyK$phy.
#' @param ivec Defines i, the current vector of polyK$Y and
#' the current orthogonal base of pParam$phy.
#' @param weight The weighing vector.
#'
#' @return \code{uNew} The model parameterization, that is:
#' The residual orthogonal vector that can be included into
#' the current orthogonal base. If the current base is empty,
#' uNew is equal to the input vector of $Y; if the base is
#' complete, uNew equals 0.
#'
#' @author Sylvain Mangiarotti
#'
#' #@export
GSproc <- function(polyK, ivec, weight = NULL) {
  # initiate uNew and keep a memory in u
  uNew <- u <- polyK$Y[, ivec]
  if (ivec > 1) {
    # Gram-Schmidt iterations
    for (i in 1:(ivec - 1)) {
      v <- polyK$phi[,i]
      # compute the norm
      normV <- wInProd(v, v, weight)
      # compute the projection
      proj <- as.vector(wInProd(u, v, weight) / normV) * v
      # compute the new vector
      uNew <- uNew - proj
    }
  }
  # output vector
  uNew
}
