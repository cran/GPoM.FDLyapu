#' @title GPoM.FDLyapu package: Lyapunov Exponents and Kaplan-Yorke Dimension 
#' @description Estimation of the spectrum of Lyapunov Exponents and the Kaplan-Yorke 
#' dimension of any low-dimensional model of polynomial form.
#' It can be applied, for example, to systems such as the chaotic
#' Lorenz-1963 system or the hyperchaotic Rossler-1979 system.
#' It can also be applied to dynamical models in Ordinary Differential Equations 
#' (ODEs) directly obtained from observational time series using the 'GPoM' package. The used approach 
#' is semi-formal, the Jacobian matrix being estimated automatically from the polynomial equations.
#' Two methods are made available : one introduced by Wolf et al. (1985) [1]
#' and the other one by Grond et al. (2003) [2].
#' @author Sylvain Mangiarotti, Mireille Huc.
#'
#' Maintainer: M. Huc <mireille.huc@@cesbio.cnes.fr>
#'
#' @references
#' [1] A. Wolf, J. B. Swift, H. L. Swinney & J. A. Vastano,
#' Determining Lyapunov exponents from a time series,
#' Physica D, 285-317, 1985. \cr
#' [2] F. Grond, H. H. Diebner, S. Sahle, A. Mathias, S. Fischer,
#' O. E. Rossler, A robust, locally interpretable algorithm for
#' Lyapunov exponents, Chaos, Solitons \& Fractals, 16, 841-852 (2003). \cr
#'
#' @note
#' FOR USERS \cr
#' This package was developped at Centre d'Etudes Spatiales de
#' la Biosphere (Cesbio, UMR 5126, UPS-CNRS-CNES-IRD,
#' http://www.cesbio.ups-tlse.fr).
#' An important part of the developments were funded by
#' the French program Les Enveloppes Fluides et l'Environnement
#' (LEFE, MANU, projets GloMo, SpatioGloMo and MoMu).
#' The French program Défi InFiNiTi (CNRS) and PNTS
#' are also acknowledged (projects Crops'IChaos and Musc & SlowFast).
#'
#'
#' @encoding UTF-8
#'
#' @docType package
#' @name GPoM.FDLyapu-package
#' @concept Lyapunov exponents
#' @concept Kaplan-Yorke dimension
NA
