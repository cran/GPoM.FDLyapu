% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GSproc.R
\name{GSproc}
\alias{GSproc}
\title{GSproc : Gram-Schmidt procedure}
\usage{
GSproc(polyK, ivec, weight = NULL)
}
\arguments{
\item{polyK}{One list including $Y and $phy with:
$Y a matrix for which the ith column will be used
to add one orthogonal vector to the (i-1)th vectors of the
current orthogonal base;
and $phy such as the current orthogonal base is
given by the (i-1)th first columns of matrix polyK$phy.}

\item{ivec}{Defines i, the current vector of polyK$Y and
the current orthogonal base of pParam$phy.}

\item{weight}{The weighing vector.}
}
\value{
\code{uNew} The model parameterization, that is:
The residual orthogonal vector that can be included into
the current orthogonal base. If the current base is empty,
uNew is equal to the input vector of $Y; if the base is
complete, uNew equals 0.
}
\description{
Computes regressors coefficients
using the Gram-Schmidt procedure.
}
\author{
Sylvain Mangiarotti

#@export
}
