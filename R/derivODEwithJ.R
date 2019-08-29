#' @import GPoM
derivODEwithJ <- function (t, XandJ, listParam, polySeries = NULL, show = NULL, btnStop = NULL)
{
#    require(RGtk2)
#    if (!is.null(show)) {
#        checkPtrType(show, "GtkProgressBar")
#        show$setFraction(t/show$pulseStep)
#    }
#
#    stop <- F
#    if (!is.null(btnStop)) {
#        if (btnStop$active) {
#            stop <- T
#        }
#    }

    K <- listParam[[1]]
    jaco <- listParam[[2]]

    nVar <- dim(K)[2]
    pMax <- dim(K)[1]
    x <- XandJ[1:nVar]
    #print(x)
    J <- XandJ[(nVar+1):(nVar*(nVar+1))]
    #print(J)

#    if (!stop) {
#        if (is.null(polySeries)) {
            dMax <- p2dMax(length(x), dim(K)[1])
            polySeries <- regSeries(nVar, dMax, x)
            temp <- NULL
#        }
#        else {
#            temp <- polySeries[2:dim(polySeries)[1], ]
#            polySeries <- polySeries[1, ]
#        }
#    }
#    else {
#        return(list(0 * x, polySeries = 0 * x))
#    }

    # Computes the incrementation
    derives <- polySeries %*% K

    # derivates the formal Jacobian matrix
    #jaco <- jacobi(nVar,dMax,coeffF)
    #print(t(matrix(polySeries %*% t(jaco$J), nrow=nVar, ncol=nVar)))

    # computes the formal matrix
    Jnew <- t(matrix(polySeries %*% t(jaco$J), nrow=nVar, ncol=nVar)) %*% matrix(J, nrow=nVar, ncol=nVar)
    XandJout <- rbind(t(derives),matrix(Jnew, nrow=nVar*nVar, ncol=1))

    list(XandJout, polySeries = temp)
}
