#' Light box
#'
#' Toolbox for working with univariate models for purposes of analysis and forecasting
#'
#' \tabular{ll}{ Package: \tab lightbox\cr Type: \tab Package\cr Date: \tab
#' 2024-12-25 - Inf\cr License: \tab LGPL-2.1 \cr } The following functions are
#' included in the package:
#' \itemize{
#' \item \link[lightbox]{lightlm} - Advanced Linear Model - regression, estimated using
#' likelihood with specified distribution (e.g. Laplace or Chi-Squared).
#' \item \link[lightbox]{lightstep} - Stepwise based on information criteria and partial
#' correlations. Efficient and fast.
#' \item \link[lightbox]{lightcombine} - Function combines lm models from the estimated
#' based on information criteria weights.
#' }
#'
#' @name lightbox
#' @aliases lightbox-package
#' @template author
#'
#' @seealso \code{\link[lightbox]{lightstep}, \link[lightbox]{lightcombine}}
#'
#' @template keywords
#'
#' @examples
#'
#' \donttest{
#' xreg <- cbind(rnorm(100,10,3),rnorm(100,50,5))
#' xreg <- cbind(100+0.5*xreg[,1]-0.75*xreg[,2]+rnorm(100,0,3),xreg,rnorm(100,300,10))
#' colnames(xreg) <- c("y","x1","x2","Noise")
#'
#' lightstep(xreg)
#'}
#'
#' @importFrom graphics abline layout legend lines par points polygon plot
#' @importFrom stats AIC BIC logLik cov deltat end frequency is.ts cor start time ts var lm as.formula residuals coef fitted
#' @importFrom utils packageVersion tail
#' @useDynLib lightbox
NULL



