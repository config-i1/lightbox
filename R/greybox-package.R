#' Light box
#'
#' Toolbox for working with univariate models for purposes of analysis and forecasting
#'
#' \tabular{ll}{ Package: \tab lightbox\cr Type: \tab Package\cr Date: \tab
#' 2024-12-25 - Inf\cr License: \tab LGPL-2.1 \cr } The following functions are
#' included in the package:
#' \itemize{
#' \item \link[lightbox]{determination} - Coefficients of determination between different
#' exogenous variables.
#' \item \link[lightbox]{alm} - Advanced Linear Model - regression, estimated using
#' likelihood with specified distribution (e.g. Laplace or Chi-Squared).
#' \item \link[lightbox]{stepwise} - Stepwise based on information criteria and partial
#' correlations. Efficient and fast.
#' \item \link[lightbox]{lmCombine} - Function combines lm models from the estimated
#' based on information criteria weights.
#' \item \link[lightbox]{lmDynamic} - Dynamic regression based on point AIC.
#' }
#'
#' @name lightbox
#' @aliases lightbox-package
#' @template author
#'
#' @seealso \code{\link[lightbox]{stepwise}, \link[lightbox]{lmCombine}}
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
#' stepwise(xreg)
#'}
#'
#' @importFrom graphics abline layout legend lines par points polygon plot
#' @importFrom stats AIC BIC logLik cov deltat end frequency is.ts cor start time ts var lm as.formula residuals
#' @importFrom utils packageVersion
NULL



