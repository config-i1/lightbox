#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

/* # Function allows to multiply polynomials */
// [[Rcpp::export]]
arma::mat corCpp(arma::vec const &y, arma::mat const &x){
    return arma::cor(y, x);
}