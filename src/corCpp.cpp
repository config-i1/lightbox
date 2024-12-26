#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

/* # This is the correlation implemented in C++ - consumes less memory */
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
arma::mat corCpp(arma::vec const &y, arma::mat const &x){
    return arma::cor(y, x);
}