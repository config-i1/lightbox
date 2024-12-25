#include <Rcpp.h>
using namespace Rcpp;

/* # Function allows to multiply polynomials */
NumericVector corCpp(NumericVector const &poly1, NumericVector const &poly2){

    int poly1Nonzero = poly1.size()-1;
    int poly2Nonzero = poly2.size()-1;

    NumericVector poly3(poly1Nonzero + poly2Nonzero + 1);

    for(int i = 0; i <= poly1Nonzero; ++i){
        for(int j = 0; j <= poly2Nonzero; ++j){
            poly3[i+j] += poly1[i] * poly2[j];
        }
    }

    return poly3;
}