#include <Rcpp.h>
using namespace Rcpp;
//' @title simplify2array
//' @description This function tries to convert list to matrix or numeric vector.
//' 
//' @param x - a list
//' @return A matrix, numeric vector or a list. If it is feasible to simplify (all elements of given x list are numeric vectors of the same length) then matrix or numeric vector is returned. Otherwise function returns unchanged given list x.
//' 
//' @export
// [[Rcpp::export]]
RObject simplify2array(List x) {

  int nrow = x.size();
  int row_len;

  for(int i=0; i<nrow; i++) {
    if(Rf_isNumeric(x[i])) {
      NumericVector n(x[i]);
      if(i==0) {
          row_len = n.size();
      } else {
        if(n.size()!=row_len) {
          return x;
        }
      }
    } else {
      return x;
    }
  }

  if(row_len==1) {
    NumericVector v(nrow);
    for(int i=0; i<nrow; i++) {
      NumericVector n(x[i]);
      v[i] = n[0];
    }
    return v;
  } else {
    NumericMatrix v(row_len,nrow);
    for(int i=0; i<nrow; i++) {
      NumericVector n(x[i]);
      for(int j=0; j<row_len; j++) {
        v(j,i) = n[j];
      }
    }
    return v;
  }
  
}