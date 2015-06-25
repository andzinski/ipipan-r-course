#include <Rcpp.h>
using namespace Rcpp;
//' @title mode
//' @description This function determines the most frequently occurring value in an integer vector (mode).
//' 
//' @param x - a numeric vector
//' @return Value that occurs most frequently in given numeric vector x. NA elements are ignored, if given vector x contains only NA elements then NA value will be returned.
//' 
//' @export
// [[Rcpp::export]]
int mode(NumericVector x) {
  
  int x_size = x.size();
  
  if(x_size==0) {
    stop("x is empty!");
  }
  
  std::map<int,int> counts;
  int c = 0;
  int v;
  
  for(int i=0; i<x_size; i++) {
    if(!NumericVector::is_na(x[i])) {
      counts[x[i]]++;
    }
  }
  
  if(counts.empty()) {
    return NA_INTEGER;
  }
  
  for(std::map<int,int>::iterator el=counts.begin(); el!=counts.end(); ++el) {
    if(el->second > c) {
      v = el->first;
      c = el->second;
    }
  }
  
  return v;
}