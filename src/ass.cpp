#include <Rcpp.h>
using namespace Rcpp;
//' @title ass
//' @description This function for some given integer n generates all possible 0-1 assignment vectors of 2n survey participants in such a way that exactly n of them are assigned to group 0 (control) and the other n ones are assigned to group 1 (treatment).
//' 
//' @param n - a numeric value. Due to contrains n can't be larger than 30. 
//' @return A matrix with 2n columns and an appropriate number of rows. 
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix ass(int n) {

  if(n>30) stop("n is too big, I can only cope with n<=30. Sorry! ");

  struct countBits {
    // Hamming weight
    int count(unsigned long x) {
      x = x - ((x >> 1) & 0x55555555);
      x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
      x = (x + (x >> 4)) & 0x0F0F0F0F;
      x = x + (x >> 8);
      x = x + (x >> 16);
      return x & 0x0000003F;
    }
  };

  countBits counter;

  std::set<unsigned long> counts;

  unsigned long s = pow(2,n*2);

  for(unsigned long i=0; i<s; i++) {
    if(counter.count(i) == n) {
      counts.insert(i);
    }
  }
  
  NumericMatrix v(counts.size(),n*2);
  int i = 0;
  for (std::set<unsigned long>::iterator it=counts.begin(); it!=counts.end(); ++it) {
    for(int j=0; j<(n*2); j++) {
      v(i,j) = ((*it>>j) & 0x1);
    }
    i++;
  }
  
  return v;
}