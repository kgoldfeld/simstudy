#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

int vecMultinom(NumericVector probs) {

  int k = probs.size();

  IntegerVector ans(k);
  rmultinom(1, probs.begin(), k, ans.begin());

  int total = 0;

  for(int i = 0; i < k; ++i) {
    total += ans[i] * (i + 1);
  }

  return(total);

}

// [[Rcpp::export]]

Rcpp::IntegerVector matMultinom(Rcpp::NumericMatrix probmatrix) {

  int rows = probmatrix.nrow();

  Rcpp::IntegerVector ans(rows);

  for(int i = 0; i < rows; ++i) {
    ans[i] = vecMultinom(probmatrix(i, _));
  }

  return(ans);

}
