#include <Rcpp.h>
using namespace Rcpp;

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

Rcpp::IntegerVector singleMarkovChain(NumericMatrix P, 
                                      int chainLen, int state0) {
  
  IntegerVector states(chainLen);
  states(0) = state0;
  
  for(int i = 1; i < chainLen; i++) {
    states(i) = vecMultinom( P(states(i-1) - 1, _) );
  }
  
  return states;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix markovChains(int nchains, NumericMatrix P, 
                                 int chainLen, IntegerVector state0) {
  
  IntegerMatrix stateMat(nchains, chainLen);
  
  for(int k = 0; k < nchains; k++) {
    stateMat(k, _) = singleMarkovChain(P, chainLen, state0(k));
  }
  
  return(stateMat);
  
}

// [[Rcpp::export]]
Rcpp::IntegerVector clipVec(IntegerVector id, IntegerVector seq, IntegerVector event) {
  
  int uid = unique(id).length();
  IntegerVector last(uid);
  
  int maxperiod = max(seq);
  int xid;
  
  for(int i=0; i<id.length(); ++i) {
    
    if (seq[i] == 1) {
      xid = id[i] - 1;
    }
    
    if (last[xid] == 0) {
      
      if (seq[i] < maxperiod) {
        if (event[i] == 1) last[xid] = seq[i];
      } else last[xid] = seq[i];
      
    }
    
  }
  return last;
}


