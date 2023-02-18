#include <Rcpp.h>

// [[Rcpp::depends(pbv)]]
#include <pbv.h>

#include<string.h>
using namespace std;
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
IntegerVector matMultinom(NumericMatrix probmatrix) {

  int rows = probmatrix.nrow();

  IntegerVector ans(rows);

  for(int i = 0; i < rows; ++i) {
    ans[i] = vecMultinom(probmatrix(i, _));
  }

  return(ans);
}

IntegerVector singleMarkovChain(NumericMatrix P, 
                                      int chainLen, int state0) {
  
  IntegerVector states(chainLen);
  states(0) = state0;
  
  for(int i = 1; i < chainLen; i++) {
    states(i) = vecMultinom( P(states(i-1) - 1, _) );
  }
  
  return states;
}

// [[Rcpp::export]]
IntegerMatrix markovChains(int nchains, NumericMatrix P, 
                                 int chainLen, IntegerVector state0) {
  
  IntegerMatrix stateMat(nchains, chainLen);
  
  for(int k = 0; k < nchains; k++) {
    stateMat(k, _) = singleMarkovChain(P, chainLen, state0(k));
  }
  
  return(stateMat);
  
}

// [[Rcpp::export]]
IntegerVector clipVec(IntegerVector id, IntegerVector seq, 
                            IntegerVector event) {
  
  int uid = unique(id).length();
  IntegerVector last(uid);
  
  int maxperiod = max(seq);
  int x_uid = 0;
  
  for(int i=0; i<id.length(); ++i) {
    
    if (seq[i] == 1) {
      x_uid = id[i] - 1;
    }
    
    if (last[x_uid] == 0) {
      
      if (seq[i] < maxperiod) {
        if (event[i] == 1) last[x_uid] = seq[i];
      } else last[x_uid] = seq[i];
      
    }
    
  }
  return last;
}

// [[Rcpp::export]]
bool chkNonIncreasing(NumericMatrix adjmatrix) {
  
  int nr = adjmatrix.nrow(), nc = adjmatrix.ncol();
  bool unsorted = FALSE;
  
  int i = 0;
  
  do {
  
    for (int j = 1; j < nc; j++) {
        if (adjmatrix(i, j-1) > adjmatrix(i, j)) {
          unsorted = TRUE;
        };
      }
    
    ++i;
    
  } while(i<nr && unsorted == FALSE);
  
  return(unsorted);
}

// [[Rcpp::export]]
void checkBoundsBin(double p1, double p2, double d) {
  
  double l =  (p1 * p2) / ((1 - p1) * (1 - p2));
  double L = std::max(-sqrt(l), -sqrt(1 / l));
  
  double u = (p1 * (1 - p2)) / (p2 * (1 - p1));
  double U = std::min(sqrt(u), sqrt(1 / u));
  
  string LU;
  string stopText;
  
  if ((d < L)  | (d > U)) {
    LU = "(" + to_string(L) + " ... " + to_string(U) + ")";
    stopText = "Specified correlation " + to_string(d) + " out of range " + LU;
    stop(stopText);
  } 
}

// [[Rcpp::export]]
double findRhoBin(double p1, double p2, double d) {
  
  checkBoundsBin(p1, p2, d);

  double target;
  double Max = 1;
  double Min = -1;
  double test = 0;
  bool found = FALSE;
  NumericVector bounds(2);
  double est;
  double rho;
  NumericVector check(2);

 target = d * sqrt(p1 * p2 * (1 - p1) * (1 - p2)) + p1 * p2;
 bounds(0) = R::qnorm(p1, 0, 1, TRUE, FALSE);
 bounds(1) = R::qnorm(p2, 0, 1, TRUE, FALSE); 
  
// given p1, p2 & d, bisection search for corresponding rho
    
  while (!found) {

      est = pbv::pbv_rcpp_pbvnorm0(bounds(0), bounds(1), test);
      
      check(0) = est;
      check(1) = target;
      check = round(check, 5);
      
      if (check(0) == check(1)) {
        found = TRUE;
        rho = test;
      } 
      else if (est < target) {
        Min = test;
        test = (Min + Max) / 2;
      } 
      else {
        Max = test;
        test = (Min + Max) / 2;
      } 
    }
    
    return(rho);
} 

// [[Rcpp::export]]
NumericMatrix getRhoMat(int N, NumericVector P, NumericMatrix TCORR) {
  
  NumericMatrix PCORR(TCORR.nrow(), TCORR.ncol());
  double p1;
  double p2;
  double rho;
  
  for (int i = 0; i < (N - 1); i++) {
    for (int j = (i+1); j < N; j++) {
      p1 = P[i];
      p2 = P[j];
      rho = findRhoBin(p1, p2, TCORR(i, j));
      PCORR(i, j) = rho;
      PCORR(j, i) = rho;
    }
  }
  
  for (int i = 0; i < N; i++) {
    PCORR(i, i) = 1;
  }
  
  return(PCORR);
  
}

