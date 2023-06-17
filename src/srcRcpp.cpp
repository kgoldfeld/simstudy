#include <Rcpp.h>
#include <pbv.h>
#include <string.h>

// [[Rcpp::depends(pbv)]]

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

// [[Rcpp::export]]
double getBeta0(NumericVector lvec, double popPrev, double tolerance) {
  
  double intLow = -10;
  double intHigh = 10;
  double B0;
  double PREV;
  NumericVector ps(lvec.length());
  NumericVector nvec(lvec.length());
  
  while(abs(intHigh - intLow) > tolerance){
    
    B0 = (intLow + intHigh)/2;
    for (int i = 0; i < lvec.length(); i++) {
      nvec(i) = lvec(i) + B0;
    }
    ps = Rcpp::plogis(nvec);
    PREV = mean(ps);
    
    if (PREV < popPrev) {
      intLow = B0;
    } else {
      intHigh = B0;
    }
    
  }
  
  B0 =  (intLow + intHigh)/2;
  
  return(B0);
  
}



// [[Rcpp::export]]
double estAUC(NumericMatrix dmatrix, NumericVector y) {
  
  int aucN = 1000000;
  
  int N0 = y.length() - sum(y);
  int N1 = sum(y);
  
  NumericVector P1(N1);
  NumericVector P0(N0);
  
  // Obtaining namespace of fastglm package
  Environment pkg = Environment::namespace_env("fastglm");
  Function f = pkg["fastglm"];
  Function p = pkg["predict.fastglm"];

  // model should be include "Named("family", "binomial")", but occasionally 
  // throws off warning. Using Gaussian works very slightly less well, but
  // there are no warnings
  
  List fit = f(Named("x", dmatrix), Named("y", y)); 
  NumericVector pred = p(Named("object", fit), Named("newdata", dmatrix));
  
  int i1 = 0;
  int i0 = 0;
  
  for (int i = 0; i < y.length(); i++) {
    if ( y(i) == 1) {
      P1[i1] = pred(i);
      i1++;
    } else {
      P0[i0] = pred(i);
      i0++;
    }
  } 
  
  NumericVector s1 = P1[floor(Rcpp::runif(aucN, 0, N1))];
  NumericVector s0 = P0[floor(Rcpp::runif(aucN, 0, N0))];
  
  double n_greater = 0;
  
  for (int i = 0; i < aucN; i++) {
    if (s1(i) > s0(i)) n_greater++;
  }
  
  double AUC = n_greater/aucN;
  
  return(AUC);
  
}

// [[Rcpp::export]]
NumericVector getBeta_auc(NumericMatrix covmat, NumericVector coefs, double auc,
    double popPrev, double tolerance) {
  
  Environment base("package:base");
  Function mat_Mult = base["%*%"];
  
  int N = covmat.nrow();
  
  double intLow = 0;
  double intHigh = 40;
  
  double alpha;
  double B0;
  double aStat;
  NumericVector ps(N);
  NumericVector lvec(N);
  NumericVector avec(coefs.length());
  NumericVector avecint(coefs.length() + 1);
  
  NumericVector onevec(N); 
  NumericVector y(N); 
  
  NumericVector results(2);
  
  for (int i = 0; i < N; i++) {
    onevec(i) = 1;
  }
  
  NumericMatrix dmatrix = cbind(onevec, covmat);
  
  lvec = mat_Mult(covmat, coefs);
  B0 = getBeta0(lvec, popPrev, tolerance);
  avecint(0) = B0;
  
  aStat = 0;
  
  while(abs(aStat - auc) > tolerance) {
    
    alpha = (intLow + intHigh)/2;
    
    for (int i = 0; i < coefs.length(); i++) {
      avecint(i+1) = alpha * coefs(i);
    }
    
    lvec = mat_Mult(dmatrix, avecint);
    ps = Rcpp::plogis(lvec);
    
    for (int i=0; i<N; i++) {
      y(i) = as<double>(rbinom(1, 1, ps(i)));
    }
    
    aStat = estAUC(dmatrix, y);
    
    // Rcout << auc << " " << alpha << " " << aStat << "\n";
    
    if (aStat < auc) {
      intLow = alpha;
    } else {
      intHigh = alpha;
    }
    
  }
  
  alpha =  (intLow + intHigh)/2;
  
  for (int i = 0; i < coefs.length(); i++) {
    avec(i) = alpha * coefs(i);
  }
  
  lvec = mat_Mult(covmat, avec);
  B0 = getBeta0(lvec, popPrev, tolerance);
  
  results(0) = B0;
  results(1) = alpha;
  
  return(results);
  
}

