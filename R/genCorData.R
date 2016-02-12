#' Create correlated data
#'
#' @param n Number of observations
#' @param nvars Number of columns of correlated data
#' @param mu A vector of means. The length of mu must be nvars.
#' @param sigma Common variance shared by all variables
#' @param rho Correlation coefficient, -1 <= rho <= 1.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "ind" for an independence 
#' structure, "cs" for a compound symmetry structure, and "ar1" for an
#' autoregressive structure.
#' @param cnames Explicit column names. A single string with names separated
#' by commas. If no string is provided, the default names will be V#, where #
#' represents the column.
#' @param idname The name of the index id name. Defaults to "id."
#' @return A data.table with n rows and nvars + 1 columns
#' @export
#'

# dtName must contain id for now

genCorData <- function(n, nvars, mu, sigma, rho, corstr = "ind", 
                       cnames=NULL, idname = "id") {
  
  x <- diag(nvars)
  
  if (corstr == "ind") {
    x <- sigma * x
  } else if (corstr == "cs") {
    x <- sigma * rho^(row(x) != col(x))    
  } else if (corstr == "ar1") {
    x <- sigma * rho^abs(row(x)-col(x))    
  }
  
  dt <- data.table(mvnfast::rmvn(n = n , mu = mu, sigma = x))
  
  if (length(cnames)) {
    nnames <- trimws(unlist(strsplit(cnames, split = ",")))
    setnames(dt, nnames)
  }
  
  dtid <- data.table(1:nrow(dt))
  setnames(dtid,idname)
  
  cbind(dtid, dt)
  
}
