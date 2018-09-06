#' Generate variance for random effects that produce desired intra-class
#' coefficients (ICCs) for clustered data.
#'
#' @param ICC Vector of values between 0 and 1 that represent the
#' target ICC levels
#' @param dist The distribution that describes the outcome data at the 
#' individual level. Possible distributions include "normal", "binary",
#' "poisson", or "gamma"
#' @param varTotal Numeric value that represents the total variation for a
#' normally distributed model. If "normal" distribution is specified, either
#' varTotal or varWithin must be specified, but not both.
#' @param varWithin Numeric value that represents the variation within a
#' cluster for a normally distributed model. If "normal" distribution is 
#' specified, either varTotal or varWithin must be specified, but not both.
#' @param lambda Numeric value that represents the grand mean. Must be specified
#' when distribution is "poisson".
#' @param disp Numeric value that represents the dispersion parameter that is used
#' to define a gamma distribution with a log link. Must be specified when
#' distribution is "gamma".
#' @return A vector of values that represents the variances of random effects
#' at the cluster level that correspond to the ICC vector.
#' @examples
#' targetICC <- seq(0.05, 0.20, by = .01)
#'
#' iccRE(targetICC, "poisson", lambda = 30)
#'
#' iccRE(targetICC, "binary")
#'
#' iccRE(targetICC, "normal", varTotal = 100)
#' iccRE(targetICC, "normal", varWithin = 100)
#'
#' iccRE(targetICC, "gamma", disp = .5)
#' 
#' @export
#'
iccRE <- function(ICC, dist, varTotal = NULL, varWithin = NULL, lambda = NULL, disp =NULL) {
  
  if (dist == "poisson") {
    
    if (is.null(lambda)) stop("Specify a value for lambda")
    
    vars <- unlist(lapply(ICC, function(x) .findPoisVar(1/lambda * x / (1-x))))
    
  } else if (dist == "binary") {
    
    vars <- unlist(lapply(ICC, function(x) ( x/(1-x) * (pi^2)/3 )))
    
  } else if (dist == "normal") {
    
    if (!is.null(varTotal) & !is.null(varWithin)) 
      stop("Do not specify total and within variance simultaneously")
    
    if (is.null(varTotal) & is.null(varWithin)) 
      stop("Specify either total or within variance")
    
    if (!is.null(varTotal)) {
      
      vars <- unlist(lapply(ICC, function(x) x * varTotal)) 
      
    }
    
    if (!is.null(varWithin)) {
      
      vars <- unlist(lapply(ICC, function(x) (x / (1 - x)) * varWithin)) 
      
    }
    
  } else if (dist == "gamma") {
    
    if (is.null(disp)) stop("Specify dispersion")  
    
    vars <- unlist(lapply(ICC, function(x) (x / (1 - x)) * trigamma(1/disp)))
    
  } else stop("Specify appropriate distribution")
  
  return(vars)
  
}
