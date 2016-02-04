#### Treatment group assignment ####

#' Internal function called by generate - returns treatment assignment
#'
#' @param n The number of observations required in the data set
#' @param formula String that specifies the formula for the mean
#' @param link Link function: Identity or Logit
#' @param cMethod Method of allocating subjects to treatment group. Can be
#'        equal assingment or variable assignment based on covariate or 
#'        probability. Variable assignment currently works for binary only.
#' @param nTrt Number of treatment groups
#' @param dtSim Incomplete simulated data.table
#' @return A data.frame with the updated simulated data

genRx <- function(n,formula,link,cMethod,nTrt=2,dtSim) {
  
  if (cMethod != "Variable" | is.na(cMethod)) { # Equal assignment
    new <- rep(0 : (nTrt - 1), each = n / nTrt)
  } else { # variable assignment based on covariate or probablity - binary only
    if (link == "Logit") {
      logit <- with(dtSim,eval(parse(text = as.character(formula))))
      p <- 1 / (1 + exp(-logit))
    } else {
      p <- with(dtSim, eval(parse(text = as.character(formula))))
    }
    new <- rbinom(n, 1, p)
  }
  
  return(new)
  
}





