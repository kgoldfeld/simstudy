#### Poisson ####

#' Internal function called by generate - returns Poisson count data          
#' truncated at 1
#'
#' @param n The number of observations required in the data set
#' @param formula String that specifies the formula for the mean
#' @param link Link function: Identity or Log
#' @param dtSim Incomplete simulated data.table
#' @return A data.frame column with the updated simulated data

genpoisTrunc <- function(n,formula,link,dtSim) {
  
  if (link=="Log") {
    logmean <- with(dtSim,eval(parse(text = as.character(formula))))
    mean = exp(logmean)
  } else {
    mean = with(dtSim,eval(parse(text = as.character(formula))))
  }
  
  u <- runif(n, min = 0, max = 1)

  x <- qpois(ppois(0,lambda = mean) + u * (ppois(Inf, lambda = mean) - 
                      ppois(0, lambda = mean)), lambda = mean)
  
  return(x)
  
}

