#### Poisson ####

# Internal function called by .generate - returns Poisson count data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param link Link function: Identity or Log
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data


.getPoissonMean <- function(dtSim, formula, link, n) {
  mean <- .evalWith(formula, .parseDotVars(formula), dtSim, n)
  
  if (link=="log") {
    mean <- exp(mean)
  }
  
  return(mean)
}


.genpois <- function(n,formula,link,dtSim) {

  mean <- .getPoissonMean(dtSim, formula, link, n)

  return(stats::rpois(n,mean))
}

