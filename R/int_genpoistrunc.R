#### Poisson ####

# Internal function called by .generate - returns Poisson count data
# truncated at 1
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param link Link function: Identity or Log
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data

.genpoisTrunc <- function(n,formula,link,dtSim) {

  mean <- .getPoissonMean(dtSim, formula, link, n)

  u <- stats::runif(n, min = 0, max = 1)

  x <- stats::qpois(stats::ppois(0,lambda = mean) +
                      u * (stats::ppois(Inf, lambda = mean) -
                      stats::ppois(0, lambda = mean)), lambda = mean)

  return(x)
}

