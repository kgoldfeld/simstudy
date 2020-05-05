#### Normal distribution ####

# Internal function called by .generate - returns Normal data
#
# @param n The number of observations required in the data set
# @param forumula String that specifies the formula for the mean
# @param variance Variance of the normal distribution
# @param link Link function not used in normal distribution
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.getNormalMean <- function(dtSim, formula, n) {
  .evalWith(formula, .parseDotVars(formula), dtSim, n)
}

.gennorm <- function(n,formula,variance,link,dtSim) {

  mean <- .getNormalMean(dtSim, formula, n)
  v <- .evalWith(variance, .parseDotVars(variance), dtSim, n)
  
  return(stats::rnorm(n, mean, sqrt(v)))

}
