#### Normal distribution ####

# Internal function called by .generate - returns Normal data
#
# @param n The number of observations required in the data set
# @param forumula String that specifies the formula for the mean
# @param variance Variance of the normal distribution
# @param link Link function not used in normal distribution
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.getNormalMean <- function(dtSim, formula) {

  mean <- with(dtSim, eval(parse(text = as.character(formula))))

  return(mean)

}

.gennorm <- function(n,formula,variance,link,dtSim) {

  mean <- .getNormalMean(dtSim, formula)
  v <- with(dtSim, eval(parse(text = as.character(variance))))

  return(stats::rnorm(n, mean, sqrt(v)))

}
