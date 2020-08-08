#### Gamma distribution ####

# Internal function called by .generate - returns gamma data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities
# @return A data.frame column with the updated simulated data

.getGammaMean <- function(dtSim, formula, link, n) {
  mean <- .evalWith(formula, .parseDotVars(formula), dtSim, n)
  if (link == "log") {
    mean <- exp(mean)
  }
  
  return(mean)
}

.gengamma <- function(n, formula, dispersion, link="identity", dtSim) {

  mean <- .getGammaMean(dtSim, formula, link, n)
  d <- .evalWith(dispersion, .parseDotVars(dispersion), dtSim, n)

  sr <- gammaGetShapeRate(mean = mean, dispersion = d)
  new <- stats::rgamma(n, shape = sr$shape, rate = sr$rate)

  return(new)
}
