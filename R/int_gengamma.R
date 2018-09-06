#### Gamma distribution ####

# Internal function called by .generate - returns gamma data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities
# @return A data.frame column with the updated simulated data

.getGammaMean <- function(dtSim, formula, link) {

  if (link == "log") {
    logmean <- with(dtSim,eval(parse(text = as.character(formula))))
    mean <- exp(logmean)
  } else {
    mean <- with(dtSim,eval(parse(text = as.character(formula))))
  }
}

.gengamma <- function(n, formula, dispersion, link="identity", dtSim) {

  mean <- .getGammaMean(dtSim, formula, link)

  d <- as.numeric(as.character(dispersion))


  sr <- gammaGetShapeRate(mean = mean, dispersion = d)
  new <- stats::rgamma(n, shape = sr$shape, rate = sr$rate)

  return(new)

}
