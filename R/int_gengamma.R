#### Gamma distribution - not yet implemented ####

# Internal function called by generate - returns gamma data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities
# @return A data.frame column with the updated simulated data

gengamma <- function(n, formula, dispersion, link="identity", dtSim) {

  if (link == "Log") {
    logmean <- with(dtSim,eval(parse(text = as.character(formula))))
    mean <- exp(logmean)
  } else {
    mean <- with(dtSim,eval(parse(text = as.character(formula))))
  }

  d <- as.numeric(as.character(dispersion))

  variance = d*(mean^2)

  shape <- (mean^2)/variance
  rate <- mean/variance

  new <- rgamma(n, shape = shape, rate = rate)

  return(new)

}
