#### Beta distribution ####

# Internal function called by .generate - returns exp data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean (lambda)
# @return A data.frame column with the updated simulated data

.getBetaMean <- function(dtSim, formula, link, n ) {
  mean <- .evalWith(formula, .parseDotVars(formula), dtSim, n)
  if (link == "logit") {
    mean <- 1 / (1 + exp(-mean))
  }
  
  return(mean)
}

.genbeta <- function(n, formula, precision, link="identity", dtSim) {
  
  mean <- .getBetaMean(dtSim, formula, link, n)
  
  d <- .evalWith(precision, .parseDotVars(precision), dtSim, n)
  
  sr <- betaGetShapes(mean = mean, precision = d)
  new <- stats::rbeta(n, shape = sr$shape1, shape2 = sr$shape2)
  
  return(new)
  
}
