#### Beta distribution ####

# Internal function called by .generate - returns exp data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean (lambda)
# @return A data.frame column with the updated simulated data

.getBetaMean <- function(dtSim, formula, link ) {
  
  if (link == "logit") {
    
    logit <- with(dtSim, eval(parse(text = as.character(formula))))
    mean <- 1 / (1 + exp(-logit))
    
  } else {
    
    mean <- with(dtSim,eval(parse(text = as.character(formula))))
  }
  
  return(mean)
  
}

.genbeta <- function(n, formula, precision, link="identity", dtSim) {
  
  mean <- .getBetaMean(dtSim, formula, link)
  
  d <- as.numeric(as.character(precision))
  
  sr <- betaGetShapes(mean = mean, precision = d)
  new <- stats::rbeta(n, shape = sr$shape1, shape2 = sr$shape2)
  
  return(new)
  
}
