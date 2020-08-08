#### Negative binomial distribution ####

# Internal function called by .generate - returns negative binomial data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean
# @return A data.frame column with the updated simulated data

.getNBmean <- function(dtSim, formula, link, n) {
  mean <- .evalWith(formula, .parseDotVars(formula), dtSim, n)
  if (link == "log") {
    mean <- exp(mean)
  } 
  
  return(mean)
}

.gennegbinom <- function(n, formula, dispersion, link="identity", dtSim) {

  mean <- .getNBmean(dtSim, formula, link, n)
  d <- as.numeric(as.character(dispersion))

  sp <- negbinomGetSizeProb(mean = mean, dispersion = d)
  new <- stats::rnbinom(n, size = sp$size,  prob = sp$prob)

  return(new)

}
