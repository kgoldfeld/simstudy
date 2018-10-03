#### Negative binomial distribution ####

# Internal function called by .generate - returns negative binomial data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean
# @return A data.frame column with the updated simulated data

.getNBmean <- function(dtSim, formula, link) {
  
  if (link == "log") {
    logmean <- with(dtSim,eval(parse(text = as.character(formula))))
    mean <- exp(logmean)
  } else {
    mean <- with(dtSim,eval(parse(text = as.character(formula))))
  }
  
  return(mean)
  
}

.gennegbinom <- function(n, formula, dispersion, link="identity", dtSim) {

  mean <- .getNBmean(dtSim, formula, link)
  d <- as.numeric(as.character(dispersion))

  sp <- negbinomGetSizeProb(mean = mean, dispersion = d)
  new <- stats::rnbinom(n, size = sp$size,  prob = sp$prob)

  return(new)

}
