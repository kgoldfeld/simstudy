#### Exponential distribution ####

# Internal function called by generate - returns exp data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean (lambda)
# @return A data.frame column with the updated simulated data

genexp <- function(n, formula, link="identity", dtSim) {

  if (link == "log") {
    logmean <- with(dtSim,eval(parse(text = as.character(formula))))
    mean <- exp(logmean)
  } else {
    mean <- with(dtSim,eval(parse(text = as.character(formula))))
  }

  new <- stats::rexp(n, rate = 1/mean)

  return(new)

}
