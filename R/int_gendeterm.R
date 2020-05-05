#### Non-random ####

# Internal function called by .generate - returns non-random data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.gendeterm <- function(n, formula, link, dtSim) {

  new <- .evalWith(formula, .parseDotVars(formula), dtSim, n)

  if (link=="log") new <- exp(new)
  else if (link == "logit") new <- 1 / (1 + exp(-new))

  new
}