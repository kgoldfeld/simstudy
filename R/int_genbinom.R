#### Binary ####

# Internal function called by .generate - returns Binary data
#
# @param n The number of observations required in the data set
# @param forumula String that specifies the formula for the probability
# @param link Link function: Identity or Logit
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.getBinaryMean <- function(dtSim, formula, Size, link, n) {
  size <- .evalWith(Size, .parseDotVars(Size), dtSim, n)
  p <- .evalWith(formula, .parseDotVars(formula), dtSim, n)
  
  if (link=="logit") {
    p <- 1 / (1 + exp(-p))
  }
  
  return(list(p, size))
}

.genbinom <- function(n, formula, Size, link, dtSim) {
  
  params <- .getBinaryMean(dtSim, formula, Size, link, n)

  return(stats::rbinom(n, params[[2]], params[[1]]))

}

