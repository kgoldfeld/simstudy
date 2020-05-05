#### Uniform (integer) ####

# Internal function called by .generate - returns Uniform integer data
#
# @param n The number of observations required in the data set
# @param formula Two formulas (separated by ;) for min and max
# @param dtSim Incomplete simulated data set
# @return A data.frame column  with the updated simulated data

.genUnifInt <- function(n, formula, dtSim) {
  range <- .parseUnifFormula(formula,dtSim,n)
  
  if (any(! sapply(range, function(x) floor(x) == x)))
    stop(paste(
      "For 'uniformInt' min and max must be integers,",
      "did you mean to use 'uniform'?"
    ))
  
  unifCont <- stats::runif(n, range$min, range$max + 1)

  return(as.integer(floor(unifCont)))

}
