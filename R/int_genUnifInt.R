#### Uniform (integer) ####

# Internal function called by .generate - returns Uniform integer data
#
# @param n The number of observations required in the data set
# @param formula Two formulas (separated by ;) for min and max
# @param dtSim Incomplete simulated data set
# @return A data.frame column  with the updated simulated data

.genUnifInt <- function(n, formula, dtSim) {

  range <- unlist(strsplit(as.character(formula),split=";", fixed=TRUE))
  rangeMin = with(dtSim,eval(parse(text = as.character(range[1]))))
  rangeMax = with(dtSim,eval(parse(text = as.character(range[2]))))

  unifCont <- stats::runif(n, floor(rangeMin), ceiling(rangeMax) + 1)

  return(as.integer(floor(unifCont)))

}
