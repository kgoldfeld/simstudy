#### Poisson ####

# Internal function called by generate - returns Poisson count data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param link Link function: Identity or Log
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data

genpois <- function(n,formula,link,dtSim) {

  if (link=="log") {
    logmean <- with(dtSim,eval(parse(text = as.character(formula))))
    mean = exp(logmean)
  } else {
    mean = with(dtSim,eval(parse(text = as.character(formula))))
  }

  return(stats::rpois(n,mean))

}

