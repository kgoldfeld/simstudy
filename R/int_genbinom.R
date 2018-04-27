#### Binary ####

# Internal function called by generate - returns Binary data
#
# @param n The number of observations required in the data set
# @param forumula String that specifies the formula for the probability
# @param link Link function: Identity or Logit
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

getBinaryMean <- function(dtSim, formula, Size, link) {
  
  size <- with(dtSim, eval(parse(text = as.character(Size))))  
  

  if (link=="logit") {
    logit <- with(dtSim, eval(parse(text = as.character(formula))))
    p <- 1 / (1 + exp(-logit))
  } else {
    p <- with(dtSim, eval(parse(text = as.character(formula))))
  }

  
  return(list(p, size))

}

genbinom <- function(n, formula, Size, link, dtSim) {
  
  params <- getBinaryMean(dtSim, formula, Size, link)

  return(stats::rbinom(n, params[[2]], params[[1]]))

}

