#### Uniform ####

# Internal function called by generate - returns Uniform data
#
# @param n The number of observations required in the data set
# @param min Lower value of uniform distribution
# @param max Upper value of uniform distribution
# @return A data.frame column  with the updated simulated data

genunif <- function(n, formula) {

  range <-as.numeric(unlist(strsplit(as.character(formula),",")))
  return(runif(n, range[1], range[2]))

}
