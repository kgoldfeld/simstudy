#### Categorical ####

#' Internal function called by generate - returns categorical data
#'
#' @param n The number of observations required in the data set
#' @param formula String that specifies the probabilities
#' @return A data.frame column with the updated simulated data

gencat <- function(n,formula) {
  
  p <-as.numeric(unlist(strsplit(as.character(formula),",")))
  breaks <- c(0,cumsum(p))
  new <- as.numeric(cut(runif(n),breaks = breaks,labels=FALSE))
  
  return(new)
}



