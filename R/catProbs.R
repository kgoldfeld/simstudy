#' Create a semi-colon delimited string of probabilities to be used to define categorical data
#'
#' @param ... one or more numeric values to be concatenated, delimited by ";".
#' @param n number of probabilities (categories) to be generated - all with equal probability.
#' @return string with multinomial probabilities.
#' @details The function accepts a number of probabilities or a value of n, but not both.
#'
#' If probabilities are passed, the string that is returned depends on the nature of those probabilities.
#' If the sum of the probabilities is less than 1, an additional category is created with the probability
#' 1 - sum(provided probabilities). If the sum of the probabilities is equal to 1, then the number of
#' categories is set to the number of probabilities provided. If the sum of the probabilities exceeds one
#' (and there is more than one probability), the probabilities are standardized by dividing by the sum
#' of the probabilities provided.
#'
#' If n is provided, n probabilities are included in the string, each with a probability equal to 1/n.
#' @examples
#' catProbs(0.25, 0.25, 0.50)
#' catProbs(1/3, 1/2)
#' catProbs(1,2,3)
#' catProbs(n=5)
#' @export
#'
catProbs <- function(..., n = 0) {

  nProbs <-  ...length()
  probs <-  unlist(list(...))

  if (nProbs ==  0 & n==0) stop("Need to specify probabilities or n.")
  if (nProbs > 0 & n > 0) stop("Specify probabilities or n, not both.")
  if(n < 0) stop("Negative values for n are not valid.")
  if(floor(n) != n) stop("'n' must be a whole number.")
  if (length(probs) == 1 && probs >= 1) stop("Single probability must be less than 1.")

  if (nProbs > 0) {
    pnew <- .adjustProbs(probs)
  } 
  
  if(n > 0) {
    pnew <- rep(1/n, n)
  }
  
  return(paste0(pnew, collapse = ";"))
  
}

.adjustProbs <- function(probs) {
  if (is.matrix(probs)) {
    sumProbs <- rowSums(probs)
  } else{
    sumProbs <- sum(probs)
  }
  
  if (isTRUE(all.equal(mean(sumProbs), 1))) {
    probs
  } else if (any(sumProbs < 1)) {
    remainder <- 1 - sumProbs
    warning(paste0(
      "Probabilities do not sum to 1.\n",
      "Adding category with p = ",
      remainder
    ))
    if (is.matrix(probs)) {
      cbind(probs, remainder)
    } else{
      c(probs, remainder)
    }
  } else if (any(sumProbs > 1)) {
    warning("Sum of probabilities > 1. Probabilities will be normalized.")
    probs / sumProbs
  }
}
