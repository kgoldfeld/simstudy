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

  p <- list(...)
  lp <- length(p)

  if (lp ==  0 & n==0) stop("Need to specify probabilities or n")
  if (lp > 0 & n > 0) stop("Specify probabilities or n, not both")

  if (lp == 1) {
    
    if (length(p[[1]]) > 1)  {
      p <- as.list(p[[1]])
    } else {
      if (p[[1]] >= 1) stop("Single probability must be less than 1")
    }
    
  }

  if (lp > 0) {

    tProb <- sum(unlist(p))

    if (tProb > 1) {
      pnew <- lapply(p, function(x) x/tProb)
    } else if (tProb < 1) {
      pnew <- p
      pnew[[lp + 1]] <- 1 - sum(unlist(p))
    } else if (tProb == 1) {
      pnew <- p
    }

  } else { # n > 0
    pnew <- as.list(rep(1/n, n))
  }

  return(paste(pnew, collapse = ";"))

}
