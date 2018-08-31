#' Convert beta mean and precision parameters to two shape parameters
#'
#' @param mean The mean of a beta distribution
#' @param precision The precision parameter (phi) of a beta distribution
#' @return A list that includes the shape parameters of the beta distribution
#' @details In simstudy, users specify the beta distribution as a function of two parameters - 
#' a mean and precision, where 0 < mean < 1 and precision > 0. In this case, the variance 
#' of the specified distribution is (mean)*(1-mean)/(1+precision). The base R function rbeta 
#' uses the two shape parameters to specify the beta distribution. This function converts 
#' the mean and precision into the shape1 and shape2 parameters.
#' @examples
#' set.seed(12345)
#' mean = 0.3; precision = 1.6
#' rs <- betaGetShapes(mean, precision)
#' c(rs$shape1, rs$shape2)
#' vec <- rbeta(1000, shape1 = rs$shape1, shape2 = rs$shape2)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean*(1-mean)/(1+precision)))
#' (theoryMoments <- with(rs, c(shape1/(shape1 + shape2), 
#'   (shape1*shape2) / ((shape1 + shape2)^2*(1 + shape1 + shape2)))))
#' @export

betaGetShapes <- function(mean, precision) {
  
  if (any(mean <= 0) | any(mean >= 1)) stop("Mean out of range: must be between 0 and 1")
  if (any(precision <= 0)) stop("Precision out of range: must be greater than 0")
  
  a <- mean * precision
  b <- (1 - mean) * precision
  
  
  return(list(shape1 = a, shape2 = b))
  
}
