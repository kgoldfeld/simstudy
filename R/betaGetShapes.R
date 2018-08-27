#' Convert beta mean and dispersion parameters to two shape parameters
#'
#' @param mean The mean of a beta distribution
#' @param dispersion The dispersion parameter of a beta distribution
#' @return A list that includes the shape parameters of the beta distribution
#' @details In simstudy, users specify the beta distribution as a function of two parameters - 
#' a mean and dispersion, where 0 < mean < 1 and dispersion > 0. In this case, the variance 
#' of the specified distribution is (mean)*(1-mean)/(1+dispersion). The base R function rbeta 
#' uses the two shape parameters to specify the beta distribution. This function converts 
#' the mean and dispersion into the shape1 and shape2 parameters.
#' @examples
#' set.seed(12345)
#' mean = 0.3; dispersion = 1.6
#' rs <- betaGetShapes(mean, dispersion)
#' c(rs$shape1, rs$shape2)
#' vec <- rbeta(1000, shape1 = rs$shape1, shape2 = rs$shape2)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean*(1-mean)/(1+dispersion)))
#' (theoryMoments <- with(rs, c(shape1/(shape1 + shape2), 
#'   (shape1*shape2) / ((shape1 + shape2)^2*(1 + shape1 + shape2)))))
#' @export

betaGetShapes <- function(mean, dispersion) {
  
  if (any(mean <= 0) | any(mean >= 1)) stop("Mean out of range: must be between 0 and 1")
  if (any(dispersion <= 0)) stop("Mean out of range: must be greater than 0")
  
  a <- mean * dispersion
  b <- (1 - mean) * dispersion
  
  
  return(list(shape1 = a, shape2 = b))
  
}
