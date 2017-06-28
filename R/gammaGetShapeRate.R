#' Convert gamma mean and dispersion parameters to shape and rate parameters
#'
#' @param mean The mean of a gamma distribution
#' @param dispersion The dispersion parameter of a gamma distribution
#' @return A list that includes the shape and rate parameters of the gamma distribution
#' @details In simstudy, users specify the gamma distribution as a function of two parameters - a mean
#' and dispersion. In this case, the variance of the specified distribution is (mean^2)*dispersion.
#' The base R function rgamma uses the shape and rate parameters to specify the gamma distribution.
#' This function converts the mean and dispersion into the shape and rate.
#' @examples
#' set.seed(12345)
#' mean = 5; dispersion = 1.5
#' rs <- gammaGetShapeRate(mean, dispersion)
#' c(rs$shape, rs$rate)
#' vec <- rgamma(1000, shape = rs$shape, rate = rs$rate)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean^2*dispersion))
#' (theoryMoments <- c(rs$shape/rs$rate, rs$shape/rs$rate^2))
#' @export

gammaGetShapeRate <- function(mean, dispersion) {

  variance = dispersion*(mean^2)

  shape <- (mean^2)/variance
  rate <- mean/variance

  return(list(shape = shape, rate = rate))

}
