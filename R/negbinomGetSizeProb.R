#' Convert negative binomial mean and dispersion parameters to size and prob parameters
#'
#' @param mean The mean of a gamma distribution
#' @param dispersion The dispersion parameter of a gamma distribution
#' @return A list that includes the size and prob parameters of the neg binom distribution
#' @details In simstudy, users specify the negative binomial distribution as a function of
#' two parameters - a mean and dispersion. In this case, the variance of the specified
#' distribution is mean + (mean^2)*dispersion. The base R function rnbinom uses the size
#' and prob parameters to specify the negative binomial distribution. This function converts
#' the mean and dispersion into the size and probability parameters.
#' @examples
#' set.seed(12345)
#' mean = 5; dispersion = 0.5
#' sp <- negbinomGetSizeProb(mean, dispersion)
#' c(sp$size, sp$prob)
#' vec <- rnbinom(1000, size = sp$size, prob = sp$prob)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean + mean^2*dispersion))
#' (theoryMoments <- c(sp$size*(1-sp$prob)/sp$prob, sp$size*(1-sp$prob)/sp$prob^2))

#' @export

negbinomGetSizeProb <- function(mean, dispersion) {

  variance = mean + dispersion*(mean^2)

  size <- (mean ^ 2) / (variance - mean)
  prob <- mean/variance

  return(list(size = size, prob = prob))

}
