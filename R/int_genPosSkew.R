#### Gamma distribution - variation to generate positive skew ####

# Internal function called by .generate - returns gamma data
#
# @param n The number of observations required in the data set
# @param mean The mean
# @param variance The variance
# @return A data.frame column with the updated simulated data

.genPosSkew <- function(n, mean, dispersion = 0) {

  if (dispersion == 0) {

    new <- rep(mean, n)

  } else {

    variance = mean^2 * dispersion

    shape <- (mean^2)/variance
    rate <- mean/variance

    new <- stats::rgamma(n, shape = shape, rate = rate)

  }

  return(new)
}
