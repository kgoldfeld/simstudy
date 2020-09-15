#### Multinomial data generationa ####

# Internal function genExposure - returns categorical data
#
# @param p a vector of probabilities
# @return An integer (group) ranging from 1 to length of the probability vector

.rmult <- function(p) {
  nums <- length(p)
  t(stats::rmultinom(n = 1, size = 1, p = p)) %*% c(1:nums)
}
