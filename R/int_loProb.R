#### Generate missing data matrix ####

# Internal function convert log odds to probability
#
# @param logodds Log odds
# @return Probability

.loProb <- function (logodds) {

  exp(logodds)/(1 + exp(logodds))

}
