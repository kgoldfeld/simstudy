# Internal function called by gendt
#
# @useDynLib simstudy
# @importFrom Rcpp sourceCpp
#
# @param args One row from data definitions data.table
# @param n The number of observations required in the data set
# @param dt Incomplete simulated data.table
# @return A data.frame with the updated simulated data
# @keywords internal

generate <- function(args, n, dfSim) {

  if (args$dist=="Normal") {
    newColumn <- gennorm(n, args$formula, args$variance, args$link, dfSim)
  } else if (args$dist == "Poisson") {
    newColumn <- genpois(n, args$formula, args$link, dfSim)
  } else if (args$dist == "NoZeroPoisson") {
    newColumn <- genpoisTrunc(n, args$formula, args$link, dfSim)
  } else if (args$dist == "Binary") {
    newColumn <- genbinom(n, args$formula, args$link, dfSim)
  } else if (args$dist=="Uniform") {      # Uniform cannot be function of data
    newColumn <- genunif(n, args$formula)
  } else if (args$dist=="Categorical") {  # Not of function of data, yet
    newColumn <- gencat(n, args$formula)
  } else if (args$dist == "Gamma") {  # Not of function of data, yet
    newColumn <- gengamma(n, args$formula, args$variance, dfSim)
  } else if (args$dist == "Nonrandom") {
    newColumn <- gendeterm(n, args$formula, dfSim)
  }

  # Create data frame

  if (is.null(dfSim))
    dfNew <- data.frame(newColumn)
  else
    dfNew <- data.frame(dfSim, newColumn)

  setnames(dfNew, "newColumn", as.character(args$varname))

  return(dfNew)
}
