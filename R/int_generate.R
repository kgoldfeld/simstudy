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

generate <- function(args, n, dfSim, idname) {

  if (! args$dist %in% c("normal","poisson","noZeroPoisson","binary","uniform","categorical","gamma","nonrandom")) {
    stop(paste(args$dist, "not a valid distribution. Please check spelling."), call. = FALSE)
  }

  if (args$dist=="normal") {
    newColumn <- gennorm(n, args$formula, args$variance, args$link, dfSim)
  } else if (args$dist == "poisson") {
    newColumn <- genpois(n, args$formula, args$link, dfSim)
  } else if (args$dist == "noZeroPoisson") {
    newColumn <- genpoisTrunc(n, args$formula, args$link, dfSim)
  } else if (args$dist == "binary") {
    newColumn <- genbinom(n, args$formula, args$link, dfSim)
  } else if (args$dist=="uniform") {
    newColumn <- genunif(n, args$formula, dfSim)
  } else if (args$dist=="categorical") {
    newColumn <- gencat(n, args$formula, args$link, dfSim)
  } else if (args$dist == "gamma") {
    newColumn <- gengamma(n, args$formula, args$variance, args$link, dfSim)
  } else if (args$dist == "nonrandom") {
    newColumn <- gendeterm(n, args$formula, args$link, dfSim)
  }

  # Create data frame

  if (is.null(dfSim))
    dfNew <- data.frame(newColumn)
  else
    dfNew <- data.frame(dfSim, newColumn)

  setnames(dfNew, "newColumn", as.character(args$varname))

  return(dfNew)
}
