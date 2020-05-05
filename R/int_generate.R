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

.generate <- function(args, n, dfSim, idname) {
  newColumn <- switch(args$dist,
    beta = .genbeta(n, args$formula, args$variance, args$link, dfSim),
    binary = {
      args$variance <- 1
      .genbinom(n, args$formula, args$variance, args$link, dfSim)
    },
    binomial = .genbinom(n, args$formula, args$variance, args$link, dfSim),
    categorical = .gencat(n, args$formula, args$link, dfSim),
    exponential = .genexp(n, args$formula, args$link, dfSim),
    gamma = .gengamma(n, args$formula, args$variance, args$link, dfSim),
    mixture = .genmixture(n, args$formula, dfSim),
    negBinomial = .gennegbinom(n, args$formula, args$variance, args$link, dfSim),
    nonrandom = .gendeterm(n, args$formula, args$link, dfSim),
    normal = .gennorm(n, args$formula, args$variance, args$link, dfSim),
    noZeroPoisson = .genpoisTrunc(n, args$formula, args$link, dfSim),
    poisson = .genpois(n, args$formula, args$link, dfSim),
    uniform = .genunif(n, args$formula, dfSim),
    uniformInt = .genUnifInt(n, args$formula, dfSim),
    default = stop(
      paste(args$dist, "not a valid distribution. Please check spelling."),
      call. = FALSE
    )
  )

  # Create data frame
  if (is.null(dfSim)) {
    dfNew <- data.frame(newColumn)
  } else {
    dfNew <- cbind(dfSim,newColumn)
  }

  names(dfNew)[ncol(dfNew)] <-  as.character(args$varname)

  return(dfNew)
}
