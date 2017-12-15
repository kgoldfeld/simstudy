#### Quantile for copula data generation ####

# Internal function called by genCorGen and addCorGen - returns data.table
#
# @param nvars Number of new variables to generate
# @param n Number records to generate
# @param rho Correlation coefficient
# @param corstr Correlation structure
# @param corMatrix Correlation matrix
# @return A data.frame column with correlated uniforms

genQuantU <- function(nvars, n, rho, corstr, corMatrix) {

  # "Declare" vars to avoid R CMD warning

  seqid <- NULL
  period <- NULL
  Unew <- NULL
  Y <- NULL

  ####

  mu <- rep(0, nvars)
  if (is.null(corMatrix)) {

    dt <- genCorData(n, mu, sigma = 1, rho = rho, corstr = corstr )

  } else {

    dt <- genCorData(n, mu, sigma = 1, corMatrix = corMatrix )

  }

  dtM <- melt(dt, id.vars = "id", variable.factor = TRUE, value.name = "Y", variable.name = "seq")
  dtM[, period := as.integer(seq) - 1]
  setkey(dtM, "id")
  dtM[, seqid := .I]
  dtM[, Unew := stats::pnorm(Y)]

  return(dtM[, -"Y"])

}
