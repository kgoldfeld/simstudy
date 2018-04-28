#' Create correlated data
#'
#' @param n Number of observations
#' @param mu A vector of means. The length of mu must be nvars.
#' @param sigma Standard deviation of variables. If standard deviation differs for
#' each variable, enter as a vector with the same length as the mean vector mu. If
#' the standard deviation is constant across variables, as single value can be entered.
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical and
#' positive semi-definite. It is not a required field; if a matrix is not provided, then a
#' structure and correlation coefficient rho must be specified.
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "ind" for an independence
#' structure, "cs" for a compound symmetry structure, and "ar1" for an
#' autoregressive structure.
#' @param cnames Explicit column names. A single string with names separated
#' by commas. If no string is provided, the default names will be V#, where #
#' represents the column.
#' @param idname The name of the index id name. Defaults to "id."
#' @return A data.table with n rows and the k + 1 columns, where k is the number of
#' means in the vector mu.
#' @examples
#' mu <- c(3, 8, 15)
#' sigma <- c(1, 2, 3)
#'
#' corMat <- matrix(c(1, .2, .8, .2, 1, .6, .8, .6, 1), nrow = 3)
#'
#' dtcor1 <- genCorData(1000, mu = mu, sigma = sigma, rho = .7, corstr = "cs")
#' dtcor2 <- genCorData(1000, mu = mu, sigma = sigma, corMatrix = corMat)
#'
#' dtcor1
#' dtcor2
#'
#' round(var(dtcor1[,.(V1, V2, V3)]), 3)
#' round(cor(dtcor1[,.(V1, V2, V3)]), 2)
#'
#' round(var(dtcor2[,.(V1, V2, V3)]), 3)
#' round(cor(dtcor2[,.(V1, V2, V3)]), 2)
#' @export
#'

genCorData <- function(n, mu, sigma, corMatrix = NULL, rho, corstr = "ind",
                       cnames=NULL, idname = "id") {

  nvars <- length(mu)

  if (is.null(corMatrix)) {

    corMatrix <- diag(nvars) # do not modify if indepdendent

    if (corstr == "cs") {
      corMatrix <- rho^(row(corMatrix) != col(corMatrix))
    } else if (corstr == "ar1") {
      corMatrix <- rho^abs(row(corMatrix)-col(corMatrix))
    }

  } else if (! is.null(corMatrix)) { # check if positive definite/symmetric

    if (nvars != length(diag(corMatrix))) {
      stop("Length of mean vector mismatched with correlation matrix")
    }

    if (! isSymmetric(corMatrix)) {
      stop("Correlation matrix not symmetric")
    }

    if (! all(eigen(corMatrix)$values > 0)) {
      stop("Correlation matrix not positive definite")
    }

  }

  if (length(sigma) == 1) {

    varMatrix <- (sigma^2) * corMatrix

  } else if (length(sigma) > 0) {


    D <- diag(sigma)

    if (length(diag(corMatrix)) != length(sigma)) {
      stop("Improper number of standard deviations")
    }

    varMatrix <- (D %*% corMatrix) %*% D

  }


  dt <- data.table(mvnfast::rmvn(n = n , mu = mu, sigma = varMatrix))

  if (length(cnames)) {

    if (length(cnames) != nvars) {
      stop("Invalid number of variable names")
    }

    nnames <- trimws(unlist(strsplit(cnames, split = ",")))
    setnames(dt, nnames)
  }

  dtid <- data.table(1:nrow(dt))
  setnames(dtid,idname)

  dt <- cbind(dtid, dt)
  setkeyv(dt, idname)

  return(dt[])

}
