#' Add correlated data to existing data.table
#'
#' @param dtOld Data table that is the new columns will be appended to.
#' @param idname Character name of id field, defaults to "id".
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
#' @return The original data table with the additional correlated columns
#' @examples
#' def <- defData(varname="xUni", dist="uniform", formula="10;20", id = "myID")
#' def <- defData(def, varname="xNorm", formula="xUni * 2", dist="normal", variance=8)
#'
#' dt <- genData(250, def)
#'
#' mu <- c(3, 8, 15)
#' sigma <- c(1, 2, 3)
#'
#' dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, rho = .7, corstr = "cs")
#' dtAdd
#'
#' round(var(dtAdd[,.(V1, V2, V3)]), 3)
#' round(cor(dtAdd[,.(V1, V2, V3)]), 2)
#'
#' dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, rho = .7, corstr = "ar1")
#' round(cor(dtAdd[,.(V1, V2, V3)]), 2)
#'
#' corMat <- matrix(c(1, .2, .8, .2, 1, .6, .8, .6, 1), nrow = 3)
#'
#' dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, corMatrix = corMat)
#' round(cor(dtAdd[,.(V1, V2, V3)]), 2)
#' @export
#'

# dtName must contain id for now

addCorData <- function(dtOld, idname, mu, sigma, corMatrix = NULL,
                       rho, corstr = "ind", cnames=NULL) {

  dtTemp <- copy(dtOld)
  data.table::setkeyv(dtTemp, idname)

  n <- nrow(dtTemp)

  dtNew <-simstudy::genCorData(n = n, mu = mu, sigma = sigma,
                               corMatrix = corMatrix, rho = rho,
                               corstr = corstr, cnames = cnames,
                               idname = idname)

  data.table::setkeyv(dtNew, idname)

  dtTemp <- mergeData(dtTemp, dtNew, idname)

  return(dtTemp[])

}
