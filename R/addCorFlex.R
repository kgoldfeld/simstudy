#' Create multivariate (correlated) data - for general distributions
#'
#' @param dt Data table that will be updated.
#' @param defs Field definition table created by function `defDataAdd`.
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.
#' @param tau Correlation based on Kendall's tau. If tau is specified, then it is
#' used as the correlation even if rho is specified. If tau is NULL, then the specified
#' value of rho is used, or rho defaults to 0.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "cs" for a compound symmetry structure
#' and "ar1" for an autoregressive structure. Defaults to "cs".
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical and
#' positive semi-definite. It is not a required field; if a matrix is not provided, then a
#' structure and correlation coefficient rho must be specified.
#' @return data.table with added column(s) of correlated data
#' @examples
#' defC <- defData(varname = "nInds", formula = 50, dist = "noZeroPoisson", id = "idClust")
#'
#' dc <- genData(10, defC)
#' #### Normal only
#'
#' dc <- addCorData(dc, mu = c(0,0,0,0), sigma = c(2, 2, 2, 2), rho = .2,
#'                  corstr = "cs", cnames = c("a","b","c","d"), idname = "idClust")
#'
#' di <- genCluster(dc, "idClust", "nInds", "id")
#'
#' defI <- defDataAdd(varname = "A", formula = "-1 + a", variance = 3,
#'                    dist = "normal")
#' defI <- defDataAdd(defI, varname = "B", formula = "4.5 + b", variance = .5,
#'                    dist = "normal")
#' defI <- defDataAdd(defI, varname = "C", formula = "5*c", variance = 3,
#'                    dist = "normal")
#' defI <- defDataAdd(defI, varname = "D", formula = "1.6 + d", variance = 1,
#'                    dist = "normal")
#'
#' #### Generate new data
#'
#' di <- addCorFlex(di, defI, rho = 0.4, corstr = "cs")
#'
#' # Check correlations by cluster
#'
#' for (i in 1:nrow(dc)) {
#'   print(cor(di[idClust == i, list(A, B, C, D)]))
#' }
#'
#' # Check global correlations - should not be as correlated
#' cor(di[, list(A, B, C, D)])
#' @export
#'
addCorFlex <- function(dt, defs, rho = 0, tau = NULL, corstr = "cs", corMatrix = NULL) {

  # "Declare" vars to avoid R CMD warning

  X <- NULL
  Unew <- NULL
  param1 <- NULL
  param2 <- NULL
  id <- NULL
  period <- NULL
  dist <- NULL
  formula <- NULL
  link <- NULL
  variance <- NULL

  #### Check args

  ## Other checks? ##

  # check that names are not already used

  ###

  if (!all(defs[,dist] %in% c("normal", "gamma", "binary", "poisson", "negBinomial"))) {

    stop("Only implemented for the following distributions: binary, normal, poisson, gamma, and negative binomial")

  }

  ####

  dtCopy <- copy(dt)
  n <- nrow(dtCopy)

  corDefs <- copy(defs)
  nvars <- nrow(corDefs)

  ### Start generating data (first, using copula)

  ### Convert tau to rho

  if (!is.null(tau)) {
    rho <- sin(tau * pi/2)
  }

  ###

  dx <- .genQuantU(nvars, n, rho, corstr, corMatrix)

  dFinal <- dx[period == 0, list(id)]

  for (i in 1:nvars) {

    dTemp <- dx[period == (i - 1)]
    dTemp <- dTemp[dtCopy]

    iDist <- corDefs[i, dist]
    iFormula <- corDefs[i, formula]
    iLink <- corDefs[i, link]

    if (iDist == "binary") {

      params <- .getBinaryMean(dTemp, formula = iFormula, Size = 1, link = iLink )

      V <- dTemp[, stats::qbinom(Unew, 1, params[[1]])]

    } else if (iDist == "poisson") {

      param1 <- .getPoissonMean(dTemp, formula = iFormula, link = iLink )

      V <- dTemp[, stats::qpois(Unew, param1)]

    } else if (iDist == "gamma") {

      mn <- .getGammaMean(dTemp, formula = iFormula, link = iLink )

      ### Gamma parameters need to be transformed

      sr <- gammaGetShapeRate(mn, corDefs[i, variance])
      param1 <- sr[[1]]
      param2 <- sr[[2]]

      V <- dTemp[, stats::qgamma(Unew, param1, param2)]
      
    } else if (iDist == "negBinomial") {
        
      mn <- .getNBmean(dTemp, formula = iFormula, link = iLink )
        
      ### NB parameters need to be transformed
        
      sp <-  negbinomGetSizeProb(mn, corDefs[i, variance])
      param1 <- sp[[1]]
      param2 <- sp[[2]]
        
      V <- dTemp[, stats::qnbinom(Unew, param1, param2)]

    } else if (iDist == "normal") {

      param1 <- .getNormalMean(dTemp, formula = iFormula)
      param2 <- sqrt(corDefs[i, variance])

      V <- dTemp[, stats::qnorm(Unew, param1, param2)]

    }

    dFinal <- cbind(dFinal, V)
    setnames(dFinal, "V", corDefs$varname[i])

  }

  dFinal <- dtCopy[dFinal]

  return(dFinal[])

}
