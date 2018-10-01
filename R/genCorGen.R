#' Create multivariate (correlated) data - for general distributions
#'
#' @param n Number of observations
#' @param nvars Number of variables
#' @param params1 A single vector specifying the mean of the distribution. The vector is of
#' length 1 if the mean is the same across all observations, otherwise the vector is of length
#' nvars. In the case of the uniform distribution the vector specifies the minimum.
#' @param params2 A single vector specifying a possible second parameter for the distribution.
#' For the normal distribution, this will be the variance; for the gamma distribution, this
#' will be the dispersion; and for the uniform distribution, this will be the maximum. The
#' vector is of length 1 if the mean is the same across all observations, otherwise the vector
#' is of length nvars.
#' @param dist A string indicating "binary", "poisson" or "gamma", "normal", or "uniform".
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "cs" for a compound symmetry structure
#' and "ar1" for an autoregressive structure.
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical and
#' positive semi-definite. It is not a required field; if a matrix is not provided, then a
#' structure and correlation coefficient rho must be specified.
#' @param wide The layout of the returned file - if wide = TRUE, all new correlated
#' variables will be returned in a single record, if wide = FALSE, each new variable
#' will be its own record (i.e. the data will be in long form). Defaults to FALSE.
#' @param cnames Explicit column names. A single string with names separated
#' by commas. If no string is provided, the default names will be V#, where #
#' represents the column.
#' @param method Two methods are available to generate correlated data. (1) "copula" uses 
#' the multivariate Gaussian copula method that is applied to all other distributions; this
#' applies to all available distributions. (2) "ep" uses an algorithm developed by 
#' Emrich and Piedmonte.
#' @param idname Character value that specifies the name of the id variable.
#' 
#' @return data.table with added column(s) of correlated data
#' @examples
#' set.seed(23432)
#' l <- c(8, 10, 12)
#'
#' genCorGen(1000, nvars = 3, params1 = l, dist = "poisson", rho = .7, corstr = "cs")
#' genCorGen(1000, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs")
#' genCorGen(1000, nvars = 3, params1 = l, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
#' genCorGen(1000, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
#'
#' genCorGen(1000, nvars = 3, params1 = l, dist = "poisson", rho = .7, corstr = "cs",
#'           cnames = "new_var")
#' genCorGen(1000, nvars = 3, params1 = l, dist = "poisson", rho = .7, corstr = "cs",
#'           wide = TRUE, cnames = "a, b, c")
#'
#' genCorGen(1000, nvars = 3, params1 = c(.3, .5, .7), dist = "binary", rho = .3, corstr = "cs")
#' genCorGen(1000, nvars = 3, params1 = l, params2 = c(1,1,1), dist = "gamma", rho = .3,
#'           corstr = "cs", wide = TRUE)
#'           
#' genCorGen(1000, nvars = 3, params1 = c(.3, .5, .7), dist = "binary", 
#'           corMatrix = genCorMat(3), method = "ep")
#' genCorGen(1000, nvars = 3, params1 = c(.3, .5, .7), dist = "binary", 
#'           corMatrix = genCorMat(3), method = "copula")
#'
#' @export
#'
genCorGen <- function(n, nvars, params1, params2 = NULL, dist, rho, corstr,
                      corMatrix = NULL, wide = FALSE, cnames = NULL, method = "copula",
                      idname = "id") {

  # "Declare" vars to avoid R CMD warning

  param1 <- NULL
  seqid <- NULL
  X <- NULL
  Unew <- NULL
  param2 <- NULL
  id <- NULL
  period <- NULL

  #### Check args

  if ( !(dist %in% c("poisson", "binary", "gamma", "uniform", "negBinomial", "normal"))) {
    stop("Distribution not properly specified.")
  }

  if (class(params1) != "numeric") stop("Parameters must be numeric")

  if (!is.null(params2)) {
    if (class(params2) != "numeric") stop("Parameters must be numeric")
  }

  nparams <- as.numeric(!is.null(params1)) + as.numeric(!is.null(params2))

  if ( ((nparams > 1) & (dist %in% c("poisson", "binary")))) {
    stop(paste0("Too many parameter vectors (", nparams, ") for " , dist))
  }

  if ( ((nparams < 2) & (dist %in% c("gamma", "uniform", "normal", "negBinomial")))) {
    stop(paste0("Too few parameter vectors (", nparams, ") for " , dist))
  }

  if (length(params1) == 1) {
    params1 = rep(params1, nvars)
  }

  if (!is.null(params2)) {
    if (length(params2) == 1) {
      params2 = rep(params2, nvars)
    }
  }

  if (length(params1) != nvars) {
      stop(paste0("Length of vector 1 = ", length(params1),
                  ", not equal to number of correlated variables: ", nvars))
  }

  if (!is.null(params2)) {
    if (length(params2) != nvars) {
      stop(paste0("Length of vector 2 = ", length(params2),
                  ", not equal to number of correlated variables: ", nvars))
    }
  }
  
  if (!(method %in% c("copula", "ep"))) {
    stop(paste(method, "is not a valid method"))
  }
  
  if (dist != "binary" & method == "ep") {
    stop("Method `ep` applies only to binary data generation")
  }

  ####

  if (method == "copula") {
    
    mu <- rep(0, nvars)
    
    dtM <- .genQuantU(nvars, n, rho, corstr, corMatrix)

    if (dist == "binary") {
      dtM[, param1 := params1[seq], keyby = seqid]
      dtM[, X := stats::qbinom(p = Unew, 1, prob = param1)]
    } else if (dist == "poisson") {
      dtM[, param1 := params1[seq], keyby = seqid]
      dtM[, X := stats::qpois(p = Unew, lambda = param1)]
    } else if (dist == "negBinomial") {
      sp <- negbinomGetSizeProb(params1, params2)
      dtM[, param1 := sp[[1]][seq]]
      dtM[, param2 := sp[[2]][seq]]
      dtM[, X := stats::qnbinom(p=Unew, size = param1,  prob = param2)]
    } else if (dist == "uniform") {
      dtM[, param1 := params1[seq], keyby = seqid]
      dtM[, param2 := params2[seq], keyby = seqid]
      dtM[, X := stats::qunif(p = Unew, min = param1, max = param2)]
    } else if (dist == "gamma") {
      sr <- gammaGetShapeRate(params1, params2)
      dtM[, param1 := sr[[1]][seq]]
      dtM[, param2 := sr[[2]][seq]]
      dtM[, X := stats::qgamma(p = Unew, shape = param1, rate = param2)]
    } else if (dist == "normal") {
      dtM[, param1 := params1[seq], keyby = seqid]
      dtM[, param2 := params2[seq], keyby = seqid]
      dtM[, X := stats::qnorm(p = Unew, mean = param1, sd = sqrt(param2))]
    }
    
  } else if (method == "ep") {
    
    corMatrix <- .buildCorMat(nvars, corMatrix, corstr, rho)
    dtM <-.genBinEP(n, params1, corMatrix)
  }
  
  setkey(dtM, "id")

  if (wide == FALSE) {

    dFinal <- dtM[, list(id, period, X)]

    if (!is.null(cnames)) setnames(dFinal, "X", cnames)

  } else {

    dFinal <- dcast(dtM, id ~ seq, value.var="X")
    if (!is.null(cnames)) {
      nnames <- trimws(unlist(strsplit(cnames, split = ",")))
      setnames(dFinal, paste0("V",1:nvars), nnames)
    }
  }
  
  setnames(dFinal, "id", idname)

  return(dFinal[])

}
