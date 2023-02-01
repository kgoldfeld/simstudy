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
#' round(var(dtcor1[, .(V1, V2, V3)]), 3)
#' round(cor(dtcor1[, .(V1, V2, V3)]), 2)
#'
#' round(var(dtcor2[, .(V1, V2, V3)]), 3)
#' round(cor(dtcor2[, .(V1, V2, V3)]), 2)
#' @export
#' @concept correlated
genCorData <- function(n, mu, sigma, corMatrix = NULL, rho, corstr = "ind",
                       cnames = NULL, idname = "id") {
  nvars <- length(mu)

  if (!is.null(cnames)) {
    nnames <- trimws(unlist(strsplit(cnames, split = ",")))

    if (length(nnames) != nvars) {
      stop("Invalid number of variable names")
    }
  }

  corMatrix <- .buildCorMat(nvars, corMatrix, corstr, rho)

  if (length(sigma) == 1) {
    varMatrix <- (sigma^2) * corMatrix
  } else if (length(sigma) > 0) {
    D <- diag(sigma)

    if (length(diag(corMatrix)) != length(sigma)) {
      stop("Improper number of standard deviations")
    }

    varMatrix <- (D %*% corMatrix) %*% D
  }

  dt <- data.table(mvnfast::rmvn(n = n, mu = mu, sigma = varMatrix))


  if (!is.null(cnames)) setnames(dt, nnames)

  dtid <- data.table(1:nrow(dt))
  setnames(dtid, idname)

  dt <- cbind(dtid, dt)
  setkeyv(dt, idname)

  return(dt[])
}

#' Create multivariate (correlated) data - for general distributions
#'
#' @param n Number of observations
#' @param defs Field definition table created by function `defData`. All definitions
#' must be scalar. Definition specifies distribution, mean, and variance, with all
#' caveats for each of the distributions. (See defData).
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.
#' @param tau Correlation based on Kendall's tau. If tau is specified, then it is
#' used as the correlation even if rho is specified. If tau is NULL, then the specified
#' value of rho is used, or rho defaults to 0.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "cs" for a compound symmetry structure
#' and "ar1" for an autoregressive structure. Defaults to "cs".
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical and
#' positive semi-definite. It is not a required field; if a matrix is not provided, then a
#' structure and correlation coefficient rho must be specified. This is only used if tau
#' is not specified.
#' @return data.table with added column(s) of correlated data
#' @examples
#' def <- defData(varname = "xNorm", formula = 0, variance = 4, dist = "normal")
#' def <- defData(def, varname = "xGamma1", formula = 15, variance = 2, dist = "gamma")
#' def <- defData(def, varname = "xBin", formula = 0.5, dist = "binary")
#' def <- defData(def, varname = "xUnif1", formula = "0;10", dist = "uniform")
#' def <- defData(def, varname = "xPois", formula = 15, dist = "poisson")
#' def <- defData(def, varname = "xUnif2", formula = "23;28", dist = "uniform")
#' def <- defData(def, varname = "xUnif3", formula = "100;150", dist = "uniform")
#' def <- defData(def, varname = "xGamma2", formula = 150, variance = 0.003, dist = "gamma")
#' def <- defData(def, varname = "xNegBin", formula = 5, variance = .8, dist = "negBinomial")
#'
#' dt <- genCorFlex(1000, def, tau = 0.3, corstr = "cs")
#'
#' cor(dt[, -"id"])
#' cor(dt[, -"id"], method = "kendall")
#' var(dt[, -"id"])
#' apply(dt[, -"id"], 2, mean)
#' @export
#' @concept correlated
genCorFlex <- function(n, defs, rho = 0, tau = NULL, corstr = "cs", corMatrix = NULL) {

  # "Declare" vars to avoid R CMD warning

  X <- NULL
  Unew <- NULL
  param1 <- NULL
  param2 <- NULL
  id <- NULL
  period <- NULL
  dist <- NULL
  formula <- NULL
  variance <- NULL

  #### Check args

  ## Other checks? ##

  if (!all(defs[, dist] %in% c("normal", "gamma", "uniform", "binary", "poisson", "negBinomial"))) {
    stop("Only implemented for the following distributions: binary, uniform, normal, poisson, gamma, and negative binomial")
  }

  ####

  corDefs <- copy(defs)

  nvars <- nrow(corDefs)

  ### Uniform parameters entered as string

  nUniform <- corDefs[dist == "uniform", .N]

  if (nUniform > 0) {
    rangeV <- 2 * (1:nUniform)
    rangeF <- rangeV - 1

    range <- corDefs[dist == "uniform", unlist(strsplit(as.character(formula), split = ";", fixed = TRUE))]
    corDefs[dist == "uniform", formula := range[rangeF]]
    corDefs[dist == "uniform", variance := as.numeric(range[rangeV])]
  }

  chkWarn <- tryCatch(corDefs[, formula := as.numeric(formula)],
    warning = function(w) {
      "warning"
    }
  )

  if (class(chkWarn)[1] == "character") stop("Non-scalar values in definitions")

  ### Gamma parameters need to be transformed

  sr1 <- corDefs[dist == "gamma", gammaGetShapeRate(formula, variance)[[1]]]
  sr2 <- corDefs[dist == "gamma", gammaGetShapeRate(formula, variance)[[2]]]
  corDefs[dist == "gamma", `:=`(formula = sr1, variance = sr2)]

  ### negBinomial parameters need to be transformed

  sp1 <- corDefs[dist == "negBinomial", negbinomGetSizeProb(formula, variance)[[1]]]
  sp2 <- corDefs[dist == "negBinomial", negbinomGetSizeProb(formula, variance)[[2]]]
  corDefs[dist == "negBinomial", `:=`(formula = sp1, variance = sp2)]

  ### Check for non-scalar values in definitions

  if (corDefs[is.na(formula), .N] > 0) stop("Non-scalar values in definitions")

  ### Convert tau to rho

  if (!is.null(tau)) {
    rho <- sin(tau * pi / 2)
  }

  ### Start generating data (first, using copula)

  dx <- .genQuantU(nvars, n, rho, corstr, corMatrix)

  dx[, dist := rep(corDefs[, dist], length.out = .N)]
  dx[, param1 := rep(corDefs[, formula], length.out = .N)]
  dx[, param2 := rep(corDefs[, variance], length.out = .N)]

  dFinal <- dx[period == 0, list(id)]

  for (i in 1:nvars) {
    dTemp <- dx[period == (i - 1)]
    type <- corDefs[i, dist]

    if (type == "binary") {
      V <- dTemp[, stats::qbinom(Unew, 1, param1)]
    } else if (type == "poisson") {
      V <- dTemp[, stats::qpois(Unew, param1)]
    } else if (type == "uniform") {
      V <- dTemp[, stats::qunif(Unew, param1, param2)]
    } else if (type == "gamma") {
      V <- dTemp[, stats::qgamma(Unew, param1, param2)]
    } else if (type == "normal") {
      V <- dTemp[, stats::qnorm(Unew, param1, sqrt(param2))]
    } else if (type == "negBinomial") {
      V <- dTemp[, stats::qnbinom(Unew, param1, param2)]
    }

    dFinal <- cbind(dFinal, V)
    setnames(dFinal, "V", corDefs$varname[i])
  }

  return(dFinal[])
}

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
#' Emrich and Piedmonte (1991).
#' @param idname Character value that specifies the name of the id variable.
#'
#' @return data.table with added column(s) of correlated data
#' @references Emrich LJ, Piedmonte MR. A Method for Generating High-Dimensional
#' Multivariate Binary Variates. The American Statistician 1991;45:302-4.
#' @examples
#' set.seed(23432)
#' lambda <- c(8, 10, 12)
#'
#' genCorGen(100, nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs")
#' genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs")
#' genCorGen(100, nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
#' genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
#'
#' genCorGen(100,
#'   nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs",
#'   cnames = "new_var"
#' )
#' genCorGen(100,
#'   nvars = 3, params1 = lambda, dist = "poisson", rho = .7, corstr = "cs",
#'   wide = TRUE, cnames = "a, b, c"
#' )
#' @export
#' @concept correlated
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

  if (!(dist %in% c("poisson", "binary", "gamma", "uniform", "negBinomial", "normal"))) {
    stop("Distribution not properly specified.")
  }

  if (!is(params1, "numeric")) stop("Parameters must be numeric")

  if (!is.null(params2)) {
    if (!is(params2, "numeric")) stop("Parameters must be numeric")
  }

  nparams <- as.numeric(!is.null(params1)) + as.numeric(!is.null(params2))

  if (((nparams > 1) & (dist %in% c("poisson", "binary")))) {
    stop(paste0("Too many parameter vectors (", nparams, ") for ", dist))
  }

  if (((nparams < 2) & (dist %in% c("gamma", "uniform", "normal", "negBinomial")))) {
    stop(paste0("Too few parameter vectors (", nparams, ") for ", dist))
  }

  if (length(params1) == 1) {
    params1 <- rep(params1, nvars)
  }

  if (!is.null(params2)) {
    if (length(params2) == 1) {
      params2 <- rep(params2, nvars)
    }
  }

  if (length(params1) != nvars) {
    stop(paste0(
      "Length of vector 1 = ", length(params1),
      ", not equal to number of correlated variables: ", nvars
    ))
  }

  if (!is.null(params2)) {
    if (length(params2) != nvars) {
      stop(paste0(
        "Length of vector 2 = ", length(params2),
        ", not equal to number of correlated variables: ", nvars
      ))
    }
  }

  if (!(method %in% c("copula", "ep"))) {
    stop(paste(method, "is not a valid method"))
  }

  if (dist != "binary" & method == "ep") {
    stop("Method `ep` applies only to binary data generation")
  }
  
  if (!is.null(corMatrix)) {
    assertClass(corMatrix = corMatrix, class = "matrix")
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
      dtM[, X := stats::qnbinom(p = Unew, size = param1, prob = param2)]
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
    dtM <- .genBinEP(n, params1, corMatrix)
  }

  setkey(dtM, "id")

  if (wide == FALSE) {
    dFinal <- dtM[, list(id, period, X)]

    if (!is.null(cnames)) setnames(dFinal, "X", cnames)
  } else {
    dFinal <- data.table::dcast(dtM, id ~ seq, value.var = "X")
    if (!is.null(cnames)) {
      nnames <- trimws(unlist(strsplit(cnames, split = ",")))
      setnames(dFinal, paste0("V", 1:nvars), nnames)
    }
  }

  setnames(dFinal, "id", idname)

  return(dFinal[])
}

#'
.genBinEP <- function(n, p, tcorr) {

  # "Declare" vars to avoid R CMD warning
  id <- NULL
  period <- NULL
  seqid <- NULL

  np <- length(p)
  phicorr <- diag(length(p))
  
  phicorr <- getRhoMat(np, p, tcorr)
  
  # check that phicorr is positive definite (PD), if not adjust to nearest PD matrix
  if (!all(eigen(phicorr)$values > 0)) {
    phicorr <- Matrix::nearPD(phicorr)$mat
  }

  normvars <- mvnfast::rmvn(n, mu = rep(0, length(p)), sigma = phicorr)
  z <- matrix(rep(stats::qnorm(p), nrow(normvars)), nrow = nrow(normvars), byrow = TRUE)
  binvars <- matrix(as.integer(normvars < z), nrow = nrow(z))

  dtX <- data.table(binvars)
  dtX[, id := .I]

  dtM <- melt(dtX, id.vars = "id", variable.factor = TRUE, value.name = "X", variable.name = "seq")

  dtM[, period := as.integer(seq) - 1]
  setkey(dtM, "id")
  dtM[, seqid := .I]

  return(dtM[])
}

#' Create a correlation matrix
#'
#' @param nvars number of rows and columns (i.e. number of variables) for correlation matrix. It can be
#' a scalar or vector (see details).
#' @param cors vector of correlations.
#' @param rho Correlation coefficient, \code{-1 <= rho <= 1}. Use if corMatrix is not provided. It can
#' be a scalar or vector (see details).
#' @param corstr Correlation structure. Options include "cs" for a compound symmetry structure, "ar1" 
#' for an autoregressive structure of order 1, "arx" for an autoregressive structure 
#' that has a general decay pattern, and "structured" that imposes a prescribed
#' pattern between observation based on distance (see details).
#' @param nclusters An integer that indicates the number of matrices that will be generated.
#' @return A single correlation matrix of size \code{nvars x nvars}, or a list of matrices of potentially
#' different sizes with length indicated by \code{nclusters}.
#' @details This function can generate correlation matrices randomly or deterministically, 
#' depending on the combination of arguments provided. A single matrix will be
#' generated when \code{nclusters == 1} (the default), and a list of matrices of matrices will be generated when
#' \code{nclusters > 1}.
#' 
#' If the vector `cors` is specified with length `nvars - 1` then `corstr` must be "structured". If
#' `cors` is specified with length `choose(nvars, 2)` then `corstr` should not be specified as
#' "structured". In this case the `cors` vector should be interpreted as the lower triangle of the correlation
#' matrix, and is specified by reading down the columns. For example, if \bold{CM} is the correlation matrix and
#' \code{nvars = 3}, then \code{CM[2,1] = CM[1,2] = cors[1]},  \code{CM[3,1] = CM[1,3] = cors[2]}, 
#' and \code{CM[3,2] = CM[2,3] = cors[3]}.
#' 
#' If the vector \code{cors} and \code{rho} are not specified, random correlation matrices are generated
#' based on the specified \code{corstr}. If the structure is "arx", then a random vector of 
#' length \code{nvars - 1} is randomly generated and sorted in descending order; the correlation matrix
#' will be generated base on this set of structured correlations. If the structure is \emph{not} specified
#' as "arx" then a random positive definite of dimensions nvars x nvars with no structural 
#' assumptions is generated.
#' 
#' If \code{cors} is not specified but \code{rho} is specified, then a matrix with either a "cs" or "ar1" 
#' structure is generated.
#' 
#' If \code{nclusters > 1}, \code{nvars} can be of length 1 or \code{nclusters}. If it is of length 1,
#' each cluster will have correlation matrices with the same dimension. Likewise, if \code{nclusters > 1}, 
#' \code{rho} can be of length 1 or \code{nclusters}. If length of \code{rho} is 1,
#' each cluster will have correlation matrices with the same autocorrelation.
#' 
#' @examples
#' genCorMat(nvars = 3, cors = c(.3, -.2, .1))
#' genCorMat(nvars = 3)
#'
#' genCorMat(nvars = 4, c(.3, -.2, .1, .2, .5, .2))
#' genCorMat(4)
#' 
#' genCorMat(nvars = 4, cors = c(.3, .2, .1), corstr = "structured") 
#' genCorMat(nvars = 4, corstr = "arx") 
#' 
#' genCorMat(nvars = 4, rho = .4, corstr = "cs") 
#' genCorMat(nvars = 4, rho = .4, corstr = "ar1") 
#' 
#' genCorMat(nvars = c(3, 2, 5), rho = c(.4, .8, .7), corstr = "ar1", nclusters = 3) 
#' 
#' @export
#' @concept correlated
genCorMat <- function(nvars, cors = NULL, rho = NULL, corstr = "cs", nclusters = 1) {
  
  assertNotMissing(nvars = missing(nvars))
  assertInteger(nvars = nvars, nclusters = nclusters)
  assertLength(corstr = corstr, length = 1)
  assertOption(corstr = corstr, options = c("cs", "ar1", "arx", "structured"))
  
  .randMat <- function(nvars) {
    
    posDef <- FALSE
    
    while (!posDef) {
      
      ev <- stats::runif(nvars, 0, 10)
      Z <- matrix(ncol = nvars, stats::rnorm(nvars^2))
      decomp <- qr(Z)
      Q <- qr.Q(decomp)
      R <- qr.R(decomp)
      d <- diag(R)
      ph <- d / abs(d)
      O <- Q %*% diag(ph)
      Z <- t(O) %*% diag(ev) %*% O
      
      cm <- stats::cov2cor(Z)
      
      eigenValues <- unlist(eigen(cm, only.values = TRUE))
      if (all(eigenValues > 0)) posDef <- TRUE
    }
    
    assertPositiveDefinite(corMat = cm)
    cm
  }
  
  .structCors <- function(x) {
    for (i in seq_along(x)) {
      if (i == 1) y <- x[1]
      else y <- c(x[1:i], y)
    }
    
    return(y)
  }
  
  .rhoMat <- function(nvars, rho, corstr) {
    assertNumeric(rho = rho)
    assertInRange(rho = rho, range = c(-1, 1))
    
    cm <- .buildCorMat(nvars = nvars, corMatrix = NULL, corstr, rho)
    assertPositiveDefinite(corMat = cm)
    
    cm
    
  }
  
  .fillCor <- function(nvars, cors) {
    
    cmLower <- matrix(0, nrow = nvars, ncol = nvars)
    cmLower[lower.tri(cmLower)] <- cors
    cmUpper <- t(cmLower)
    
    cm <- cmLower + cmUpper
    diag(cm) <- 1
    cm
  }
  
  .corMat <- function(nvars, cors) {
    
    assertLength(cors = cors, length = choose(nvars, 2))
    cm <- .fillCor(nvars, cors)
    assertPositiveDefinite(corMat = cm)
    
    cm
    
  }
  
  .arxRandMat <- function(nvars) {
    
    posDef <- FALSE
    
    while (!posDef) {
      x <- stats::rbeta((nvars - 1), 1, 1)
      x <- x[order(x, decreasing = TRUE)]
      cm <- .fillCor(nvars, cors = .structCors(x))
      
      eigenValues <- unlist(eigen(cm, only.values = TRUE))
      if (all(eigenValues > 0)) posDef <- TRUE
    }
    
    assertPositiveDefinite(corMat = cm)
    cm
    
  }
  
  if ( !is.null(cors) & (corstr == "structured")) {
    
    assertEqual(nlusters = nclusters, val = 1)
    assertLength(nvars = nvars, length = 1)
    assertLength(cors = cors, length = (nvars - 1))
    cm <- .corMat(nvars, cors = .structCors(cors))
      
  } else if ( !is.null(cors) & (corstr != "structured") ) {  
    
    assertEqual(nlusters = nclusters, val = 1)
    assertLength(nvars = nvars, length = 1)
    cm <- .corMat(nvars, cors)

  } else if ( is.null(cors) & is.null(rho) & (corstr == "arx") ) {
    
    if (nclusters == 1) {
      
      assertLength(nvars = nvars, length = 1)
      cm <- .arxRandMat(nvars)
      
    } else {
      
      if (length(nvars) == 1) nvars <- rep(nvars, nclusters)
      assertLength(nvars = nvars, length = nclusters)
      
      cm <- lapply(nvars, function(x) .arxRandMat(x))
      
    }

  } else if ( is.null(cors) & is.null(rho) & (corstr != "arx") )  {

    if (nclusters == 1) {
      
      assertLength(nvars = nvars, length = 1)
      cm <- .randMat(nvars)
      
    } else {
      
      if (length(nvars) == 1) nvars <- rep(nvars, nclusters)
        
      assertLength(nvars = nvars, length = nclusters)
      cm <- lapply(nvars, function(x) .randMat(x))
      
    }

  } else if  (is.null(cors) & !is.null(rho)) {

    if (nclusters == 1) {
      
      assertLength(nvars = nvars, length = nclusters)
      assertLength(rho = rho, length = nclusters)
      cm <- .rhoMat(nvars, rho, corstr)
      
    } else {

      if (length(nvars) == 1) nvars <- rep(nvars, nclusters)
      if (length(rho) == 1) rho <- rep(rho, nclusters)
      assertLength(nvars = nvars, length = nclusters)
      assertLength(rho = rho, length = nclusters)

      xx <- data.table(nvars = nvars, rho = rho, corstr = rep(corstr, nclusters))
      cm <- lapply(split(xx, seq(nrow(xx))), function(x) .rhoMat(x$nvars, x$rho, x$corstr))
      
    }
  }
  
  cm
  
}

#' @title Generate correlated ordinal categorical data
#' @description This function is deprecated, please use [genOrdCat] instead.
#' @export
#' @md
#' @keywords internal
genCorOrdCat <- function(dtName, idname = "id", adjVar = NULL, baseprobs,
                         prefix = "grp", rho, corstr, corMatrix = NULL) {
  .Deprecated("genOrdCat")
  genOrdCat(
    dtName = dtName,
    adjVar = adjVar,
    baseprobs = baseprobs,
    idname = idname,
    prefix = prefix,
    rho = rho,
    corstr = corstr,
    corMatrix = corMatrix,
    asFactor = FALSE
  )
}

#' Create a block correlation matrix
#' @description  The function genBlockMat() generates correlation matrices that 
#' can accommodate clustered observations over time where the within-cluster 
#' between-individual correlation in the same time period can be different from the 
#' within-cluster between-individual correlation across time periods.The matrix
#' generated here can be used in function addCorGen().
#' @param nInds The number of units (individuals) in each period.
#' @param nPeriods The number periods that data are observed.
#' @param rho_w The within-period correlation coefficient between -1 and 1.
#' @param rho_b The between-period correlation coefficient between -1 and 1.
#' @param rho_a The between-period within individual auto-correlation coefficient 
#' between -1 and 1.
#' @param r The decay parameter if correlation declines over time, and can have values of
#' "exp" or "prop". See details.
#' @param decay The decay type can be "exp" or "prop". The decay structure
#' @param nclusters An integer that indicates the number of matrices that will be generated.
#' @return A single correlation matrix of size \code{nvars x nvars}, or a list of matrices of potentially
#' different sizes with length indicated by \code{nclusters}.
#' @details This function can generate correlation matrices randomly or deterministically, 
#' depending on the combination of arguments provided. A single matrix will be
#' generated when \code{nclusters == 1} (the default), and a list of matrices of matrices will be generated when
#' \code{nclusters > 1}.
#' 
#'\code{decay = "exp"} when the desired pattern is cross-sectional and
#' is decaying in an exponential manner (as described in Li et al. Mixed-effects models for the 
#' design and analysis of stepped wedge cluster randomized trials: An overview. 
#' Statistical Methods in Medical Research. 2021;30(2):612-639. doi:10.1177/0962280220932962). When
#' the data have repeated measurements over time, \code{decay} = "prop" for proportional decay.
#' 
#' When there is no decay, the cross-sectional exchangeable structure is specifed by setting \code{rho_b}.
#' For the repeated measures case, \code{rho_a} can be specified for the within-individual/between-period
#' correlation.
#' 
#' See vignettes for more details.
#' 
#' @examples
#' genBlockMat(nInds = 4, nPeriods = 3, rho_w = .8)
#' genBlockMat(nInds = 4, nPeriods = 3, rho_w = .8, rho_b = 0.5)
#' genBlockMat(nInds = 4, nPeriods = 3, rho_w = .8, rho_b = 0.5, rho_a = 0.7)
#' 
#' genBlockMat(nInds = 4, nPeriods = 3, rho_w = .8, r = .9, decay = "exp")
#' genBlockMat(nInds = 4, nPeriods = 3, rho_w = .8, r = .9, decay = "prop")
#' 
#' genBlockMat(nInds = c(2, 3), nPeriods = 2, rho_w = .8, r = .9, 
#'   decay = "prop", nclusters=2)
#' 
#' 
#' 
#' @export
#' @concept correlated
genBlockMat <- function(nInds, nPeriods, rho_w, rho_b = 0, rho_a = NULL, 
                        r = NULL, decay = NULL, nclusters = 1) {
  
  if (!requireNamespace("blockmatrix", quietly = TRUE)) {
    stop(
      "Package \"blockmatrix\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  ### Checking
  
  assertNotMissing(nInds = missing(nInds), nPeriods = missing(nPeriods),
                  rho_w = missing(rho_w))
  
  assertInteger(nInds = nInds, nPeriods = nPeriods)
  assertAtLeast(nPeriods = nPeriods, minVal = 2)
  assertInRange(rho_w = rho_w, range = c(-1,1))
  
  if (!is.null(r)) {
    assertNotNull(decay = decay)
  }
  
  if (is.null(decay)) {
    assertInRange(rho_b = rho_b, range = c(-1,1))
    
    if (!is.null(rho_a)) {
      assertInRange(rho_a = rho_a, range = c(-1,1))
    }
  }
  
  if (!is.null(decay)) {
    assertNotNull(r = r)
    assertOption(decay = decay, options = c("exp", "prop"))
  }
  
  ###
  
  .assignDiag <- function(block, value) {
    diag(block) <- value
    return(block)
  }
  
  ### generate blocks
  
  .genMat <- function(nInds, nPeriods, rho_w, rho_b, rho_a, r, decay) {
    diagblock <- matrix(rho_w, nInds, nInds)
    diag(diagblock) <- 1
    
    if (is.null(decay)) {
      edgeblocks <- lapply(1:(nPeriods-1), function(x) matrix(rho_b, nInds, nInds))
      if (!is.null(rho_a)) {
        edgeblocks <- lapply(1:(nPeriods-1), function(x) .assignDiag(edgeblocks[[x]], rho_a))  
      }
    } else {
      edgeblocks <- lapply(1:(nPeriods-1), function(x) matrix(rho_w * (r^x), nInds, nInds))
      if (decay == "prop") {
        edgeblocks <- lapply(1:(nPeriods-1), function(x) .assignDiag(edgeblocks[[x]], r^x))  
      }
    }
    
    # create list of blocks 
    
    blocks <- NULL
    blocks[[1]] <- diagblock
    blocks <- append(blocks, edgeblocks)
    
    names(blocks) <- paste0("M", c(1 : nPeriods))
    
    ### put blocks together
    
    block_str <- sapply(nPeriods:1, 
                        function(x) data.table::shift(1:nPeriods, nPeriods - x, fill = 0))
    block_str[upper.tri(block_str)] = t(block_str)[upper.tri(block_str)]
    block_str <- matrix(paste0("M", block_str), nPeriods)
    
    bm <- blockmatrix::blockmatrix(value = block_str, list = blocks, dim=c(nPeriods, nPeriods))
    newCorMatrix <-  as.matrix(bm)
    assertPositiveDefinite(newCorMatrix = newCorMatrix)
    
    newCorMatrix
   
  }
  
  if (nclusters == 1) {
    
    assertLength(nInds = nInds, length = nclusters)
    assertLength(rho_w = rho_w, length = nclusters)
    assertLength(rho_b = rho_b, length = nclusters)
    if (!is.null(rho_a)) assertLength(rho_a = rho_a, length = nclusters)
    
    cm <- .genMat(nInds, nPeriods, rho_w, rho_b, rho_a, r, decay)

  } else {
    
    if (length(nInds) == 1) nInds <- rep(nInds, nclusters)
    if (length(rho_w) == 1) rho_w <- rep(rho_w, nclusters)
    if (length(rho_b) == 1) rho_b <- rep(rho_b, nclusters)
    if (length(rho_a) == 1) rho_a <- rep(rho_a, nclusters)
    
    assertLength(nInds = nInds, length = nclusters)
    assertLength(rho_w = rho_w, length = nclusters)
    assertLength(rho_b = rho_b, length = nclusters)
    if (!is.null(rho_a)) assertLength(rho_a = rho_a, length = nclusters)
    
    dd <- data.table(
      nInds = nInds,
      rho_w = rho_w,
      rho_b = rho_b,
      rho_a = rho_a
    )
    
    cm <- lapply(split(dd, seq(nrow(dd))), 
      function(x) .genMat(x$nInds, nPeriods, x$rho_w, x$rho_b, x$rho_a, r, decay)
    )
  }
  
  cm
  
}
