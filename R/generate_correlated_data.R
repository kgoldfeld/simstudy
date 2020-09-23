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
#' genCorGen(1000,
#'   nvars = 3, params1 = l, dist = "poisson", rho = .7, corstr = "cs",
#'   cnames = "new_var"
#' )
#' genCorGen(1000,
#'   nvars = 3, params1 = l, dist = "poisson", rho = .7, corstr = "cs",
#'   wide = TRUE, cnames = "a, b, c"
#' )
#'
#' genCorGen(1000, nvars = 3, params1 = c(.3, .5, .7), dist = "binary", rho = .3, corstr = "cs")
#' genCorGen(1000,
#'   nvars = 3, params1 = l, params2 = c(1, 1, 1), dist = "gamma", rho = .3,
#'   corstr = "cs", wide = TRUE
#' )
#'
#' genCorGen(1000,
#'   nvars = 3, params1 = c(.3, .5, .7), dist = "binary",
#'   corMatrix = genCorMat(3), method = "ep"
#' )
#' genCorGen(1000,
#'   nvars = 3, params1 = c(.3, .5, .7), dist = "binary",
#'   corMatrix = genCorMat(3), method = "copula"
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

  if (class(params1) != "numeric") stop("Parameters must be numeric")

  if (!is.null(params2)) {
    if (class(params2) != "numeric") stop("Parameters must be numeric")
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
    dFinal <- dcast(dtM, id ~ seq, value.var = "X")
    if (!is.null(cnames)) {
      nnames <- trimws(unlist(strsplit(cnames, split = ",")))
      setnames(dFinal, paste0("V", 1:nvars), nnames)
    }
  }

  setnames(dFinal, "id", idname)

  return(dFinal[])
}


# TODO Implement Emrich and Piedmonte algorithm for correlated binary data?
#' Internal functions called by genCorGen and addCorGen - returns matrix
#'
#' @param nvars Number of new variables to generate
#' @param corMatrix Correlation matrix
#' @param rho Correlation coefficient
#' @param corstr Correlation structure
#' @return A correlation matrix
#' @noRd
.checkBoundsBin <- function(p1, p2, d) {
  l <- (p1 * p2) / ((1 - p1) * (1 - p2))
  L <- max(-sqrt(l), -sqrt(1 / l))

  u <- (p1 * (1 - p2)) / (p2 * (1 - p1))
  U <- min(sqrt(u), sqrt(1 / u))

  if ((d < L & isTRUE(all.equal(d, L)) == FALSE) |
    (d > U & isTRUE(all.equal(d, U)) == FALSE)) {
    LU <- paste0("(", round(L, 2), " ... ", round(U, 2), ")")
    stopText <- paste("Specified correlation", d, "out of range", LU)
    stop(stopText)
  }
}


#'
.findRhoBin <- function(p1, p2, d) {
  .checkBoundsBin(p1, p2, d)

  target <- d * sqrt(p1 * p2 * (1 - p1) * (1 - p2)) + p1 * p2

  # given p1, p2 & d, bisection search for corresponding rho

  Max <- 1
  Min <- -1
  test <- 0
  found <- FALSE

  while (!found) {
    corr <- diag(2)
    corr[1, 2] <- corr[2, 1] <- test

    est <- mvtnorm::pmvnorm(lower = rep(-Inf, 2), upper = c(stats::qnorm(p1), stats::qnorm(p2)), mean = c(0, 0), corr = corr)

    if (round(est, 5) == round(target, 5)) {
      found <- TRUE
      rho <- test
    } else if (est < target) {
      Min <- test
      test <- (Min + Max) / 2
    } else {
      Max <- test
      test <- (Min + Max) / 2
    }
  }

  return(rho)
}

#'
.genBinEP <- function(n, p, tcorr) {

  # "Declare" vars to avoid R CMD warning
  id <- NULL
  period <- NULL
  seqid <- NULL

  np <- length(p)
  phicorr <- diag(length(p))

  for (i in (1:(np - 1))) {
    for (j in ((i + 1):np)) {
      p1 <- p[i]
      p2 <- p[j]

      phicorr[j, i] <- phicorr[i, j] <- .findRhoBin(p1, p2, tcorr[i, j])
    }
  }

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
#' @param nvars number of rows and columns (i.e. number of variables) for correlation matrix
#' @param cors vector of correlations.
#' @return correlation matrix of size nvars x nvars
#' @details If the vector cors is not specified, a random correlation matrix is generated with no assumptions.
#' If the vector is provided, it should be interpreted as the lower triangle of the correlation
#' matrix, and is specified by reading down the columns. For example, if CM is the correlation matrix and
#' nvars = 3, then CM[2,1] = cors[1],  CM[3,1] = cors[2], and CM[3,2] = cors[3].
#' @examples
#' genCorMat(3, c(.3, -.2, .1))
#' genCorMat(3)
#'
#' genCorMat(5, c(.3, -.2, .1, .2, .5, .2, -.1, .3, .1, .2))
#' genCorMat(5)
#' @export
#' @concept correlated
genCorMat <- function(nvars, cors = NULL) {
  if (is.null(cors)) {
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
  } else {
    if (choose(nvars, 2) != length(cors)) stop("Correlations improperly specified")

    cmLower <- matrix(0, nrow = nvars, ncol = nvars)
    cmLower[lower.tri(cmLower)] <- cors
    cmUpper <- t(cmLower)

    cm <- cmLower + cmUpper

    diag(cm) <- 1
  }

  assertPositiveDefinite(corMat = cm)
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
