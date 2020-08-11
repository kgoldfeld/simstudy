#' Add correlated data to existing data.table
#'
#' @param dtOld Data table that is the new columns will be appended to.
#' @param idname Character name of id field, defaults to "id".
#' @param mu A vector of means. The length of mu must be nvars.
#' @param sigma Standard deviation of variables. If standard deviation differs
#' for each variable, enter as a vector with the same length as the mean vector
#' mu. If the standard deviation is constant across variables, as single value
#' can be entered.
#' @param corMatrix Correlation matrix can be entered directly. It must be
#' symmetrical and positive semi-definite. It is not a required field; if a
#' matrix is not provided, then a structure and correlation coefficient rho must
#' be specified.
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not
#' provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "ind" for an independence
#' structure, "cs" for a compound symmetry structure, and "ar1" for an
#' autoregressive structure.
#' @param cnames Explicit column names. A single string with names separated
#' by commas. If no string is provided, the default names will be V#, where #
#' represents the column.
#' @return The original data table with the additional correlated columns
#' @examples
#' def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
#' def <- defData(def,
#'   varname = "xNorm", formula = "xUni * 2", dist = "normal",
#'   variance = 8
#' )
#'
#' dt <- genData(250, def)
#'
#' mu <- c(3, 8, 15)
#' sigma <- c(1, 2, 3)
#'
#' dtAdd <- addCorData(dt, "myID",
#'   mu = mu, sigma = sigma,
#'   rho = .7, corstr = "cs"
#' )
#' dtAdd
#'
#' round(var(dtAdd[, .(V1, V2, V3)]), 3)
#' round(cor(dtAdd[, .(V1, V2, V3)]), 2)
#'
#' dtAdd <- addCorData(dt, "myID",
#'   mu = mu, sigma = sigma,
#'   rho = .7, corstr = "ar1"
#' )
#' round(cor(dtAdd[, .(V1, V2, V3)]), 2)
#'
#' corMat <- matrix(c(1, .2, .8, .2, 1, .6, .8, .6, 1), nrow = 3)
#'
#' dtAdd <- addCorData(dt, "myID",
#'   mu = mu, sigma = sigma,
#'   corMatrix = corMat
#' )
#' round(cor(dtAdd[, .(V1, V2, V3)]), 2)
#' @export

addCorData <- function(dtOld, idname, mu, sigma, corMatrix = NULL,
                       rho, corstr = "ind", cnames = NULL) {
  # dtName must contain id for now
  dtTemp <- copy(dtOld)
  data.table::setkeyv(dtTemp, idname)

  n <- nrow(dtTemp)

  dtNew <- simstudy::genCorData(
    n = n, mu = mu, sigma = sigma,
    corMatrix = corMatrix, rho = rho,
    corstr = corstr, cnames = cnames,
    idname = idname
  )

  data.table::setkeyv(dtNew, idname)

  dtTemp <- mergeData(dtTemp, dtNew, idname)

  return(dtTemp[])
}

#' Create multivariate (correlated) data - for general distributions
#'
#' @param dt Data table that will be updated.
#' @param defs Field definition table created by function `defDataAdd`.
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not
#' provided.
#' @param tau Correlation based on Kendall's tau. If tau is specified, then it
#' is used as the correlation even if rho is specified. If tau is NULL, then the
#' specified value of rho is used, or rho defaults to 0.
#' @param corstr Correlation structure of the variance-covariance matrix defined
#' by sigma and rho. Options include "cs" for a compound symmetry structure
#' and "ar1" for an autoregressive structure. Defaults to "cs".
#' @param corMatrix Correlation matrix can be entered directly. It must be
#' symmetrical and positive semi-definite. It is not a required field; if a
#' matrix is not provided, then a structure and correlation coefficient rho must
#' be specified.
#' @return data.table with added column(s) of correlated data
#' @examples
#' defC <- defData(
#'   varname = "nInds", formula = 50, dist = "noZeroPoisson",
#'   id = "idClust"
#' )
#'
#' dc <- genData(10, defC)
#' #### Normal only
#'
#' dc <- addCorData(dc,
#'   mu = c(0, 0, 0, 0), sigma = c(2, 2, 2, 2), rho = .2,
#'   corstr = "cs", cnames = c("a", "b", "c", "d"),
#'   idname = "idClust"
#' )
#'
#' di <- genCluster(dc, "idClust", "nInds", "id")
#'
#' defI <- defDataAdd(
#'   varname = "A", formula = "-1 + a", variance = 3,
#'   dist = "normal"
#' )
#' defI <- defDataAdd(defI,
#'   varname = "B", formula = "4.5 + b", variance = .5,
#'   dist = "normal"
#' )
#' defI <- defDataAdd(defI,
#'   varname = "C", formula = "5*c", variance = 3,
#'   dist = "normal"
#' )
#' defI <- defDataAdd(defI,
#'   varname = "D", formula = "1.6 + d", variance = 1,
#'   dist = "normal"
#' )
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
addCorFlex <- function(dt, defs, rho = 0, tau = NULL, corstr = "cs",
                       corMatrix = NULL) {

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

  if (!all(defs[, dist] %in% c("normal", "gamma", "binary", "poisson", "negBinomial"))) {
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
    rho <- sin(tau * pi / 2)
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
      params <- .getBinaryMean(dTemp, formula = iFormula, Size = 1, link = iLink)

      V <- dTemp[, stats::qbinom(Unew, 1, params[[1]])]
    } else if (iDist == "poisson") {
      param1 <- .getPoissonMean(dTemp, formula = iFormula, link = iLink)

      V <- dTemp[, stats::qpois(Unew, param1)]
    } else if (iDist == "gamma") {
      mn <- .getGammaMean(dTemp, formula = iFormula, link = iLink)

      ### Gamma parameters need to be transformed

      sr <- gammaGetShapeRate(mn, corDefs[i, variance])
      param1 <- sr[[1]]
      param2 <- sr[[2]]

      V <- dTemp[, stats::qgamma(Unew, param1, param2)]
    } else if (iDist == "negBinomial") {
      mn <- .getNBmean(dTemp, formula = iFormula, link = iLink)

      ### NB parameters need to be transformed

      sp <- negbinomGetSizeProb(mn, corDefs[i, variance])
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

#' Create multivariate (correlated) data - for general distributions
#'
#' @param dtOld If an existing data.table is specified, then wide will be set to TRUE and n
#' will be set to the nrow(dt) without any warning or error.
#' @param nvars Number of new variables to create for each id.
#' @param idvar String variable name of column represents individual level id for correlated
#' data.
#' @param dist A string indicating "normal", "binary", "poisson" or "gamma".
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "cs" for a compound symmetry structure
#' and "ar1" for an autoregressive structure.
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical and
#' positive semi-definite. It is not a required field; if a matrix is not provided, then a
#' structure and correlation coefficient rho must be specified.
#' @param param1  A string that represents the column in dtOld that contains the parameter
#' for the mean of the distribution. In the case of the uniform distribution the column
#' specifies the minimum.
#' @param param2 A string that represents the column in dtOld that contains a possible second
#' parameter for the distribution. For the normal distribution, this will be the variance;
#' for the gamma distribution, this will be the dispersion; and for the uniform distribution,
#' this will be the maximum.
#' @param cnames Explicit column names. A single string with names separated
#' by commas. If no string is provided, the default names will be V#, where #
#' represents the column.
#' @param method Two methods are available to generate correlated data. (1) "copula" uses
#' the multivariate Gaussian copula method that is applied to all other distributions; this
#' applies to all available distributions. (2) "ep" uses an algorithm developed by
#' Emrich and Piedmonte.
#' @param formSpec The formula (as a string) that was used to generate the binary
#' outcome in the `defDataAdd` statement. This is only necessary when method "ep" is
#' requested.
#' @param periodvar A string value that indicates the name of the field that indexes
#' the repeated measurement for an individual unit. The value defaults to "period".
#' @return Original data.table with added column(s) of correlated data
#' @examples
#' # Wide example
#'
#' def <- defData(varname = "xbase", formula = 5, variance = .4, dist = "gamma", id = "cid")
#' def <- defData(def, varname = "lambda", formula = ".5 + .1*xbase", dist = "nonrandom", link = "log")
#' def <- defData(def, varname = "p", formula = "-2 + .3*xbase", dist = "nonrandom", link = "logit")
#'
#' dt <- genData(500, def)
#'
#' dtX1 <- addCorGen(
#'   dtOld = dt, idvar = "cid", nvars = 3, rho = .7, corstr = "cs",
#'   dist = "poisson", param1 = "lambda"
#' )
#'
#' dtX2 <- addCorGen(
#'   dtOld = dt, idvar = "cid", nvars = 4, rho = .4, corstr = "ar1",
#'   dist = "binary", param1 = "p"
#' )
#'
#' # Long example
#'
#' def <- defData(varname = "xbase", formula = 5, variance = .4, dist = "gamma", id = "cid")
#' def <- defData(def, "nperiods", formula = 3, dist = "noZeroPoisson")
#'
#' def2 <- defDataAdd(
#'   varname = "lambda", formula = ".5+.5*period + .1*xbase",
#'   dist = "nonrandom", link = "log"
#' )
#' def2 <- defDataAdd(def2,
#'   varname = "p", formula = "-3+.2*period + .3*xbase",
#'   dist = "nonrandom", link = "logit"
#' )
#' def2 <- defDataAdd(def2,
#'   varname = "gammaMu", formula = ".2*period + .3*xbase",
#'   dist = "nonrandom", link = "log"
#' )
#' def2 <- defDataAdd(def2, varname = "gammaDis", formula = 1, dist = "nonrandom")
#' def2 <- defDataAdd(def2, varname = "normMu", formula = "5+period + .5*xbase", dist = "nonrandom")
#' def2 <- defDataAdd(def2, varname = "normVar", formula = 4, dist = "nonrandom")
#' def2 <- defDataAdd(def2, varname = "unifMin", formula = "5 + 2*period + .2*xbase", dist = "nonrandom")
#' def2 <- defDataAdd(def2, varname = "unifMax", formula = "unifMin + 20", dist = "nonrandom")
#'
#' dt <- genData(1000, def)
#'
#' dtLong <- addPeriods(dt, idvars = "cid", nPeriods = 3)
#' dtLong <- addColumns(def2, dtLong)
#'
#' # Poisson distribution
#'
#' dtX3 <- addCorGen(
#'   dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "cs",
#'   dist = "poisson", param1 = "lambda", cnames = "NewPois"
#' )
#' dtX3
#'
#' # Binomial distribution - copula method
#'
#' dtX4 <- addCorGen(
#'   dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "cs",
#'   dist = "binary", param1 = "p", cnames = "NewBin"
#' )
#'
#' dtX4
#'
#' # Gamma distribution
#'
#' dtX6 <- addCorGen(
#'   dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "ar1",
#'   dist = "gamma", param1 = "gammaMu", param2 = "gammaDis",
#'   cnames = "NewGamma"
#' )
#'
#' dtX6
#'
#' # Normal distribution
#'
#' dtX7 <- addCorGen(
#'   dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "ar1",
#'   dist = "normal", param1 = "normMu", param2 = "normVar",
#'   cnames = "NewNorm"
#' )
#'
#' # Binary outcome - ep method
#'
#' probform <- "-2 + .3*period"
#'
#' def1 <- defDataAdd(
#'   varname = "p", formula = probform,
#'   dist = "nonrandom", link = "logit"
#' )
#'
#' dx <- genData(100)
#' dx <- addPeriods(dx, nPeriods = 4)
#' dx <- addColumns(def1, dx)
#'
#' dg <- addCorGen(dx,
#'   nvars = 4,
#'   corMatrix = NULL, rho = .3, corstr = "cs",
#'   dist = "binary", param1 = "p",
#'   method = "ep", formSpec = probform,
#'   periodvar = "period"
#' )
#' @export
#'
addCorGen <- function(dtOld, nvars, idvar = "id", rho, corstr, corMatrix = NULL,
                      dist, param1, param2 = NULL, cnames = NULL,
                      method = "copula", formSpec = NULL, periodvar = "period") {

  # "Declare" vars to avoid R CMD warning

  id <- NULL
  N <- NULL
  .U <- NULL
  Unew <- NULL
  .XX <- NULL
  X <- NULL
  timeID <- NULL
  .param1 <- NULL
  .param2 <- NULL

  ## Need to check if wide or long

  dtTemp <- copy(dtOld)

  #### Check args

  if (!(dist %in% c("poisson", "binary", "gamma", "uniform", "negBinomial", "normal"))) {
    stop("Distribution not properly specified.")
  }

  if (!(idvar %in% names(dtTemp))) {
    stop(paste(idvar, "(id) not a valid field/column."))
  }

  if (!(param1 %in% names(dtTemp))) {
    stop(paste(param1, "(parameter 1) not a valid field/column."))
  }

  if (!is.null(param2)) {
    if (!(param2 %in% names(dtTemp))) {
      stop(paste(param2, "(parameter 2) not a valid field/column."))
    }
  }

  nparams <- as.numeric(!is.null(param1)) + as.numeric(!is.null(param2))

  if (((nparams > 1) & (dist %in% c("poisson", "binary")))) {
    stop(paste0("Too many parameters (", nparams, ") for ", dist))
  }

  if (((nparams < 2) & (dist %in% c("gamma", "uniform", "normal", "negBinomial")))) {
    stop(paste0("Too few parameters (", nparams, ") for ", dist))
  }

  if (!(method %in% c("copula", "ep"))) {
    stop(paste(method, "is not a valid method"))
  }

  if (dist != "binary" & method == "ep") {
    stop("Method `ep` applies only to binary data generation")
  }

  ####

  setnames(dtTemp, idvar, "id")

  ####

  # wide(ness) is determined by incoming data structure.

  maxN <- dtTemp[, .N, by = id][, max(N)]
  if (maxN == 1) wide <- TRUE
  else wide <- FALSE

  if (wide == FALSE) {
    if (maxN != nvars) stop(paste0("Number of records per id (", maxN, ") not equal to specified nvars (", nvars, ")."))
  } else {
    if (maxN > 1) stop(paste0("Data are in long format and parameter wide as been specified as TRUE"))
  }

  if (!is.null(cnames)) {
    nnames <- trimws(unlist(strsplit(cnames, split = ",")))
    lnames <- length(nnames)

    if (wide == TRUE) {
      if (lnames != nvars) stop(paste0("Number of names (", lnames, ") not equal to specified nvars (", nvars, ")."))
    } else {
      if (lnames > 1) stop(paste("Long format can have only 1 name.", lnames, "have been provided."))
    }
  }

  ####

  if (method == "copula") {
    n <- length(unique(dtTemp[, id])) # should check if n's are correct
    dtM <- .genQuantU(nvars, n, rho, corstr, corMatrix)

    xid = "id"
    if (wide == TRUE) {
      dtTemp <- dtM[dtTemp[, c(xid, param1, param2), with = FALSE]]
      dtTemp[, .U := Unew]
    } else {
      dtTemp[, .U := dtM$Unew]
      dtTemp[, seq := dtM$seq]
    }


    if (dist == "poisson") {
      setnames(dtTemp, param1, ".param1")
      dtTemp[, .XX := stats::qpois(p = .U, lambda = .param1)]
    } else if (dist == "binary") {
      setnames(dtTemp, param1, ".param1")
      dtTemp[, .XX := stats::qbinom(p = .U, size = 1, prob = .param1)]
    } else if (dist == "negBinomial") {
      setnames(dtTemp, param1, ".param1")
      setnames(dtTemp, param2, ".param2")
      sp <- negbinomGetSizeProb(dtTemp$.param1, dtTemp$.param2)
      dtTemp[, .param1 := sp[[1]]]
      dtTemp[, .param2 := sp[[2]]]
      dtTemp[, .XX := stats::qnbinom(p = .U, size = .param1, prob = .param2)]
    } else if (dist == "uniform") {
      setnames(dtTemp, param1, ".param1")
      setnames(dtTemp, param2, ".param2")
      dtTemp[, .XX := stats::qunif(p = .U, min = .param1, max = .param2)]
    } else if (dist == "gamma") {
      setnames(dtTemp, param1, ".param1")
      setnames(dtTemp, param2, ".param2")
      sr <- gammaGetShapeRate(dtTemp$.param1, dtTemp$.param2)
      dtTemp[, .param1 := sr[[1]]]
      dtTemp[, .param2 := sr[[2]]]
      dtTemp[, .XX := stats::qgamma(p = .U, shape = .param1, rate = .param2)]
    } else if (dist == "normal") {
      setnames(dtTemp, param1, ".param1")
      setnames(dtTemp, param2, ".param2")
      dtTemp[, .XX := stats::qnorm(p = .U, mean = .param1, sd = sqrt(.param2))]
    }
  } else if (method == "ep") {
    if (is.null(formSpec)) {
      stop("Must specify formula used to generate probability")
    }

    if (!periodvar %in% names(dtTemp)) {
      stop(paste(periodvar, "not in data set."))
    }

    newExpress <- try(parse(text = formSpec), silent = TRUE)
    if (.iserror(newExpress)) stop("!")

    .Vars = all.vars(newExpress)
    .Vars <- .Vars[.Vars != periodvar]
    listvar <- c(.Vars, periodvar, param1)

    dperms <- dtTemp[, .N, keyby = listvar]
    dcombos <- dperms[, .SD[1, list(N = N)], keyby = .Vars]

    setkeyv(dperms, c(.Vars, periodvar))

    numcombos <- nrow(dcombos)

    dres <- vector("list", numcombos)

    if (numcombos == 1) { # no covariates, only repeated data

      p <- dperms$p
      nindiv <- dcombos$N

      dres[[1]] <- data.table(genCorGen(
        n = nindiv, nvars = length(p), params1 = p,
        dist = "binary", rho = rho, corstr = corstr,
        method = method
      ))
    } else { # covariates


      if (numcombos > 200) {
        cat(paste0(
          "\nNumber of covariate combinations is large: ", numcombos,
          ". Data generation might be slow.\n\n"
        ))
      }

      for (i in 1:numcombos) {
        p <- dperms[dcombos[i, ], p]
        n <- dcombos[i, N]

        dres[[i]] <- data.table(
          dcombos[i, ],
          genCorGen(
            n = n, nvars = length(p), params1 = p,
            dist = "binary", rho = rho, corstr = corstr,
            method = method
          )
        )
      }
    }


    dres <- rbindlist(dres)

    setkeyv(dres, c(.Vars, periodvar, "id"))

    setkeyv(dtTemp, c(.Vars, periodvar, "id"))
    dtTemp[, .XX := dres[, X]]

    setkeyv(dtTemp, c("id", periodvar))
  }


  if (wide == TRUE) {
    dtTemp <- dtTemp[, list(id, seq, .XX)]


    dWide <- dcast(dtTemp, id ~ seq, value.var = ".XX")
    dtTemp <- copy(dtOld)

    dtTemp <- dtTemp[dWide]

    if (!is.null(cnames)) {
      setnames(dtTemp, paste0("V", 1:nvars), nnames)
    }

    setnames(dtTemp, idvar, "id")
  } else if (wide == FALSE) {
    if (method == "copula") {
      dtTempLong <- dtTemp[, list(timeID, .XX)]
      dtTemp <- copy(dtOld)

      setkey(dtTempLong, timeID)
      setkey(dtTemp, timeID)

      dtTemp <- dtTemp[dtTempLong]

      setnames(dtTemp, idvar, "id")
    }

    if (!is.null(cnames)) setnames(dtTemp, ".XX", cnames)
    else setnames(dtTemp, ".XX", "X")
  }

  setnames(dtTemp, "id", idvar)


  return(dtTemp[])
}

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
#'

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
#'
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
    params1 = rep(params1, nvars)
  }

  if (!is.null(params2)) {
    if (length(params2) == 1) {
      params2 = rep(params2, nvars)
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
#'

genCorMat <- function(nvars, cors = NULL) {
  if (is.null(cors)) {
    ev = stats::runif(nvars, 0, 10)

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

    if (!all(eigen(cm)$values > 0)) stop("Not a positive definite matrix")
  }

  return(cm)
}

#### Generate correlated ordinal categorical data ####

#' @title Generate correlated ordinal categorical data
#' @description Correlated ordinal categorical data is added to an existing data set.
#' @param dtName Name of complete data set
#' @param idname A string. The id of the data.table that identifies a unique record.
#' Defaults to "id".
#' @param adjVar Vector of adjustment variables name in dtName - determines
#' logistic shift. This is specified assuming a cumulative logit
#' link. The vector can be NULL, of length 1, or a length equal to the
#' number of new categorical variables.
#' @param baseprobs A matrix of baseline probabilities. The row values must sum to 1.
#' The number of rows represents the number of new categorical variables. The number
#' of columns represents the number of possible responses - if an particular category
#' has fewer possible responses, assign zero probability to non-relevant columns.
#' @param prefix A string.The names of the new variables will be a concatenation of
#' the prefix and a sequence of integers indicating the variable number.
#' @param rho Correlation coefficient, -1 < rho < 1. Use if corMatrix is not provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "ind" for an independence
#' structure, "cs" for a compound symmetry structure, and "ar1" for an
#' autoregressive structure.
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical
#' and positive semi-definite. It is not a required field; if a matrix is not provided,
#' then a structure and correlation coefficient rho must be specified.
#' @return Original data.table with added categorical fields
#' @examples
#' #### Set definitions
#'
#' baseprobs <- matrix(c(
#'   0.2, 0.1, 0.1, 0.6,
#'   0.7, 0.2, 0.1, 0,
#'   0.5, 0.2, 0.3, 0,
#'   0.4, 0.2, 0.4, 0,
#'   0.6, 0.2, 0.2, 0
#' ),
#' nrow = 5, byrow = TRUE
#' )
#'
#' set.seed(333)
#' dT <- genData(1000)
#'
#' dX <- genCorOrdCat(dT,
#'   adjVar = NULL, baseprobs = baseprobs,
#'   prefix = "q", rho = .125, corstr = "cs"
#' )
#'
#' dM <- data.table::melt(dX, id.vars = "id")
#' dProp <- dM[, prop.table(table(value)), by = variable]
#' dProp[, response := c(1:4, 1:3, 1:3, 1:3, 1:3)]
#'
#' data.table::dcast(dProp, variable ~ response,
#'   value.var = "V1", fill = 0
#' )
#' @export
genCorOrdCat <- function(dtName, idname = "id", adjVar = NULL, baseprobs,
                         prefix = "grp", rho, corstr, corMatrix = NULL) {

  # "declares" to avoid global NOTE

  logisZ <- NULL
  period <- NULL

  # Check arguments

  if (!exists(deparse(substitute(dtName)), envir = parent.frame())) {
    stop("Data table does not exist.")
  }

  if (!(idname %in% names(dtName))) {
    stop(paste("idname", idname, "not in", deparse(substitute(dtName))))
  }

  if (!all(adjVar %in% names(dtName))) {
    missVars <- adjVar[!(adjVar %in% names(dtName))]
    stop(paste(
      "Variable (s)", paste(missVars, collapse = ", "), "not in",
      deparse(substitute(dtName))
    ))
  }

  if (!is.character(prefix)) {
    stop("prefix must be a string")
  }

  if (is.null(baseprobs)) {
    stop("Proability vector is empty")
  }

  baseprobs <- round(baseprobs, 10)
  if (!isTRUE(all.equal(rep(1, nrow(baseprobs)),
    apply(baseprobs, 1, sum),
    tolerance = .Machine$double.eps^0.5
  ))) {
    stop("Probabilities are not properly specified: each row must sum to one")
  }

  if (length(adjVar) > 1) {
    if (nrow(baseprobs) != length(adjVar)) {
      stop("Number of categories implied by baseprobs and adjVar do not match")
    }
  }

  if (length(adjVar) == 1) {
    adjVar <- rep(adjVar, nrow(baseprobs))
  }

  N <- nrow(dtName)
  nq <- nrow(baseprobs)
  zs <- .genQuantU(nq, N, rho = rho, corstr, corMatrix = corMatrix)
  zs[, logisZ := stats::qlogis(p = zs$Unew)]
  cprop <- t(apply(baseprobs, 1, cumsum))
  quant <- t(apply(cprop, 1, stats::qlogis))

  mycat <- list()

  for (i in 1:nq) {
    iLogisZ <- zs[period == i - 1, logisZ]
    matlp <- matrix(rep(quant[i, ], nrow(dtName)),
      ncol = ncol(cprop),
      byrow = TRUE
    )
    if (!is.null(adjVar)) {
      z <- dtName[, adjVar[i], with = FALSE][[1]]
      matlp <- matlp - z
    }
    locateGrp <- (iLogisZ > cbind(-Inf, matlp))
    assignGrp <- apply(locateGrp, 1, sum)
    mycat[[i]] <- data.table(
      id = dtName[, idname, with = FALSE][[1]],
      var = paste0(prefix, i),
      cat = assignGrp
    )
  }
  dcat <- data.table::rbindlist(mycat)
  cats <- data.table::dcast(dcat, id ~ var, value.var = "cat")

  setnames(cats, "id", idname)
  setkeyv(cats, idname)
  dtName <- dtName[cats]
  return(dtName[])
}
