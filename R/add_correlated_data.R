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
#' @concept correlated
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
#' @param envir Environment the data definitions are evaluated in.
#'  Defaults to [base::parent.frame].
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
#' @concept correlated
#' @md
#' @export
addCorFlex <- function(dt, defs, rho = 0, tau = NULL, corstr = "cs",
                       corMatrix = NULL, envir = parent.frame()) {

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
      params <- .getBinaryMean(dTemp,
        formula = iFormula,
        size = 1,
        link = iLink,
        envir = envir
      )

      V <- dTemp[, stats::qbinom(Unew, 1, params[[1]])]
    } else if (iDist == "poisson") {
      param1 <- .getPoissonMean(
        dtSim = dTemp,
        formula = iFormula,
        link = iLink,
        envir = envir
      )

      V <- dTemp[, stats::qpois(Unew, param1)]
    } else if (iDist == "gamma") {
      mn <- .getGammaMean(
        dtSim = dTemp,
        formula = iFormula,
        link = iLink,
        envir = envir
      )

      ### Gamma parameters need to be transformed

      sr <- gammaGetShapeRate(mn, corDefs[i, variance])
      param1 <- sr[[1]]
      param2 <- sr[[2]]

      V <- dTemp[, stats::qgamma(Unew, param1, param2)]
    } else if (iDist == "negBinomial") {
      mn <- .getNBmean(dTemp, formula = iFormula, link = iLink, envir = envir)

      ### NB parameters need to be transformed

      sp <- negbinomGetSizeProb(mn, corDefs[i, variance])
      param1 <- sp[[1]]
      param2 <- sp[[2]]

      V <- dTemp[, stats::qnbinom(Unew, param1, param2)]
    } else if (iDist == "normal") {
      param1 <- .getNormalMean(dtSim = dTemp, formula = iFormula, envir = envir)
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
#' def2 <- defDataAdd(def2,
#'   varname = "gammaDis",
#'   formula = 1, dist = "nonrandom"
#' )
#' def2 <- defDataAdd(def2,
#'   varname = "normMu",
#'   formula = "5+period + .5*xbase", dist = "nonrandom"
#' )
#' def2 <- defDataAdd(def2,
#'   varname = "normVar", formula = 4,
#'   dist = "nonrandom"
#' )
#' def2 <- defDataAdd(def2,
#'   varname = "unifMin",
#'   formula = "5 + 2*period + .2*xbase", dist = "nonrandom"
#' )
#' def2 <- defDataAdd(def2,
#'   varname = "unifMax",
#'   formula = "unifMin + 20", dist = "nonrandom"
#' )
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
#' @concept correlated
#' @export
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
  if (maxN == 1) {
    wide <- TRUE
  } else {
    wide <- FALSE
  }

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

    xid <- "id"
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
    if (.isError(newExpress)) stop("!")

    .Vars <- all.vars(newExpress)
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

    if (!is.null(cnames)) {
      setnames(dtTemp, ".XX", cnames)
    } else {
      setnames(dtTemp, ".XX", "X")
    }
  }

  setnames(dtTemp, "id", idvar)


  return(dtTemp[])
}