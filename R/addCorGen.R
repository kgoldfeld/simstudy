#' Create multivariate (correlated) data - for general distributions
#'
#' @param dtOld If an existing data.table is specified, then wide will be set to TRUE and n
#' will be set to the nrow(dt) without any warning or error.
#' @param nvars Number of new variables to create for each id.
#' @param idvar String variable name of column represents individual level id for correlated
#' data.
#' @param dist A string indicating "binary", "poisson" or "gamma".
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
#' @return Original data.table with added column(s) of correlated data
#' @examples
#' # Wide example
#'
#' def <- defData(varname = "xbase", formula = 5, variance = .4, dist = "gamma", id = "cid")
#' def <- defData(def, varname = "lambda", formula = ".5 + .1*xbase", dist="nonrandom", link = "log")
#' def <- defData(def, varname = "p", formula = "-2 + .3*xbase", dist="nonrandom", link = "logit")
#'
#' dt <- genData(500, def)
#'
#' dtX1 <- addCorGen(dtOld = dt, idvar = "cid", nvars = 3, rho = .7, corstr = "cs",
#'                     dist = "poisson", param1 = "lambda")
#'
#' dtX2 <- addCorGen(dtOld = dtX1, idvar = "cid", nvars = 4, rho = .4, corstr = "ar1",
#'                     dist = "binary", param1 = "p")
#'
#' # Long example
#'
#' def <- defData(varname = "xbase", formula = 5, variance = .4, dist = "gamma", id = "cid")
#' def <- defData(def, "nperiods", formula = 3, dist = "noZeroPoisson")
#'
#' def2 <- defDataAdd(varname = "lambda", formula = ".5+.5*period + .1*xbase",
#'                    dist="nonrandom", link = "log")
#' def2 <- defDataAdd(def2, varname = "p", formula = "-3+.2*period + .3*xbase",
#'                    dist="nonrandom", link = "logit")
#' def2 <- defDataAdd(def2, varname = "gammaMu", formula = ".2*period + .3*xbase",
#'                    dist="nonrandom", link = "log")
#' def2 <- defDataAdd(def2, varname = "gammaDis", formula = 1, dist="nonrandom")
#' def2 <- defDataAdd(def2, varname = "normMu", formula = "5+period + .5*xbase", dist="nonrandom")
#' def2 <- defDataAdd(def2, varname = "normVar", formula = 4, dist="nonrandom")
#' def2 <- defDataAdd(def2, varname = "unifMin", formula = "5 + 2*period + .2*xbase", dist="nonrandom")
#' def2 <- defDataAdd(def2, varname = "unifMax", formula = "unifMin + 20", dist="nonrandom")
#'
#' dt <- genData(1000, def)
#'
#' dtLong <- addPeriods(dt, idvars = "cid", nPeriods = 3)
#' dtLong <- addColumns(def2, dtLong)
#'
#' # Poisson distribution
#'
#' dtX3 <- addCorGen(dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "cs",
#'                   dist = "poisson", param1 = "lambda", cnames = "NewPois")
#' dtX3
#'
#' # Binomial distribution
#'
#' dtX4 <- addCorGen(dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "cs",
#'                   dist = "binary", param1 = "p", cnames = "NewBin")
#'
#' dtX4
#'
#' # Gamma distribution
#'
#' dtX6 <- addCorGen(dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "ar1",
#'                   dist = "gamma", param1 = "gammaMu", param2 = "gammaDis",
#'                   cnames = "NewGamma")
#'
#' dtX6
#'
#' # Normal distribution
#'
#' dtX7 <- addCorGen(dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "ar1",
#'                   dist = "normal", param1 = "normMu", param2 = "normVar",
#'                   cnames = "NewNorm")
#'
#'
#' @export
#'
addCorGen <- function(dtOld, nvars, idvar, rho, corstr, corMatrix = NULL,
                      dist, param1, param2 = NULL, cnames = NULL) {

  # "Declare" vars to avoid R CMD warning

  id <- NULL
  N <- NULL
  U <- NULL
  Unew <- NULL
  X <- NULL
  timeID <- NULL

  ## Need to check if wide or long

  dtTemp <- copy(dtOld)

  #### Check args

  if ( !(dist %in% c("poisson", "binary", "gamma", "uniform", "negbinom", "normal"))) {
    stop("Distribution not properly specified.")
  }

  if (!(idvar %in% names(dtTemp))){
    stop(paste(idvar, "(id) not a valid field/column."))
  }

  if (!(param1 %in% names(dtTemp))){
    stop(paste(param1, "(parameter 1) not a valid field/column."))
  }

  if (!is.null(param2)) {
    if (!(param2 %in% names(dtTemp))){
      stop(paste(param2, "(parameter 2) not a valid field/column."))
    }
  }

  nparams <- as.numeric(!is.null(param1)) + as.numeric(!is.null(param2))

  if ( ((nparams > 1) & (dist %in% c("poisson", "binary")))) {
    stop(paste0("Too many parameters (", nparams, ") for " , dist))
  }

  if ( ((nparams < 2) & (dist %in% c("gamma", "uniform", "normal")))) {
    stop(paste0("Too few parameters (", nparams, ") for " , dist))
  }

  ####

  setnames(dtTemp, idvar, "id")

  ####

  maxN <- dtTemp[, .N, by = id][,max(N)]
  if (maxN == 1) wide <- TRUE
  else wide <- FALSE

  if (wide == FALSE) {
    if ( maxN != nvars) stop(paste0("Number of records per id (", maxN, ") not equal to specified nvars (", nvars,")."))
  } else {
    if ( maxN > 1) stop(paste0("Data are in long format and parameter wide as been specified as TRUE"))
  }

  if (!is.null(cnames)) {

    nnames <- trimws(unlist(strsplit(cnames, split = ",")))
    lnames <- length(nnames)

    if (wide == TRUE) {
      if (lnames != nvars) stop(paste0("Number of names (", lnames, ") not equal to specified nvars (", nvars,")."))
    } else {
      if (lnames > 1) stop(paste("Long format can have only 1 name.", lnames, "have been provided."))
    }
  }

  ####

  n <- length(unique(dtTemp[, id])) # should check if n's are correct
  dtM <- genQuantU(nvars, n, rho, corstr, corMatrix)


  xid = "id"
  if (wide == TRUE) {
    dtTemp <- dtM[dtTemp[, c(xid, param1, param2), with = FALSE]]
    dtTemp[, U := Unew]
  } else {
    dtTemp[, U :=  dtM$Unew]
    dtTemp[, seq :=  dtM$seq]
  }

  if (dist == "poisson") {
    setnames(dtTemp, param1, "param1")
    dtTemp[, X := stats::qpois(p = U, lambda = param1)]
  } else if (dist == "binary") {
    setnames(dtTemp, param1, "param1")
    dtTemp[, X := stats::qbinom(p = U, size = 1, prob = param1)]
  } else if (dist == "uniform") {
    setnames(dtTemp, param1, "param1")
    setnames(dtTemp, param2, "param2")
    dtTemp[, X := stats::qunif(p = U, min = param1, max = param2)]
  } else if (dist == "gamma") {
    setnames(dtTemp, param1, "param1")
    setnames(dtTemp, param2, "param2")
    sr <- gammaGetShapeRate(dtTemp$param1, dtTemp$param2)
    dtTemp[, param1 := sr[[1]]]
    dtTemp[, param2 := sr[[2]]]
    dtTemp[, X := stats::qgamma(p = U, shape = param1, rate = param2)]
  } else if (dist == "normal") {
    setnames(dtTemp, param1, "param1")
    setnames(dtTemp, param2, "param2")
    dtTemp[, X := stats::qnorm(p = U, mean = param1, sd = sqrt(param2))]
  }

  if (wide == TRUE) {

    dtTemp <- dtTemp[, list(id, seq, X)]
    dWide <- dcast(dtTemp, id ~ seq, value.var="X")
    dtTemp <- dtOld[dWide]

    if (!is.null(cnames)) {
      setnames(dtTemp, paste0("V",1:nvars), nnames)
    }

  } else {

    dtTemp <- dtTemp[, list(timeID, X)]
    dtTemp <- dtOld[dtTemp]

    if (!is.null(cnames)) setnames(dtTemp, "X", cnames)

  }

  return(dtTemp[])
}
