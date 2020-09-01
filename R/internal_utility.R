#' Parse value of external variables
#'
#' @description This function fetches the value of variables that lie
#'  outside the scope of the definition table so that they can be used in data
#'  generation.
#'
#' @param formula Some object or literal containing data definition formula(s). 
#' @return A named list with the value of the external variables.
#' @examples
#' extVar1 <- 23
#' extVar2 <- 42
#' .parseDotVars("a + ..extVar1 | b + ..extVar2")
#' .parseDotVars(c("a + ..extVar1","b + ..extVar2"))
#' .parseDotVars(data.frame("a + ..extVar1","b + ..extVar2"))
#' @noRd
.parseDotVars <- function(formula) {
  vars <- all.vars(parse(text = formula))
  dotVars <- startsWith(vars, "..")
  # TODO clarify inheritance in case of non globalEnvs in documentation
  varValues <- mget(sub("..", "", vars[dotVars]), inherits = TRUE)
  names(varValues) <- vars[dotVars]
  varValues
}

#' Evaluate data frame with external variables
#'
#' @param formula A data definition formula.
#' @param extVars Named list with values of ..vars in `formula`
#' @param dtSim data.table with previously generated data or NULL.
#' @param n Number of observations to be generated
#' @return A matrix of the generated Data.
.evalWith <- function(formula, extVars, dtSim, n = nrow(dtSim)) {
  if (missing(dtSim) && missing(n)) n <- 1

  e <- list2env(extVars)

  if (!is.null(dtSim))
    e <- list2env(dtSim, e)

  if (!is.null(e$formula2parse))
    stop("'formula2parse' is a reserved variable name!")

  evalFormula <- function(x) {
    e$formula2parse <- x
    res <- with(e, eval(parse(text = formula2parse)))

    if (length(res) == 1)
      rep(res, n)
    else
      res
  }
  parsedValues <- sapply(formula, evalFormula)

  if (!is.matrix(parsedValues))
    t(parsedValues)
  else
    parsedValues
}

#' Get Distributions
#'
#' @return A character vector containing the names of all valid distributions.
#' @noRd
.getDists <-
  function() {
    c(
      "normal",
      "binary",
      "binomial",
      "poisson",
      "noZeroPoisson",
      "uniform",
      "categorical",
      "gamma",
      "beta",
      "nonrandom",
      "uniformInt",
      "negBinomial",
      "exponential",
      "mixture"
    )
  }

#' Is Formula Scalar?
#'
#' @param formula String or Number
#'
#' @return Boolean
#' @noRd
.isFormulaScalar <- function(formula) {
  if (is.character(formula))
    if (";" %in% strsplit(formula, "")[[1]])
      FALSE
    else
      is.numeric(eval(parse(text = formula)))
  else if (is.numeric(formula))
    TRUE
  else
    FALSE
}

#' Check if variable name is valid.
#'
#' @param x Name(s) to check for validity.
#' @param allowReserved Should reserved names be allowed (..., ..1 etc.)
#' @param unique Should the names be unique?
#' @return A boolean value for each checked name.
#' @details Adapted from:
#' \url{https://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
#' @noRd
.isValidVarName <- function(x, allowReserved = FALSE, unique = FALSE) {
  ok <- rep.int(TRUE, length(x))

  # is name too long?
  maxLength <- if (getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > maxLength] <- FALSE

  # is it a reserved variable, i.e.
  # an ellipsis or two dots then a number?
  if (!allowReserved) {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }

  # are names valid (and maybe unique)
  ok[x != make.names(x, unique = unique)] <- FALSE

  ok
}

#' Search formulas for "LAG()" function
#'
#' @param formulas Formulas to check.
#' @return boolean indicator that that at least one formula includes
#' "LAG()" function
#' @noRd
.checkLags <- function(formulas) {
  nLAGS <- length(unlist(regmatches(
    formulas,
    gregexpr("(?=LAG\\().*?(?<=\\))", formulas, perl = T)
  )))

  return(nLAGS > 0)
}

#' Add temp lag fields and update formulas
#'
#' @param oldDT data.table to be modified
#' @param formsdt string of formulas to be modified
#' @return list of modified data.table, modified formulas, and vector of
#' names of temporary variables.
#'@noRd
.addLags <- function(oldDT, formsdt) {

  # "Declare" vars to avoid R CMD warning
  # TODO "declare vars"
  id <- NULL
  N <- NULL

  ##

  lagdt <- data.table::copy(oldDT)
  lagforms <- data.table::copy(formsdt)
  origNames <- data.table::copy(names(oldDT))

  if (!any(lagdt[, .N, keyby = id][, N > 1])) stop("Data not longitudinal")

  nforms <- length(lagforms)

  for (p in 1:nforms) {
    lags <- regmatches(
      lagforms[p],
      gregexpr("(?<=LAG\\().*?(?=\\))", lagforms[p], perl = T)
    )[[1]]

    if (length(lags) == 0) next # No lags in current formula

    if (any(table(lags) > 1)) {
      stop("Repeated lag term in formula")
    }

    if (!all(is.element(lags, origNames))) {
      stop(paste(setdiff(lags, origNames), "not in data table. "))
    }

    lags.1 <- paste0(".", lags, "1")
    if (is.element(lags.1, origNames)) {
      stop("Please do not use .*1 names")
    }

    # Everything is OK: update formula

    regmatches(
      lagforms[p],
      gregexpr("(?=LAG\\().*?(?<=\\))", lagforms[p], perl = T)
    ) <- list(lags.1)

    # Add new column(s) for lagged data

    for (i in 1:length(lags[p])) {
      if (!is.element(lags.1[i], origNames)) {
        lagdt[, (lags.1[i]) := shift(.SD[, lags, with = FALSE], n = 1, fill = 0),
          by = id
        ]
      }
    }
  }

  ####

  lagNames <- setdiff(names(lagdt), origNames)

  list(lagdt, lagforms, lagNames)
}

#' Assign treatment
#'
#' @param dt data table
#' @param strata vector of string names representing strata
#' @return An integer (group) ranging from 1 to length of the probability vector
#' @noRd 
.addStrataCode <- function(dt, strata) {

  # 'Declare' var
  # TODO "declare vars"
  .stratum <- NULL

  #

  dtWork <- copy(dt)

  strataOnly <- dtWork[, eval(strata), with = FALSE]
  data.table::setkeyv(strataOnly, names(strataOnly))

  uniqueStrata <- unique(strataOnly)
  uniqueStrata[, .stratum := (1:.N)]

  data.table::setkeyv(dtWork, names(strataOnly))
  dtWork <- uniqueStrata[dtWork]

  data.table::setkeyv(dtWork, key(dt))

  dtWork[]
}

#' Stratified sample
#'
#' @param nrow Number of rows in the stratum
#' @param ncat Number of treatment categories
#' @return A sample draw from a stratum
#' @noRd
.stratSamp <- function(nrow, ncat, ratio) {
  if (is.null(ratio)) ratio <- rep(1, ncat)

  neach <- floor(nrow / sum(ratio))
  distrx <- rep(c(1:ncat), times = (neach * ratio))
  extra <- nrow - length(distrx)
  sample(c(distrx, sample(rep(1:ncat, times = ratio), extra)))
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

.genBinEP <- function(n, p, tcorr) {

  # "Declare" vars to avoid R CMD warning
  # TODO "declare vars"
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

#' Check or create correlation matrix
#'
#' @param nvars Number of new variables to generate
#' @param corMatrix Correlation matrix
#' @param rho Correlation coefficient
#' @param corstr Correlation structure
#' @return A correlation matrix
#' @details  Internal function called by genCorData and addCorGen
#' @noRd
.buildCorMat <- function(nvars, corMatrix, corstr, rho) {
  if (is.null(corMatrix)) {
    corMatrix <- diag(nvars) # do not modify if indepdendent

    if (corstr == "cs") {
      corMatrix <- rho^(row(corMatrix) != col(corMatrix))
    } else if (corstr == "ar1") {
      corMatrix <- rho^abs(row(corMatrix) - col(corMatrix))
    }
  } else if (!is.null(corMatrix)) { # check if positive definite/symmetric

    if (nvars != length(diag(corMatrix))) {
      stop("Length of mean vector mismatched with correlation matrix")
    }

    if (!isSymmetric(corMatrix)) {
      stop("Correlation matrix not symmetric")
    }

    if (!all(eigen(corMatrix)$values > 0)) {
      stop("Correlation matrix not positive definite")
    }
  }

  return(corMatrix)
}

#' Find variance associated with ICC
#
#' @param j Value of function
#' @return inverse of Poisson ICC function

.findPoisVar <- function(j) {

  # 'declare' var
  # TODO "declare vars"
  y <- NULL

  ##

  a <- seq(0, 20, by = 0.01)
  dx <- data.table::data.table(a = a, y = exp(3 * a / 2) - exp(a / 2))

  amin <- dx[y <= j][.N, a]

  a <- seq(amin, amin + 1e-02, length = 101)
  dx <- data.table::data.table(a = a, y = exp(3 * a / 2) - exp(a / 2))

  amin <- dx[y <= j][.N, a]

  a <- seq(amin, amin + 1e-04, length = 1001)
  dx <- data.table::data.table(a = a, y = exp(3 * a / 2) - exp(a / 2))


  return(dx[y <= j][.N, a])
}

#' Gamma distribution - variation to generate positive skew
#'
#' @param n The number of observations required in the data set
#' @param mean The mean
#' @param variance The variance
#' @return A data.frame column with the updated simulated data
#' @details Internal function called by .generate - returns gamma data
#' @noRd 
.genPosSkew <- function(n, mean, dispersion = 0) {
  if (dispersion == 0) {
    new <- rep(mean, n)
  } else {
    variance <- mean^2 * dispersion

    shape <- (mean^2) / variance
    rate <- mean / variance

    new <- stats::rgamma(n, shape = shape, rate = rate)
  }

  return(new)
}

#' Quantile for copula data generation
#'
#' @param nvars Number of new variables to generate
#' @param n Number records to generate
#' @param rho Correlation coefficient
#' @param corstr Correlation structure
#' @param corMatrix Correlation matrix
#' @param idname Name of id variable
#' @return A data.frame column with correlated uniforms
#' @details Internal function called by genCorGen and addCorGen
#' @noRd
.genQuantU <- function(nvars, n, rho, corstr, corMatrix, idname = "id") {

  # "Declare" vars to avoid R CMD warning
  # TODO "declare vars"
  seqid <- NULL
  period <- NULL
  Unew <- NULL
  Y <- NULL

  mu <- rep(0, nvars)
  if (is.null(corMatrix)) {
    dt <- genCorData(n, mu, sigma = 1, rho = rho, corstr = corstr, idname = idname)
  } else {
    dt <- genCorData(n, mu, sigma = 1, corMatrix = corMatrix, idname = idname)
  }

  dtM <- melt(dt, id.vars = idname, variable.factor = TRUE, value.name = "Y", variable.name = "seq")

  dtM[, period := as.integer(seq) - 1]
  setkeyv(dtM, idname)
  dtM[, seqid := .I]
  dtM[, Unew := stats::pnorm(Y)]

  return(dtM[, -"Y"])
}

#' Check if error
#'
#' @param tryObject the result of a try() call.
#' @return TRUE or FALSE
#' @noRd

.isError <- function(tryobject) {
  if (class(tryobject)[1] == "try-error") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Convert log odds to probability
#'
#' @param logodds Log odds
#' @return Probability
#' @noRd
.log2Prob <- function(logodds) {
  exp(logodds) / (1 + exp(logodds))
}
