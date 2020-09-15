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
  varValues <- mget(sub("..", "", vars[dotVars]),envir = parent.frame(), inherits = TRUE)
  names(varValues) <- vars[dotVars]
  varValues
}

#' Evaluate data frame with external variables
#'
#' @param formula A data definition formula.
#' @param extVars Named list with values of ..vars in `formula`
#' @param dtSim data.table with previously generated data.
#' @param n Number of observations to be generated
#' @return A matrix of the generated Data.
#' @noRd
.evalWith <- function(formula,
                      extVars,
                      dtSim = data.frame(),
                      n = nrow(dtSim)) {

  if (missing(dtSim) && missing(n)) {
    n <- 1
  }

  if (!missing(dtSim) && !is.null(dtSim) && n != nrow(dtSim)) {
    stop(glue(
      "Both 'dtSim' and 'n' are set but are of different length: ",
       "{nrow(dtSim)} != {n}"
    ))
  }

  e <- list2env(extVars)

  if (!missing(dtSim) && !is.null(dtSim)) {
    e <- list2env(dtSim, e)
  }

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

  # If only a single formula with 1 rep is eval'ed output would be not be
  # matrix, so transpose for uniform output.
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
#' @return Boolean
#' @details currently not in use
#' @noRd
.isFormulaScalar <- function(formula) {
  if (is.character(formula)) {
    if (";" %in% strsplit(formula, "")[[1]]) {
      FALSE
    } else {
      !.isError(
        try(is.numeric(eval(parse(text = formula),
                        envir = NULL, enclos = NULL
        )), silent = TRUE)
      )
    }
  }
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
#' @noRd 
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
.isError <- function(tryObject) {
  methods::is(tryObject, "try-error")
}


#' Convert log odds to probability
#'
#' @param logodds Log odds
#' @return Probability
#' @noRd
.log2Prob <- function(logOdds) {
  exp(logOdds) / (1 + exp(logOdds))
}
