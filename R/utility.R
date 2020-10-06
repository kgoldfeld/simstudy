#' Generate Mixture Formula
#'
#' @description Generates a mixture formula from a vector of variable names and
#' an optional vector of probabilities.
#' @param vars Character vector/list of variable names.
#' @param probs Numeric vector/list of probabilities. Has to be same length as
#' vars or NULL. Probabilities will be normalized if the sum to > 1.
#' @param varLength If `vars` is of length one and varLength is set to any
#' integer > 0, `vars` will be interpreted as array of length `varLength` and
#' all elements will used in sequence.
#' @return The mixture formula as a string.
#' @examples
#' genMixFormula(c("a", "..b[..i]", "c"))
#' genMixFormula(c("a", "..b", "c"), c(.2, .5, .3))
#'
#' # Shorthand to use external vectors/lists
#' genMixFormula("..arr", varLength = 5)
#' @export
#' @concept utility
#' @md
genMixFormula <- function(vars, probs = NULL, varLength = NULL) {
  assertNotMissing(vars = missing(vars))

  if (length(vars) == 1 && !is.null(varLength)) {
    assertType(vars = vars, type = "character")
    assertInteger(varLength = varLength)

    if (!startsWith(vars, "..")) {
      valueError(
        vars,
        list(
          "'{names}' not dot-dot-var, only external",
          " arrays can be used to generate a mixture formula."
        )
      )
    }

    vars <- glue("{vars}[[{1:varLength}]]")
  }

  if (is.null(probs)) {
    n <- length(vars)
    probs <- rep(1 / n, n)
  } else {
    assertNumeric(probs = probs)
    probs <- .adjustProbs(unlist(probs))
    assertLengthEqual(vars = vars, probs = probs)
  }

  paste(vars, probs, sep = " | ", collapse = " + ")
}

#' Convert beta mean and precision parameters to two shape parameters
#'
#' @param mean The mean of a beta distribution
#' @param precision The precision parameter (phi) of a beta distribution
#' @return A list that includes the shape parameters of the beta distribution
#' @details In simstudy, users specify the beta distribution as a function of
#' two parameters - a mean and precision, where 0 < mean < 1 and precision > 0.
#' In this case, the variance of the specified distribution is
#' (mean)*(1-mean)/(1+precision). The base R function rbeta uses the two shape
#' parameters to specify the beta distribution. This function converts the mean
#' and precision into the shape1 and shape2 parameters.
#' @examples
#' set.seed(12345)
#' mean <- 0.3
#' precision <- 1.6
#' rs <- betaGetShapes(mean, precision)
#' c(rs$shape1, rs$shape2)
#' vec <- rbeta(1000, shape1 = rs$shape1, shape2 = rs$shape2)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean * (1 - mean) / (1 + precision)))
#' (theoryMoments <- with(rs, c(
#'   shape1 / (shape1 + shape2),
#'   (shape1 * shape2) / ((shape1 + shape2)^2 * (1 + shape1 + shape2))
#' )))
#' @export
#' @concept utility
betaGetShapes <- function(mean, precision) {
  assertInRange(
    mean = mean, range = c(0, 1),
    minCheck = ">", maxCheck = "<"
  )
  assertInRange(
    precision = precision, range = c(0, Inf),
    minCheck = ">", maxCheck = "<"
  )

  a <- mean * precision
  b <- (1 - mean) * precision

  return(list(shape1 = a, shape2 = b))
}

#' Generate Categorical Formula
#' @description This function is deprecated, please use [genCatFormula] instead.
#' @export
#' @md
#' @keywords internal
catProbs <- function(..., n = 0) {
  .Deprecated("genCatFormula")
  genCatFormula(..., n = n)
}

#' Generate Categorical Formula
#'
#' @description Create a semi-colon delimited string of probabilities to be used
#' to define categorical data.
#' @param ... one or more numeric values to be concatenated, delimited by ";".
#' @param n Number of probabilities (categories) to be generated - all with
#' equal probability.
#' @return string with multinomial probabilities.
#' @details The function accepts a number of probabilities or a value of n, but
#' not both.
#'
#' If probabilities are passed, the string that is returned depends on the
#' nature of those probabilities. If the sum of the probabilities is less than
#' 1, an additional category is created with the probability 1 - sum(provided
#' probabilities). If the sum of the probabilities is equal to 1, then the
#' number of categories is set to the number of probabilities provided. If the
#' sum of the probabilities exceeds one (and there is more than one
#' probability), the probabilities are standardized by dividing by the sum of
#' the probabilities provided.
#'
#' If n is provided, n probabilities are included in the string, each with a probability equal to 1/n.
#' @examples
#' genCatFormula(0.25, 0.25, 0.50)
#' genCatFormula(1 / 3, 1 / 2)
#' genCatFormula(1, 2, 3)
#' genCatFormula(n = 5)
#' @export
#' @concept utility
genCatFormula <- function(..., n = 0) {
  nProbs <- ...length()
  probs <- unlist(list(...))

  if (nProbs == 0 & n == 0) stop("Need to specify probabilities or n.")
  if (nProbs > 0 & n > 0) stop("Specify probabilities or n, not both.")
  if (n < 0) stop("Negative values for n are not valid.")
  if (floor(n) != n) stop("'n' must be a whole number.")
  if (length(probs) == 1 && probs >= 1) stop("Single probability must be less than 1.")

  if (nProbs > 0) {
    pnew <- .adjustProbs(probs)
  }

  if (n > 0) {
    pnew <- rep(1 / n, n)
  }

  return(paste0(pnew, collapse = ";"))
}

#' Delete columns from existing data set
#'
#' @param dtOld Name of data table that is to be updated.
#' @param vars Vector of column names (as strings).
#' @return An updated data.table without `vars`.
#' @examples
#' # New data set
#'
#' def <- defData(varname = "x", dist = "noZeroPoisson", formula = 7, id = "idnum")
#' def <- defData(def, varname = "xUni", dist = "uniformInt", formula = "x-3;x+3")
#'
#' dt <- genData(10, def)
#' dt
#'
#' # Delete column
#'
#' dt <- delColumns(dt, "x")
#' dt
#' @export
#' @md
#' @concept utility
delColumns <- function(dtOld, vars) {
  assertNotMissing(dtOld = missing(dtOld), vars = missing(vars))
  assertValue(dtOld = dtOld, vars = vars)
  assertType(vars = vars, type = "character")
  assertInDataTable(vars, dtOld)

  if (any(key(dtOld) %in% vars)) {
    stop(paste0("Cannot delete the index key"), call. = FALSE)
  }

  return(dtOld[, -c(vars), with = FALSE])
}

#' Convert gamma mean and dispersion parameters to shape and rate parameters
#'
#' @param mean The mean of a gamma distribution
#' @param dispersion The dispersion parameter of a gamma distribution
#' @return A list that includes the shape and rate parameters of the gamma distribution
#' @details In simstudy, users specify the gamma distribution as a function of two parameters - a mean
#' and dispersion. In this case, the variance of the specified distribution is (mean^2)*dispersion.
#' The base R function rgamma uses the shape and rate parameters to specify the gamma distribution.
#' This function converts the mean and dispersion into the shape and rate.
#' @examples
#' set.seed(12345)
#' mean <- 5
#' dispersion <- 1.5
#' rs <- gammaGetShapeRate(mean, dispersion)
#' c(rs$shape, rs$rate)
#' vec <- rgamma(1000, shape = rs$shape, rate = rs$rate)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean^2 * dispersion))
#' (theoryMoments <- c(rs$shape / rs$rate, rs$shape / rs$rate^2))
#' @export
#' @concept utility
gammaGetShapeRate <- function(mean, dispersion) {
  variance <- dispersion * (mean^2)

  shape <- (mean^2) / variance
  rate <- mean / variance

  return(list(shape = shape, rate = rate))
}

#' Generate variance for random effects that produce desired intra-class
#' coefficients (ICCs) for clustered data.
#'
#' @param ICC Vector of values between 0 and 1 that represent the
#' target ICC levels
#' @param dist The distribution that describes the outcome data at the
#' individual level. Possible distributions include "normal", "binary",
#' "poisson", or "gamma"
#' @param varTotal Numeric value that represents the total variation for a
#' normally distributed model. If "normal" distribution is specified, either
#' varTotal or varWithin must be specified, but not both.
#' @param varWithin Numeric value that represents the variation within a
#' cluster for a normally distributed model. If "normal" distribution is
#' specified, either varTotal or varWithin must be specified, but not both.
#' @param lambda Numeric value that represents the grand mean. Must be specified
#' when distribution is "poisson" or "negative binomial".
#' @param disp Numeric value that represents the dispersion parameter that is used
#' to define a gamma or negative binomial distribution with a log link. Must be
#' specified when distribution is "gamma".
#' @return A vector of values that represents the variances of random effects
#' at the cluster level that correspond to the ICC vector.
#' @examples
#' targetICC <- seq(0.05, 0.20, by = .01)
#'
#' iccRE(targetICC, "poisson", lambda = 30)
#'
#' iccRE(targetICC, "binary")
#'
#' iccRE(targetICC, "normal", varTotal = 100)
#' iccRE(targetICC, "normal", varWithin = 100)
#'
#' iccRE(targetICC, "gamma", disp = .5)
#'
#' iccRE(targetICC, "negBinomial", lambda = 40, disp = .5)
#' @export
#' @concept utility
iccRE <- function(ICC, dist, varTotal = NULL, varWithin = NULL, lambda = NULL, disp = NULL) {
  if (dist == "poisson") {
    if (is.null(lambda)) stop("Specify a value for lambda")

    vars <- unlist(lapply(ICC, function(x) .findPoisVar(1 / lambda * x / (1 - x))))
  } else if (dist == "binary") {
    vars <- unlist(lapply(ICC, function(x) (x / (1 - x) * (pi^2) / 3)))
  } else if (dist == "normal") {
    if (!is.null(varTotal) & !is.null(varWithin)) {
      stop("Do not specify total and within variance simultaneously")
    }

    if (is.null(varTotal) & is.null(varWithin)) {
      stop("Specify either total or within variance")
    }

    if (!is.null(varTotal)) {
      vars <- unlist(lapply(ICC, function(x) x * varTotal))
    }

    if (!is.null(varWithin)) {
      vars <- unlist(lapply(ICC, function(x) (x / (1 - x)) * varWithin))
    }
  } else if (dist == "gamma") {
    if (is.null(disp)) stop("Specify dispersion")

    vars <- unlist(lapply(ICC, function(x) (x / (1 - x)) * trigamma(1 / disp)))
  } else if (dist == "negBinomial") {
    if (is.null(disp)) stop("Specify dispersion")
    if (is.null(lambda)) stop("Specify lambda")

    vars <- unlist(lapply(ICC, function(x) (x / (1 - x)) * trigamma((1 / lambda + disp)^(-1))))
  } else {
    stop("Specify appropriate distribution")
  }

  return(vars)
}

#' Merge two data tables
#'
#' @param dt1 Name of first data.table
#' @param dt2 Name of second data.table
#' @param idvars Vector of string names to merge on
#' @return A new data table that merges dt2 with dt1
#' @examples
#' def1 <- defData(varname = "x", formula = 0, variance = 1)
#' def1 <- defData(varname = "xcat", formula = ".3;.2", dist = "categorical")
#'
#' def2 <- defData(varname = "yBin", formula = 0.5, dist = "binary", id = "xcat")
#' def2 <- defData(def2, varname = "yNorm", formula = 5, variance = 2)
#'
#' dt1 <- genData(20, def1)
#' dt2 <- genData(3, def2)
#'
#' dtMerge <- mergeData(dt1, dt2, "xcat")
#' dtMerge
#' @export
#' @concept utility
mergeData <- function(dt1, dt2, idvars) {
  oldkey <- data.table::key(dt1)

  setkeyv(dt1, idvars)
  setkeyv(dt2, idvars)

  dtmerge <- dt1[dt2]
  data.table::setkeyv(dtmerge, oldkey)

  return(dtmerge[])
}

#' Convert negative binomial mean and dispersion parameters to size and prob parameters
#'
#' @param mean The mean of a gamma distribution
#' @param dispersion The dispersion parameter of a gamma distribution
#' @return A list that includes the size and prob parameters of the neg binom
#' distribution
#' @details In simstudy, users specify the negative binomial distribution as a
#' function of two parameters - a mean and dispersion. In this case, the
#' variance of the specified distribution is mean + (mean^2)*dispersion. The
#' base R function rnbinom uses the size and prob parameters to specify the
#' negative binomial distribution. This function converts the mean and
#' dispersion into the size and probability parameters.
#' @examples
#' set.seed(12345)
#' mean <- 5
#' dispersion <- 0.5
#' sp <- negbinomGetSizeProb(mean, dispersion)
#' c(sp$size, sp$prob)
#' vec <- rnbinom(1000, size = sp$size, prob = sp$prob)
#' (estMoments <- c(mean(vec), var(vec)))
#' (theoryMoments <- c(mean, mean + mean^2 * dispersion))
#' (theoryMoments <- c(sp$size * (1 - sp$prob) / sp$prob, sp$size * (1 - sp$prob) / sp$prob^2))
#' @export
#' @concept utility
negbinomGetSizeProb <- function(mean, dispersion) {
  variance <- mean + dispersion * (mean^2)

  size <- (mean^2) / (variance - mean)
  prob <- mean / variance

  return(list(size = size, prob = prob))
}

#' Trim longitudinal data file once an event has occurred
#'
#' @param dtOld name of data table to be trimmed
#' @param seqvar string referencing column that indexes the sequence or period
#' @param eventvar string referencing event data column
#' @param idvar string referencing id column
#' @return an updated data.table removes all rows following the first event for each
#' individual
#' @examples
#' eDef <- defDataAdd(varname = "e", formula = "u==4", dist = "nonrandom")
#'
#' P <- t(matrix(c(
#'   0.4, 0.3, 0.2, 0.1,
#'   0.0, 0.4, 0.3, 0.3,
#'   0.0, 0.0, 0.5, 0.5,
#'   0.0, 0.0, 0.0, 1.0
#' ),
#' nrow = 4
#' ))
#'
#' dp <- genMarkov(
#'   n = 100, transMat = P,
#'   chainLen = 8, id = "id",
#'   pername = "period",
#'   varname = "u"
#' )
#'
#' dp <- addColumns(eDef, dp)
#' dp <- trimData(dp, seqvar = "period", eventvar = "e", idvar = "id")
#'
#' dp
#' @export
#' @concept utility
trimData <- function(dtOld, seqvar, eventvar, idvar = "id") {
  dx <- copy(dtOld)

  id <- dx[, get(idvar)]
  seq <- dx[, get(seqvar)]
  event <- dx[, get(eventvar)]

  last <- clipVec(id = id, seq = seq, event = event)

  dd <- data.table(id = dx[, unique(id)], last)
  setkeyv(dd, idvar)

  dd <- dx[dd]
  dd[get(seqvar) <= last][, last := NULL][]
}

#' @title Update definition table
#' @description Updates row definition table created by function
#' defData or defRead. (For tables created using defDataAdd
#' and defReadAdd use updateDefAdd.)
#' @param dtDefs Definition table that will be modified
#' @param changevar Name of field definition that will be changed
#' @param newformula New formula definition (defaults to NULL)
#' @param newvariance New variance specification (defaults to NULL)
#' @param newdist New distribution definition (defaults to NULL)
#' @param newlink New link specification (defaults to NULL)
#' @param remove If set to TRUE, remove definition (defaults to FALSE)
#' @return A string that represents the desired formula
#' @examples
#'
#' # Example 1
#'
#' defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
#' defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
#' defs <- defData(defs, varname = "z", formula = "4 + 3*x - 2*y", variance = 1, dist = "normal")
#'
#' defs
#'
#' updateDef(dtDefs = defs, changevar = "y", newformula = "x + 5", newvariance = 2)
#' updateDef(dtDefs = defs, changevar = "z", newdist = "poisson", newlink = "log")
#'
#' # Example 2
#'
#' defs <- defData(varname = "w", formula = 0, variance = 3, dist = "normal")
#' defs <- defData(defs, varname = "x", formula = "1 + w", variance = 1, dist = "normal")
#' defs <- defData(defs, varname = "z", formula = 4, variance = 1, dist = "normal")
#'
#' defs
#'
#' updateDef(dtDefs = defs, changevar = "x", remove = TRUE)
#' updateDef(dtDefs = defs, changevar = "z", remove = TRUE)
#' @export
#' @concept utility
#' @concept define_data
updateDef <- function(dtDefs, changevar, newformula = NULL,
                      newvariance = NULL, newdist = NULL, newlink = NULL,
                      remove = FALSE) {

  # "declares" to avoid global NOTE
  formula <- NULL
  variance <- NULL
  dist <- NULL
  link <- NULL
  varname <- NULL

  # Check args

  if (!exists(deparse(substitute(dtDefs)), envir = parent.frame())) {
    stop("Data definition does not exist.")
  }

  if (!(changevar %in% dtDefs[, varname])) {
    stop(paste("Variable", changevar, "not in definition table"))
  }

  # checks completed

  xdef <- copy(dtDefs)
  rowvar <- which(changevar == xdef$varname)

  if (!is.null(newformula)) {
    xdef[rowvar, formula := as.character(newformula)]
  }

  if (!is.null(newvariance)) {
    xdef[rowvar, variance := newvariance]
  }

  if (!is.null(newdist)) {
    xdef[rowvar, dist := newdist]
  }

  if (!is.null(newlink)) {
    xdef[rowvar, link := newlink]
  }

  if (remove) {
    xdef <- xdef[-rowvar, ]
  }

  # Check table to make sure references work after update

  if (!remove) { # check changed row

    prevVars <- xdef$varname[1:(rowvar - 1)]
    if (rowvar == 1) prevVars <- ""

    .evalDef(newvar = xdef[rowvar, varname], newform = xdef[rowvar, formula], newdist = xdef[rowvar, dist], defVars = prevVars)
  } else if (remove & (rowvar <= nrow(xdef))) { # check all rows after deleted row

    for (i in (rowvar:nrow(xdef))) {
      if (i == 1) {
        prevVars <- ""
      } else {
        prevVars <- xdef$varname[1:(i - 1)]
      }

      .evalDef(newvar = xdef[i, varname], newform = xdef[i, formula], newdist = xdef[i, dist], defVars = prevVars)
    }
  }

  return(xdef)
}

#' @title Update definition table
#' @description Updates row definition table created by functions
#' defDataAdd and defReadAdd. (For tables created using defData
#' or defRead use updateDef.)
#' @param dtDefs Definition table that will be modified
#' @param changevar Name of field definition that will be changed
#' @param newformula New formula definition (defaults to NULL)
#' @param newvariance New variance specification (defaults to NULL)
#' @param newdist New distribution definition (defaults to NULL)
#' @param newlink New link specification (defaults to NULL)
#' @param remove If set to TRUE, remove definition (defaults to FALSE)
#' @return A string that represents the desired formula
#' @examples
#'
#' # Define original data
#'
#' defs <- defData(varname = "w", formula = 0, variance = 3, dist = "normal")
#' defs <- defData(defs, varname = "x", formula = "1 + w", variance = 1, dist = "normal")
#' defs <- defData(defs, varname = "z", formula = 4, variance = 1, dist = "normal")
#'
#' # Define additional columns
#'
#' defsA <- defDataAdd(varname = "a", formula = "w + x + z", variance = 2, dist = "normal")
#'
#' set.seed(2001)
#' dt <- genData(10, defs)
#' dt <- addColumns(defsA, dt)
#' dt
#'
#' # Modify definition of additional column
#'
#' defsA <- updateDefAdd(dtDefs = defsA, changevar = "a", newformula = "w+z", newvariance = 1)
#'
#' set.seed(2001)
#' dt <- genData(10, defs)
#' dt <- addColumns(defsA, dt)
#' dt
#' @export
#' @concept utility
#' @concept define_data
updateDefAdd <- function(dtDefs, changevar, newformula = NULL,
                         newvariance = NULL, newdist = NULL, newlink = NULL,
                         remove = FALSE) {

  # "declares" to avoid global NOTE
  formula <- NULL
  variance <- NULL
  dist <- NULL
  link <- NULL
  varname <- NULL

  # Check args

  if (!exists(deparse(substitute(dtDefs)), envir = parent.frame())) {
    stop("Data definition does not exist.")
  }

  if (!(changevar %in% dtDefs[, varname])) {
    stop(paste("Variable", changevar, "not in definition table"))
  }

  # checks completed

  xdef <- copy(dtDefs)
  rowvar <- which(changevar == xdef$varname)

  if (!is.null(newformula)) {
    xdef[rowvar, formula := newformula]
  }

  if (!is.null(newvariance)) {
    xdef[rowvar, variance := newvariance]
  }

  if (!is.null(newdist)) {
    xdef[rowvar, dist := newdist]
  }

  if (!is.null(newlink)) {
    xdef[rowvar, link := newlink]
  }

  if (remove) {
    xdef <- xdef[-rowvar, ]
  }

  return(xdef)
}

#' Plot basis spline functions
#'
#' @param knots A vector of values between 0 and 1, specifying cut-points for splines
#' @param degree Integer specifying degree of curvature.
#' @return A ggplot object that contains a plot of the basis functions. In total, there
#' will be length(knots) + degree + 1 functions plotted.
#' @examples
#' knots <- c(0.25, 0.50, 0.75)
#' viewBasis(knots, degree = 1)
#'
#' knots <- c(0.25, 0.50, 0.75)
#' viewBasis(knots, degree = 2)
#'
#' knots <- c(0.25, 0.50, 0.75)
#' viewBasis(knots, degree = 3)
#' @export
#' @concept utility
#' @concept splines
viewBasis <- function(knots, degree) {

  # 'declare vars'
  value <- NULL
  basis <- NULL

  x <- seq(0, 1, length.out = 1000)
  reqparam <- length(knots) + degree + 1
  theta1 <- rep(1, reqparam)

  sdata <- .genbasisdt(x, knots, degree, theta1)

  dtbasis <- as.data.table(sdata$basis)
  dtbasis[, x := x]
  dtmelt <- data.table::melt(
    data = dtbasis, id = "x",
    variable.name = "basis",
    variable.factor = TRUE
  )

  ggplot2::ggplot(data = dtmelt, ggplot2::aes(x = x, y = value, group = basis)) +
    ggplot2::geom_line(ggplot2::aes(color = basis), size = 1) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, knots, 1)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2")
}

#' Plot spline curves
#'
#' @param knots A vector of values between 0 and 1, specifying cut-points for splines
#' @param degree Integer specifying degree of curvature.
#' @param theta A vector or matrix of values between 0 and 1. Each column of the matrix
#' represents the weights/coefficients that will be applied to the basis functions
#' determined by the knots and degree. Each column of theta represents a separate
#' spline curve.
#' @return A ggplot object that contains a plot of the spline curves. The number of
#' spline curves in the plot will equal the number of columns in the matrix (or it
#' will equal 1 if theta is a vector).
#' @examples
#' knots <- c(0.25, 0.5, 0.75)
#' theta1 <- c(0.1, 0.8, 0.4, 0.9, 0.2, 1.0)
#'
#' viewSplines(knots, degree = 2, theta1)
#'
#' theta2 <- matrix(c(
#'   0.1, 0.2, 0.4, 0.9, 0.2, 0.3,
#'   0.1, 0.3, 0.3, 0.8, 1.0, 0.9,
#'   0.1, 0.4, 0.3, 0.8, 0.7, 0.5,
#'   0.1, 0.9, 0.8, 0.2, 0.1, 0.6
#' ),
#' ncol = 4
#' )
#'
#' viewSplines(knots, degree = 2, theta2)
#' @export
#' @concept splines
#' @concept utility
viewSplines <- function(knots, degree, theta) {

  # 'declare'

  Spline <- 0
  index <- 0
  y.spline <- 0

  x <- seq(0, 1, length.out = 1000)

  matTheta <- as.matrix(theta)
  ncols <- ncol(matTheta)

  dx <- data.table()

  for (i in 1:ncols) {
    sdata <- .genbasisdt(x, knots, degree, matTheta[, i])
    dxi <- sdata$dt
    dxi[, index := i]

    dx <- rbind(dx, dxi)
  }

  dx[, Spline := factor(index)]


  p <- ggplot2::ggplot(data = dx) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y.spline, color = Spline), size = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = knots) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_color_brewer(palette = "Dark2")

  if (length(levels(factor(dx$index))) == 1) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}
