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
#' @references Nakagawa, Shinichi, and Holger Schielzeth. "A general and simple 
#' method for obtaining R2 from generalized linear mixed‐effects models." 
#' Methods in ecology and evolution 4, no. 2 (2013): 133-142.
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

    vars <- unlist(lapply(ICC, function(x) .findPoisVar(x, lambda)))
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
#' and defReadAdd use updateDefAdd.) Does not modify in-place.
#' @param dtDefs Definition table that will be modified
#' @param changevar Name of field definition that will be changed
#' @param newformula New formula definition (defaults to NULL)
#' @param newvariance New variance specification (defaults to NULL)
#' @param newdist New distribution definition (defaults to NULL)
#' @param newlink New link specification (defaults to NULL)
#' @param remove If set to TRUE, remove `changevar`from
#'  definition (defaults to FALSE).
#' @return The updated data definition table.
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
#'
#' # No changes to original definition:
#' defs
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
  xdef[] <- lapply(xdef, as.character)
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
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

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
    ggplot2::geom_line(ggplot2::aes(color = basis), linewidth = 1) +
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
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

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
    ggplot2::geom_line(ggplot2::aes(x = x, y = y.spline, color = Spline), 
        linewidth = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = knots) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_color_brewer(palette = "Dark2")

  if (length(levels(factor(dx$index))) == 1) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}

#' Get survival curve parameters
#'
#' @param points A list of two-element vectors specifying the desired time and 
#' probability pairs that define the desired survival curve
#' @return A vector of parameters that define the survival curve optimized for
#' the target points. The first element of the vector represents the "f"
#' parameter and the second element represents the "shape" parameter.
#' @examples
#' points <- list(c(60, 0.90), c(100, .75), c(200, .25), c(250, .10))
#' survGetParams(points)
#' @export
#' @concept utility
survGetParams <- function(points) {
  
  assertNotMissing(points = missing(points))
  assertClass(points = points, class = "list")
  
  time <- vapply(points, function(x) x[1], FUN.VALUE = numeric(1))
  p <- vapply(points, function(x) x[2],  FUN.VALUE = numeric(1))
  
  assertPositive(time)
  assertAscending(time)
  
  assertProbability(p)  
  assertDescending(p)
  
  loss_surv <- function(params, points) {
    
    loss <- function(a, params) {
      ( params[2]*(log(-log(a[2])) - params[1]) - log(a[1]) ) ^ 2
    }
    
    sum(vapply(points, function(a) loss(a, params), FUN.VALUE = numeric(1)))
    
  }
  
  optim_results <- stats::optim(
    par = c(1, 1), 
    fn = loss_surv, 
    points = points,
    method = "L-BFGS-B", 
    lower = c(-Inf, 0),
    upper = c(Inf, Inf)
  )
  
  if (optim_results$convergence !=0) stop("Optimization did not converge")
  
  return(optim_results$par)
}

#' Plot survival curves
#' 
#' @param formula This is the "formula" parameter of the Weibull-based survival curve
#' that can be used to define the scale of the distribution.
#' @param shape The parameter that defines the shape of the distribution.
#' @param points An optional list of two-element vectors specifying the desired 
#' time and probability pairs that define the desired survival curve. If no list
#' is specified then the plot will not include any points.
#' @param n The number of points along the curve that will be used to 
#' define the line. Defaults to 100.
#' @param scale An optional scale parameter that defaults to 1. If the value is 
#' 1, the scale of the distribution is determined entirely by the argument "f".
#' @param limits A vector of length 2 that specifies x-axis limits for the plot. 
#' The default is NULL, in which case no limits are imposed.
#' @return A ggplot of the survival curve defined by the specified parameters.
#' If the argument points is specified, the plot will include them
#' @examples
#' points <- list(c(60, 0.90), c(100, .75), c(200, .25), c(250, .10))
#' r <- survGetParams(points)
#' survParamPlot(r[1], r[2])
#' survParamPlot(r[1], r[2], points = points)
#' survParamPlot(r[1], r[2], points = points, limits = c(0, 100))
#' @export
#' @concept utility
survParamPlot <- function(formula, shape, points = NULL, n = 100, scale = 1, 
                          limits = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  assertNotMissing(formula = missing(formula), shape = missing(shape))
  assertPositive(shape)
  
  # "declares" to avoid global NOTE
  
  V1 <- NULL
  V2 <- NULL
  
  ###
  
  u <- seq(1, 0.001, length = n)
  
  dd <- data.table::data.table(
    formula = formula,
    scale = scale,
    shape = shape,
    T = (-(log(u)*scale/exp(formula)))^(shape),
    p = round(1 - cumsum(rep(1/length(u), length(u))), 3)
  )
  
  if (!is.null(limits)) {
    dd <- dd[ ( ( T >= limits[1] ) & ( T <= limits[2] ) ) ]  
  }
  
  p <- ggplot2::ggplot(data = dd, ggplot2::aes(x = T, y = p)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_y_continuous(limits = c(0,1), name = "probability of survival") +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 7.5),
                   axis.title = ggplot2::element_text(size = 8, face = "bold")
    )
  
  if (!is.null(limits)) {
    p <- p + ggplot2::scale_x_continuous(name = "time", limits = limits)
  } else {
    p <- p + ggplot2::scale_x_continuous(name = "time") 
  }
  
  if (!is.null(points)) {
    
    dpoints <- data.table::as.data.table(do.call(rbind, points))  
    
    if (!is.null(limits)) {
      dpoints <- dpoints[ ( ( V1 >= limits[1] ) & ( V1 <= limits[2] ) ) ]  
    }
    
    return(
      p +     
        ggplot2::geom_point(data = dpoints, ggplot2::aes(x = V1, y = V2), 
                            pch = 21, fill = "#DCC949", size = 2.5) 
    )
    
  } else return(p)
  
}

#' Generating single competing risk survival variable
#' 
#' @param dtName Name of complete data set to be updated
#' @param events Vector of column names that include
#' time-to-event outcome measures
#' @param timeName A string to indicate the name of the combined competing risk
#' time-to-event outcome that reflects the minimum observed value of all 
#' time-to-event outcomes.
#' @param censorName The name of a time-to-event variable that is the censoring
#' variable. Must be one of the "events" names. Defaults to NULL.
#' @param eventName The name of the new numeric/integer column representing the
#' competing event outcomes. If censorName is specified, the integer value for
#' that event will be 0. Defaults to "event", but will be ignored 
#' if timeName is NULL.
#' @param typeName The name of the new character column that will indicate the
#' event type. The type will be the unique variable names in survDefs. Defaults
#' to "type", but will be ignored if timeName is NULL.
#' @param keepEvents Indicator to retain original "events" columns. Defaults
#' to FALSE.
#' @param idName Name of id field in existing data set.
#' @return An updated data table
#' @examples
#' d1 <- defData(varname = "x1", formula = .5, dist = "binary")
#' d1 <- defData(d1, "x2", .5, dist = "binary")
#' 
#' dS <- defSurv(varname = "reinc", formula = "-10 - 0.6*x1 + 0.4*x2", shape = 0.3)
#' dS <- defSurv(dS, "death", "-6.5 + 0.3*x1 - 0.5*x2", shape = 0.5)
#' dS <- defSurv(dS, "censor", "-7", shape = 0.55)
#' 
#' dd <- genData(10, d1)
#' dd <- genSurv(dd, dS)
#' 
#' addCompRisk(dd, c("reinc","death", "censor"), timeName = "time",
#'    censorName = "censor", keepEvents = FALSE)
#' 
#' @export
#' @concept utility
addCompRisk <- function(dtName, events, timeName, 
  censorName = NULL, eventName = "event", typeName = "type",
  keepEvents = FALSE, idName = "id") {
  
  assertNotMissing(
    dtName = missing(dtName),
    events = missing(events),
    timeName = missing(timeName),
    call = sys.call(-1)
  )
  assertAtLeastLength(events = events, length = 2)
  assertInDataTable(events, dtName)
  assertInDataTable(idName, dtName)
  if (!is.null(censorName)) {assertInDataTable(censorName, dtName)}
  assertNotInDataTable(vars = c(eventName, typeName), dtName)
  if (keepEvents == TRUE) assertNotInDataTable(vars = timeName, dtName)
  
  # 'declare'
  ..temp_time <- NULL
  ..temp_event <- NULL
  ..temp_type <- NULL
  id <- NULL

  dtSurv <- copy(dtName)
  setnames(dtSurv, idName, "id")
  
  dtSurv[, ..temp_time := min(sapply(1:length(events), 
              function(a) get(events[a]))), keyby = id]
  dtSurv[, ..temp_event := which.min(sapply(1:length(events), 
              function(a) get(events[a]))), keyby = id]
  dtSurv[, ..temp_type := events[..temp_event], keyby = id]
  
  if (! keepEvents) {
    for (i in seq_along(events)) { dtSurv[, events[i] := NULL] }  
  }
  
  if (!is.null(censorName)) {
    dtSurv[..temp_type == censorName, ..temp_event := 0 ]
    dtSurv[, ..temp_event := as.numeric(factor(..temp_event)) - 1]
  }
  
  data.table::setnames(
    dtSurv, 
    c("..temp_time", "..temp_event", "..temp_type"),
    c(timeName, eventName, typeName)
  )
  
  setnames(dtSurv, "id", idName)
  dtSurv[]
}

#' Determine intercept, treatment/exposure and covariate coefficients that can 
#' be used for binary data generation with a logit link and a set of covariates
#' @description  This is an implementation of an iterative bisection procedure 
#' that can be used to determine coefficient values for a target population 
#' prevalence as well as a target risk ratio, risk difference, or AUC. These 
#' coefficients can be used in a subsequent data generation process to simulate
#' data with these desire characteristics.
#' @param defCovar A definition table for the covariates in the underlying
#' population. This tables specifies the distribution of the covariates.
#' @param coefs A vector of coefficients that reflect the relationship between 
#' each of the covariates and the log-odds of the outcome.
#' @param popPrev The target population prevalence of the outcome. 
#' A value between 0 and 1.
#' @param rr The target risk ratio, which must be a value between 0 and
#' 1/popPrev. Defaults to NULL.
#' @param rd The target risk difference, which must be between
#' -(popPrev) and (1 - popPrev). Defaults to NULL
#' @param auc The target AUC, which must be a value between 0.5 and 1.0 . 
#' Defaults to NULL.
#' @param tolerance The minimum stopping distance between the adjusted low and high
#' endpoints. Defaults to 0.001.
#' @param sampleSize The number of units to generate for the bisection algorithm. 
#' The default is 1e+05. To get a reliable estimate, the value 
#' should be no smaller than the default, though larger values can be used, though
#' computing time will increase.
#' @param trtName If either a risk ratio or risk difference is the target statistic,
#' a treatment/exposure variable name can be provided. Defaults to "A".
#' @details If no specific target statistic is specified, then only the intercept
#' is returned along with the original coefficients. Only one target statistic (risk ratio, risk
#' difference or AUC) can be specified with a single function call; in all three cases, a target
#' prevalence is still required.
#' @references Austin, Peter C. "The iterative bisection procedure: a useful 
#' tool for determining parameter values in data-generating processes in 
#' Monte Carlo simulations." BMC Medical Research Methodology 23, 
#' no. 1 (2023): 1-10.
#' @return A vector of parameters including the intercept and covariate 
#' coefficients for the logistic model data generating process.
#' @examples
#' \dontrun{
#' d1 <- defData(varname = "x1", formula = 0, variance = 1)
#' d1 <- defData(d1, varname = "b1", formula = 0.5, dist = "binary")
#' 
#' coefs <- log(c(1.2, 0.8))
#'
#' logisticCoefs(d1, coefs, popPrev = 0.20) 
#' logisticCoefs(d1, coefs, popPrev = 0.20, rr = 1.50, trtName = "rx") 
#' logisticCoefs(d1, coefs, popPrev = 0.20, rd = 0.30, trtName = "rx")
#' logisticCoefs(d1, coefs, popPrev = 0.20, auc = 0.80)
#' }
#' @export
#' @concept utility
#' 
logisticCoefs <- function(defCovar, coefs, popPrev, rr = NULL, rd = NULL, 
  auc = NULL, tolerance = 0.001, sampleSize = 1e+05, trtName = "A") {
  
  ### "initialize" variables 
  
  varname <- NULL
  y <- NULL
  py <- NULL
  
  ###
  
  num_notNull <- sum(sapply(list(rr, rd, auc), function(x) !is.null(x)))
  
  if ( num_notNull > 1) {
    stop("Specify only one target statistic")
  }
  
  assertNotMissing(popPrev = missing(popPrev), 
                   defCovar = missing(defCovar), 
                   coefs = missing(coefs))
  
  assertLength(coefs = coefs, length = nrow(defCovar))

  if (!is.null(rr)) {
    assertAtLeast(rr = rr, minVal = 0)
    if (rr > 1/popPrev) {
      stop(paste("rr is", rr, "but must be at most", formatC(1/popPrev, digits = 3), "(1/popPrev)"))
    }
  } 
  
  if (!is.null(rd)) { 
    assertInRange(rd = rd, range = c(-popPrev, 1-popPrev))
  }
  
  if (!is.null(auc)) {
    assertInRange(auc = auc, range = c(0.5, 1))
  } 
    
  assertInRange(popPrev = popPrev, range = c(0, 1))
  assertNumeric(coefs = coefs)
  
  ####
  
  if (num_notNull == 0) targetStat <- "prev"
  if (!is.null(rr)) targetStat <- "rr"
  if (!is.null(rd)) targetStat <- "rd"
  if (!is.null(auc)) targetStat <- "auc"
  
  # Get any double dot vars from calling environment into this environment
  
  .formvars <- all.vars(parse(text = defCovar$formula))
  .dotVars <- .formvars[startsWith(.formvars, "..")]
  .vars <- gsub("^\\.{2}", "", .dotVars)
  
  for (i in seq_along(.vars)) {
    assign(.vars[i], get(.vars[i], envir = parent.frame()))  
  }
  
  #
  
  dd <- genData(sampleSize, defCovar)
  
  if (targetStat == "prev") {
    
    B0 <- getBeta0(lvec = as.matrix(dd[, -1]) %*% coefs, popPrev, tolerance)
    B <- c(B0, coefs)
    names(B) <- c("B0", defCovar$varname)
    
  } else if (targetStat %in% c("rr", "rd")) {
    
    if (targetStat == "rr") {
      statValue <- rr
    } else {
      statValue <- rd
    }
    
    B0 <- getBeta0(lvec = as.matrix(dd[, -1]) %*% coefs, popPrev, tolerance)
    
    lvec <- cbind(1, as.matrix(dd[, -1])) %*% c(B0, coefs)
    
    intLow <- -10
    intHigh <- 10
    
    while(abs(intHigh - intLow) > tolerance){
      
      Delta <- (intLow + intHigh)/2
      
      if (targetStat == "rr") {
        rStat <- mean(stats::plogis(lvec + Delta)) / mean(stats::plogis(lvec))
      } else {
        rStat <- mean(stats::plogis(lvec + Delta)) - mean(stats::plogis(lvec))
      }
      
      if (rStat < statValue) {
        intLow <- Delta
      } else {
        intHigh <- Delta
      }
      
    }
    
    Delta <-  (intLow + intHigh)/2
    
    B <- c(B0, Delta, coefs)
    names(B) <- c("B0", trtName, defCovar$varname)
    
  } else if (targetStat == "auc") {
    
    dmatrix <- as.matrix(dd[, -1])
    results <- getBeta_auc(dmatrix, coefs, auc = auc, popPrev = popPrev, tolerance = tolerance)
    
    B <- c(results[1], results[2]*coefs)
    names(B) <- c("B0", defCovar$varname)
    
  }
  
  return(B)
  
}

