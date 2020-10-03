#' Internal function called by gendt
#'
#' @useDynLib simstudy
#' @importFrom Rcpp sourceCpp
#'
#' @param args One row from data definitions data.table
#' @param n The number of observations required in the data set
#' @param dt Incomplete simulated data.table
#' @param envir Environment the data definitions are evaluated in.
#'  Defaults to [base::parent.frame].
#' @return A data.frame with the updated simulated data
#' @noRd
.generate <- function(args, n, dfSim, idname, envir = parent.frame()) {
  newColumn <- switch(args$dist,
    beta = .genbeta(
      n = n,
      formula = args$formula,
      precision = args$variance,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    binary = {
      args$variance <- 1
      .genbinom(
        n = n,
        formula = args$formula,
        size = args$variance,
        link = args$link,
        dtSim = dfSim,
        envir = envir
      )
    },
    binomial = .genbinom(
      n = n,
      formula = args$formula,
      size = args$variance,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    categorical = .gencat(
      n = n,
      formula = args$formula,
      link = args$link,
      dfSim = dfSim,
      envir = envir
    ),
    exponential = .genexp(
      n = n,
      formula = args$formula,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    gamma = .gengamma(
      n = n,
      formula = args$formula,
      dispersion = args$variance,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    mixture = .genmixture(
      n = n,
      formula = args$formula,
      dtSim = dfSim,
      envir = envir
    ),
    negBinomial = .gennegbinom(
      n = n,
      formula = args$formula,
      dispersion = args$variance,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    nonrandom = .gendeterm(
      n = n,
      formula = args$formula,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    normal = .gennorm(
      n = n,
      formula = args$formula,
      variance = args$variance,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    noZeroPoisson = .genpoisTrunc(
      n = n,
      formula = args$formula,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    poisson = .genpois(
      n = n,
      formula = args$formula,
      link = args$link,
      dtSim = dfSim,
      envir = envir
    ),
    uniform = .genunif(
      n = n,
      formula = args$formula,
      dtSim = dfSim,
      envir = envir
    ),
    uniformInt = .genUnifInt(
      n = n,
      formula = args$formula,
      dtSim = dfSim,
      envir = envir
    ),
    default = stop(
      paste(args$dist, "not a valid distribution. Please check spelling."),
      call. = FALSE
    )
  )

  # Create data frame
  if (is.null(dfSim)) {
    dfNew <- data.frame(newColumn)
  } else {
    dfNew <- cbind(dfSim, newColumn)
  }

  names(dfNew)[ncol(dfNew)] <- as.character(args$varname)

  return(dfNew)
}

# Internal function called by viewSplines, viewBasis,
# and genSplines - function returns a list object
# @param x The support of the function
# @param knots The cutpoints of the spline function
# @param degree The polynomial degree of curvature
# @param theta A vector of coefficents/weights
# @return A list that includes a data.table with x and y values,
# a matrix of basis values (basis functions), the knots, and degree

.genbasisdt <- function(x, knots, degree, theta) {

  ### check arguments

  reqparam <- length(knots) + degree + 1
  if (length(theta) != reqparam) {
    stop(paste0(
      "Number of specified paramaters (theta) not correct. Needs to be ",
      reqparam, "."
    ), call. = FALSE)
  }

  if (!all(theta <= 1)) {
    if (sum(theta > 1) == 1) {
      valueS <- " value of theta exceeds 1.00"
    } else {
      valueS <- " values of theta exceed 1.00"
    }

    stop(paste0(sum(theta > 1), valueS))
  }

  if (!is.null(knots) & !all(knots < 1) & !all(knots > 0)) {
    stop("All knots must be between 0 and 1")
  }

  basis <- splines::bs(
    x = x, knots = knots, degree = degree,
    Boundary.knots = c(0, 1), intercept = TRUE
  )

  y.spline <- basis %*% theta

  dt <- data.table(x, y.spline = as.vector(y.spline))

  return(list(dt = dt, basis = basis, knots = knots, degree = degree))
}

# Internal function called by .generate - returns exp data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean (lambda)
# @return A data.frame column with the updated simulated data

.getBetaMean <- function(dtSim, formula, link, n = nrow(dtSim)) {
  mean <- .evalWith(formula, .parseDotVars(formula), dtSim, n)
  if (link == "logit") {
    mean <- 1 / (1 + exp(-mean))
  }

  return(mean)
}

# TODO document internal functions
.genbeta <- function(n, formula, precision, link = "identity", dtSim, envir) {
  mean <- .getBetaMean(dtSim, formula, link, n)

  d <- .evalWith(precision, .parseDotVars(precision, envir), dtSim, n)

  sr <- betaGetShapes(mean = mean, precision = d)
  new <- stats::rbeta(n, shape = sr$shape1, shape2 = sr$shape2)

  return(new)
}

# Internal function called by .generate - returns Binary data
#
# @param n The number of observations required in the data set
# @param forumula String that specifies the formula for the probability
# @param link Link function: Identity or Logit
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.getBinaryMean <- function(dtSim,
                           formula,
                           size,
                           link,
                           n = nrow(dtSim),
                           envir = parent.frame()) {
  size <- .evalWith(size, .parseDotVars(size, envir), dtSim, n)
  p <- .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)

  if (link == "logit") {
    p <- 1 / (1 + exp(-p))
  }

  return(list(p, size))
}

.genbinom <- function(n, formula, size, link, dtSim, envir) {
  params <- .getBinaryMean(
    dtSim = dtSim,
    formula = formula,
    size = size,
    link = link,
    n = n,
    envir = envir
  )

  return(stats::rbinom(n, params[[2]], params[[1]]))
}

# Internal function called by .generate - returns categorical data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities, each separated by ";"
# @param dfSim Incomplete simulated data set
# @param idkey Key of incomplete data set
# @param envir Environment the data definitions are evaluated in.
#  Defaults to [base::parent.frame].
# @return A data.frame column with the updated simulated data
.gencat <- function(n, formula, link, dfSim, envir) {
  formulas <- .splitFormula(formula)

  if (length(formulas) < 2) {
    stop(paste0(
      "The formula for 'categorical' must contain atleast",
      " two probabilities."
    ))
  }

  parsedProbs <-
    .evalWith(formulas, .parseDotVars(formulas, envir), dfSim, n)

  if (link == "logit") {
    parsedProbs <- exp(parsedProbs)
    parsedProbs <- parsedProbs / (1 + rowSums(parsedProbs))
  } else {
    parsedProbs <- .adjustProbs(parsedProbs)
  }

  parsedProbs <- cbind(parsedProbs, 1 - rowSums(parsedProbs))

  .Call(`_simstudy_matMultinom`, parsedProbs, PACKAGE = "simstudy")
}

# Internal function called by .generate - returns non-random data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.gendeterm <- function(n, formula, link, dtSim, envir) {
  new <- .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)

  if (link == "log") {
    new <- exp(new)
  } else if (link == "logit") new <- 1 / (1 + exp(-new))

  new
}

# Internal function called by .generate - returns exp data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean (lambda)
# @return A data.frame column with the updated simulated data
.genexp <- function(n, formula, link = "identity", dtSim, envir) {
  mean <- .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)
  if (link == "log") {
    mean <- exp(mean)
  }
  new <- stats::rexp(n, rate = 1 / mean)

  return(new)
}


# Internal function called by .generate - returns gamma data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities
# @return A data.frame column with the updated simulated data

.getGammaMean <- function(dtSim, formula, link, n = nrow(dtSim), envir) {
  mean <- .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)
  if (link == "log") {
    mean <- exp(mean)
  }

  return(mean)
}

.gengamma <- function(n, formula, dispersion, link = "identity", dtSim, envir) {
  mean <- .getGammaMean(dtSim, formula, link, n, envir)
  d <- .evalWith(dispersion, .parseDotVars(dispersion, envir), dtSim, n)

  sr <- gammaGetShapeRate(mean = mean, dispersion = d)
  new <- stats::rgamma(n, shape = sr$shape, rate = sr$rate)

  return(new)
}

.genmixture <- function(n, formula, dtSim, envir) {
  origFormula <- formula
  formula <- .rmWS(formula)
  var_pr <- strsplit(formula, "+", fixed = T)
  var_dt <- strsplit(var_pr[[1]], "|", fixed = T)
  formDT <- as.data.table(do.call(rbind, var_dt))
  ps <-
    cumsum(.evalWith(unlist(formDT[, 2]), .parseDotVars(formDT[, 2], envir)))

  if (!isTRUE(all.equal(max(ps), 1))) {
    valueError(origFormula,
      msg = list(
        "Probabilities in mixture",
        " formula: '{names}' have to sum to 1!"
      ),
      call = NULL
    )
  }

  conditions <- paste0("(interval==", 1:nrow(formDT[, 1]), ")")
  f1 <- paste(unlist(formDT[, 1]), conditions, sep = "*")
  interval_formula <- paste(f1, collapse = "+")

  dvars <- .parseDotVars(formula)

  u <- stats::runif(n)
  dvars$interval <- findInterval(u, ps, rightmost.closed = TRUE) + 1

  .evalWith(interval_formula, dvars, dtSim, n)
}

# Internal function called by .generate - returns negative binomial data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the mean
# @return A data.frame column with the updated simulated data

.getNBmean <- function(dtSim, formula, link, n = nrow(dtSim), envir = parent.frame()) {
  mean <- .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)
  if (link == "log") {
    mean <- exp(mean)
  }

  return(mean)
}

.gennegbinom <- function(n, formula, dispersion, link = "identity", dtSim, envir) {
  mean <- .getNBmean(dtSim, formula, link, n, envir)
  d <- as.numeric(as.character(dispersion))

  sp <- negbinomGetSizeProb(mean = mean, dispersion = d)
  new <- stats::rnbinom(n, size = sp$size, prob = sp$prob)

  return(new)
}

# Internal function called by .generate - returns Normal data
#
# @param n The number of observations required in the data set
# @param forumula String that specifies the formula for the mean
# @param variance Variance of the normal distribution
# @param link Link function not used in normal distribution
# @param dtSim Incomplete simulated data.table
# @return A data.frame column  with the updated simulated data

.getNormalMean <- function(dtSim,
                           formula,
                           n = nrow(dtSim),
                           envir = parent.frame()) {
  .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)
}

.gennorm <- function(n, formula, variance, link, dtSim, envir) {
  mean <- .getNormalMean(dtSim, formula, n, envir)
  v <- .evalWith(variance, .parseDotVars(variance, envir), dtSim, n)

  return(stats::rnorm(n, mean, sqrt(v)))
}

# Internal function called by .generate - returns Poisson count data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param link Link function: Identity or Log
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data

.getPoissonMean <- function(dtSim, formula, link, n = nrow(dtSim), envir = parent.frame()) {
  mean <- .evalWith(formula, .parseDotVars(formula, envir), dtSim, n)

  if (link == "log") {
    mean <- exp(mean)
  }

  return(mean)
}

.genpois <- function(n, formula, link, dtSim, envir) {
  mean <- .getPoissonMean(dtSim, formula, link, n, envir)

  return(stats::rpois(n, mean))
}

# Internal function called by .generate - returns Poisson count data
# truncated at 1
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mean
# @param link Link function: Identity or Log
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data

.genpoisTrunc <- function(n, formula, link, dtSim, envir) {
  mean <- .getPoissonMean(dtSim, formula, link, n, envir)

  u <- stats::runif(n, min = 0, max = 1)

  x <- stats::qpois(stats::ppois(0, lambda = mean) +
    u * (stats::ppois(Inf, lambda = mean) -
      stats::ppois(0, lambda = mean)), lambda = mean)

  return(x)
}

# Internal function called by .generate - returns Uniform data
#
# @param n The number of observations required in the data set
# @param formula Two formulas (separated by ;) for min and max
# @param dtSim Incomplete simulated data set
# @return A data.frame column  with the updated simulated data

.genunif <- function(n, formula, dtSim, envir) {
  if (!is.null(dtSim) && n != nrow(dtSim)) {
    stop("Length mismatch between 'n' and 'dtSim'")
  }

  range <- .parseUnifFormula(formula, dtSim, n, envir)

  return(stats::runif(n, range$min, range$max))
}

.parseUnifFormula <- function(formula, dtSim, n, envir) {
  range <- .splitFormula(formula)

  if (length(range) != 2) {
    stop(
      paste(
        "Formula for unifrom distributions must have",
        "the format: 'min;max'. See ?distributions"
      )
    )
  }

  parsedRange <- .evalWith(range, .parseDotVars(range, envir), dtSim, n)

  r_min <- parsedRange[, 1]
  r_max <- parsedRange[, 2]


  if (any(r_min == r_max)) {
    warning(
      paste0(
        "Formula warning: ",
        "'min' and 'max' are equal in ",
        sum(r_min == r_max),
        " rows.",
        "This results in all Data being the same!"
      )
    )
  }

  if (any(r_max < r_min)) {
    stop(paste0("Formula invalid: 'max' < 'min' in ", r_max < r_min, " rows."))
  }

  list(min = r_min, max = r_max)
}

# Internal function called by .generate - returns Uniform integer data
#
# @param n The number of observations required in the data set
# @param formula Two formulas (separated by ;) for min and max
# @param dtSim Incomplete simulated data set
# @return A data.frame column  with the updated simulated data

.genUnifInt <- function(n, formula, dtSim, envir) {
  range <- .parseUnifFormula(formula, dtSim, n, envir)

  if (any(!sapply(range, function(x) floor(x) == x))) {
    stop(paste(
      "For 'uniformInt' min and max must be integers,",
      "did you mean to use 'uniform'?"
    ))
  }

  unifCont <- stats::runif(n, range$min, range$max + 1)

  return(as.integer(floor(unifCont)))
}