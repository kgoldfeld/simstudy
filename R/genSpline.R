#' Generate spline curves
#'
#' @param dt data.table that will be modified
#' @param newvar Name of new variable to be created
#' @param predictor Name of field in old data.table that is predicting new value
#' @param theta A vector or matrix of values between 0 and 1. Each column of the matrix
#' represents the weights/coefficients that will be applied to the basis functions
#' determined by the knots and degree. Each column of theta represents a separate
#' spline curve.
#' @param knots A vector of values between 0 and 1, specifying quantile
#' cut-points for splines. Defaults to c(0.25, 0.50, 0.75).
#' @param degree Integer specifying polynomial degree of curvature.
#' @param newrange Range of the spline function , specified as a string
#' with two values separated by a semi-colon. The first value represents the
#' minimum, and the second value represents the maximum. Defaults to NULL, which
#' sets the range to be between 0 and 1.
#' @param noise.var Add to normally distributed noise to observation - where mean
#' is value of spline curve.
#' @return A modified data.table with an added column named newvar.
#' @examples
#' ddef <- defData(varname = "age", formula = "0;1", dist = "uniform")
#'
#' theta1 = c(0.1, 0.8, 0.6, 0.4, 0.6, 0.9, 0.9)
#' knots <- c(0.25, 0.5, 0.75)
#'
#' viewSplines(knots = knots, theta = theta1, degree = 3)
#'
#' set.seed(234)
#' dt <- genData(1000, ddef)
#'
#' dt <- genSpline(dt = dt, newvar = "weight",
#'                 predictor = "age", theta = theta1,
#'                 knots = knots, degree = 3,
#'                 noise.var = .025)
#'
#' dt
#'
#' @export
#'
genSpline <- function(dt, newvar, predictor, theta,
                      knots = c(0.25, 0.50, 0.75), degree = 3,
                      newrange = NULL, noise.var = 0) {

  # "declares" to avoid global NOTE

  y.spline <- NULL

  # Check arguments

  if (!exists(deparse(substitute(dt)),  envir = parent.frame())) {
    stop("Data table does not exist.")
  }

  if (!predictor %in% names(dt)) {
    stop(paste0("Variable ", predictor, " not in data.table"))
  }


  if (!is.character(newvar)) {
    stop("newvar must be a string")
  }

  if (!(is.null(newrange))) {

    rangestr <- unlist(strsplit(as.character(newrange), split = ";", fixed = TRUE))
    rangenum <- as.numeric(rangestr)

    if (length(rangenum) != 2) {
      stop("Range not specified as two values")
    }

    if ( !(all (!is.na(rangenum)) ) ) {
      stop("Non-numbers entered in range")
    }

    newmin <- min(rangenum)
    newmax <- max(rangenum)

  }

  ### All checks passed

  x <- dt[ , predictor, with = FALSE][[1]]

  if ( !(min(x) >= 0 & max(x) <= 1)) {

    x.normalize <- ( x - min(x) ) / ( max(x) - min(x) )

  } else {

    x.normalize <- copy(x)

  }

  qknots <- stats::quantile(x = x.normalize, probs = knots)

  sdata <- .genbasisdt(x.normalize, qknots, degree, theta) # Call internal function

  if (is.null(newrange)) {

    newy <- sdata$dt[, y.spline]

  } else {

    newy <- sdata$dt[, y.spline *(newmax - newmin) + newmin]    # de-normalize

  }

  newy <- stats::rnorm(length(newy), newy, sqrt(noise.var))   # add noise

  dt[, newy := newy]
  data.table::setnames(dt, "newy", newvar)

  return(dt[])

}
