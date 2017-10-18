#### genbasisdt ####

# Internal function called by viewSplines, viewBasis,
# and genSplines - function returns a list object
# @param x The support of the function
# @param knots The cutpoints of the spline function
# @param degree The polynomial degree of curvature
# @param theta A vector of coefficents/weights
# @return A list that includes a data.table with x and y values,
# a matrix of basis values (basis functions), the knots, and degree


genbasisdt <- function(x, knots, degree, theta) {

  ### check arguments

  reqparam <- length(knots) + degree + 1
  if (length(theta) != reqparam) {
    stop(paste0("Number of specified paramaters (theta) not correct. Needs to be ",
                reqparam, "."),call. = FALSE)
  }

  if ( ! all(theta <= 1) ) {

    if (sum(theta>1) == 1) valueS <- " value of theta exceeds 1.00"
    else valueS <- " values of theta exceed 1.00"

    stop(paste0(sum(theta>1), valueS))
  }

  if ( !is.null(knots) & !all(knots < 1) & !all(knots > 0) ) {

    stop("All knots must be between 0 and 1")

  }

  ###

  basis <- splines::bs(x = x, knots = knots, degree = degree,
              Boundary.knots = c(0,1), intercept = TRUE)

  y.spline <- basis %*% theta

  dt <- data.table(x, y.spline = as.vector(y.spline))

  return(list(dt = dt, basis = basis, knots = knots, degree = degree))

}
