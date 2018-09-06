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
#' theta1 = c(0.1, 0.8, 0.4, 0.9, 0.2, 1.0)
#'
#' viewSplines(knots, degree = 2, theta1)
#'
#' theta2 = matrix(c(0.1, 0.2, 0.4, 0.9, 0.2, 0.3,
#'                   0.1, 0.3, 0.3, 0.8, 1.0, 0.9,
#'                   0.1, 0.4, 0.3, 0.8, 0.7, 0.5,
#'                   0.1, 0.9, 0.8, 0.2, 0.1, 0.6),
#'                ncol = 4)
#'
#' viewSplines(knots, degree = 2, theta2)
#' @export
#'

viewSplines <- function(knots, degree, theta) {

  # 'declare'

  Spline <- 0
  index <- 0
  y.spline <- 0

  #

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
    ggplot2::scale_color_brewer(palette="Dark2")

  if (length(levels(factor(dx$index))) == 1) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)

}
