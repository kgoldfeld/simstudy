#' Plot basis spline functions
#'
#' @param knots A vector of values between 0 and 1, specifying cut-points for splines
#' @param degree Integer specifying degree of curvature.
#' @return A ggplot object that contains a plot of the basis functions. In total, there
#' will be length(knots) + degree + 1 functions plotted.
#' @examples
#' knots <- c(0.25, 0.50, 0.75 )
#' viewBasis(knots, degree = 1)
#'
#' knots <- c(0.25, 0.50, 0.75 )
#' viewBasis(knots, degree = 2)
#'
#' knots <- c(0.25, 0.50, 0.75 )
#' viewBasis(knots, degree = 3)
#' @export
#'

viewBasis <- function(knots, degree) {

  # 'declare vars'

  value <- NULL
  basis <- NULL

  #

  x <- seq(0, 1, length.out = 1000)
  reqparam <- length(knots) + degree + 1
  theta1 <- rep(1, reqparam)

  sdata <- .genbasisdt(x, knots, degree, theta1)

  dtbasis <- as.data.table(sdata$basis)
  dtbasis[, x := x]
  dtmelt <- data.table::melt(data = dtbasis, id = "x",
                             variable.name = "basis",
                             variable.factor = TRUE)

  ggplot2::ggplot(data=dtmelt, ggplot2::aes(x = x, y = value, group = basis)) +
    ggplot2::geom_line(ggplot2::aes(color = basis), size = 1) +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(limits = c(0, 1),
                       breaks = c(0, knots, 1)) +
    ggplot2::scale_color_brewer(palette = "Dark2")

}
