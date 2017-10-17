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

  x <- seq(0, 1, length.out = 1000)
  reqparam <- length(knots) + degree + 1
  theta1 <- rep(1, reqparam)

  sdata <- genbasisdt(x, knots, degree, theta1)

  dtbasis <- as.data.table(sdata$basis)
  dtbasis[, x := x]
  dtmelt <- data.table::melt(data = dtbasis, id = "x",
                             variable.name = "basis",
                             variable.factor = TRUE)

  ggplot2::ggplot(data=dtmelt, aes(x = x, y = value, group = basis)) +
    geom_line(aes(color = basis), size = 1) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, 1),
                       breaks = c(0, knots, 1)) +
    theme(panel.grid.minor = element_blank()) +
    scale_color_brewer(palette = "Dark2")

}
