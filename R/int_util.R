

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
