

#' Get Distributions
#'
#' @return A character vector containing the names of all valid distributions.
#' @keyword internal
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

.isFormulaScalar <- function(formula) {
  if(is.character(formula))
    if(";" %in% strsplit(formula,"")[[1]])
      res <- F
    else
      res <- is.numeric(eval(parse(text = formula)))
  else if(is.numeric(formula))
    res <- T
  else 
    res <- F
}
