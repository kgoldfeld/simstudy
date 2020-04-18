

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

#' Adapted from:
#' \url{https://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
#'
#' @param x 
#' @param allow_reserved 
#' @param unique 
#'
#' @noRd
is_valid_variable_name <- function(x, allow_reserved = FALSE, unique = FALSE) 
{
  ok <- rep.int(TRUE, length(x))
  
  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE
  
  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE    
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE  
  }
  
  #are names valid (and maybe unique)
  ok[x != make.names(x, unique = unique)] <- FALSE
  
  ok
}
