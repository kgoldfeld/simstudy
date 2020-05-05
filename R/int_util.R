.parseDotVars <- function(formula) {
  vars <- all.vars(parse(text = formula))
  dotVars <- startsWith(vars,"..")
  varValues <- mget(sub("..","", vars[dotVars]),envir = .GlobalEnv)
  names(varValues) <- vars[dotVars]
  varValues
}

.evalWith <- function(formula,extVars,dtSim,n = nrow(dtSim)) {
  if(missing(dtSim) && missing(n)) n <- 1
  
  e <- list2env(extVars)
  
  if(!is.null(dtSim))
    e <- list2env(dtSim,e)
  
  if(!is.null(e$formula2parse)) 
    stop("'formula2parse' is a reserved variable name!")
  
  evalFormula <- function(x) {
    e$formula2parse <- x
    res <- with(e, eval(parse(text = formula2parse)))
    
    if (length(res) == 1)
      rep(res, n)
    else
      res
  }
  parsedValues <- sapply(formula,evalFormula)
  
  if(!is.matrix(parsedValues))
    t(parsedValues)
  else
    parsedValues
}

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

parseT <- function(txt) parse(text = txt)

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

