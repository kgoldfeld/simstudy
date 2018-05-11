#' Generate multi-factorial data
#'
#' @param nFactors Number of factors (columns) to generate.
#' @param each Number of replications for each combination of factors. Must be specified.
#' @param levels Vector or scalar. If a vector is specified, it must be
#' the same length as nFatctors. Each value of the vector represents the
#' number of levels of each corresponding factor. If a scalar is specified,
#' each factor will have the same number of levels. The default is 2 levels
#' for each factor.
#' @param coding String value to specify if "dummy" or "effect" coding is used.
#' Defaults to "dummy".
#' @param colNames A vector of strings, with a length of nFactors. The strings
#' represent the name for each factor.
#' @param idName A string that specifies the id of the record. Defaults to "id".
#' @return A data.table that contains the added simulated data. Each column contains
#' an integer.
#' @examples
#' genMultiFac(nFactors = 2, each = 5)
#' genMultiFac(nFactors = 2, each = 4, levels = c(2, 3))
#' genMultiFac(nFactors = 3, each = 1, coding = "effect", 
#'    colNames = c("Fac1","Fac2", "Fac3"), id = "block")
#' @export
#'

genMultiFac <- function(nFactors, each, levels = 2, coding = "dummy", colNames = NULL, idName = "id") {
  
  
  if (nFactors < 2) stop("Must specify at least 2 factors")
  if (length(levels) > 1 & (length(levels) != nFactors)) stop("Number of levels does not match factors")
  
  x <- list()
  
  if ( all(levels==2) ) {
    
    if (coding == "effect") {
      opts <- c(-1, 1) 
    } else if (coding == "dummy") {
      opts <- c(0, 1) 
    } else {
      stop("Need to specify 'effect' or 'dummy' coding")
    }
    
    for (i in 1:nFactors) {
      
      x[[i]] <- opts
      
    }
    
  } else {
    
    if (length(levels) == 1) levels <- rep(levels, nFactors)
    
    for (i in 1:nFactors)  x[[i]] <- c(1 : levels[i])
    
  }
  
  dt <- data.table(as.data.frame(lapply(expand.grid(x), function(x) rep(x, each = each))))
  
  if (!is.null(colNames)) setnames(dt, colNames)
  
  origNames <- copy(names(dt))
  
  dt[ , (idName) := 1:.N]
  
  setcolorder(dt, c(idName, origNames) )
  setkeyv(dt, idName)
  
  return(dt[])
  
}
