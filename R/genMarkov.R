#### Generate markov chain ####

#' @title  Generate Markov chain
#' @description Generate a Markov chain for n individuals or units by
#' specifying a transition matrix.
#' @param n number of individual chains to generate
#' @param transMat Square transition matrix where the sum of each row
#' must equal 1. The dimensions of the matrix equal the number of possible
#' states.
#' @param chainLen Length of each chain that will be generated for each
#' chain; minimum chain length is 2.
#' @param wide Logical variable (TRUE or FALSE) indicating whether the
#' resulting data table should be returned in wide or long format. The
#' wide format includes all elements of a chain on a single row; the long
#' format includes each element of a chain in its own row. The default is
#' wide = FALSE, so the long format is returned by default.
#' @param id Character string that represents name of "id" field. 
#' Defaults to "id".
#' @param pername Character string that represents the variable name of the
#' chain sequence in the long format. Defaults "period",
#' @param varname Character string that represents the variable name of the
#' state in the long format. Defaults to "state".
#' @param widePrefix Character string that represents the variable name 
#' prefix for the state fields in the wide format. Defaults to "S".
#' @return A data table with n rows if in wide format, or n by chainLen rows 
#' if in long format.
#' @examples
#' P <- t(matrix(c( 0.7, 0.2, 0.1,
#'                  0.5, 0.3, 0.2,
#'                  0.0, 0.1, 0.9), nrow=3, ncol=3))
#'                 
#' xLong <- genMarkov(n = 10, transMat = P, chainLen = 5)
#' xWide <- genMarkov(n = 10, transMat = P, chainLen = 5, wide = TRUE)
#' 
#' @export

genMarkov <- function(n, transMat, chainLen, wide = FALSE, id = "id",
                      pername = "period", varname = "state", 
                      widePrefix = "S") {
  
  # 'declare' vars created in data.table
  
  variable = NULL
  
  # check transMat is square matrix and row sums = 1
  
  if (   !is.matrix(transMat)  | 
       ( length(dim(transMat)) != 2 ) |
       ( dim(transMat)[1] != dim(transMat)[2] )
  ) {
    
    stop("Transition matrix needs to be a square matrix")
  }
  
  # check row sums = 1
  
  if ( !all(apply(transMat, 1, sum) == 1) ) {
    stop("Rows in transition matrix must sum to 1")
  }
  
  # check chainLen is > 1
  
  if (chainLen <= 1) stop("Chain length must be greater than 1")
  
  ####
  
  xmat <- markovChains(n, transMat, chainLen)
  
  dd <- data.table::data.table(id = seq(1:n), xmat)
  data.table::setnames(dd, "id", id)

  if (wide == TRUE) {
    
    defnames <- paste0("V",seq(1:chainLen))
    newnames <- paste0(widePrefix, seq(1:chainLen))
    data.table::setnames(dd, defnames, newnames)
    
  } else {           # wide = FALSE, so long format
    
    dd <- data.table::melt(dd, id.vars = id, 
              value.name = varname, variable.factor = TRUE)
    
    dd[, variable := as.integer(variable)]
    data.table::setnames(dd, "variable", pername)
    
  }
    
  setkeyv(dd, id)
  dd[]
  
}

