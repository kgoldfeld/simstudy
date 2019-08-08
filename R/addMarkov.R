#### Add markov chain to existing data set ####

#' @title  Add Markov chain
#' @description Generate a Markov chain for n individuals or units by
#' specifying a transition matrix.
#' @param dd data.table with a unique identifier
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
#' @param start0lab Character string that represents name of the integer 
#' field containing starting state (State 0) of the chain for each individual.
#' If it is NULL, starting state defaults to 1. Default is NULL.
#' @return A data table with n rows if in wide format, or n by chainLen rows 
#' if in long format.
#' @examples
#' def1 <- defData(varname = "x1", formula = 0, variance = 1)
#' def1 <- defData(def1, varname = "x2", formula = 0, variance = 1)
#' def1 <- defData(def1, varname = "S0", formula = ".6;.3;.1", 
#'                 dist="categorical")
#' 
#' dd <- genData(20, def1)
#' 
#' # Transition matrix P
#' 
#' P <- t(matrix(c( 0.7, 0.2, 0.1,
#'                  0.5, 0.3, 0.2,
#'                  0.0, 0.7, 0.3),
#'               nrow = 3))
#'               
#' d1 <- addMarkov(dd, P, chainLen = 3)
#' d2 <- addMarkov(dd, P, chainLen = 5, wide = TRUE)
#' d3 <- addMarkov(dd, P, chainLen = 5, wide = TRUE, start0lab = "S0")
#' 
#' @export

addMarkov <- function(dd, transMat, chainLen, wide = FALSE, id = "id",
                      pername = "period", varname = "state", 
                      widePrefix = "S", start0lab = NULL) {
  
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
  
  if ( !all(round(apply(transMat, 1, sum), 5) == 1) ) {
    stop("Rows in transition matrix must sum to 1")
  }
  
  # check chainLen is > 1
  
  if (chainLen <= 1) stop("Chain length must be greater than 1")
  
  # verify id is in data.table dd
  
  if (!(id %in% names(dd))) stop(paste(id, "is not in data table"))
  
  ####
  
  n <- nrow(dd)
  
  if (is.null(start0lab)) {
    s0 <- rep(1, n)
  } else if (!(start0lab %in% names(dd))) {
      stop(paste("Start state field", start0lab, "does not exist")) 
  } else {
      s0 <- dd[, get(start0lab)]
  }
  
  idlab <- id
  ids <- dd[, get(idlab)]
  xmat <- markovChains(n, transMat, chainLen, s0)
  
  dx <- data.table::data.table(id = ids, xmat)
  data.table::setnames(dx, id, ".id")
  
  defnames <- paste0("V",seq(1:chainLen))
  tempnames <- paste0(".V", seq(1:chainLen))
  data.table::setnames(dx, defnames, tempnames)
  
  dx <- merge(dd, dx, by.x = id, by.y = ".id")
  
  if (wide == TRUE) {
    
    defnames <- paste0(".V",seq(1:chainLen))
    newnames <- paste0(widePrefix, seq(1:chainLen))
    data.table::setnames(dx, defnames, newnames)
    
  } else {           # wide = FALSE, so long format
    
    dx <- data.table::melt(dx, id.vars = names(dd), 
                           value.name = varname, variable.factor = TRUE)
    
    dx[, variable := as.integer(variable)]
    data.table::setnames(dx, "variable", pername)
    
  }
  

  setkeyv(dx, id)
  dx[]
  
}
