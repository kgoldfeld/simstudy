#' Add multi-factorial data
#'
#' @param dtOld data.table that is to be modified
#' @param nFactors Number of factors (columns) to generate.
#' @param levels Vector or scalar. If a vector is specified, it must be
#' the same length as nFatctors. Each value of the vector represents the
#' number of levels of each corresponding factor. If a scalar is specified,
#' each factor will have the same number of levels. The default is 2 levels
#' for each factor.
#' @param coding String value to specify if "dummy" or "effect" coding is used.
#' Defaults to "dummy".
#' @param colNames A vector of strings, with a length of nFactors. The strings
#' represent the name for each factor.
#' @return A data.table that contains the added simulated data. Each new column contains
#' an integer.
#' @examples
#' defD <-defData(varname = "x", formula = 0, variance = 1)
#'
#' DT <- genData(360, defD)
#' DT <- addMultiFac(DT, nFactors = 3, levels = c(2, 3, 3), colNames = c("A", "B", "C"))
#' DT
#' DT[, .N, keyby = .(A, B, C)]
#' 
#' DT <- genData(300, defD)
#' DT <- addMultiFac(DT, nFactors = 3, levels = 2)
#' DT[, .N, keyby = .(Var1, Var2, Var3)]
#'
#' @export
#'

addMultiFac <- function(dtOld, nFactors, levels = 2, coding = "dummy", colNames = NULL) {
  
  # 'declare' vars
  
  count <- NULL
  
  ###
  
  if (nFactors < 2) stop("Must specify at least 2 factors")
  if (length(levels) > 1 & (length(levels) != nFactors)) stop("Number of levels does not match factors")
  
  if (is.null(colNames)) {
    cn <- paste0("Var", 1:nFactors)
    if (any(cn %in% names(dtOld))) stop("Default column name(s) already in use")
  } else {
    if (any(colNames %in% names(dtOld))) stop("At least one column name already in use")
  }
  
  if (length(levels) == 1) {
    combos <- prod(rep(levels, nFactors))
  } else combos <- prod(levels)
  
  each <- ceiling(nrow(dtOld)/combos)
  extra <- nrow(dtOld) %% combos
  
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
  
  dnew <- data.table(as.data.frame(lapply(expand.grid(x), 
                                        function(x) rep(x, each = each))))
  dnew[, count := c(1:each)]
  neworder <- sample(1:nrow(dnew),nrow(dnew), replace = FALSE)
  dnew <- dnew[neworder]
  
  if (extra > 0) {
    
    full <- dnew[count < each]
    partial <- dnew[count == each][1:extra]
    
    all <- rbind(full, partial)  
    
  } else {
    
    all <- copy(dnew)
    
  }
  
  all <- all[,-"count"]
  
  if (!is.null(colNames)) setnames(all, colNames)
  
  origNames <- copy(names(all))
  dreturn <- cbind(dtOld, all)
  
  return(dreturn[])
  
}
