#### Mixture ####

# Internal function called by .generate - returns mixture of multiple 
# columns
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mixture
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data

.genmixture <- function(n, formula, dtSim) {
  
  dT <- data.table::as.data.table(dtSim)
  
  fcompress <- gsub(" ", "", formula, fixed = TRUE)  # compress formula
  fsplit <- strsplit(fcompress, "+", fixed = TRUE)[[1]] # split variables
    
  flist <- lapply(fsplit, function(x) unlist(strsplit(x, "|", fixed=TRUE) ))
  ps <- cumsum(as.numeric(unlist(lapply(flist, function(x) (x[2])))))
  vars <- unlist(lapply(flist, function(x) (x[1])))
    
  conditions <- paste0("(interval==", 1:length(vars), ")" )
  f1 <- paste(vars, conditions, sep = "*")
  f1 <- paste(f1, collapse = "+")
    
  dvars <- dT[, vars, with = FALSE]
  
  u <- stats::runif(n)
  dvars$interval <- findInterval(u, ps, rightmost.closed = TRUE) + 1
  
  new <- with(dvars, eval(parse(text = as.character(f1))))
  
  return(new)
    
}

