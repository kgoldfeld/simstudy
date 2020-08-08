#### Mixture ####

# Internal function called by .generate - returns mixture of multiple 
# columns
#
# @param n The number of observations required in the data set
# @param formula String that specifies the formula for the mixture
# @param dtSim Incomplete simulated data.table
# @return A data.frame column with the updated simulated data

.genmixture <- function(n, formula, dtSim) {
  formula <- .rmWS(formula)
  var_pr <- strsplit(formula, "+", fixed = T)
  var_dt <- strsplit(var_pr[[1]], "|", fixed = T)
  formDT <- as.data.table(do.call(rbind, var_dt))
  ps <-
    cumsum(.evalWith(unlist(formDT[, 2]), .parseDotVars(formDT[, 2]), NULL, 1))
  
  conditions <- paste0("(interval==", 1:nrow(formDT[, 1]), ")")
  f1 <- paste(unlist(formDT[, 1]), conditions, sep = "*")
  interval_formula <- paste(f1, collapse = "+")
  
  dvars <- .parseDotVars(formula)
  
  u <- stats::runif(n)
  dvars$interval <- findInterval(u, ps, rightmost.closed = TRUE) + 1
  
  .evalWith(interval_formula, dvars, dtSim, n)
}

