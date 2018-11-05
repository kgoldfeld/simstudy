#### LAGS ####

# Internal functions called by genMiss
#


# .checkLags searches formulas for "LAG()" function
#
# @param formulas
# @return boolean indicator that that at least one formula includes
# "LAG()" function

.checkLags <- function(formulas) {
  
  nLAGS <- length(unlist(regmatches(formulas, 
    gregexpr("(?=LAG\\().*?(?<=\\))", formulas, perl=T))))
  
  return(nLAGS > 0)
}
  
# .addLags adds temp lag fields and returns data.table and updated formulas
#
# @param olddt data.table to be modified
# @param formsdt string of formulas to be modified
# @return list of modified data.table, modified formulas, and vector of 
# names of temporary variables.

.addLags <- function(olddt, formsdt) {
  
  # "Declare" vars to avoid R CMD warning
  
  id <- NULL
  N <- NULL
  
  ##
  
  lagdt <- copy(olddt)
  lagforms <- copy(formsdt)
  origNames <- copy(names(olddt))
  
  if (! any(lagdt[, .N, keyby = id][, N > 1])) stop("Data not longitudinal")
  
  nforms <- length(lagforms)
  
  for (p in 1:nforms) {
    
    lags <- regmatches(lagforms[p], 
               gregexpr("(?<=LAG\\().*?(?=\\))", lagforms[p], perl=T))[[1]]
    
    if (length(lags) == 0) next # No lags in current formula
    
    if (any(table(lags) > 1)) {
      stop("Repeated lag term in formula")
    }
    
    if (! all(is.element(lags, origNames))) {
      stop(paste(setdiff(lags, origNames), "not in data table. "))
    }
    
    lags.1 <- paste0(".", lags, "1")
    if (is.element(lags.1, origNames)) {
      stop("Please do not use .*1 names")
    }
    
    # Everything is OK: update formula
    
    regmatches(lagforms[p], 
        gregexpr("(?=LAG\\().*?(?<=\\))", lagforms[p], perl=T)) <- list(lags.1)
    
    # Add new column(s) for lagged data
    
    for (i in 1:length(lags[p])) {
      if (! is.element(lags.1[i], origNames)) {
        lagdt[, (lags.1[i]) := shift( .SD[, lags, with = FALSE], n=1, fill = 0), 
           by = id ]
      }
    }
  }
  
  ####
  
  lagNames <- setdiff(names(lagdt), origNames)
  
  list(lagdt, lagforms, lagNames)
} 
