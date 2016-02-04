#### Cluster group assignment #### 

#' Simulate data set that is one layer down in a multilevel data context
#'
#' @param dtDefs dtDefs Name of definitions data.table/data.frame
#' @param dtClust Name of cluster level data
#' @param cLevelVar Variable name (string) of cluster id in dtClust
#' @param numIndsVar Variable name (string) of number of observations 
#' per cluster in dtClust
#' @return A simulated data table
#' @export


gendtClust <- function(dtDefs,dtClust,cLevelVar,numIndsVar) {
  
  dt.temp <- dtClust[,.(id2 = get(cLevelVar), n = get(numIndsVar))][,.(id2 = rep(id2, n))]
  
  dt <- addDt(dtDefs,dt.temp)

  dt[, eval(cLevelVar) := id2]
  dt[, id2 := NULL]
  dt[, eval(dtDefs[1,id]) := (1 : nrow(dt))]
  
  return(dt)
  
}

