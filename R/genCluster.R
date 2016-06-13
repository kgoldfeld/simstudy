#### Cluster group assignment ####

#' Simulate data set that is one level down in a multilevel data context. The
#' level "2" data set must contain a field that specifies the number of
#' individual records in a particular cluster.
#'
#' @param dtClust Name of existing data set that contains the level "2" data
#' @param cLevelVar Variable name (string) of cluster id in dtClust
#' @param numIndVar Variable name (string) of number of observations
#' per cluster in dtClust
#' @param level2ID Name of id field in new level "1" data set
#' @param allLevel2 Indicator: if set to TRUE (default), the returned data set
#' includes all of the Level 2 data columns. If FALSE, the returned data set
#' only includes the Levels 1 and 2 ids.
#' @return A simulated data table with level "1" data
#' @export


genCluster <- function(dtClust,cLevelVar,numIndsVar, level1ID, allLevel2 = TRUE) {

  #### Check missing arguments

  if (missing(dtClust)) stop("argument 'dtClust' is missing", call. = FALSE)
  if (missing(cLevelVar)) stop("argument 'cLevelVar' is missing", call. = FALSE)
  if (missing(numIndsVar)) stop("argument 'numIndsVar' is missing", call. = FALSE)
  if (missing(level1ID)) stop("argument 'level1ID' is missing", call. = FALSE)

  ####

  dt <- dtClust[,.(id2 = get(cLevelVar), n = get(numIndsVar))][,.(id2 = rep(id2, n))]

  dt[, eval(cLevelVar) := id2]
  dt[, id2 := NULL]
  dt[, eval(level1ID) := (1 : nrow(dt))]

  if (allLevel2) dt <- mergeData(dtClust, dt, cLevelVar)

  return(dt)

}

