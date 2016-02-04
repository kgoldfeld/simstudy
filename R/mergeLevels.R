#### Merge two data tables #### 

#' Merge two data tables - used to merge multilevel data
#'
#' @param dt2 Name of level 2 data set
#' @param dt1 Name of level 1 data set
#' @param idvars Vector of string names to merge on
#' @return A new data table that merges dt2 with dt1
#' @export

mergeLevels <- function(dt2, dt1, idvars) {
  
  setkeyv(dt1, idvars)
  setkeyv(dt2, idvars)
  
  dt2[dt1]
  
}