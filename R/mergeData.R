#### Merge two data tables - more general #### 

#' Merge two data tables
#'
#' @param dt1 Name of first data.table
#' @param dt2 Name of second data.table
#' @param idvars Vector of string names to merge on
#' @return A new data table that merges dt2 with dt1
#' @export

mergeData <- function(dt1, dt2, idvars) {
  
  setkeyv(dt1, idvars)
  setkeyv(dt2, idvars)
  
  dt1[dt2]
  
}