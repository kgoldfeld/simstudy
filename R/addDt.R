#' Add columns to existing data set
#'
#' @param dtDefs Name of definitions for added columns
#' @param dtOld Name of data table that is to be updated
#' @return An updated data.table that contains the added simulated data
#' @export
#'

addDt <- function(dtDefs,dtOld) {
  
  dtDefs <- dtDefs[-1, ]
  
  iter = nrow(dtDefs)
  n = nrow(dtOld)
  for (i in (1 : iter)) {
    dtOld <- generate(dtDefs[i,], n, dtOld)
  }
  
  data.table::data.table(dtOld)
  
}

