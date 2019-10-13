#' Trim longitudinal data file once an event has occurred
#'
#' @param dtOld name of data table to be trimmed
#' @param seqvar string referencing column that indexes the sequence or period
#' @param eventvar string referencing event data column
#' @param idvar string referencing id column
#' @return an updated data.table removes all rows folloing the first event for each 
#' individual
#' @examples
#' eDef <- defDataAdd(varname = "e", formula = "u==4", dist = "nonrandom")
#' 
#' P <- t(matrix(c( 0.4, 0.3, 0.2, 0.1,
#'                  0.0, 0.4, 0.3, 0.3,
#'                  0.0, 0.0, 0.5, 0.5,
#'                  0.0, 0.0, 0.0, 1.0),
#'               nrow = 4))
#' 
#' dp <- genMarkov(n = 100, transMat = P, 
#'                 chainLen = 8, id = "id", 
#'                 pername = "period",
#'                 varname = "u")
#' 
#' dp <- addColumns(eDef, dp)
#' dp <- trimData(dp, seqvar = "period", eventvar = "e", idvar = "id")
#' 
#' dp
#' 
#' @export
#'
trimData <- function(dtOld, seqvar, eventvar, idvar = "id") {
  
  dx <- copy(dtOld)
  
  id =   dx[, get(idvar)]
  seq = dx[, get(seqvar)]
  event = dx[, get(eventvar)]
  
  last <- clipVec(id = id , seq = seq, event = event)
  
  dd <- data.table(id = dx[, unique(id)], last)
  setkeyv(dd, idvar)
  
  dd <- dx[dd]
  dd[get(seqvar) <= last][, last := NULL][]
  
}