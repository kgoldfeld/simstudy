#' Generate event data using longitudinal data, and restrict output to time
#' until the nth event.
#'
#' @param dtName name of existing data table
#' @param defEvent data definition table (created with defDataAdd) that
#' determines the event generating process. 
#' @param nEvents maximum number of events that will be generated (the nth
#' event).
#' @param perName variable name for period field. Defaults to "period"
#' @param id string representing name of the id 
#' field in table specified by dtName
#' @return data.table that stops after "nEvents" are reached.
#' @examples
#' defD <- defData(varname = "effect", formula = 0, variance = 1, 
#'                 dist = "normal")
#' defE <- defDataAdd(varname = "died", formula = "-2.5 + 0.3*period + effect", 
#'                    dist = "binary", link = "logit")
#' 
#' d <- genData(1000, defD)
#' d <- addPeriods(d, 10)
#' dx <- genNthEvent(d, defEvent = defE, nEvents = 3)
#' @export
#'

genNthEvent <- function(dtName, defEvent, nEvents = 1,
                        perName = "period", id = "id") {
  
  # "Declare" vars to avoid R CMD warning
  
  .event <- NULL
  .id <- NULL
  .period <- NULL
  .first <- NULL
  
  #
  
  dd <- copy(dtName)
  dd <- addColumns(defEvent, dd)
  
  data.table::setnames(dd, c(defEvent$varname, id, perName), 
                       c(".event", ".id", ".period"))
  
  dsd <- dd[dd[.event == 1, .I[nEvents], keyby = .id]$V1]
  
  df <- dsd[!is.na(.period), list(.id, .first = .period)]
  
  devent <- merge(dd, df, by = ".id")[.period <= .first, ][, .first := NULL]
  dnone <- merge(dd, df, by = ".id", all.x = TRUE)[is.na(.first)][, .first := NULL]
  
  dx <- data.table::rbindlist(list(devent, dnone))
  data.table::setkeyv(dx, key(dd))
  
  data.table::setnames(dx, c(".id",".period",".event"), 
                       c(id, perName, defEvent$varname))
  dx[]
  
}