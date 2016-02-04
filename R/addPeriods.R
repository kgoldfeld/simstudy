#' Create longitudinal/panel data
#'
#' @param dtName Name of existing data table
#' @param nPeriods Number of time periods for each record
#' @param idvars Names of index variables (in a string vector) that will be
#' repeated during each time period
#' @param timevars Names of time dependent variables. Defaults to NULL.
#' @param timevarName Name of new time dependent varialbe
#' @return An updated data.table that that has multiple rows 
#' per observation in dtName
#' @export
#'

# dtName must contain id for now

addPeriods <- function(dtName, nPeriods, idvars = "id", 
                       timevars = NULL, timevarName = "timevar") {
  
  dtX1 <- copy(dtName)
  
  if (!is.null(timevars)) {
    dtX1[, eval(timevars) := NULL, with=TRUE] 
  }
  
  dtTimes1 <- dtX1[, .(period = (0 : (nPeriods - 1))), keyby = idvars]
  data.table::setkeyv(dtX1, idvars)
  dtTimes1 <- dtTimes1[dtX1]
  data.table::setkeyv(dtTimes1, c(idvars, "period"))
  
  if (!is.null(timevars)) {
    
    dtX2 <- copy(dtName)
    varX2 <- names(dtX2)[!(names(dtX2) %in% c(idvars,timevars))]
    
    if (length(varX2)) {
      dtX2[, eval(varX2) := NULL, with=TRUE]      
    }
    
    dtTimes2 <- data.table::melt(dtX2,id.vars=idvars, 
                                 value.name = timevarName,
                                 variable.name = "period",
                                 variable.factor = TRUE)
    
    dtTimes2[, period := factor(period, timevars)]
    dtTimes2[, period := as.integer(period) - 1]
    data.table::setkeyv(dtTimes2, c(idvars, "period"))
    
    return(dtTimes1[dtTimes2])
    
  } else {
    
    return(dtTimes1)
    
  }
  
}
