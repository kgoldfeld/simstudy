#' Create longitudinal/panel data
#'
#' @param dtName Name of existing data table
#' @param nPeriods Number of time periods for each record
#' @param idvars Names of index variables (in a string vector) that will be
#' repeated during each time period
#' @param timevars Names of time dependent variables. Defaults to NULL.
#' @param timevarName Name of new time dependent variable
#' @return An updated data.table that that has multiple rows
#' per observation in dtName
#' @export
#'

addPeriods <-  function(dtName, nPeriods = NULL, idvars = "id",
                         timevars = NULL, timevarName = "timevar") {

  # "Declare" vars that exist in dtName

  nCount = NULL
  period = NULL
  vInterval = NULL
  mInterval = NULL
  timeElapsed = NULL
  time = NULL

  #

  dtX1 <- copy(dtName)

  if (!is.null(nPeriods) & !is.null(timevars)) {
    if (! (nPeriods == length(timevars))) {
      warning("Number of periods <> number of time dependent variables:
      periods based on time-dependent variables")
    }

  }

  # if there are time dependent vars, remove for now

  if (!is.null(timevars)) {
    dtX1[, eval(timevars) := NULL, with=TRUE]
    nPeriods <- length(timevars)
  }

  # create data.table with appropriate number of periods

  if (!is.null(nPeriods)) { # same number for each subject

    dtTimes1 <- dtX1[, list(period = (0 : (nPeriods - 1))), keyby = idvars]

  } else {

    if ("nCount" %in% names(dtX1)) { # specified for each subject

      dtTimes1 <- dtX1[, list(period = (0 : (nCount - 1))), keyby = idvars]

    } else {  # not specified for each subject or for all

      stop("No period or count parameter provided")

    }
  }

  # Add other fields back to dataset with time periods

  data.table::setkeyv(dtX1, idvars)
  dtTimes1 <- dtTimes1[dtX1]
  data.table::setkeyv(dtTimes1, c(idvars, "period"))

  # do extra manipulation based on situation

  if (!is.null(nPeriods)) {

    # explicitly same number of periods for each subject

    if (!is.null(timevars)) { # if time dependent variables specified

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

  } else {

    if (all(c("nCount", "mInterval") %in% names(dtX1))) {

      if (!("vInterval" %in% names(dtX1))) dtTimes1[, vInterval := 0]

      dtTimes1[,timeElapsed := genPosSkew(1, mInterval, vInterval), keyby = c(idvars,"period")]
      dtTimes1[period == 0, timeElapsed := 0]

      dtTimes1[,time := round(cumsum(timeElapsed)), keyby=idvars]
      dtTimes1[, c("timeElapsed","nCount", "mInterval", "vInterval") := NULL]

      return(dtTimes1)

    } else {

      stop("No period or count parameter provided")

    }

  }

  # if specified different measurement intervals:

}
