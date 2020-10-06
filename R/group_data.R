#' Create longitudinal/panel data
#'
#' @param dtName Name of existing data table
#' @param nPeriods Number of time periods for each record
#' @param idvars Names of index variables (in a string vector) that will be
#' repeated during each time period
#' @param timevars Names of time dependent variables. Defaults to NULL.
#' @param timevarName Name of new time dependent variable
#' @param timeid Variable name for new index field. Defaults to "timevar"
#' @param perName Variable name for period field. Defaults to "period"
#' @return An updated data.table that that has multiple rows
#' per observation in dtName
#' @examples
#' tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
#' tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
#' tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 + 5 + 5 * T", variance = 1)
#' tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y0 + 10 + 5 * T", variance = 1)
#'
#' dtTrial <- genData(5, tdef)
#' dtTrial
#'
#' dtTime <- addPeriods(dtTrial,
#'   nPeriods = 3, idvars = "id",
#'   timevars = c("Y0", "Y1", "Y2"), timevarName = "Y"
#' )
#' dtTime
#'
#' # Varying # of periods and intervals - need to have variables
#' # called nCount and mInterval
#'
#' def <- defData(varname = "xbase", dist = "normal", formula = 20, variance = 3)
#' def <- defData(def, varname = "nCount", dist = "noZeroPoisson", formula = 6)
#' def <- defData(def, varname = "mInterval", dist = "gamma", formula = 30, variance = .01)
#' def <- defData(def, varname = "vInterval", dist = "nonrandom", formula = .07)
#'
#' dt <- genData(200, def)
#' dt[id %in% c(8, 121)]
#'
#' dtPeriod <- addPeriods(dt)
#' dtPeriod[id %in% c(8, 121)] # View individuals 8 and 121 only
#' @export
#' @concept group_data
addPeriods <- function(dtName,
                       nPeriods = NULL,
                       idvars = "id",
                       timevars = NULL,
                       timevarName = "timevar",
                       timeid = "timeID",
                       perName = "period") {

  # "Declare" vars that exist in dtName
  nCount <- NULL
  .period <- NULL
  vInterval <- NULL
  mInterval <- NULL
  timeElapsed <- NULL
  time <- NULL

  dtX1 <- copy(dtName)

  if (!is.null(nPeriods) & !is.null(timevars)) {
    if (!(nPeriods == length(timevars))) {
      warning("Number of periods <> number of time dependent variables:
      periods based on time-dependent variables")
    }
  }

  # if there are time dependent vars, remove for now

  if (!is.null(timevars)) {
    dtX1[, eval(timevars) := NULL, with = TRUE]
    nPeriods <- length(timevars)
  }

  # create data.table with appropriate number of periods

  if (!is.null(nPeriods)) { # same number for each subject

    dtTimes1 <- dtX1[, list(.period = (0:(nPeriods - 1))), keyby = idvars]
  } else {
    if ("nCount" %in% names(dtX1)) { # specified for each subject

      dtTimes1 <- dtX1[, list(.period = (0:(nCount - 1))), keyby = idvars]
    } else { # not specified for each subject or for all

      stop("No period or count parameter provided")
    }
  }

  # Add other fields back to dataset with time periods

  data.table::setkeyv(dtX1, idvars)
  dtTimes1 <- dtTimes1[dtX1]
  data.table::setkeyv(dtTimes1, c(idvars, ".period"))

  # Create code for final index assignment

  cmd <- quote(dtTimes1[, x])
  pmd <- quote(x := 1:.N)
  pmd[[2]] <- parse(text = timeid)[[1]]
  cmd[[4]] <- pmd

  # do extra manipulation based on situation

  if (!is.null(nPeriods)) {

    # explicitly same number of periods for each subject

    if (!is.null(timevars)) { # if time dependent variables specified

      dtX2 <- copy(dtName)
      varX2 <- names(dtX2)[!(names(dtX2) %in% c(idvars, timevars))]

      if (length(varX2)) {
        dtX2[, eval(varX2) := NULL, with = TRUE]
      }

      dtTimes2 <- data.table::melt(dtX2,
        id.vars = idvars,
        value.name = timevarName,
        variable.name = ".period",
        variable.factor = TRUE
      )

      dtTimes2[, .period := factor(.period, timevars)]
      dtTimes2[, .period := as.integer(.period) - 1]
      data.table::setkeyv(dtTimes2, c(idvars, ".period"))

      dtTimes1 <- dtTimes1[dtTimes2]

      eval(cmd)
      data.table::setkeyv(dtTimes1, timeid)

      data.table::setnames(dtTimes1, old = ".period", new = perName)
      return(dtTimes1[])
    } else {
      eval(cmd)
      data.table::setkeyv(dtTimes1, timeid)

      data.table::setnames(dtTimes1, old = ".period", new = perName)
      return(dtTimes1[])
    }
  } else { # is.null(nPeriods) == TRUE

    if (all(c("nCount", "mInterval") %in% names(dtX1))) {
      if (!("vInterval" %in% names(dtX1))) dtTimes1[, vInterval := 0]

      dtTimes1[, timeElapsed := .genPosSkew(1, mInterval, vInterval), keyby = c(idvars, ".period")]
      dtTimes1[.period == 0, timeElapsed := 0]

      dtTimes1[, time := round(cumsum(timeElapsed)), keyby = idvars]
      dtTimes1[, c("timeElapsed", "nCount", "mInterval", "vInterval") := NULL]

      eval(cmd)
      data.table::setkeyv(dtTimes1, timeid)

      data.table::setnames(dtTimes1, old = ".period", new = perName)
      return(dtTimes1[])
    } else {
      stop("No period or count parameter provided")
    }
  }

  # if specified different measurement intervals:
}

#' @title  Simulate clustered data
#' @description Simulate data set that is one level down in a multilevel data context. The
#' level "2" data set must contain a field that specifies the number of
#' individual records in a particular cluster.
#' @param dtClust Name of existing data set that contains the level "2" data
#' @param cLevelVar Variable name (string) of cluster id in dtClust
#' @param numIndsVar Variable name (string) of number of observations
#' per cluster in dtClust. Can also be a single integer value that will
#' be used for all clusters.
#' @param level1ID Name of id field in new level "1" data set
#' @param allLevel2 Indicator: if set to TRUE (default), the returned data set
#' includes all of the Level 2 data columns. If FALSE, the returned data set
#' only includes the Levels 1 and 2 ids.
#' @return A simulated data table with level "1" data
#' @examples
#' gen.school <- defData(
#'   varname = "s0", dist = "normal",
#'   formula = 0, variance = 3, id = "idSchool"
#' )
#' gen.school <- defData(gen.school,
#'   varname = "nClasses",
#'   dist = "noZeroPoisson", formula = 3
#' )
#'
#' dtSchool <- genData(3, gen.school) #'
#' dtSchool
#'
#' dtClass <- genCluster(dtSchool,
#'   cLevelVar = "idSchool",
#'   numIndsVar = "nClasses", level1ID = "idClass"
#' )
#' dtClass
#'
#' dtClass <- genCluster(dtSchool,
#'   cLevelVar = "idSchool",
#'   numIndsVar = 3, level1ID = "idClass"
#' )
#' dtClass
#' @export
#' @concept group_data
genCluster <- function(dtClust,
                       cLevelVar,
                       numIndsVar,
                       level1ID,
                       allLevel2 = TRUE) {

  # 'declare' var
  id2 <- NULL
  n <- NULL

  #### Check missing arguments
  if (missing(dtClust)) stop("argument 'dtClust' is missing", call. = FALSE)
  if (missing(cLevelVar)) stop("argument 'cLevelVar' is missing", call. = FALSE)
  if (missing(numIndsVar)) stop("argument 'numIndsVar' is missing", call. = FALSE)
  if (missing(level1ID)) stop("argument 'level1ID' is missing", call. = FALSE)


  if (is.character(numIndsVar)) {
    dt <- dtClust[, list(
      id2 = get(cLevelVar),
      n = get(numIndsVar)
    )][, list(id2 = rep(id2, n))]
  } else if (is.numeric(numIndsVar)) {
    dt <- dtClust[, list(
      id2 = get(cLevelVar),
      n = as.integer(numIndsVar)
    )][, list(id2 = rep(id2, n))]
  }

  # dt <- dtClust[,list(id2 = get(cLevelVar),
  #                     n = get(numIndsVar))][,list(id2 = rep(id2, n))]

  dt[, eval(cLevelVar) := id2]
  dt[, id2 := NULL]
  dt[, eval(level1ID) := (1:.N)]

  if (allLevel2) dt <- mergeData(dtClust, dt, cLevelVar)

  data.table::setkeyv(dt, level1ID)

  return(dt[])
}

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
#' defD <- defData(
#'   varname = "effect", formula = 0, variance = 1,
#'   dist = "normal"
#' )
#' defE <- defDataAdd(
#'   varname = "died", formula = "-2.5 + 0.3*period + effect",
#'   dist = "binary", link = "logit"
#' )
#'
#' d <- genData(1000, defD)
#' d <- addPeriods(d, 10)
#' dx <- genNthEvent(d, defEvent = defE, nEvents = 3)
#' @export
#' @concept group_data
genNthEvent <- function(dtName, defEvent, nEvents = 1,
                        perName = "period", id = "id") {

  # "Declare" vars to avoid R CMD warning
  .event <- NULL
  .id <- NULL
  .period <- NULL
  .first <- NULL

  dd <- copy(dtName)
  dd <- addColumns(defEvent, dd)

  data.table::setnames(
    dd, c(defEvent$varname, id, perName),
    c(".event", ".id", ".period")
  )

  dsd <- dd[dd[.event == 1, .I[nEvents], keyby = .id]$V1]

  df <- dsd[!is.na(.period), list(.id, .first = .period)]

  devent <- merge(dd, df, by = ".id")[.period <= .first, ][, .first := NULL]
  dnone <- merge(dd, df, by = ".id", all.x = TRUE)[is.na(.first)][, .first := NULL]

  dx <- data.table::rbindlist(list(devent, dnone))
  data.table::setkeyv(dx, key(dd))

  data.table::setnames(
    dx, c(".id", ".period", ".event"),
    c(id, perName, defEvent$varname)
  )
  dx[]
}

#' Assign treatment
#'
#' @param dtName data table
#' @param nTrt number of treatment groups
#' @param balanced indicator for treatment assignment process
#' @param strata vector of strings representing stratifying variables
#' @param grpName string representing variable name for treatment or
#' exposure group
#' @param ratio vector of values indicating relative proportion of group
#' assignment
#' @return An integer (group) ranging from 1 to length of the
#' probability vector
#' @seealso \code{\link{trtObserve}}
#' @examples
#' dt <- genData(15)
#'
#' dt1 <- trtAssign(dt, nTrt = 3, balanced = TRUE)
#' dt1[, .N, keyby = trtGrp]
#'
#' dt2 <- trtAssign(dt, nTrt = 3, balanced = FALSE)
#' dt2[, .N, keyby = trtGrp]
#'
#' def <- defData(varname = "male", formula = .4, dist = "binary")
#' dt <- genData(1000, def)
#' dt
#'
#' dt3 <- trtAssign(dt, nTrt = 5, strata = "male", balanced = TRUE, grpName = "Group")
#' dt3
#' dt3[, .N, keyby = .(male, Group)]
#' dt3[, .N, keyby = .(Group)]
#'
#' dt4 <- trtAssign(dt, nTrt = 5, strata = "male", balanced = FALSE, grpName = "Group")
#' dt4[, .N, keyby = .(male, Group)]
#' dt4[, .N, keyby = .(Group)]
#'
#' dt5 <- trtAssign(dt, nTrt = 5, balanced = TRUE, grpName = "Group")
#' dt5[, .N, keyby = .(male, Group)]
#' dt5[, .N, keyby = .(Group)]
#'
#' dt6 <- trtAssign(dt, nTrt = 3, ratio = c(1, 2, 2), grpName = "Group")
#' dt6[, .N, keyby = .(Group)]
#' @export
#' @concept group_data
trtAssign <- function(dtName, nTrt = 2, balanced = TRUE,
                      strata = NULL, grpName = "trtGrp", ratio = NULL) {

  # 'declare' vars
  .stratum <- NULL
  .n <- NULL
  grpExp <- NULL

  if (missing(dtName)) {
    stop("Data table argument is missing", call. = FALSE)
  }
  if (grpName %in% names(dtName)) {
    stop("Group name has previously been defined in data table", call. = FALSE)
  }
  if (!is.null(ratio)) {
    if (length(ratio) != nTrt) {
      stop("Number of treatments does not match specified ratio", call. = FALSE)
    }
  }

  dt <- copy(dtName)

  if (balanced) {
    if (is.null(strata)) {
      dt[, .stratum := 1]
    } else {
      dt <- .addStrataCode(dt, strata)
    }

    dt[, .n := .N, keyby = .stratum]
    dtrx <- dt[, list(grpExp = .stratSamp(.n[1], nTrt, ratio)), keyby = .stratum]
    dt[, grpExp := dtrx$grpExp]
    dt[, `:=`(.stratum = NULL, .n = NULL)]

    if (nTrt == 2) dt[, grpExp := grpExp - 1]
    data.table::setnames(dt, "grpExp", grpName)
    data.table::setkeyv(dt, key(dtName))
  } else { # balanced is FALSE - strata are not relevant

    if (is.null(ratio)) {
      if (nTrt == 2) {
        formula <- .5
      } else {
        formula <- rep(1 / nTrt, nTrt)
      }
    } else { # ratio not null
      formula <- round(ratio / sum(ratio), 8)
    }


    dt <- trtObserve(dt, formulas = formula, logit.link = FALSE, grpName)
  }

  return(dt[])
}

#' Add strata code to data table
#'
#' @param dt data table
#' @param strata vector of string names representing strata
#' @return The old data table with an add column containing an integer ranging
#' from one to `2^length(strata)`.
#' @md
#' @noRd
.addStrataCode <- function(dt, strata) {

  # 'Declare' var
  .stratum <- NULL

  dtWork <- copy(dt)

  strataOnly <- dtWork[, eval(strata), with = FALSE]
  data.table::setkeyv(strataOnly, names(strataOnly))

  uniqueStrata <- unique(strataOnly)
  uniqueStrata[, .stratum := (1:.N)]

  data.table::setkeyv(dtWork, names(strataOnly))
  dtWork <- uniqueStrata[dtWork]

  data.table::setkeyv(dtWork, key(dt))
  setcolorder(dtWork, colnames(dt))

  dtWork
}

#' Stratified sample
#' @description Helper function to randomly assign a treatment group to the
#' elements of a stratum.
#' @param nrow Number of rows in the stratum
#' @param ncat Number of treatment categories
#' @param ratio vector of values indicating relative proportion of group
#' assignment
#' @return A vector of length(nrow) containing the group assignments for each elemen of the
#'  stratum.
#' @noRd
.stratSamp <- function(nrow, ncat, ratio = NULL) {
  if (is.null(ratio)) ratio <- rep(1, ncat)

  neach <- floor(nrow / sum(ratio))
  distrx <- rep(c(1:ncat), times = (neach * ratio))
  extra <- nrow - length(distrx)
  strata <- c(distrx, sample(rep(1:ncat, times = ratio), extra))

  if (length(strata) == 1) {
    return(strata)
  }

   sample(strata)
}

#' Observed exposure or treatment
#'
#' @param dt data table
#' @param formulas collection of formulas that determine probabilities
#' @param logit.link indicator that specifies link. If TRUE, then logit link
#' is used. If FALSE, the identity link is used.
#' @param grpName character string representing name of treatment/exposure group
#' variable
#' @return An integer (group) ranging from 1 to length of the probability vector
#' @seealso \code{\link{trtAssign}}
#' @examples
#' def <- defData(varname = "male", dist = "binary", formula = .5, id = "cid")
#' def <- defData(def, varname = "over65", dist = "binary", formula = "-1.7 + .8*male", link = "logit")
#' def <- defData(def, varname = "baseDBP", dist = "normal", formula = 70, variance = 40)
#'
#' dtstudy <- genData(1000, def)
#' dtstudy
#'
#' formula1 <- c("-2 + 2*male - .5*over65", "-1 + 2*male + .5*over65")
#' dtObs <- trtObserve(dtstudy, formulas = formula1, logit.link = TRUE, grpName = "exposure")
#' dtObs
#'
#' # Check actual distributions
#'
#' dtObs[, .(pctMale = round(mean(male), 2)), keyby = exposure]
#' dtObs[, .(pctMale = round(mean(over65), 2)), keyby = exposure]
#'
#' dtSum <- dtObs[, .N, keyby = .(male, over65, exposure)]
#' dtSum[, grpPct := round(N / sum(N), 2), keyby = .(male, over65)]
#' dtSum
#' @export
#' @concept group_data
trtObserve <- function(dt, formulas, logit.link = FALSE, grpName = "trtGrp") {
  if (missing(dt)) {
    stop("Data table argument is missing", call. = FALSE)
  }
  if (grpName %in% names(dt)) {
    stop("Group name has previously been defined in data table", call. = FALSE)
  }

  ncols <- ncol(dt)

  ncat <- length(formulas)
  def <- NULL

  for (i in 1:ncat) {
    def <- defDataAdd(def,
      varname = paste0("e", i),
      dist = "nonrandom",
      formula = formulas[i]
    )
  }

  dtnew <- addColumns(def, dt)

  dtmatrix <- as.matrix(dtnew[,
    .SD,
    .SDcols = c((ncols + 1):(ncols + ncat))
  ])

  if (logit.link) {
    dtmatrix <- exp(dtmatrix)
    dtmatrix <- dtmatrix / (1 + apply(dtmatrix, 1, sum))
  }

  dtmatrix <- cbind(dtmatrix, 1 - apply(dtmatrix, 1, sum))

  grpExp <- .Call(`_simstudy_matMultinom`, dtmatrix, PACKAGE = "simstudy")

  dtnew <- cbind(dt[, .SD, .SDcols = key(dt)], grpExp)
  data.table::setkeyv(dtnew, key(dt))

  dtnew <- dtnew[dt]

  if (length(formulas) == 1) dtnew[grpExp == 2, grpExp := 0]

  data.table::setnames(dtnew, "grpExp", grpName)

  return(dtnew[])
}

#' Assign treatment for stepped-wedge design
#'
#' @param dtName data table
#' @param clustID string representing name of column of cluster level ids
#' @param nWaves number of treatment waves
#' @param lenWaves the number of periods between waves
#' @param startPer the starting period of the first wave
#' @param perName string representing name of column of time periods
#' @param grpName string representing variable name for treatment or
#' exposure group
#' @param lag integer representing length of transition period
#' @param xrName string representing name of the field that
#' indicates whether the cluster status is in transition status
#'
#' @return A data.table with the added treatment assignment
#' @seealso \code{\link{trtObserve} \link{trtAssign}}
#' @examples
#' defc <- defData(
#'   varname = "ceffect", formula = 0, variance = 0.10,
#'   dist = "normal", id = "cluster"
#' )
#' defc <- defData(defc, "m", formula = 10, dist = "nonrandom")
#'
#' # Will generate 3 waves of 4 clusters each - starting 2, 5, and 8
#'
#' dc <- genData(12, defc)
#' dp <- addPeriods(dc, 12, "cluster")
#' dp <- trtStepWedge(dp, "cluster",
#'   nWaves = 3,
#'   lenWaves = 3, startPer = 2
#' )
#' dp
#'
#' dp <- addPeriods(dc, 12, "cluster")
#' dp <- trtStepWedge(dp, "cluster",
#'   nWaves = 2,
#'   lenWaves = 1, startPer = 4, lag = 3
#' )
#' dp
#' @export
#' @concept group_data
trtStepWedge <- function(dtName, clustID, nWaves, lenWaves,
                         startPer, perName = "period", grpName = "rx",
                         lag = 0, xrName = "xr") {

  # 'declare' vars created in data.table
  rx <- NULL
  period <- NULL
  xr <- NULL

  if (lag == 0) xrName <- "xr" # override - will be deleted from dd

  if (missing(dtName)) {
    stop("Data table argument is missing", call. = FALSE)
  }
  if (grpName %in% names(dtName)) {
    stop("Group name has previously been defined in data table", call. = FALSE)
  }
  if (!(perName %in% names(dtName))) {
    stop("Period name has not been defined in data table", call. = FALSE)
  }

  dd <- copy(dtName)
  data.table::setnames(dd, perName, "period")

  nClust <- length(dd[, unique(get(clustID))])
  nPer <- length(dd[, unique(period)])
  cPerWave <- nClust / nWaves

  if (nClust %% nWaves != 0) {
    stop(paste(
      "Cannot create equal size waves with", nClust, "clusters and",
      nWaves, "waves."
    ))
  }

  if ((nPer) < (startPer + (nWaves - 1) * lenWaves + 1)) {
    stop(paste(
      "Design requires", (startPer + (nWaves - 1) * lenWaves + 1),
      "periods but only", nPer, "generated."
    ))
  }

  startTrt <- rep((0:(nWaves - 1)) * lenWaves, each = cPerWave) + startPer
  dstart <- data.table::data.table(cid = 1:nClust, startTrt)
  data.table::setnames(dstart, "cid", clustID)
  data.table::setkeyv(dstart, clustID)

  data.table::setkeyv(dd, clustID)
  dd <- dd[dstart]
  dd[, xr := ((period >= startTrt) & (period < (startTrt + lag))) * 1]
  dd[, rx := ((startTrt + lag) <= period) * 1]
  data.table::setnames(
    dd, c("period", "rx", "xr"),
    c(perName, grpName, xrName)
  )

  if (lag == 0) dd[, `:=`(xr = NULL)]
  return(dd[])
}