#' @title Definitions for missing data
#' @description Add single row to definitions table for missing data
#' @param dtDefs Definition data.table to be modified
#' @param varname Name of variable with missingness
#' @param formula Formula to describe pattern of missingness
#' @param logit.link Indicator set to TRUE when the probability of missingness
#' is based on a logit model.
#' @param baseline Indicator is set to TRUE if the variable is a baseline
#' measure and should be missing throughout an entire observation period. This
#' is applicable to repeated measures/longitudinal data.
#' @param monotonic Indicator set to TRUE if missingness at time t is followed
#' by missingness at all follow-up times > t.
#' @return A data.table named dtName that is an updated data definitions table
#' @seealso \code{\link{genMiss}}, \code{\link{genObs}}
#' @examples
#' def1 <- defData(varname = "m", dist = "binary", formula = .5)
#' def1 <- defData(def1, "u", dist = "binary", formula = .5)
#' def1 <- defData(def1, "x1", dist = "normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x2", dist = "normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x3", dist = "normal", formula = "20*m + 20*u", variance = 2)
#'
#' dtAct <- genData(1000, def1)
#'
#' defM <- defMiss(varname = "x1", formula = .15, logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x2", formula = ".05 + m * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x3", formula = ".05 + u * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed
#' defM
#'
#' # Generate missing data matrix
#'
#' missMat <- genMiss(dtName = dtAct, missDefs = defM, idvars = "id")
#' missMat
#'
#' # Generate observed data from actual data and missing data matrix
#'
#' dtObs <- genObs(dtAct, missMat, idvars = "id")
#' dtObs
#' @export
#' @concept missing
defMiss <- function(dtDefs = NULL,
                    varname,
                    formula,
                    logit.link = FALSE,
                    baseline = FALSE,
                    monotonic = FALSE) {
  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()
  }

  dtNew <- data.table::data.table(
    varname,
    formula,
    logit.link,
    baseline,
    monotonic
  )

  l <- list(dtDefs, dtNew)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
}

#' Generate missing data
#'
#' @param dtName Name of complete data set
#' @param missDefs Definitions of missingness
#' @param idvars Index variables
#' @param repeated Indicator for longitudinal data
#' @param periodvar Name of variable that contains period
#' @return Missing data matrix indexed by idvars (and period if relevant)
#' @seealso \code{\link{defMiss}}, \code{\link{genObs}}
#' @examples
#' def1 <- defData(varname = "m", dist = "binary", formula = .5)
#' def1 <- defData(def1, "u", dist = "binary", formula = .5)
#' def1 <- defData(def1, "x1", dist = "normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x2", dist = "normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x3", dist = "normal", formula = "20*m + 20*u", variance = 2)
#'
#' dtAct <- genData(1000, def1)
#'
#' defM <- defMiss(varname = "x1", formula = .15, logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x2", formula = ".05 + m * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x3", formula = ".05 + u * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed
#' defM
#'
#' # Generate missing data matrix
#'
#' missMat <- genMiss(dtAct, defM, idvars = "id")
#' missMat
#'
#' # Generate observed data from actual data and missing data matrix
#'
#' dtObs <- genObs(dtAct, missMat, idvars = "id")
#' dtObs
#' @export
#' @concept missing
genMiss <- function(dtName, missDefs, idvars,
                    repeated = FALSE, periodvar = "period") {

  # "Declare" vars to avoid R CMD warning
  varname <- NULL
  period <- NULL
  baseline <- NULL
  monotonic <- NULL
  fmiss <- NULL
  formula <- NULL


  includesLags <- FALSE

  data.table::setkeyv(dtName, idvars)
  tmDefs <- data.table::copy(missDefs)

  if (!repeated) {
    dtMiss <- dtName[, c(idvars), with = FALSE]
    # names(dtMiss) <- c(idvars) # removed 2017919 - possible error in CRAN check

    for (i in (1:nrow(tmDefs))) {
      dtTemp <- data.table::copy(dtName)
      mat1 <- .genMissDataMat(dtName, dtTemp, idvars, tmDefs[i, ])
      vec1 <- mat1[, tmDefs[i, varname], with = FALSE]

      dtMiss <- cbind(dtMiss, vec1)
    }
  } else { # repeated

    includesLags <- .checkLags(tmDefs[, formula])

    if (includesLags) {
      lags <- .addLags(dtName, tmDefs[, formula])

      tmDefs[, formula := lags[[2]]]
      dtName <- lags[[1]]
    }

    dtMiss <- dtName[, c(idvars, periodvar), with = FALSE]
    colnames <- c(idvars, "period")
    data.table::setnames(dtMiss, colnames)

    nPeriods <- dtMiss[, max(period)] + 1

    for (i in (1:nrow(tmDefs))) {
      if (tmDefs[i, baseline]) {
        dtTemp <- dtName[period == 0]
        mat1 <- .genMissDataMat(
          dtName[period == 0], dtTemp,
          idvars, tmDefs[i, ]
        )
        vec1 <- addPeriods(mat1, nPeriods, idvars)[, tmDefs[i, varname],
          with = FALSE
        ]

        dtMiss <- cbind(dtMiss, vec1)
      } else { # not just baseline can be missing

        dtTemp <- data.table::copy(dtName)
        mat1 <- .genMissDataMat(dtName, dtTemp, idvars, tmDefs[i, ])
        vec1 <- mat1[, tmDefs[i, varname], with = FALSE]
        dtMiss <- cbind(dtMiss, vec1)

        if (tmDefs[i, monotonic]) { # monotonic missing

          dt.fmiss <- dtMiss[eval(parse(text = tmDefs[i, varname])) == 1, list(fmiss = min(period)), keyby = eval(idvars)]
          data.table::setkeyv(dtMiss, idvars)
          dtMiss <- dt.fmiss[dtMiss]
          dtMiss[period > fmiss, eval(tmDefs[i, varname]) := 1]
          dtMiss[, fmiss := NULL]
        }
      }
    }
  }

  dims <- dim(dtName[, !names(dtMiss), with = FALSE])
  addon <- data.table(matrix(0, nrow = dims[1], ncol = dims[2]))
  names(addon) <- names(dtName[, !names(dtMiss), with = FALSE])

  dtbind <- cbind(dtMiss, addon)

  if (includesLags) {
    dtbind[, (lags[[3]]) := NULL]
  }

  dtbind[]
}


#' Search formulas for "LAG()" function
#'
#' @param formulas Formulas to check.
#' @return boolean indicator that that at least one formula includes
#' "LAG()" function
#' @noRd
.checkLags <- function(formulas) {
  "LAG" %in% all.names(parse(text = formulas))
}

#' Add temp lag fields and update formulas
#'
#' @description Creates new columns with shifted values for "LAG(x)" fields to
#' allow for MAR-missignes. Replaces "LAG(x)" with temp var ".x1".
#' @param oldDT data.table to be modified
#' @param formsdt string of formulas to be modified
#' @return list of modified data.table, modified formulas, and vector of
#' names of temporary variables.
#' @noRd
.addLags <- function(oldDT, formsdt) {
  # TODO add LAG function to documentation
  # "Declare" vars to avoid R CMD warning
  id <- NULL
  N <- NULL

  lagdt <- data.table::copy(oldDT)
  lagforms <- data.table::copy(formsdt)
  origNames <- data.table::copy(names(oldDT))

  if (!any(lagdt[, .N, keyby = id][, N > 1])) stop("Data not longitudinal")

  nforms <- length(lagforms)

  for (p in 1:nforms) {
    lags <- regmatches(
      lagforms[p],
      gregexpr("(?<=LAG\\().*?(?=\\))", lagforms[p], perl = T)
    )[[1]]

    if (length(lags) == 0) next # No lags in current formula

    # TODO remove/adjust this error
    if (any(table(lags) > 1)) {
      stop("Repeated lag term in formula")
    }

    if (!all(is.element(lags, origNames))) {
      stop(paste(setdiff(lags, origNames), "not in data table. "))
    }

    lags1 <- paste0(".", lags, "1")
    if (is.element(lags1, origNames)) {
      stop("Please do not use .*1 names")
    }

    # Everything is OK: update formula

    regmatches(
      lagforms[p],
      gregexpr("(?=LAG\\().*?(?<=\\))", lagforms[p], perl = T)
    ) <- list(lags1)

    # Add new column(s) for lagged data

    for (i in 1:length(lags[p])) {
      if (!is.element(lags1[i], origNames)) {
        lagdt[, (lags1[i]) := shift(.SD[, lags, with = FALSE], n = 1, fill = 0),
          by = id
        ]
      }
    }
  }

  lagNames <- setdiff(names(lagdt), origNames)

  list(lagdt, lagforms, lagNames)
}

#' Internal function called by genMiss - returns a missing data matrix
#'
#' @param dtName Name of complete data set
#' @param dtTemp Name of data set with unique ids only
#' @param idvars To be filled in
#' @param missDefs To be filled in
#' @return A missing data matrix of 0/1, where 1 indicates missing
#' @noRd

.genMissDataMat <- function(dtName, dtTemp, idvars, missDefs) {

  # 'declare vars
  varname <- NULL
  logit.link <- NULL
  formula <- NULL

  dtMissP <- dtTemp[, idvars, with = FALSE]

  Expression <- parse(text = as.character(missDefs[, varname]))
  ColName <- as.character(missDefs[, varname]) # new data.table (changed 2016-12-05)
  Formula <- parse(text = as.character(missDefs[, formula]))

  if (!missDefs[, logit.link]) {
    # dtMissP[, eval(Expression) := dtName[, eval(Formula)]] # old data.table

    dtMissP[, (ColName) := dtName[, eval(Formula)]]
  } else {
    # dtMissP[, eval(Expression) := dtName[, .log2Prob(eval(Formula))]] # old data.table
    dtMissP[, (ColName) := dtName[, .log2Prob(eval(Formula))]]
  }
  matMiss <- dtMissP[, idvars, with = FALSE]
  # matMiss[, eval(Expression) := stats::rbinom(nrow(dtMissP), 1, dtMissP[,
  #                                     eval(Expression)])] # old data.table

  matMiss[, (ColName) := stats::rbinom(
    nrow(dtMissP), 1,
    dtMissP[, eval(Expression)]
  )]

  return(matMiss)
}

#' Create an observed data set that includes missing data
#'
#' @param dtName Name of complete data set
#' @param dtMiss Name of missing data matrix
#' @param idvars Index variables that cannot be missing
#' @return A data table that represents observed data, including
#' missing data
#' @seealso \code{\link{defMiss}}, \code{\link{genMiss}}
#' @examples
#' def1 <- defData(varname = "m", dist = "binary", formula = .5)
#' def1 <- defData(def1, "u", dist = "binary", formula = .5)
#' def1 <- defData(def1, "x1", dist = "normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x2", dist = "normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x3", dist = "normal", formula = "20*m + 20*u", variance = 2)
#'
#' dtAct <- genData(1000, def1)
#'
#' defM <- defMiss(varname = "x1", formula = .15, logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x2", formula = ".05 + m * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x3", formula = ".05 + u * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed
#' defM
#'
#' # Generate missing data matrix
#'
#' missMat <- genMiss(dtAct, defM, idvars = "id")
#' missMat
#'
#' # Generate observed data from actual data and missing data matrix
#'
#' dtObs <- genObs(dtAct, missMat, idvars = "id")
#' dtObs
#' @export
#' @concept missing
genObs <- function(dtName, dtMiss, idvars) {
  if (missing(dtName)) {
    stop("Argument dtName is missing", call. = FALSE)
  }

  if (missing(dtMiss)) {
    stop("Argument dtMiss is missing", call. = FALSE)
  }

  if (missing(idvars)) {
    stop("Argument idvars is missing", call. = FALSE)
  }

  if (("period" %in% names(dtName)) & !("period" %in% idvars)) {
    idvars <- c(idvars, "period")
  }

  dtTemp <- dtName[, !idvars, with = FALSE]

  for (i in names(dtTemp)) {
    selectV <- as.vector(dtMiss[, i, with = FALSE] == 1)
    # dtTemp[selectV, i:= as.integer(NA), with = FALSE]  # old version - remove warning
    dtTemp[selectV, (i) := as.integer(NA)]
  }

  return(cbind(dtName[, idvars, with = FALSE], dtTemp))
}
