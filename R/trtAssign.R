#### Assign treatment ####

#' Assign treatment
#'
#' @param dtName data table
#' @param nTrt number of treatment groups
#' @param balanced indicator for treatment assignment process
#' @param strata vector of strings representing stratifying variables
#' @param grpName string representing variable name for treatment or
#' exposure group
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
#' @export

trtAssign <- function(dtName, nTrt = 2, balanced = TRUE,
                       strata = NULL, grpName = "trtGrp") {

  # 'declare' vars

  stratum = NULL
  id = NULL

  #

  if (missing(dtName)) {
    stop("Data table argument is missing", call. = FALSE)
  }
  if (grpName %in% names(dtName)) {
    stop("Group name has previously been defined in data table", call. = FALSE)
  }

  dt <- copy(dtName)

  if (balanced) {

    if (is.null(strata)) {
      dt[, stratum := 1]
    } else {
      dt <- addStrataCode(dt, strata)
    }

    nStrata = length(dt[,unique(stratum)])
    grpExp = data.table::data.table()

    for (i in (1 : nStrata)) {
      dts <- dt[stratum == i]
      data.table::setnames(dts, key(dts), "id")
      grpExps <- dts[,
                     list(id, grpExp = sample(rep(c(1 : nTrt),
                                               each = ceiling(nrow(dts) / nTrt)),
                                           nrow(dts),
                                           replace = FALSE)
                     )
      ]

      grpExp <- data.table::rbindlist(list(grpExp, grpExps))
    }

    data.table::setnames(grpExp, "id", key(dt))
    data.table::setkeyv(grpExp,key(dt))

    dt <- grpExp[dtName]

    if (nTrt==2) dt[grpExp == 2, grpExp := 0]

    data.table::setnames(dt, "grpExp", grpName)

  } else { # balanced is FALSE - strata are not relevant

    if (nTrt == 2) {
      formula <- .5
    } else {
      formula <- rep(1 / nTrt, nTrt)
    }

    dt <- trtObserve(dt, formulas = formula, logit.link = FALSE, grpName)

  }

  return(dt[])

}
