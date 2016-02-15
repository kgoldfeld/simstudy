#### Assign treatment ####

#' Assign treatment
#'
#' @param dt data table
#' @param nTrt number of treatment groups
#' @param allocEqual indicator for treatment assignment process
#' @param strata vector of strings representing strativying variables
#' @param grpName string representing variable name for treatment or
#' exposure group
#' @return An integer (group) ranging from 1 to length of the
#' probability vector
#' @export

trtAssign <- function(dtx, n = 2, allocEqual = TRUE,
                       strata = NULL, grpName = "trtGrp") {

  dt <- copy(dtx)

  if (allocEqual) {

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
      grpExps <- dts[, .(id, grpExp = sample(rep(c(1 : n), each = ceiling(nrow(dts) / n)),
                                             nrow(dts),
                                             replace = FALSE)
      )
      ]
      grpExp <- data.table::rbindlist(list(grpExp, grpExps))
    }

    data.table::setnames(grpExp, "id", key(dt))
    data.table::setkeyv(grpExp,key(dt))

    dt <- grpExp[dtx]
    data.table::setnames(dt, "grpExp", grpName)

  } else { # allocEqual is FALSE - strata are not relevant

    dt <- trtObserve(dt, formulas = rep(1 / n, n), logit.link = FALSE, grpName)

  }

  return(dt)

}
