#### Generate missing data ####

#' Generate missing data
#'
#' @param dtName Name of complete data set
#' @param missDefs Definitions of missingness
#' @param idvars Index variables
#' @param repeated Indicator for longitudinal data
#' @param periodvar Name of variable that contains period
#' @return Missing data matrix indexed by idvars (and period if relevant)
#' @export

genMiss <- function(dtName, missDefs, idvars,
                    repeated = FALSE, periodvar = "period") {

  # 'declare'

  varname = NULL
  period = NULL
  baseline = NULL
  monotonic = NULL
  fmiss = NULL

  #

  setkeyv(dtName, idvars)

  if (! repeated) {

    dtMiss <- dtName[, c(idvars), with = FALSE]
    names(dtMiss) <- c(idvars)

    for (i in (1 : nrow(missDefs))) {
      dtTemp = copy(dtName)
      mat1 <- genMissDataMat(dtName, dtTemp, idvars, missDefs[i,])
      vec1 <- mat1[, missDefs[i, varname], with = FALSE]

      dtMiss <- cbind(dtMiss, vec1)

    }

  } else { # repeated

    dtMiss <- dtName[, c(idvars, periodvar), with = FALSE]
    names(dtMiss) <- c(idvars, "period")

    nPeriods <- dtMiss[,max(period)] + 1

    for (i in (1 : nrow(missDefs))) {

      if (missDefs[i, baseline]) {

        dtTemp <- dtName[period == 0]
        mat1 <- genMissDataMat(dtName[period == 0], dtTemp, idvars, missDefs[i,])
        vec1 <- addPeriods(mat1, nPeriods, idvars)[, missDefs[i, varname],
                                                   with=FALSE]

        dtMiss <- cbind(dtMiss, vec1)

      } else { # not just baseline can be missing

        dtTemp = copy(dtName)
        mat1 <- genMissDataMat(dtName, dtTemp, idvars, missDefs[i,])
        vec1 <- mat1[, missDefs[i, varname], with = FALSE]
        dtMiss <- cbind(dtMiss, vec1)

        if (missDefs[i, monotonic]) { # monotonic missing

          dt.fmiss <- dtMiss[eval(parse(text=missDefs[i, varname])) == 1, list(fmiss = min(period)), keyby = eval(idvars)]
          data.table::setkeyv(dtMiss, idvars)
          dtMiss <- dt.fmiss[dtMiss]
          dtMiss[period > fmiss, eval(missDefs[i, varname]) := 1]
          dtMiss[,fmiss := NULL]

        }
      }
    }
  }

  dims <- dim(dtName[, !names(dtMiss), with = FALSE])
  addon <- data.table(matrix(0, nrow = dims[1], ncol = dims[2]))
  names(addon) <- names(dtName[, !names(dtMiss), with = FALSE])


  return(cbind(dtMiss, addon))

}
