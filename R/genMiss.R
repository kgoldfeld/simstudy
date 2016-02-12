#### Generate missing data ####

#' Generate missing data
#'
#' @param dtName Name of complete data set
#' @param missDefs Definitions of missingness
#' @param idvars Index variables
#' @param nPeriods Number of periods (if longitudinal data)
#' @param periodvar Name of variable that contains period
#' @return Missing data matrix indexed by idvars (and period if relevant)
#' @export

genMiss <- function(dtName, missDefs, idvars, nPeriods = 0, periodvar = "") {

  setkeyv(dtName, idvars)

  if (periodvar == "") {

    dtMiss <- dtName[, c(idvars), with = FALSE]
    names(dtMiss) <- c(idvars)

  } else {

    dtMiss <- dtName[, c(idvars, periodvar), with = FALSE]
    names(dtMiss) <- c(idvars, "period")

  }

  for (i in (1 : nrow(missDefs))) {

    if (missDefs[i, missType] == "baseline") {

      if (periodvar != "") {

        dtTemp <- dtName[period == 0]
        mat1 <- genMissDataMat(dtName, dtTemp, idvars, missDefs[i,])
        vec1 <- addPeriods(mat1, nPeriods, idvars)[, missDefs[i, varname],
                                                   with=FALSE]

      } else {

        dtTemp = copy(dtName)
        mat1 <- genMissDataMat(dtName, dtTemp, idvars, missDefs[i,])
        vec1 <- mat1[, missDefs[i, varname], with = FALSE]

      }

      dtMiss <- cbind(dtMiss, vec1)

    } else { # time dependent variables

      dtTemp = copy(dtName)
      mat1 <- genMissDataMat(dtName, dtTemp, idvars, missDefs[i,])
      vec1 <- mat1[, missDefs[i, varname], with = FALSE]
      dtMiss <- cbind(dtMiss, vec1)

      if (missDefs[i, missType] == "mono") { # monotonic missing

        dt.fmiss <- dtMiss[y == 1, .(fmiss = min(period)), keyby = eval(idvars)]
        data.table::setkeyv(dtMiss, idvars)
        dtMiss <- dt.fmiss[dtMiss]
        dtMiss[period > fmiss, eval(missDefs[i, varname]) := 1]
        dtMiss[,fmiss := NULL]

      }
    }
  }

  dims <- dim(dtName[, !names(dtMiss), with = FALSE])
  addon <- data.table(matrix(0, nrow = dims[1], ncol = dims[2]))
  names(addon) <- names(dtName[, !names(dtMiss), with = FALSE])


  return(cbind(dtMiss, addon))

}
