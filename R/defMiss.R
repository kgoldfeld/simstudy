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
#' def1 <- defData(def1, "x1", dist="normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x2", dist="normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x3", dist="normal", formula = "20*m + 20*u", variance = 2)
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

defMiss <- function(dtDefs = NULL,
                    varname,
                    formula,
                    logit.link = FALSE,
                    baseline=FALSE,
                    monotonic=FALSE) {

  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()
  }

  dt.new <- data.table::data.table(varname,
                                   formula,
                                   logit.link,
                                   baseline,
                                   monotonic)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)

  return(defNew[])

}
