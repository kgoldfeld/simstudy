#' Add single row to survival definitions
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Variable name
#' @param formula Covariates predicting survival
#' @param scale Scale parameter for the Weibull distribution.
#' @param shape The shape of the Weibull distribution. Shape = 1 for
#' an exponential distribution
#' @return A data.table named dtName that is an updated data definitions table
#' @examples
#' # Baseline data definitions
#'
#' def <- defData(varname = "x1", formula = .5, dist = "binary")
#' def <- defData(def,varname = "x2", formula = .5, dist = "binary")
#' def <- defData(def,varname = "grp", formula = .5, dist = "binary")
#'
#' # Survival data definitions
#'
#' sdef <- defSurv(varname = "survTime", formula = "1.5*x1",
#'                 scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5")
#'
#' sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)
#'
#' sdef
#'
#' # Baseline data definitions
#'
#' dtSurv <- genData(300, def)
#'
#' # Add survival times
#'
#' dtSurv <- genSurv(dtSurv, sdef)
#'
#' head(dtSurv)
#'
#' @export

defSurv <- function(dtDefs = NULL,
                    varname,
                    formula = 0,
                    scale,
                    shape = 1) {

  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()
  }

  dt.new <- data.table::data.table(varname,
                                   formula,
                                   scale,
                                   shape)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)

  return(defNew[])

}
