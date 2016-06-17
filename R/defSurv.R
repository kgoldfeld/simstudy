#' Add single row to survival definitions
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Variable name
#' @param formula Covariates predicting survival
#' @param scale Scale parameter for the Weibull distribution.
#' @param shape The shape of the Weibull distribution. Shape = 1 for
#' an exponential distriubtion
#' @return A data.table named dtName that is an updated data defnitions table
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

  return(defNew)

}
