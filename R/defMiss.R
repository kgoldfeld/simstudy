#' Add single row to definitions table for missing data
#'
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
#' @return A data.table named dtName that is an updated data defnitions table
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

  return(defNew)

}
