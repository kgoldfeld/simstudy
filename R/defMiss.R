#' Add single row to definitions table
#'
#' @param dtDefs Definition data.table to be modified
#' @param varName test
#' @param formula test
#' @param logit.link test
#' @param missType test
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
