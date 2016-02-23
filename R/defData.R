#' Add single row to definitions table
#'
#' @useDynLib simstudy
#' @importFrom Rcpp sourceCpp
#'
#' @param dtDefs Definition data.table to be modified
#' @param varName test
#' @param formula test
#' @param variance test
#' @param dist test
#' @param link test
#' @param id test
#' @return A data.table named dtName that is an updated data defnitions table
#' @export

defData <- function(dtDefs = NULL,
                    varname = "",
                    formula = "",
                    variance = 0,
                    dist = "Normal",
                    link = "identity",
                    id="id") {

  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()
    attr(dtDefs,"id") <- id
  }

  dt.new <- data.table::data.table(varname,
                                   formula,
                                   variance,
                                   dist,
                                   link)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew)

}
