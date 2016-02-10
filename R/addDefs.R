#' Add single row to definitions table
#'
#' @param dtDefs Definition data.table to be modified
#' @param varName test
#' @param formula test
#' @param variance test
#' @param dist test
#' @param link test
#' @param nTrt test
#' @param cMethod test
#' @param missType test
#' @return A data.table named dtName that is an updated data defnitions table
#' @export

addDefs <- function(dtDefs,
                    varname = "",
                    formula = "",
                    variance = 0,
                    dist = "Normal",
                    link = "identity",
                    nTrt = 0,
                    cMethod = "",
                    missType = "") {

  dt.new <- data.table::data.table(varname,
                                   formula,
                                   variance,
                                   dist,
                                   link,
                                   nTrt,
                                   cMethod,
                                   missType)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew)

}
