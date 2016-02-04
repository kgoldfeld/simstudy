#' Add single row to definitions table
#'
#' @param dtDefs Definition data.table to be modified
#' @param id Name of field that contains idnetifier
#' @param varName test
#' @param formula test
#' @param variance test
#' @param min test
#' @param max test
#' @param dist test
#' @param link test
#' @param cMethod test
#' @param nIperC test
#' @param nClust test
#' @param nTrt test
#' @param varcat test
#' @param missType test
#' @return A data.table named dtName that is an updated data defnitions table
#' @export


add.defs = function(dtDefs,
                    id = "",
                    varname = "",
                    formula = "",
                    variance = 0,
                    min = 0,
                    max = 0,
                    dist = "Normal",
                    link = "identity",
                    cMethod = "",
                    nIperC = 0,
                    nClust = 0,
                    nTrt = 0,
                    varcat = "",
                    missType = "") {
  
  dt.new <- data.table::data.table(id,
                                   varname,
                                   formula,
                                   variance,
                                   min,
                                   max,
                                   dist,
                                   link,
                                   cMethod,
                                   nIperC,
                                   nClust,
                                   nTrt,
                                   varcat,
                                   missType)
  
  l = list(dtDefs,dt.new)
  
  data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  
}
