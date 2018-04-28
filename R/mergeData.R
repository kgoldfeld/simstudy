#### Merge two data tables - more general ####

#' Merge two data tables
#'
#' @param dt1 Name of first data.table
#' @param dt2 Name of second data.table
#' @param idvars Vector of string names to merge on
#' @return A new data table that merges dt2 with dt1
#' @examples
#' def1 <- defData(varname="x", formula = 0, variance = 1)
#' def1 <- defData(varname="xcat", formula = ".3;.2", dist = "categorical")
#'
#' def2 <- defData(varname="yBin", formula = 0.5, dist = "binary", id="xcat")
#' def2 <- defData(def2, varname="yNorm", formula = 5, variance = 2)
#'
#' dt1 <- genData(20, def1)
#' dt2 <- genData(3, def2)
#'
#' dtMerge <- mergeData(dt1, dt2, "xcat")
#' dtMerge
#' @export

mergeData <- function(dt1, dt2, idvars) {

  oldkey = data.table::key(dt1)

  setkeyv(dt1, idvars)
  setkeyv(dt2, idvars)

  dtmerge <- dt1[dt2]
  data.table::setkeyv(dtmerge, oldkey)

  return(dtmerge[])

}
