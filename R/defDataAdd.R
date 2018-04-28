#' Add single row to definitions table that will be used to add data to an
#' existing data.table
#'
#' @param dtDefs Name of definition table to be modified. Null if this is a new definition.
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @return A data.table named dtName that is an updated data definitions table
#' @examples
#' # New data set
#'
#' def <- defData(varname = "xNr", dist = "nonrandom", formula=7, id = "idnum")
#' def <- defData(def, varname="xUni", dist="uniform", formula="10;20")
#'
#' dt <- genData(10, def)
#'
#' # Add columns to dt
#'
#' def2 <- defDataAdd(varname="y1", formula = 10, variance = 3)
#' def2 <- defDataAdd(def2, varname="y2", formula = .5, dist = "binary")
#' def2
#'
#' dt <- addColumns(def2, dt)
#' dt
#' @export

defDataAdd <- function(dtDefs = NULL,
                       varname,
                       formula,
                       variance = 0,
                       dist = "normal",
                       link = "identity") {

  if (is.null(dtDefs)) {

    dtDefs <- data.table::data.table()

    # attr(dtDefs,"id") <- id

  }

  dt.new <- data.table::data.table(varname,
                                   formula,
                                   variance,
                                   dist,
                                   link)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  # attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])

}
