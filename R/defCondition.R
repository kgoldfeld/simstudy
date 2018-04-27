#' Add single row to definitions table of conditions that will be used to add data to an
#' existing definitions table
#'
#' @param dtDefs Name of definition table to be modified. Null if this is a new definition.
#' @param condition Formula specifying condition to be checked
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @return A data.table named dtName that is an updated data definitions table
#' @examples
#' # New data set
#'
#' def <- defData(varname = "x", dist = "noZeroPoisson", formula=5)
#' def <- defData(def, varname="y", dist="normal", formula=0, variance=9)
#'
#' dt <- genData(10, def)
#'
#' # Add columns to dt
#'
#' defC <- defCondition(condition = "x == 1", formula = "5 + 2*y",
#'                      variance = 1,dist = "normal")
#'
#' defC <- defCondition(defC, condition = "x <= 5 & x >= 2", formula = "3 - 2*y",
#'                      variance = 1, dist="normal")
#'
#' defC <- defCondition(defC, condition = "x >= 6", formula = 1,
#'                      variance = 1, dist="normal")
#'
#' defC
#'
#' # Add conditional column with field name "z"
#'
#' dt <- addCondition(defC, dt, "z")
#' dt
#' @export

defCondition<- function(dtDefs = NULL,
                        condition,
                        formula,
                        variance = 0,
                        dist = "normal",
                        link = "identity") {

  if (is.null(dtDefs)) {

    dtDefs <- data.table::data.table()

    # attr(dtDefs,"id") <- id

  }

  dt.new <- data.table::data.table(condition,
                                   formula,
                                   variance,
                                   dist,
                                   link)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  # attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])

}
