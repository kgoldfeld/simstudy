#' Add single row to definitions table that will be used to add data to an
#' existing data.table
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @return A data.table named dtName that is an updated data defnitions table
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

  return(defNew)

}
