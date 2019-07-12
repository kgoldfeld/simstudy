#' Delete columns from existing data set
#'
#' @param dtOld Name of data table that is to be updated
#' @param vars Vector of column names (as strings)
#' @return An updated data.table that contains the added simulated data
#' @examples
#' # New data set
#'
#' def <- defData(varname = "x", dist = "noZeroPoisson", formula=7, id = "idnum")
#' def <- defData(def, varname="xUni", dist="uniformInt", formula="x-3;x+3")
#'
#' dt <- genData(10, def)
#' dt
#'
#' # Delete column
#'
#' dt <- delColumns(dt, "x")
#' dt
#' @export
#'

delColumns <- function(dtOld, vars) {

  #### Check that arguments have been passed ####

  if (missing(dtOld)) stop("argument 'dtOld' is missing", call. = FALSE)
  if (missing(vars)) stop("argument 'vars' is missing", call. = FALSE)

  #### Check that columns exist

  if (!(all(vars %in% names(dtOld)))) {
    stop(paste("All columns not in data table", deparse(substitute(dtOld))), call. = FALSE)
  }

  if (any(key(dtOld) %in% vars)) {
    stop(paste0("Cannot delete the index key"), call. = FALSE)
  }

  #### Remove columns

  return( dtOld[, -c(vars), with = FALSE] )

}



