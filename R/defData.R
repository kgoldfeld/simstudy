#' Add single row to definitions table
#'
#' @useDynLib simstudy
#' @importFrom Rcpp sourceCpp
#' @import data.table
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @param id A string indicating the field name for the unique record identifier
#' @return A data.table named dtName that is an updated data defnitions table
#' @details The possible data distributions include ""normal", "poisson",
#' "noZeroPoisson", "binary", "uniform", "categorical", "gamma", and "nonrandom."
#' @export

defData <- function(dtDefs = NULL,
                    varname,
                    formula,
                    variance = 0,
                    dist = "normal",
                    link = "identity",
                    id="id") {

  #### Check that arguments have been passed ####

  if (missing(varname)) stop("argument 'varname' is missing", call. = FALSE)
  if (missing(formula)) stop("argument 'formula' is missing", call. = FALSE)

  #### No missing arguments

  if (is.null(dtDefs)) {

    if (is.na(as.numeric(formula))) {
      stop("First defined formula must be numeric", call. = FALSE)
    }

    dtDefs <- data.table::data.table()
    attr(dtDefs,"id") <- id

  } else {

    evalDef(varname, formula, dist, dtDefs[,varname])
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
