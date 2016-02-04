#' Calling function to simulate data
#'
#' @param dtDefs name of definitions data.table/data.frame
#' @param n the number (integer) of observations required in the data set
#' @return A data.table that contains the simulated data
#' @export
#'

gendt <- function(dtDefs, n) {

  dfSimulate <- data.frame(c(1:n))     # initialize simulated data with ids
  names(dfSimulate) <- dtDefs[1, id]   # name first column the value of "id"
  dtDefs <- dtDefs[-1, ]           # first row of data defs is no longer needed
  iter <- nrow(dtDefs)       # generate a column of data for each row of dtDefs

  for (i in (1:iter)) {
    dfSimulate <- generate(dtDefs[i, ],n,dfSimulate)
  }

  return(data.table::data.table(dfSimulate))

}
