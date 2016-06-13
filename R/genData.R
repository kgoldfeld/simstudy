#' Calling function to simulate data
#'
#' @param n the number of observations required in the data set.
#' @param dtDefs name of definitions data.table/data.frame. If no definitions are provided
#' a data set with ids only is generated.
#' @param id The string defining the id of the record
#' @return A data.table that contains the simulated data.
#' @export
#'

genData <- function(n, dtDefs = NULL, id = "id") {

  #### Check that arguments have been passed ####

  if (missing(n)) stop("argument 'n' is missing", call. = FALSE)

  ####

  if (is.null(dtDefs)) {

    dt <- data.table::data.table(x = 1:n)
    data.table::setnames(dt, id)
    data.table::setkeyv(dt, id)


  } else {  # existing definitions

    dfSimulate <- data.frame(c(1 : n))      # initialize simulated data with ids
    names(dfSimulate) <- attr(dtDefs, "id") # name first column attribute "id"
    iter <- nrow(dtDefs)       # generate a column of data for each row of dtDefs

    for (i in (1 : iter)) {
      dfSimulate <- generate(dtDefs[i, ],n,dfSimulate)
    }

    dt <- data.table::data.table(dfSimulate)
    data.table::setkeyv(dt,attr(dtDefs, "id"))


  }


  return(dt)

}
