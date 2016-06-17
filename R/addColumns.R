#' Add columns to existing data set
#'
#' @param dtDefs Name of definitions for added columns
#' @param dtOld Name of data table that is to be updated
#' @return An updated data.table that contains the added simulated data
#' @export
#'

addColumns <- function(dtDefs,dtOld) {

  # "declares" varname to avoid global NOTE

  varname = NULL
  formula = NULL
  dist = NULL

  #

  for (i in 1:nrow(dtDefs)) {
    if (i == 1) {
      chkVars <- names(dtOld)

    } else { # check all previously defined vars

      chkVars <- c(dtDefs[1:(i-1), varname] , names(dtOld))
    }

    evalDef(dtDefs[i, varname], dtDefs[i,formula], dtDefs[i,dist], chkVars)
  }

  oldkey <- data.table::key(dtOld)

  iter = nrow(dtDefs)
  n = nrow(dtOld)
  for (i in (1 : iter)) {
    dtOld <- generate(dtDefs[i,], n, dtOld)
  }

  dtOld <- data.table::data.table(dtOld)
  data.table::setkeyv(dtOld, oldkey)

  return(dtOld)

}

