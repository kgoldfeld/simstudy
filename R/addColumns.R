#' Add columns to existing data set
#'
#' @param dtDefs name of definitions for added columns
#' @param dtOld name of data table that is to be updated
#' @return an updated data.table that contains the added simulated data
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

    .evalDef(newvar = dtDefs[i, varname],newform =  dtDefs[i,formula], newdist = dtDefs[i,dist], defVars =chkVars)
  }

  oldkey <- data.table::key(dtOld)

  iter = nrow(dtDefs)
  n = nrow(dtOld)
  for (i in (1 : iter)) {
    dtOld <- .generate(dtDefs[i,], n, dtOld, oldkey)
  }

  dtOld <- data.table::data.table(dtOld)
  data.table::setkeyv(dtOld, oldkey)

  return(dtOld[])

}

