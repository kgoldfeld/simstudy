#' Calling function to simulate data
#'
#' @param n the number of observations required in the data set.
#' @param dtDefs name of definitions data.table/data.frame. If no definitions are provided
#' a data set with ids only is generated.
#' @param id The string defining the id of the record
#' @return A data.table that contains the simulated data.
#' @export
#' @examples
#' genData(5)
#' genData(5, id = "grpID")
#'
#' def <- defData(varname = "xNr", dist = "nonrandom", formula=7, id = "idnum")
#' def <- defData(def, varname="xUni", dist="uniform", formula="10;20")
#' def <- defData(def, varname="xNorm", formula="xNr + xUni * 2", dist="normal", variance=8)
#' def <- defData(def, varname="xPois", dist="poisson", formula="xNr - 0.2 * xUni", link="log")
#' def <- defData(def, varname="xCat", formula = "0.3;0.2;0.5", dist="categorical")
#' def <- defData(def, varname="xGamma", dist="gamma", formula = "5+xCat", variance = 1, link = "log")
#' def <- defData(def, varname = "xBin", dist = "binary" , formula="-3 + xCat", link="logit")
#' def
#'
#' genData(5, def)
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

    idname = attr(dtDefs,"id")

    dfSimulate <- data.frame(c(1 : n))      # initialize simulated data with ids
    names(dfSimulate) <- attr(dtDefs, "id") # name first column attribute "id"
    iter <- nrow(dtDefs)       # generate a column of data for each row of dtDefs

    for (i in (1 : iter)) {
      dfSimulate <- .generate(dtDefs[i, ],n,dfSimulate, idname)
    }

    dt <- data.table::data.table(dfSimulate)
    data.table::setkeyv(dt, idname)


  }

  return(dt[])

}
