#' Add single row to definitions table
#'
#' @useDynLib simstudy, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import data.table
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @param id A string indicating the field name for the unique record identifier
#' @return A data.table named dtName that is an updated data definitions table
#' @details The possible data distributions include "normal", "poisson", 
#' "noZeroPoisson", "negBinomial" "binary", "binomial", "beta", "uniform", 
#' "uniformInt", "categorical", "gamma", "exponential", 
#' "mixture", and "nonrandom."
#' 
#' @examples
#' def <- defData(varname = "xNr", dist = "nonrandom", formula=7, id = "idnum")
#' def <- defData(def, varname="xUni", dist="uniform", formula="10;20")
#' def <- defData(def, varname="xNorm", formula="xNr + xUni * 2", dist="normal", variance=8)
#' def <- defData(def, varname="xPois", dist="poisson", formula="xNr - 0.2 * xUni", link="log")
#' def <- defData(def, varname="xCat", formula = "0.3;0.2;0.5", dist="categorical")
#' def <- defData(def, varname="xGamma", dist="gamma", formula = "5+xCat", variance = 1, link = "log")
#' def <- defData(def, varname = "xBin", dist = "binary" , formula="-3 + xCat", link="logit")
#' def
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

  if (is.null(dtDefs)) { # checking that initial formula has no variables ...

    dtDefs <- data.table::data.table()
    attr(dtDefs,"id") <- id
    defVars <- ""

  } else {
    defVars <- dtDefs[,varname]
  }

  .evalDef(varname, formula, dist, variance, link, defVars)
  

  dt.new <- data.table::data.table(varname,
                                   formula,
                                   variance,
                                   dist,
                                   link)

  l = list(dtDefs,dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])

}
