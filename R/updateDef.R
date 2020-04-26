#### Update definition table ####

#' @title Update definition table
#' @description Updates row definition table created by function
#' defData or defRead. (For tables created using defDataAdd
#' and defReadAdd use updateDefAdd.)
#' @param dtDefs Definition table that will be modified
#' @param changevar Name of field definition that will be changed
#' @param newformula New formula definition (defaults to NULL)
#' @param newvariance New variance specification (defaults to NULL)
#' @param newdist New distribution definition (defaults to NULL)
#' @param newlink New link specification (defaults to NULL)
#' @param remove If set to TRUE, remove definition (defaults to FALSE)
#' @return A string that represents the desired formula
#' @examples
#'
#' # Example 1
#'
#' defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
#' defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
#' defs <- defData(defs, varname = "z", formula = "4 + 3*x - 2*y", variance = 1, dist = "normal")
#'
#' defs
#'
#' updateDef(dtDefs = defs, changevar = "y", newformula = "x + 5", newvariance = 2)
#' updateDef(dtDefs = defs, changevar = "z", newdist = "poisson", newlink = "log")
#'
#' # Example 2
#'
#' defs <- defData(varname = "w", formula = 0, variance = 3, dist = "normal")
#' defs <- defData(defs, varname = "x", formula = "1 + w", variance = 1, dist = "normal")
#' defs <- defData(defs, varname = "z", formula = 4, variance = 1, dist = "normal")
#'
#' defs
#'
#' updateDef(dtDefs = defs, changevar = "x", remove = TRUE)
#' updateDef(dtDefs = defs, changevar = "z", remove = TRUE)
#'
#' @export

updateDef <- function(dtDefs, changevar, newformula = NULL,
                      newvariance = NULL, newdist = NULL, newlink = NULL,
                      remove = FALSE) {

  # "declares" to avoid global NOTE

  formula <- NULL
  variance <- NULL
  dist <- NULL
  link <- NULL
  varname <- NULL

  # Check args

  if (!exists(deparse(substitute(dtDefs)), envir = parent.frame())) {
    stop("Data definition does not exist.")
  }

  if (!(changevar %in% dtDefs[, varname])) {
    stop(paste("Variable" , changevar, "not in definition table"))
  }

  # checks completed

  xdef <- copy(dtDefs)
  rowvar <- which(changevar == xdef$varname)

  if (!is.null(newformula)) {
    xdef[rowvar, formula := as.character(newformula) ]
  }

  if (!is.null(newvariance)) {
    xdef[rowvar, variance := newvariance ]
  }

  if (!is.null(newdist)) {
    xdef[rowvar, dist := newdist ]
  }

  if (!is.null(newlink)) {
    xdef[rowvar, link := link ]
  }

  if (remove) {
    xdef <- xdef[-rowvar, ]
  }

  # Check table to make sure references work after update

  if (!remove) { # check changed row

    prevVars <- xdef$varname[1 : (rowvar - 1)]
    if (rowvar == 1) prevVars = ""

    .evalDef(newvar = xdef[rowvar, varname],newform= xdef[rowvar, formula], newdist = xdef[rowvar, dist],defVars = prevVars)

  } else if (remove & (rowvar <= nrow(xdef)) ){  # check all rows after deleted row

      for (i in (rowvar : nrow(xdef))) {

        if (i == 1) prevVars = ""
        else prevVars <- xdef$varname[1 : (i - 1)]

        .evalDef(newvar = xdef[i, varname], newform = xdef[i,formula],newdist =  xdef[i,dist],defVars =  prevVars)

    }
  }

  return(xdef)

}
