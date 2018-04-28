#### Update definition table - defDataAdd version ####

#' @title Update definition table
#' @description Updates row definition table created by functions
#' defDataAdd and defReadAdd. (For tables created using defData
#' or defRead use updateDef.)
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
#' # Define original data
#'
#' defs <- defData(varname = "w", formula = 0, variance = 3, dist = "normal")
#' defs <- defData(defs, varname = "x", formula = "1 + w", variance = 1, dist = "normal")
#' defs <- defData(defs, varname = "z", formula = 4, variance = 1, dist = "normal")
#'
#' # Define additional columns
#'
#' defsA <- defDataAdd(varname = "a", formula = "w + x + z", variance = 2, dist = "normal")
#'
#' set.seed(2001)
#' dt <- genData(10, defs)
#' dt <- addColumns(defsA, dt)
#' dt
#'
#' # Modify definition of additional column
#'
#' defsA <- updateDefAdd(dtDefs = defsA, changevar = "a", newformula = "w+z", newvariance = 1)
#'
#' set.seed(2001)
#' dt <- genData(10, defs)
#' dt <- addColumns(defsA, dt)
#' dt
#'
#' @export

updateDefAdd <- function(dtDefs, changevar, newformula = NULL,
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
    xdef[rowvar, formula := newformula ]
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

  return(xdef)

}
