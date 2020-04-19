#' Add a single column to existing data set based on a condition
#'
#' @param condDefs Name of definitions for added column
#' @param dtOld Name of data table that is to be updated
#' @param newvar Name of new column to add
#'
#' @return An updated data.table that contains the added simulated data
#' @examples
#'
#' # New data set
#'
#' def <- defData(varname = "x", dist = "categorical", formula = ".33;.33")
#' def <- defData(def, varname="y", dist="uniform", formula="-5;5")
#'
#' dt <- genData(1000, def)
#'
#' # Define conditions
#'
#' defC <- defCondition(condition = "x == 1", formula = "5 + 2*y-.5*y^2",
#'                      variance = 1,dist = "normal")
#' defC <- defCondition(defC, condition = "x == 2",
#'                      formula = "3 - 3*y + y^2", variance = 2, dist="normal")
#' defC <- defCondition(defC, condition = "x == 3",
#'                      formula = "abs(y)", dist="poisson")
#'
#' # Add column
#'
#' dt <- addCondition(defC, dt, "NewVar")
#'
#' # Plot data
#'
#' library(ggplot2)
#'
#' ggplot(data = dt, aes(x=y, y=NewVar, group = x)) +
#'   geom_point(aes(color = factor(x)))
#'
#' @export
#'


addCondition <- function(condDefs, dtOld, newvar) {

  # 'declare' vars

  varname = NULL
  formula = NULL
  dist = NULL

  # Checks

  if (missing(condDefs)) stop("argument 'condDefs' is missing", call. = FALSE)
  if (missing(dtOld)) stop("argument 'dtOld' is missing", call. = FALSE)
  if (missing(newvar)) stop("argument 'newvar' is missing", call. = FALSE)
        
  if (!exists(deparse(substitute(condDefs)), envir = parent.frame())) {
    stop(paste("definitions", deparse(substitute(condDefs)), "not found"), call. = FALSE)
  }

  if (!exists(deparse(substitute(dtOld)), envir = parent.frame())) {
    stop(paste("data table", deparse(substitute(dtOld)), "not found"), call. = FALSE)
  }

  cDefs <- copy(condDefs)
  cDefs[, varname := newvar]

  chkVars <- names(dtOld)

  # Check to make sure both formulas are appropriate and reference valid data

  for (i in 1:nrow(condDefs)) {

    .evalDef(newvar = newvar,newform =  cDefs[i,formula], newdist = cDefs[i,dist],defVars =  chkVars)
    .evalDef(newvar = newvar, newform = cDefs[i,condition],newdist =  "nonrandom",defVars =  chkVars)

  }

  oldkey <- data.table::key(dtOld)

  iter <- nrow(cDefs)

  dtNew <- data.table()
  dtTemp <- data.table()

  # Loop through each condition

  for (i in (1:iter)) {

    condition <- cDefs[,condition][i]
    formula <- cDefs[,formula][i]

    dtTemp <- dtOld[eval(parse(text = condition) )]
    n = nrow(dtTemp)
    
    if (n > 0) {
      
      dtTemp <- .generate(cDefs[i,], n, dtTemp, oldkey)
      
      dtTemp <- data.table::data.table(dtTemp)
      dtTemp <- dtTemp[, list(get(oldkey), get(newvar))]
      
      dtNew <- rbind(dtNew, dtTemp) 
      
    }
  }

  setnames(dtNew, c(oldkey, newvar))
  data.table::setkeyv(dtNew, oldkey)

  dtNew <- dtNew[dtOld]

  return(dtNew)

}


