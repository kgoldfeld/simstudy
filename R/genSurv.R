#### Generate survival data ####

#' @title Generate survival data
#' @description Survival data is added to an existing data set.
#' @param dtName Name of complete data set
#' @param survDefs Definitions of survival
#' @param digits Number of digits for rounding
#' @return Original matrix with survival time
#' @examples
#' # Baseline data definitions
#'
#' def <- defData(varname = "x1", formula = .5, dist = "binary")
#' def <- defData(def,varname = "x2", formula = .5, dist = "binary")
#' def <- defData(def,varname = "grp", formula = .5, dist = "binary")
#'
#' # Survival data definitions
#'
#' sdef <- defSurv(varname = "survTime", formula = "1.5*x1",
#'                 scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5")
#'
#' sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)
#'
#' sdef
#'
#' # Baseline data definitions
#'
#' dtSurv <- genData(300, def)
#'
#' # Add survival times
#'
#' dtSurv <- genSurv(dtSurv, sdef)
#'
#' head(dtSurv)
#'
#' @export

genSurv <- function(dtName, survDefs, digits = 3) {

  # 'declare

  varname = NULL
  formula = NULL

  #

  dtSurv = copy(dtName)

  for (i in (1 : nrow(survDefs))) {

    shape = dtSurv[, eval(parse(text = survDefs[i, shape]))]
    scale = dtSurv[, eval(parse(text = survDefs[i, scale]))]
    survPred = dtSurv[, eval(parse(text = survDefs[i, formula]))]

    u <- stats::runif(n = nrow(dtSurv))

    newColumn <- dtSurv[, list(survx = round( (- (log(u) / ((1/scale) * exp(survPred)))) ^ (shape), digits) ), ]

    dtSurv <- data.table::data.table(dtSurv, newColumn)

    data.table::setnames(dtSurv, "survx", as.character(survDefs[i,varname]))

  }

  return(dtSurv[])

}
