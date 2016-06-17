#### Generate survival data ####

#' Generate missing data
#'
#' @param dtName Name of complete data set
#' @param survDefs Definitions of survival
#' @return Original matrix with survival time
#' @export

genSurv <- function(dtName, survDefs) {

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
    newColumn <- dtSurv[, list(survx = round(- log(u) / ((1/scale) * exp(-survPred)) ^ (1 / shape),0)), ]

    dtSurv <- data.table::data.table(dtSurv, newColumn)

    data.table::setnames(dtSurv, "survx", as.character(survDefs[i,varname]))

  }

  return(dtSurv)

}
