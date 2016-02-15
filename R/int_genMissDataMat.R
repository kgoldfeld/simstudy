#### Generate missing data matrix ####

#' Internal function called by genMiss - returns a missing data matrix
#'
#' @param dtName Name of complete data set
#' @param dtTemp Name of data set with unique ids only
#' @param idvars To be filled in
#' @param missDefs To be filled in
#' @return A missing data matrix of 0/1, where 1 indicates missing

genMissDataMat <- function(dtName, dtTemp, idvars, missDefs) {

  dtMissP <- dtTemp[, idvars, with = FALSE]

  Expression <- parse(text = as.character(missDefs[, varname]))
  Formula <- parse(text = as.character(missDefs[, formula]))

  if (missDefs[, link] != "Logit") {
    dtMissP[, eval(Expression) := dtName[, eval(Formula)]]
  } else {
    dtMissP[, eval(Expression) := dtName[, loProb(eval(Formula))]]
  }
  matMiss <- dtMissP[, idvars, with = FALSE]
  matMiss[, eval(Expression) := rbinom(nrow(dtMissP), 1,
                                       dtMissP[, eval(Expression)])]

  return(matMiss)

}
