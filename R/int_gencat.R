#### Categorical ####

# Internal function called by .generate - returns categorical data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities, each separated by ";"
# @param dfSim Incomplete simulated data set
# @param idkey Key of incomplete data set
# @return A data.frame column with the updated simulated data
.gencat <- function(n, formula, link, dfSim) {
  formulas <- .splitFormula(formula)
  
  if (length(formulas) < 2)
    stop(paste0(
      "The formula for 'categorical' must contain atleast",
      " two probabilities."
    ))
  
  parsedProbs <-
    .evalWith(formulas, .parseDotVars(formulas), dfSim, n)
  
  if (link == "logit") {
    parsedProbs <- exp(parsedProbs)
    parsedProbs <- parsedProbs  / (1 + rowSums(parsedProbs))
  } else {
    parsedProbs <- .adjustProbs(parsedProbs)
  }
  
  parsedProbs <- cbind(parsedProbs, 1 - rowSums(parsedProbs))
  
  .Call(`_simstudy_matMultinom`, parsedProbs, PACKAGE = "simstudy")
}