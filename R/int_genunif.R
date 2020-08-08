#### Uniform ####

# Internal function called by .generate - returns Uniform data
#
# @param n The number of observations required in the data set
# @param formula Two formulas (separated by ;) for min and max
# @param dtSim Incomplete simulated data set
# @return A data.frame column  with the updated simulated data

.genunif <- function(n, formula, dtSim) {
  if (!is.null(dtSim) && n != nrow(dtSim))
    stop("Length mismatch between 'n' and 'dtSim'")
  
  range <- .parseUnifFormula(formula, dtSim, n)
  
  return(stats::runif(n, range$min, range$max))
}

.parseUnifFormula <- function(formula, dtSim, n) {
  range <- .splitFormula(formula)
  
  if (length(range) != 2)
    stop(
      paste(
        "Formula for unifrom distributions must have",
        "the format: 'min;max'. See ?distributions"
      )
    )
  
  parsedRange <- .evalWith(range, .parseDotVars(range), dtSim, n)
  
    r_min <- parsedRange[, 1]
    r_max <- parsedRange[, 2]

  
  if (any(r_min == r_max)) {
    warning(
      paste0(
        "Formula warning: ",
        "'min' and 'max' are equal in ",
        sum(r_min == r_max),
        " rows.",
        "This results in all Data being the same!"
      )
    )
  }
  
  if (any(r_max < r_min))
    stop(paste0("Formula invalid: 'max' < 'min' in ", r_max < r_min, " rows."))
  
  list(min = r_min,max = r_max)
}
