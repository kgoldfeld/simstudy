#### Categorical ####

# Internal function called by generate - returns categorical data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities, each separated by ";"
# @param dfSim Incomplete simulated data set
# @return A data.frame column with the updated simulated data

gencat <- function(n, formula, dfSim) {

  # 'declare var

  V1 = NULL

  #

  pstr <- unlist(strsplit(as.character(formula),split=";", fixed=TRUE))
  idname <- names(dfSim)[1]

  dtSim <- data.table::data.table(dfSim)

  ps <- paste0(pstr, collapse=",")
  ps <- paste0("c(", ps, ")")

  nparam = length(pstr)

  # build command based on parameters "ps"

  cmd  <- quote(dtSim[ , x , keyby = y])
  mcmd <- quote(x %*% c(1:nparam)) # x is 2
  tcmd <- quote(t(x)) # x is 2
  pcmd <- quote(stats::rmultinom(1, 1, x))

  pcmd[[4]] <- parse(text=ps)[[1]]
  tcmd[[2]] <- pcmd
  mcmd[[2]] <- tcmd
  cmd[[4]]  <- mcmd
  cmd[[5]]  <- parse(text=idname)[[1]]

  # if (!all(apply(p,1,sum) == 1)) {
  #   stop("Sums for cumulative probabilities in categorical distribution not 1")
  # }

  new <- eval(cmd)[,V1]

  return(new)
}
