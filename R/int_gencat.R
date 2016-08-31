#### Categorical ####

# Internal function called by generate - returns categorical data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities, each separated by ";"
# @param dfSim Incomplete simulated data set
# @return A data.frame column with the updated simulated data

gencat <- function(n, formula, dfSim) {

    # parse formula

    pstr <- unlist(strsplit(as.character(formula), split = ";", fixed = TRUE))

    print("Generating categories")

    # build command and parameters

    cmd <- substitute(
      (t(stats::rmultinom(nsamp, 1, probs)) %*% c(1:nparam))[,1],
      list(nsamp  = n,
           probs  = as.numeric(pstr),
           nparam = length(pstr))
    )

    # evaluate and return

    return(eval(cmd))
}
