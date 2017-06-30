#### Categorical ####

# Internal function called by generate - returns categorical data
#
# @param n The number of observations required in the data set
# @param formula String that specifies the probabilities, each separated by ";"
# @param dfSim Incomplete simulated data set
# @param idkey Key of incomplete data set
# @return A data.frame column with the updated simulated data


gencat <- function(n, formula, link, dfSim) {

    # parse formula

    pstr <- unlist(strsplit(as.character(formula), split = ";", fixed = TRUE))

    dtSim <- data.table::data.table(dfSim)

    # create matrix of probabilities

    ncols = ncol(dtSim)

    ncat <- length(pstr)
    deftemp = NULL

    for (i in 1:ncat) {
      deftemp <- defDataAdd(deftemp,
                        varname = paste0("e",i),
                        dist = "nonrandom",
                        formula = pstr[i]
      )
    }

    dtnew <-addColumns(deftemp, dtSim)

    dtmatrix <- as.matrix(dtnew[,
                                .SD,
                                .SDcols = c((ncols + 1) : (ncols + ncat))])

    if (link == "logit") {
     dtmatrix <- exp(dtmatrix)
     dtmatrix <- dtmatrix  / (1 + rowSums(dtmatrix))
    }

    dtmatrix <- cbind(dtmatrix, 1 - rowSums(dtmatrix))

    # generate random numbers

    newColumn <- .Call(simstudy_matMultinom, dtmatrix, PACKAGE = "simstudy")

    return(newColumn)

}
