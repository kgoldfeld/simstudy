#### Observe exposure/treatment ####

#' Observed exposure or treatment
#'
#' @param dt data table
#' @param formulas collection of formulas that determine probabilities
#' @param logit.link indicator that specifies link. If TRUE, then logit link
#' is used. If FALSE, the identity link is used.
#' @param grpName character string representing name of treatment/exposure group
#' variable
#' @return An integer (group) ranging from 1 to length of the probability vector
#' @export

trtObserve <- function(dt, formulas, logit.link = FALSE, grpName = "trtGrp") {

  ncols = ncol(dt)

  ncat <- length(formulas)
  def = NULL

  for (i in 1:ncat) {
    def <- defData(def,
                   varname = paste0("e",i),
                   dist = "Nonrandom",
                   formula = formulas[i]
    )
  }

  dtnew <-addColumns(def, dt)

  dtmatrix <- as.matrix(dtnew[,
                              .SD,
                              .SDcols = c((ncols + 1) : (ncols + ncat))])

  if (logit.link) {
    dtmatrix <- exp(dtmatrix)
    dtmatrix <- dtmatrix  / (1 + apply(dtmatrix, 1, sum))
  }

  dtmatrix <- cbind(dtmatrix, 1 - apply(dtmatrix, 1, sum))

  grpExp <- matMultinom(dtmatrix)

  dtnew <- cbind(dt[, .SD, .SDcols = key(dt)], grpExp)
  data.table::setkeyv(dtnew, key(dt))

  dtnew <- dtnew[dt]
  data.table::setnames(dtnew, "grpExp", grpName)

  dtnew

}
