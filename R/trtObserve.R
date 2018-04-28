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
#' @seealso \code{\link{trtAssign}}
#' @examples
#' def <- defData(varname = "male", dist = "binary", formula = .5 , id="cid")
#' def <- defData(def, varname = "over65", dist = "binary", formula = "-1.7 + .8*male", link="logit")
#' def <- defData(def, varname = "baseDBP", dist = "normal", formula = 70, variance = 40)
#'
#' dtstudy <- genData(1000, def)
#' dtstudy
#'
#' formula1 <- c("-2 + 2*male - .5*over65", "-1 + 2*male + .5*over65")
#' dtObs <- trtObserve(dtstudy, formulas = formula1, logit.link = TRUE, grpName = "exposure")
#' dtObs
#'
#' # Check actual distributions
#'
#' dtObs[, .(pctMale = round(mean(male),2)), keyby = exposure]
#' dtObs[, .(pctMale = round(mean(over65),2)), keyby = exposure]
#'
#' dtSum <- dtObs[, .N, keyby = .(male, over65, exposure)]
#' dtSum[, grpPct :=round(N/sum(N), 2), keyby = .(male, over65)]
#' dtSum
#' @export

trtObserve <- function(dt, formulas, logit.link = FALSE, grpName = "trtGrp") {

  if (missing(dt)) {
    stop("Data table argument is missing", call. = FALSE)
  }
  if (grpName %in% names(dt)) {
    stop("Group name has previously been defined in data table", call. = FALSE)
  }

  ncols = ncol(dt)

  ncat <- length(formulas)
  def = NULL

  for (i in 1:ncat) {
    def <- defDataAdd(def,
                   varname = paste0("e",i),
                   dist = "nonrandom",
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

  grpExp <- .Call(`_simstudy_matMultinom`, dtmatrix, PACKAGE = "simstudy")

  dtnew <- cbind(dt[, .SD, .SDcols = key(dt)], grpExp)
  data.table::setkeyv(dtnew, key(dt))

  dtnew <- dtnew[dt]

  if (length(formulas) == 1) dtnew[grpExp == 2, grpExp := 0]

  data.table::setnames(dtnew, "grpExp", grpName)

  return(dtnew[])

}
