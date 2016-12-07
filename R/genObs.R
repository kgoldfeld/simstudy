#### Generate observed only data ####

#' Create an observed data set that includes missing data
#'
#' @param dtName Name of complete data set
#' @param dtMiss Name of missing data matrix
#' @param idvars Index variables that cannot be missing
#' @return A data table that represents observed data, including
#' missing data
#' @seealso \code{\link{defMiss}}, \code{\link{genMiss}}
#' @examples
#' def1 <- defData(varname = "m", dist = "binary", formula = .5)
#' def1 <- defData(def1, "u", dist = "binary", formula = .5)
#' def1 <- defData(def1, "x1", dist="normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x2", dist="normal", formula = "20*m + 20*u", variance = 2)
#' def1 <- defData(def1, "x3", dist="normal", formula = "20*m + 20*u", variance = 2)
#'
#' dtAct <- genData(1000, def1)
#'
#' defM <- defMiss(varname = "x1", formula = .15, logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x2", formula = ".05 + m * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "x3", formula = ".05 + u * 0.25", logit.link = FALSE)
#' defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed
#' defM
#'
#' # Generate missing data matrix
#'
#' missMat <- genMiss(dtAct, defM, idvars = "id")
#' missMat
#'
#' # Generate observed data from actual data and missing data matrix
#'
#' dtObs <- genObs(dtAct, missMat, idvars = "id")
#' dtObs
#' @export

genObs <- function(dtName, dtMiss, idvars) {

  if (missing(dtName)) {
    stop("Argument dtName is missing", call. = FALSE)
  }

  if (missing(dtMiss)) {
    stop("Argument dtMiss is missing", call. = FALSE)
  }

  if (missing(idvars)) {
    stop("Argument idvars is missing", call. = FALSE)
  }

  if (("period" %in% names(dtName)) & !("period" %in% idvars)) {
    idvars <- c(idvars, "period")
  }

  dtTemp <- dtName[, !idvars, with = FALSE]

  for (i in names(dtTemp)) {

    selectV <- as.vector(dtMiss[, i, with = FALSE] == 1)
    # dtTemp[selectV, i:= as.integer(NA), with = FALSE]  # old version - remove warning
    dtTemp[selectV, (i) := as.integer(NA)]
  }

  return(cbind(dtName[, idvars, with = FALSE], dtTemp))

}
