#### Generate observed only data ####

#' Create an observed data set that includes missing data
#'
#' @param dtName Name of complete data set
#' @param dtMiss Name of missing data matrix
#' @param idvars Index variables that cannot be missing
#' @return A data table that represents observed data, including
#' missing data
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
    dtTemp[selectV, i:=NA, with = FALSE]
  }

  return(cbind(dtName[, idvars, with = FALSE], dtTemp))

}
