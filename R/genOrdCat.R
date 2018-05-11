#### Generate ordinal categorical data ####

#' @title Generate ordinal categorical data
#' @description Ordinal categorical data is added to an existing data set.
#' @param dtName Name of complete data set
#' @param adjVar Adjustment variable  name in dtName - determines
#' logistic shift. This is specified assuming a cumulative logit
#' link.
#' @param baseprobs Baseline probability expressed as a vector of
#' probabilities. The values must sum to <= 1. If sum(baseprobs) < 1,
#' an additional category is added with probability 1 - sum(baseprobs).
#' @param catVar Name of the new categorical field. Defaults to "cat"
#' @param asFactor If asFactor == TRUE (default), new field is returned
#' as a factor. If asFactor == FALSE, new field is returned as an integer.
#' @return Original matrix with added categorical field
#' @examples
#' #### Set definitions
#'
#' def1 <- defData(varname = "male", formula = 0.45, dist = "binary", id = "idG")
#' def1 <- defData(def1, varname = "z", formula = "1.2*male", dist = "nonrandom")
#'
#' #### Generate data
#'
#' set.seed(20)
#'
#' dx <- genData(1000, def1)
#'
#' probs<-c(0.40, 0.25, 0.15)
#' dx <- genOrdCat(dx, adjVar = "z", probs, catVar = "grp")
#'
#' @export

genOrdCat <- function(dtName, adjVar, baseprobs, catVar = "cat", asFactor = TRUE) {

  # "declares" to avoid global NOTE

  cat <- NULL

  # Check arguments

  if (!exists(deparse(substitute(dtName)),  envir = parent.frame())) {
    stop("Data table does not exist.")
  }

  if (!adjVar %in% names(dtName)) {
    stop(paste0("Variable ", adjVar, " not in data.table"))
  }

  if (!is.character(catVar)) {
    stop("catVar must be a string")
  }

  # Check probability vector

  if (is.null(baseprobs)) {
    stop("Proability vector is empty")
  }

  if (sum(baseprobs) > 1 | sum(baseprobs) <= 0) {
    stop("Probabilities are not properly specified")
  }

  if (sum(baseprobs) < 1) {
    baseprobs <- c(baseprobs, 1 - sum(baseprobs))
  }

  # checking complete

  dt <-copy(dtName)

  cprop <- cumsum(baseprobs)
  quant <- stats::qlogis(cprop)

  matlp <- matrix(rep(quant, nrow(dt)),
                  ncol = length(cprop),
                  byrow = TRUE
  )


  z <- dt[, adjVar, with=FALSE][[1]]
  matlpInd <- matlp - z

  matcump <- 1 / (1 + exp(-matlpInd))
  matcump <- cbind(0, matcump)

  p <- t(t(matcump)[-1,] - t(matcump)[-(length(baseprobs) + 1),])

  dt[, cat := matMultinom(p)]

  if (asFactor) {
    dt <- genFactor(dt, "cat", replace = TRUE)
    data.table::setnames(dt, "fcat", catVar)
  } else {
    data.table::setnames(dt, "cat", catVar)
  }

  return(dt[])

}
