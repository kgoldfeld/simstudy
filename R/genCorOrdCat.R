#### Generate correlated ordinal categorical data ####

#' @title Generate correlated ordinal categorical data
#' @description Correlated ordinal categorical data is added to an existing data set.
#' @param dtName Name of complete data set
#' @param idname A string. The id of the data.table that identifies a unique record.
#' Defaults to "id".
#' @param adjVar Vector of adjustment variables name in dtName - determines
#' logistic shift. This is specified assuming a cumulative logit
#' link. The vector can be NULL, of length 1, or a length equal to the
#' number of new categorical variables.
#' @param baseprobs A matrix of baseline probabilities. The row values must sum to 1. 
#' The number of rows represents the number of new categorical variables. The number
#' of columns represents the number of possible responses - if an particular category
#' has fewer possible responses, assign zero probability to non-relevant columns.
#' @param prefix A string.The names of the new variables will be a concatenation of
#' the prefix and a sequence of integers indicating the variable number.
#' @param rho Correlation coefficient, -1 < rho < 1. Use if corMatrix is not provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "ind" for an independence
#' structure, "cs" for a compound symmetry structure, and "ar1" for an
#' autoregressive structure.
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical 
#' and positive semi-definite. It is not a required field; if a matrix is not provided, 
#' then a structure and correlation coefficient rho must be specified.
#' @return Original data.table with added categorical fields
#' @examples
#' #### Set definitions
#'
#' baseprobs <- matrix(c(0.2, 0.1, 0.1, 0.6,
#'                       0.7, 0.2, 0.1, 0,
#'                       0.5, 0.2, 0.3, 0,
#'                       0.4, 0.2, 0.4, 0,
#'                       0.6, 0.2, 0.2, 0), 
#'                     nrow = 5, byrow = TRUE)
#'                     
#' set.seed(333)                     
#' dT <- genData(1000)
#' 
#' dX <- genCorOrdCat(dT, adjVar = NULL, baseprobs = baseprobs, 
#'                    prefix = "q", rho = .125, corstr = "cs")
#'
#' dM <- melt(dX, id.vars = "id")
#' dProp <- dM[ , prop.table(table(value)), by = variable]
#' dProp[, response := c(1:4, 1:3, 1:3, 1:3, 1:3)]
#' 
#' dcast(dProp, variable ~ response, value.var = "V1", fill = 0)
#'                    
#' @export
genCorOrdCat <- function(dtName, idname = "id", adjVar = NULL, baseprobs, 
                         prefix = "grp", rho, corstr, corMatrix = NULL)     {
  
  # "declares" to avoid global NOTE
  
  logisZ <- NULL
  period <- NULL
  
  # Check arguments
  
  if (!exists(deparse(substitute(dtName)),  envir = parent.frame())) {
    stop("Data table does not exist.")
  }
  
  if (! ( idname %in% names(dtName))) {
    stop(paste("idname", idname, "not in", deparse(substitute(dtName))))
  }
  
  if (! all( adjVar %in% names(dtName))) {
    missVars <- adjVar[! (adjVar %in% names(dtName))]
    stop(paste("Variable (s)", paste(missVars, collapse = ", "), "not in", 
               deparse(substitute(dtName))))
  }
  
  if (!is.character(prefix)) {
    stop("prefix must be a string")
  }
  
  if (is.null(baseprobs)) {
    stop("Proability vector is empty")
  }
  
  if (any(apply(baseprobs, 1, sum) != 1)) {
    stop("Probabilities are not properly specified")
  }
  
  if (length(adjVar) > 1) {
    if (nrow(baseprobs) != length(adjVar)) {
      stop("Number of categories implied by baseprobs and adjVar do not match")
    }
  }
  
  if (length(adjVar) == 1) {
    adjVar <- rep(adjVar, nrow(baseprobs))
  }
  
  N <- nrow(dtName)
  nq <- nrow(baseprobs)
  zs <- .genQuantU(nq, N, rho = rho, corstr, corMatrix = NULL)
  zs[, logisZ := stats::qlogis(p = zs$Unew)]
  cprop <- t(apply(baseprobs, 1, cumsum))
  quant <- t(apply(cprop, 1, stats::qlogis))
  
  mycat <- list()
  
  for ( i in 1:nq ) {
    iLogisZ <- zs[period == i-1, logisZ]
    matlp <- matrix(rep(quant[i, ], nrow(dtName)),
                    ncol = ncol(cprop),
                    byrow = TRUE
    )
    if (! is.null(adjVar)) {
      z <- dtName[, adjVar[i], with=FALSE][[1]]
      matlp <- matlp - z
    }
    locateGrp <- (iLogisZ > cbind(-Inf, matlp))
    assignGrp <- apply(locateGrp, 1, sum)
    mycat[[i]] <- data.table(id = dtName[, idname, with=FALSE][[1]],
                           var = paste0(prefix,i),
                           cat = assignGrp )
  }
  dcat <- rbindlist(mycat)
  cats <- dcast(dcat, id ~ var, value.var = "cat" )
  
  setnames(cats, "id", idname)
  setkeyv(cats, idname)
  dtName <- dtName[cats]
  return(dtName[])
}
