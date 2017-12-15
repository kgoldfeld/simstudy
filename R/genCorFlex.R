#' Create multivariate (correlated) data - for general distributions
#'
#' @param n Number of observations
#' @param defs Field definition table created by function `defData`. All definitions
#' must be scalar. Definition specifies distribution, mean, and variance, with all
#' caveats for each of the distributions. (See defData).
#' @param rho Correlation coefficient, -1 <= rho <= 1. Use if corMatrix is not provided.
#' @param corstr Correlation structure of the variance-covariance matrix
#' defined by sigma and rho. Options include "cs" for a compound symmetry structure
#' and "ar1" for an autoregressive structure.
#' @param corMatrix Correlation matrix can be entered directly. It must be symmetrical and
#' positive semi-definite. It is not a required field; if a matrix is not provided, then a
#' structure and correlation coefficient rho must be specified.
#' @return data.table with added column(s) of correlated data
#' @examples
#' def <- defData(varname = "xNorm", formula = 0, variance = 4, dist = "normal")
#' def <- defData(def, varname = "xGamma1", formula = 15, variance = 2, dist = "gamma")
#' def <- defData(def, varname = "xBin", formula = 0.5, dist = "binary")
#' def <- defData(def, varname = "xUnif1", formula = "0;10", dist = "uniform")
#' def <- defData(def, varname = "xPois", formula = 15, dist = "poisson")
#' def <- defData(def, varname = "xUnif2", formula = "23;28", dist = "uniform")
#' def <- defData(def, varname = "xUnif3", formula = "100;150", dist = "uniform")
#' def <- defData(def, varname = "xGamma2", formula = 150, variance = 0.003, dist = "gamma")
#'
#' dt <- genCorFlex(100000, def, rho = 0.3 , corstr = "cs")
#'
#' cor(dt[,-"id"])
#' var(dt[,-"id"])
#' apply(dt[,-"id"], 2, mean)
#'
#' @export
#'
genCorFlex <- function(n, defs, rho, corstr, corMatrix = NULL) {

  # "Declare" vars to avoid R CMD warning

  X <- NULL
  Unew <- NULL
  param1 <- NULL
  param2 <- NULL
  id <- NULL
  period <- NULL
  dist <- NULL

  #### Check args

  ## Other checks? ##

  if (!all(defs[,dist] %in% c("normal", "gamma", "uniform", "binary", "poisson"))) {

    stop("Only implemented for the following distributions: binary, uniform, normal, poisson, and gamma")

  }

  ####

  corDefs <- copy(defs)

  nvars <- nrow(corDefs)

  ### Uniform parameters entered as string

  nUniform <- corDefs[dist == "uniform", .N]

  if (nUniform > 0) {

    rangeV <- 2 * (1:nUniform)
    rangeF <- rangeV - 1

    range <- corDefs[dist=="uniform", unlist(strsplit(as.character(formula),split=";", fixed=TRUE))]
    corDefs[dist == "uniform", formula := range[rangeF]]
    corDefs[dist == "uniform", variance := as.numeric(range[rangeV])]
  }

  chkWarn <- tryCatch(corDefs[, formula := as.numeric(formula)],
                        warning = function(w) {
                          "warning"
                        }
  )

  if (class(chkWarn)[1] == "character")  stop("Non-scalar values in definitions")

  ### Gamma parameters need to be transformed

  sr1 <- corDefs[dist=="gamma" , gammaGetShapeRate(formula, variance)[[1]] ]
  sr2 <- corDefs[dist=="gamma",  gammaGetShapeRate(formula, variance)[[2]] ]
  corDefs[dist == "gamma", `:=`(formula = sr1, variance = sr2)]

  ### Check for non-scalar values in definitions

  if (corDefs[is.na(formula), .N] > 0) stop("Non-scalar values in definitions")

  ### Start generating data (first, using copula)

  dx <- simstudy:::genQuantU(nvars, n, rho, corstr, corMatrix=NULL)

  dx[, dist := corDefs[, dist]]
  dx[, param1 := corDefs[, formula]]
  dx[, param2 := corDefs[, variance]]

  dFinal <- dx[period == 0, .(id)]

  for (i in 1:nvars) {

    dTemp <- dx[period == (i - 1)]
    type <- corDefs[i, dist]

    if (type == "binary") {
      V <- dTemp[, qbinom(Unew, 1, param1)]

    } else if (type == "poisson") {
      V <- dTemp[, qpois(Unew, param1)]

    } else if (type == "uniform") {
      V <- dTemp[, qunif(Unew, param1, param2)]

    } else if (type == "gamma") {
      V <- dTemp[,qgamma(Unew, param1, param2)]

    } else if (type == "normal") {
      V <- dTemp[, qnorm(Unew, param1, sqrt(param2))]
    }

    dFinal <- cbind(dFinal, V)
    setnames(dFinal, "V", corDefs$varname[i])

  }

  return(dFinal)

}
