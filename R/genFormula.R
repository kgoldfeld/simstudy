#### Generate a linear formula ####

#' @title Generate a linear formula
#' @description Formulas for additive linear models can be generated
#' with specified coefficient values and variable names.
#' @param coefs A numerical vector that contains the values of the
#' coefficients. If length(coefs) == length(vars), then no intercept
#' is assumed. Otherwise, an intercept is assumed.
#' @param vars A vector of strings that specify the names of the
#' explanatory variables in the equation.
#' @return A string that represents the desired formula
#' @examples
#'
#' genFormula(c(.5, 2, 4), c("A", "B", "C"))
#' genFormula(c(.5, 2, 4), c("A", "B"))
#'
#' changeX <- c(7, 10)
#' genFormula(c(.5, 2, changeX[1]), c("A", "B"))
#' genFormula(c(.5, 2, changeX[2]), c("A", "B"))
#' genFormula(c(.5, 2, changeX[2]), c("A", "B", "C"))
#'
#' newForm <- genFormula(c(-2, 1), c("A"))
#'
#' def1 <- defData(varname = "A", formula = 0, variance = 3, dist = "normal")
#' def1 <- defData(def1, varname = "B", formula = newForm, dist = "binary", link = "logit")
#'
#' set.seed(2001)
#' dt <- genData(500, def1)
#' summary(glm(B ~ A, data = dt, family = binomial))
#' @export

genFormula <- function(coefs, vars) {

  lcoef <- length(coefs)
  lvars <- length(vars)

  if ( !(lcoef == lvars | lcoef == lvars + 1) ) {
    stop("Coefficients or variables not properly specified")
  }

  if (!is.numeric(coefs)) {
    stop("Coefficients must be specified as numeric values or numeric variables")
  }

  if (!is.character(vars)) {
    stop("Variable names must be specified as characters or character variables")
  }

  if (lcoef != lvars) { # Intercept

    form <- paste0(coefs[1])
    coefs <- coefs[-1]

  } else {             # no intercept

    form <- paste(coefs[1], "*", vars[1])
    coefs <- coefs[-1]
    vars <- vars[-1]

  }

  for (i in 1 : (lcoef - 1) ) {
    form <- paste(form, "+" , coefs[i], "*", vars[i])
  }

  return(form)
}
