def_data <- function(dtDefs = NULL,
                    varname,
                    formula,
                    variance = 0,
                    dist = "normal",
                    link = "identity",
                    id = "id") {

  assertNotMissing(varname = missing(varname), formula = missing(formula))

  if (is.null(dtDefs)) { # checking that initial formula has no variables ...

    dtDefs <- data.table::data.table()
    attr(dtDefs, "id") <- id
    defVars <- ""
  } else {
    defVars <- dtDefs[, varname]
  }

  .evalDef(varname, formula, dist, variance, link, defVars)


  dt.new <- data.table::data.table(
    varname,
    formula,
    variance,
    dist,
    link
  )

  l <- list(dtDefs, dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])
}


#Use rlang, dtplyr, cli
# Each distribution is an S3 class with a set API
validate <- function(x) UseMethod("validate")
validate.normal <- function(formula) .isValidArithmeticFormula(formula,"")
validate.default <- function(x) "Unkown Distribution for validate"
generate <- function(x) UseMethod("generate")
