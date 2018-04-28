#' Create dummy variables from a factor or integer variable
#'
#' @param dtName Data table with column
#' @param varname Name of factor
#' @param sep Character to be used in creating new name for dummy fields.
#' Valid characters include all letters and "_". Will default to ".". If
#' an invalid character is provided, it will be replaced by default.
#' @param replace If replace is set to TRUE (defaults to FALSE) the field
#' referenced varname will be removed.
#'
#' @examples
#'
#' # First example:
#'
#' def <- defData(varname = "cat", formula = ".2;.3;.5", dist="categorical")
#' def <- defData(def, varname = "x", formula = 5, variance = 2)
#'
#' dx <- genData(200,def)
#' dx
#'
#' dx <- genFactor(dx, "cat", labels = c("one", "two", "three"), replace = TRUE)
#' dx <- genDummy(dx, varname = "fcat", sep = "_")
#'
#' dx
#'
#' # Second example:
#'
#' dx <- genData(15)
#' dx <- trtAssign(dtName = dx, 3, grpName = "arm")
#' dx <- genDummy(dx, varname = "arm")
#' dx
#' @export
#'

genDummy <- function(dtName, varname, sep = ".", replace = FALSE) {

  # Initial data checks

  if (missing(dtName)) stop("argument 'dtName' is missing", call. = FALSE)
  if (missing(varname)) stop("argument 'varname' is missing", call. = FALSE)

  # Check if data table exists

  if (!exists(deparse(substitute(dtName)), envir = parent.frame())) {
    stop(paste("data table", deparse(substitute(dtName)), "not found"), call. = FALSE)
  }

  # Check if varname exists

  if (!(varname %in% names(dtName))) {
    stop(paste("variable", varname, "not found in data table", deparse(substitute(dtName))), call. = FALSE)
  }

  # Check if field is integer or factor

  x <- dtName[, get(varname)]

  if (!( is.integer(x) | is.factor(x) ) )  {
    stop(paste("variable", varname, "must be a factor or integer"), call. = FALSE)
  }


  if (is.integer(x)) x <- factor(x)

  # if sep is invalid, defaults to "."
  dummy.names <- make.names(paste0(varname, sep, levels(x)))
  nlevels <- length(levels(x))

  # Check to see if new field names exist

  for (i in 1:nlevels) {

    if (dummy.names[i] %in% names(dtName)) {
      stop(paste("variable", dummy.names[i], "already exists in data table", deparse(substitute(dtName))), call. = FALSE)
    }

  }

  # Create dummies for each level of factor

  dummies <- NULL

  for (i in (1:nlevels)) {
    dummies <- cbind(dummies, as.integer(x == levels(x)[i]) )
  }

  dummies <- data.table(dummies)

  setnames(dummies, dummy.names)

  if (replace == TRUE) dtName[, (varname) := NULL]

  return(cbind(dtName, dummies))

}
