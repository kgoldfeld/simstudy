#' Create factor variable from an existing (non-double) variable
#'
#' @param dtName Data table with column
#' @param varname Name of field that is to be converted
#' @param labels Factor level labels. If not provided, the generated factor
#' levels will be used as the labels.
#' @param prefix By default, the new field name will be a concatenation of "f"
#' and the old field name. A prefix string can be provided.
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
#' dx <- genFactor(dx, "cat", labels = c("one", "two", "three"))
#' dx
#'
#' # Second example:
#'
#' dx <- genData(10)
#' dx <- trtAssign(dtName = dx, 2, grpName = "studyArm")
#' dx <- genFactor(dx, varname = "studyArm", labels = c("control", "treatment"), prefix = "t_")
#' dx
#' @export
#'

genFactor <- function(dtName, varname, labels = NULL, prefix = "f", replace = FALSE) {

  # Initial data checks

  if (missing(dtName)) stop("argument 'dtName' is missing", call. = FALSE)
  if (missing(varname)) stop("argument 'varname' is missing", call. = FALSE)

  # Check if data table exists

  if (!exists(deparse(substitute(dtName)), envir = parent.frame())) {
    stop(paste("data table", deparse(substitute(dtName)), "not found"), 
         call. = FALSE)
  }

  # Check if field exists, extract, and verify it is not double

  if (!(varname %in% names(dtName))) {
    stop(paste("variable", varname, "not found in data table", 
               deparse(substitute(dtName))), 
         call. = FALSE)
  }

  xcol <- dtName[, get(varname)]

  if (is.double(xcol)) {
    
    if (!all(xcol == as.integer(xcol))) {
      stop(paste("variable", varname, "is of type 'double'"), 
           call. = FALSE)
    }
      
    xcol <- as.integer(xcol)
  }

  # Create new field name (check to see if it exists)

  fname <- make.names(paste0(prefix, varname))

  if (fname %in% names(dtName)) {
    stop(paste("variable", fname, 
               "already exists in data table", deparse(substitute(dtName))), 
         call. = FALSE)
  }

  # Create new column as factor

  if (is.null(labels)) {
    xfac <- factor(xcol)
  } else {
    xfac <- factor(xcol, labels = labels)
  }

  dtName[, (fname) := xfac]

  if (replace == TRUE) dtName[, (varname) := NULL]

  dtName[]

}
