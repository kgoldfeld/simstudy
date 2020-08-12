#' Add single row to definitions table of conditions that will be used to add data to an
#' existing definitions table
#' @md 
#' @param dtDefs Name of definition table to be modified. Null if this is a new definition.
#' @param condition Formula specifying condition to be checked
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @return A data.table named dtName that is an updated data definitions table
#' @seealso [distributions]
#' @examples
#' # New data set
#'
#' def <- defData(varname = "x", dist = "noZeroPoisson", formula = 5)
#' def <- defData(def, varname = "y", dist = "normal", formula = 0, variance = 9)
#'
#' dt <- genData(10, def)
#'
#' # Add columns to dt
#'
#' defC <- defCondition(
#'   condition = "x == 1", formula = "5 + 2*y",
#'   variance = 1, dist = "normal"
#' )
#'
#' defC <- defCondition(defC,
#'   condition = "x <= 5 & x >= 2", formula = "3 - 2*y",
#'   variance = 1, dist = "normal"
#' )
#'
#' defC <- defCondition(defC,
#'   condition = "x >= 6", formula = 1,
#'   variance = 1, dist = "normal"
#' )
#'
#' defC
#'
#' # Add conditional column with field name "z"
#'
#' dt <- addCondition(defC, dt, "z")
#' dt
#' @export

defCondition <- function(dtDefs = NULL,
                         condition,
                         formula,
                         variance = 0,
                         dist = "normal",
                         link = "identity") {
  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()

    # attr(dtDefs,"id") <- id
  }

  dt.new <- data.table::data.table(
    condition,
    formula,
    variance,
    dist,
    link
  )

  l = list(dtDefs, dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  # attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])
}

#' Add single row to definitions table
#'
#' @useDynLib simstudy, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import data.table
#' @md
#' @param dtDefs Definition data.table to be modified
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @param id A string indicating the field name for the unique record identifier
#' @return A data.table named dtName that is an updated data definitions table
#' @seealso [distributions]
#' @details The possible data distributions are: `r paste0(.getDists(),collapse = ", ")`.
#'
#' @examples
#' def <- defData(varname = "xNr", dist = "nonrandom", formula = 7, id = "idnum")
#' def <- defData(def, varname = "xUni", dist = "uniform", formula = "10;20")
#' def <- defData(def, varname = "xNorm", formula = "xNr + xUni * 2", dist = "normal", variance = 8)
#' def <- defData(def, varname = "xPois", dist = "poisson", formula = "xNr - 0.2 * xUni", link = "log")
#' def <- defData(def, varname = "xCat", formula = "0.3;0.2;0.5", dist = "categorical")
#' def <- defData(def, varname = "xGamma", dist = "gamma", formula = "5+xCat", variance = 1, link = "log")
#' def <- defData(def, varname = "xBin", dist = "binary", formula = "-3 + xCat", link = "logit")
#' def
#' @export

defData <- function(dtDefs = NULL,
                    varname,
                    formula,
                    variance = 0,
                    dist = "normal",
                    link = "identity",
                    id = "id") {

  #### Check that arguments have been passed ####

  if (missing(varname)) stop("argument 'varname' is missing", call. = FALSE)
  if (missing(formula)) stop("argument 'formula' is missing", call. = FALSE)

  #### No missing arguments

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

  l = list(dtDefs, dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])
}

#' Add single row to definitions table that will be used to add data to an
#' existing data.table
#'
#' @param dtDefs Name of definition table to be modified. Null if this is a new definition.
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @seealso [distributions]
#' @return A data.table named dtName that is an updated data definitions table
#' @examples
#' # New data set
#'
#' def <- defData(varname = "xNr", dist = "nonrandom", formula = 7, id = "idnum")
#' def <- defData(def, varname = "xUni", dist = "uniform", formula = "10;20")
#'
#' dt <- genData(10, def)
#'
#' # Add columns to dt
#'
#' def2 <- defDataAdd(varname = "y1", formula = 10, variance = 3)
#' def2 <- defDataAdd(def2, varname = "y2", formula = .5, dist = "binary")
#' def2
#'
#' dt <- addColumns(def2, dt)
#' dt
#' @export

defDataAdd <- function(dtDefs = NULL,
                       varname,
                       formula,
                       variance = 0,
                       dist = "normal",
                       link = "identity") {
  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()

    # attr(dtDefs,"id") <- id
  }

  dt.new <- data.table::data.table(
    varname,
    formula,
    variance,
    dist,
    link
  )

  l = list(dtDefs, dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  # attr(defNew, "id") <- attr(dtDefs, "id")

  return(defNew[])
}

#' Read external csv data set definitions
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @param id string that includes name of id field. Defaults to "id"
#' @return A data.table with data set definitions
#' @seealso [distributions]
#' @examples
#' # Create temporary external "csv" file
#'
#' test1 <- c(
#'   "varname,formula,variance,dist,link",
#'   "nr,7, 0,nonrandom,identity",
#'   "x1,.4, 0,binary,identity",
#'   "y1,nr + x1 * 2,8,normal,identity",
#'   "y2,nr - 0.2 * x1,0,poisson, log"
#' )
#'
#' tfcsv <- tempfile()
#' writeLines(test1, tfcsv)
#'
#' # Read external csv file stored in file "tfcsv"
#'
#' defs <- defRead(tfcsv, id = "myID")
#' defs
#'
#' unlink(tfcsv)
#'
#' # Generate data based on external definition
#'
#' genData(5, defs)
#' @export

defRead <- function(filen, id = "id") {

  # 'declare var
  # TODO "declare vars"
  varname = NULL
  formula = NULL
  dist = NULL

  #

  if (!file.exists(filen)) stop("No such file")

  read.df <- utils::read.csv(filen, header = TRUE, as.is = TRUE) # do not read as factors
  read.dt <- data.table::data.table(read.df)

  if (!all(names(read.dt) == c(
    "varname",
    "formula", "variance", "dist", "link"
  ))) {
    stop("Field names do not match")
  }

  # check validity of data set

  suppressWarnings(test <- as.numeric(unlist(strsplit(as.character(read.dt[1, formula]),
    split = ";",
    fixed = TRUE
  ))))

  if (sum(is.na(test))) {
    stop("First defined formula must be scalar", call. = FALSE)
  }

  if (nrow(read.dt) > 1) {
    for (i in 2:nrow(read.dt)) {
      .evalDef(newvar = read.dt[i, varname], newform = read.dt[i, formula], newdist = read.dt[i, dist], defVars = read.dt[1:(i - 1), varname])
    }
  }


  attr(read.dt, "id") <- id
  return(read.dt[])
}

#' Read external csv data set definitions for adding columns
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @return A data.table with data set definitions
#' @seealso [distributions]
#' @examples
#' # Create temporary external "csv" files
#'
#' test1 <- c(
#'   "varname,formula,variance,dist,link",
#'   "nr,7, 0,nonrandom,identity"
#' )
#'
#' tfcsv1 <- tempfile()
#' writeLines(test1, tfcsv1)
#'
#' test2 <- c(
#'   "varname,formula,variance,dist,link",
#'   "x1,.4, 0,binary,identity",
#'   "y1,nr + x1 * 2,8,normal,identity",
#'   "y2,nr - 0.2 * x1,0,poisson, log"
#' )
#'
#' tfcsv2 <- tempfile()
#' writeLines(test2, tfcsv2)
#'
#' # Generate data based on external definitions
#'
#' defs <- defRead(tfcsv1)
#' dt <- genData(5, defs)
#' dt
#'
#' # Add additional data based on external definitions
#'
#' defs2 <- defReadAdd(tfcsv2)
#' dt <- addColumns(defs2, dt)
#' dt
#'
#' unlink(tfcsv1)
#' unlink(tfcsv2)
#' @export

defReadAdd <- function(filen) {
  if (!file.exists(filen)) stop("No such file")

  read.df <- utils::read.csv(filen, header = TRUE, as.is = TRUE) # do not read as factors
  read.dt <- data.table::data.table(read.df)

  if (!all(names(read.dt) == c(
    "varname",
    "formula", "variance", "dist", "link"
  ))) {
    stop("Field names do not match")
  }

  return(read.dt[])
}

#' Read external csv data set definitions for adding columns
#'
#' @param filen String file name, including full path. Must be a csv file.
#' @return A data.table with data set definitions
#' @seealso [distributions]
#' @examples
#' # Create temporary external "csv" files
#'
#' test1 <- c(
#'   "varname,formula,variance,dist,link",
#'   "x,0.3;0.4;0.3,0,categorical,identity"
#' )
#'
#' tfcsv1 <- tempfile()
#' writeLines(test1, tfcsv1)
#'
#' test2 <- c(
#'   "condition,formula,variance,dist,link",
#'   "x == 1, 0.4,0,binary,identity",
#'   "x == 2, 0.6,0,binary,identity",
#'   "x >= 3, 0.8,0,binary,identity"
#' )
#'
#' tfcsv2 <- tempfile()
#' writeLines(test2, tfcsv2)
#'
#' # Generate data based on external definitions
#'
#' defs <- defRead(tfcsv1)
#' dt <- genData(2000, defs)
#' dt
#'
#' # Add column based on
#'
#' defsCond <- defReadCond(tfcsv2)
#' dt <- addCondition(defsCond, dt, "y")
#' dt
#'
#' dt[, mean(y), keyby = x]
#'
#' unlink(tfcsv1)
#' unlink(tfcsv2)
#' @export

defReadCond <- function(filen) {
  if (!file.exists(filen)) stop("No such file")

  read.df <- utils::read.csv(filen, header = TRUE, as.is = TRUE) # do not read as factors
  read.dt <- data.table::data.table(read.df)

  if (!all(names(read.dt) == c("condition", "formula", "variance", "dist", "link"))) {
    stop("field names do not match")
  }

  return(read.dt[])
}

#' Add single row to survival definitions
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Variable name
#' @param formula Covariates predicting survival
#' @param scale Scale parameter for the Weibull distribution.
#' @param shape The shape of the Weibull distribution. Shape = 1 for
#' an exponential distribution
#' @return A data.table named dtName that is an updated data definitions table
#' @examples
#' # Baseline data definitions
#'
#' def <- defData(varname = "x1", formula = .5, dist = "binary")
#' def <- defData(def, varname = "x2", formula = .5, dist = "binary")
#' def <- defData(def, varname = "grp", formula = .5, dist = "binary")
#'
#' # Survival data definitions
#'
#' sdef <- defSurv(
#'   varname = "survTime", formula = "1.5*x1",
#'   scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5"
#' )
#'
#' sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)
#'
#' sdef
#'
#' # Baseline data definitions
#'
#' dtSurv <- genData(300, def)
#'
#' # Add survival times
#'
#' dtSurv <- genSurv(dtSurv, sdef)
#'
#' head(dtSurv)
#' @export

defSurv <- function(dtDefs = NULL,
                    varname,
                    formula = 0,
                    scale,
                    shape = 1) {
  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()
  }

  dt.new <- data.table::data.table(
    varname,
    formula,
    scale,
    shape
  )

  l = list(dtDefs, dt.new)

  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)

  return(defNew[])
}
