#' Calling function to simulate data
#'
#' @param n the number of observations required in the data set.
#' @param dtDefs name of definitions data.table/data.frame. If no definitions
#'  are provided
#' a data set with ids only is generated.
#' @param id The string defining the id of the record
#' @param envir Environment the data definitions are evaluated in.
#'  Defaults to [base::parent.frame].
#' @return A data.table that contains the simulated data.
#' @export
#' @md
#' @concept generate_data
#' @examples
#' genData(5)
#' genData(5, id = "grpID")
#'
#' def <- defData(
#'   varname = "xNr", dist = "nonrandom", formula = 7,
#'   id = "idnum"
#' )
#' def <- defData(def,
#'   varname = "xUni", dist = "uniform",
#'   formula = "10;20"
#' )
#' def <- defData(def,
#'   varname = "xNorm", formula = "xNr + xUni * 2",
#'   dist = "normal", variance = 8
#' )
#' def <- defData(def,
#'   varname = "xPois", dist = "poisson",
#'   formula = "xNr - 0.2 * xUni", link = "log"
#' )
#' def <- defData(def,
#'   varname = "xCat", formula = "0.3;0.2;0.5",
#'   dist = "categorical"
#' )
#' def <- defData(def,
#'   varname = "xGamma", dist = "gamma", formula = "5+xCat",
#'   variance = 1, link = "log"
#' )
#' def <- defData(def,
#'   varname = "xBin", dist = "binary", formula = "-3 + xCat",
#'   link = "logit"
#' )
#' def
#'
#' genData(5, def)
genData <- function(n, dtDefs = NULL, id = "id", envir = parent.frame()) {
  assertNotMissing(n = missing(n))
  assertValue(n = n, id = id)
  assertType(id = id, type = "character")
  assertNumeric(n = n)

  if (is.null(dtDefs)) {
    dt <- data.table::data.table(x = 1:n)
    data.table::setnames(dt, id)
    data.table::setkeyv(dt, id)
  } else { # existing definitions
    assertClass(dtDefs = dtDefs, class = "data.table")

    idname <- attr(dtDefs, "id")

    dfSimulate <- data.frame(c(1:n)) # initialize simulated data with ids
    names(dfSimulate) <- attr(dtDefs, "id") # name first column attribute "id"
    iter <- nrow(dtDefs) # generate a column of data for each row of dtDefs

    for (i in (1:iter)) {
      dfSimulate <- .generate(
        args = dtDefs[i, ],
        n = n,
        dfSim = dfSimulate,
        idname = idname,
        envir = envir
      )
    }

    dt <- data.table::data.table(dfSimulate)
    data.table::setkeyv(dt, idname)
  }

  return(dt[])
}



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
#' def <- defData(varname = "cat", formula = ".2;.3;.5", dist = "categorical")
#' def <- defData(def, varname = "x", formula = 5, variance = 2)
#'
#' dx <- genData(200, def)
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
#' @concept generate_data
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

  if (!(is.integer(x) | is.factor(x))) {
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
    dummies <- cbind(dummies, as.integer(x == levels(x)[i]))
  }

  dummies <- data.table(dummies)

  setnames(dummies, dummy.names)

  if (replace == TRUE) dtName[, (varname) := NULL]

  return(cbind(dtName, dummies))
}

#' Create factor variable from an existing (non-double) variable
#'
#' @param dtName Data table with columns.
#' @param varname Name of field(s) to be converted.
#' @param labels Factor level labels. If not provided, the generated factor
#' levels will be used as the labels. Can be a vector (if only one new factor or
#' all factors have the same labels) or a list of character vectors of the same
#' length as varname.
#' @param prefix By default, the new field name will be a concatenation of "f"
#' and the old field name. A prefix string can be provided.
#' @param replace If replace is set to TRUE (defaults to FALSE) the field
#' referenced varname will be removed.
#'
#' @examples
#'
#' # First example:
#'
#' def <- defData(varname = "cat", formula = ".2;.3;.5", dist = "categorical")
#' def <- defData(def, varname = "x", formula = 5, variance = 2)
#'
#' dx <- genData(200, def)
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
#' @concept generate_data
genFactor <- function(dtName,
                      varname,
                      labels = NULL,
                      prefix = "f",
                      replace = FALSE) {
  assertNotMissing(dtName = missing(dtName), varname = missing(varname))
  assertValue(dtName = dtName, varname = varname)
  assertClass(dtName = dtName, class = "data.table")
  assertType(varname = varname, type = "character")
  assertUnique(varname = varname)
  assertInDataTable(vars = varname, dt = dtName)
  assertInteger(columns2Convert = dtName[, ..varname])

  ..varname <- NULL
  # Create new field name (check to see if it exists)

  fname <- make.names(glue("{prefix}{varname}"))
  assertNotInDataTable(vars = fname, dt = dtName)

  # Create new column as factor
  if (!is.null(labels)) {
    assertType(labels = labels, type = "character")

    if (is.list(labels)) {
      assertLength(labels = labels, length = length(varname))

      for (i in seq_len(length(varname))) {
        dtName[[fname[i]]] <- factor(dtName[[varname[i]]],
          labels = labels[[i]]
        )
      }
    } else {
      dtName[, (fname) := lapply(.SD, factor, labels = labels),
        .SDcols = varname
      ]
    }
  } else {
    dtName[, (fname) := lapply(.SD, factor),
      .SDcols = varname
    ]
  }

  if (replace == TRUE) dtName[, (varname) := NULL]

  dtName[]
}

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
#' @concept generate_data
genFormula <- function(coefs, vars) {
  lcoef <- length(coefs)
  lvars <- length(vars)

  if (!(lcoef == lvars | lcoef == lvars + 1)) {
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
  } else { # no intercept

    form <- paste(coefs[1], "*", vars[1])
    coefs <- coefs[-1]
    vars <- vars[-1]
  }

  for (i in 1:(lcoef - 1)) {
    form <- paste(form, "+", coefs[i], "*", vars[i])
  }

  return(form)
}

#' @title  Generate Markov chain
#' @description Generate a Markov chain for n individuals or units by
#' specifying a transition matrix.
#' @param n number of individual chains to generate
#' @param transMat Square transition matrix where the sum of each row
#' must equal 1. The dimensions of the matrix equal the number of possible
#' states.
#' @param chainLen Length of each chain that will be generated for each
#' chain; minimum chain length is 2.
#' @param wide Logical variable (TRUE or FALSE) indicating whether the
#' resulting data table should be returned in wide or long format. The
#' wide format includes all elements of a chain on a single row; the long
#' format includes each element of a chain in its own row. The default is
#' wide = FALSE, so the long format is returned by default.
#' @param id Character string that represents name of "id" field.
#' Defaults to "id".
#' @param pername Character string that represents the variable name of the
#' chain sequence in the long format. Defaults "period",
#' @param varname Character string that represents the variable name of the
#' state in the long format. Defaults to "state".
#' @param widePrefix Character string that represents the variable name
#' prefix for the state fields in the wide format. Defaults to "S".
#' @param trimvalue Integer value indicating end state. If trimvalue is not NULL,
#' all records after the first instance of state = trimvalue will be deleted.
#' @return A data table with n rows if in wide format, or n by chainLen rows
#' if in long format.
#' @examples
#'
#' # Transition matrix P
#'
#' P <- t(matrix(c(
#'   0.7, 0.2, 0.1,
#'   0.5, 0.3, 0.2,
#'   0.0, 0.1, 0.9
#' ), nrow = 3, ncol = 3))
#'
#' d1 <- genMarkov(n = 10, transMat = P, chainLen = 5)
#' d2 <- genMarkov(n = 10, transMat = P, chainLen = 5, wide = TRUE)
#' d3 <- genMarkov(
#'   n = 10, transMat = P, chainLen = 5,
#'   pername = "seq", varname = "health",
#'   trimvalue = 3
#' )
#' @export
#' @concept generate_data
genMarkov <- function(n, transMat, chainLen, wide = FALSE, id = "id",
                      pername = "period", varname = "state",
                      widePrefix = "S", trimvalue = NULL) {

  # 'declare' vars created in data.table
  variable <- NULL

  # check transMat is square matrix and row sums = 1

  if (!is.matrix(transMat) |
    (length(dim(transMat)) != 2) |
    (dim(transMat)[1] != dim(transMat)[2])
  ) {
    stop("Transition matrix needs to be a square matrix")
  }

  # check row sums = 1

  if (!all(round(apply(transMat, 1, sum), 5) == 1)) {
    stop("Rows in transition matrix must sum to 1")
  }

  # check chainLen is > 1

  if (chainLen <= 1) stop("Chain length must be greater than 1")

  ####

  dd <- genData(n = n, id = id)
  dd <- addMarkov(dd, transMat, chainLen, wide, id,
    pername, varname, widePrefix,
    start0lab = NULL,
    trimvalue = trimvalue
  )

  dd[]
}

#' Generate multi-factorial data
#'
#' @param nFactors Number of factors (columns) to generate.
#' @param each Number of replications for each combination of factors. Must be specified.
#' @param levels Vector or scalar. If a vector is specified, it must be
#' the same length as nFatctors. Each value of the vector represents the
#' number of levels of each corresponding factor. If a scalar is specified,
#' each factor will have the same number of levels. The default is 2 levels
#' for each factor.
#' @param coding String value to specify if "dummy" or "effect" coding is used.
#' Defaults to "dummy".
#' @param colNames A vector of strings, with a length of nFactors. The strings
#' represent the name for each factor.
#' @param idName A string that specifies the id of the record. Defaults to "id".
#' @return A data.table that contains the added simulated data. Each column contains
#' an integer.
#' @examples
#' genMultiFac(nFactors = 2, each = 5)
#' genMultiFac(nFactors = 2, each = 4, levels = c(2, 3))
#' genMultiFac(
#'   nFactors = 3, each = 1, coding = "effect",
#'   colNames = c("Fac1", "Fac2", "Fac3"), id = "block"
#' )
#' @export
#' @concept generate_data
genMultiFac <- function(nFactors, each, levels = 2, coding = "dummy", colNames = NULL, idName = "id") {
  if (nFactors < 2) stop("Must specify at least 2 factors")
  if (length(levels) > 1 & (length(levels) != nFactors)) stop("Number of levels does not match factors")

  x <- list()

  if (all(levels == 2)) {
    if (coding == "effect") {
      opts <- c(-1, 1)
    } else if (coding == "dummy") {
      opts <- c(0, 1)
    } else {
      stop("Need to specify 'effect' or 'dummy' coding")
    }

    for (i in 1:nFactors) {
      x[[i]] <- opts
    }
  } else {
    if (length(levels) == 1) levels <- rep(levels, nFactors)

    for (i in 1:nFactors) x[[i]] <- c(1:levels[i])
  }

  dt <- data.table(as.data.frame(lapply(expand.grid(x), function(x) rep(x, each = each))))

  if (!is.null(colNames)) setnames(dt, colNames)

  origNames <- copy(names(dt))

  dt[, (idName) := 1:.N]

  setcolorder(dt, c(idName, origNames))
  setkeyv(dt, idName)

  return(dt[])
}

#' @title Generate ordinal categorical data
#' @description Ordinal categorical data is added to an existing data set.
#' Correlations can be added via correlation matrix or `rho` and `corstr`.
#' @param dtName Name of complete data set
#' @param adjVar Adjustment variable  name in dtName - determines
#' logistic shift. This is specified assuming a cumulative logit
#' link.
#' @param baseprobs Baseline probability expressed as a vector or matrix of
#' probabilities. The values (per row) must sum to <= 1. If `rowSums(baseprobs)
#' < 1`, an additional category is added with probability `1 -
#' rowSums(baseprobs)`. The number of rows represents the number of new
#' categorical variables. The number of columns represents the number of
#' possible responses - if an particular category has fewer possible responses,
#' assign zero probability to non-relevant columns.
#' @param catVar Name of the new categorical field. Defaults to "cat". Can be a
#' character vector with a name for each new variable defined via `baseprobs`.
#' Will be overridden by `prefix` if more than one variable is defined and
#' `length(catVar) == 1`.
#' @param asFactor If `asFactor == TRUE` (default), new field is returned
#' as a factor. If `asFactor == FALSE`, new field is returned as an integer.
#' @param idname Name of the id column in `dtName`.
#' @param prefix A string. The names of the new variables will be a
#' concatenation of the prefix and a sequence of integers indicating the
#' variable number.
#' @param rho Correlation coefficient, -1 < rho < 1. Use if corMatrix is not
#' provided.
#' @param corstr Correlation structure of the variance-covariance matrix defined
#' by sigma and rho. Options include "ind" for an independence structure, "cs"
#' for a compound symmetry structure, and "ar1" for an autoregressive structure.
#' @param corMatrix Correlation matrix can be entered directly. It must be
#' symmetrical and positive definite. It is not a required field; if a matrix is
#' not provided, then a structure and correlation coefficient rho must be
#' specified. (The matrix created via `rho` and `corstr` must also be positive
#' definite.)
#' @return Original data.table with added categorical field.
#' @examples
#' # Ordinal Categorical Data ----
#'
#' def1 <- defData(
#'     varname = "male",
#'     formula = 0.45, dist = "binary", id = "idG"
#' )
#' def1 <- defData(def1,
#'     varname = "z",
#'     formula = "1.2*male", dist = "nonrandom"
#' )
#' def1
#'
#' ## Generate data
#'
#' set.seed(20)
#'
#' dx <- genData(1000, def1)
#'
#' probs <- c(0.40, 0.25, 0.15)
#' dx <- genOrdCat(dx,
#'   adjVar = "z", idname = "idG", baseprobs = probs,
#'   catVar = "grp"
#' )
#' dx
#'
#' # Correlated Ordinal Categorical Data ----
#'
#' baseprobs <- matrix(c(
#'     0.2, 0.1, 0.1, 0.6,
#'     0.7, 0.2, 0.1, 0,
#'     0.5, 0.2, 0.3, 0,
#'     0.4, 0.2, 0.4, 0,
#'     0.6, 0.2, 0.2, 0
#' ),
#' nrow = 5, byrow = TRUE
#' )
#'
#' set.seed(333)
#' dT <- genData(1000)
#'
#' dX <- genOrdCat(dT,
#'     adjVar = NULL, baseprobs = baseprobs,
#'     prefix = "q", rho = .125, corstr = "cs", asFactor = FALSE
#' )
#' dX
#'
#' dM <- data.table::melt(dX, id.vars = "id")
#' dProp <- dM[, prop.table(table(value)), by = variable]
#' dProp[, response := c(1:4, 1:3, 1:3, 1:3, 1:3)]
#'
#' data.table::dcast(dProp, variable ~ response,
#'     value.var = "V1", fill = 0
#' )
#' @export
#' @md
#' @concept generate_data
#' @concept categorical
#' @concept correlated
genOrdCat <- function(dtName,
                      adjVar = NULL,
                      baseprobs,
                      catVar = "cat",
                      asFactor = TRUE,
                      idname = "id",
                      prefix = "grp",
                      rho = 0,
                      corstr = "ind",
                      corMatrix = NULL) {

  # "declares" to avoid global NOTE
  cat <- NULL
  logisZ <- NULL
  period <- NULL

  assertNotMissing(dtName = missing(dtName), baseprobs = missing(baseprobs))
  assertValue(
    dtName = dtName,
    baseprobs = baseprobs,
    catVar = catVar,
    asFactor = asFactor,
    idname = idname,
    prefix = prefix,
    rho = rho,
    corstr = corstr
  )
  assertClass(dtName = dtName, class = "data.table")
  assertClass(rho = rho, class = "numeric")
  assertClass(
    catVar = catVar,
    prefix = prefix,
    corstr = corstr,
    idname = idname,
    class = "character"
  )
  assertInDataTable(c(adjVar, idname), dtName)
  corstr <- ensureOption(
    corstr = corstr,
    options = c("ind", "cs", "ar1"),
    default = "ind"
  )

  baseprobs <- ensureMatrix(baseprobs)
  baseprobs <- .adjustProbs(baseprobs)


  nCats <- nrow(baseprobs)
  ensureLength(catVar = catVar, n = nCats)

  if (!is.null(adjVar)) {
    adjVar <- ensureLength(
      adjVar = adjVar,
      n = seq_len(nCats), msg = list(
        "Number of categories implied",
        " by baseprobs and adjVar do not match. ",
        "{ dots$names[[1]] } should be",
        " either length 1 or { n } but",
        " is { length(var) }!"
      )
    )
  }

  if (nCats > 1 && length(catVar) != nCats) {
    catVar <- glue("{prefix}{i}", i = zeroPadInts(1:nCats))
  }
  dt <- copy(dtName)
  n <- nrow(dt)
  zs <- .genQuantU(nCats, n, rho = rho, corstr, corMatrix = corMatrix)
  zs[, logisZ := stats::qlogis(p = zs$Unew)]
  cprop <- t(apply(baseprobs, 1, cumsum))
  quant <- t(apply(cprop, 1, stats::qlogis))

  mycat <- list()

  for (i in 1:nCats) {
    iLogisZ <- zs[period == i - 1, logisZ]
    matlp <- matrix(rep(quant[i, ], n),
      ncol = ncol(cprop),
      byrow = TRUE
    )
    if (!is.null(adjVar)) {
      z <- dt[, adjVar[i], with = FALSE][[1]]
      matlp <- matlp - z
    }
    locateGrp <- (iLogisZ > cbind(-Inf, matlp))
    assignGrp <- apply(locateGrp, 1, sum)
    mycat[[i]] <- data.table(
      id = dt[, idname, with = FALSE][[1]],
      var = catVar[[i]],
      cat = assignGrp
    )
  }
  dcat <- data.table::rbindlist(mycat)
  cats <- data.table::dcast(dcat, id ~ var, value.var = "cat")

  setnames(cats, "id", idname)
  setkeyv(cats, idname)
  dt <- dt[cats]

  if (asFactor) {
    dt <- genFactor(dt, catVar, replace = TRUE)
    data.table::setnames(dt, glue("f{catVar}"), catVar)
  }

  dt[]
}

#' Generate spline curves
#'
#' @param dt data.table that will be modified
#' @param newvar Name of new variable to be created
#' @param predictor Name of field in old data.table that is predicting new value
#' @param theta A vector or matrix of values between 0 and 1. Each column of the matrix
#' represents the weights/coefficients that will be applied to the basis functions
#' determined by the knots and degree. Each column of theta represents a separate
#' spline curve.
#' @param knots A vector of values between 0 and 1, specifying quantile
#' cut-points for splines. Defaults to c(0.25, 0.50, 0.75).
#' @param degree Integer specifying polynomial degree of curvature.
#' @param newrange Range of the spline function , specified as a string
#' with two values separated by a semi-colon. The first value represents the
#' minimum, and the second value represents the maximum. Defaults to NULL, which
#' sets the range to be between 0 and 1.
#' @param noise.var Add to normally distributed noise to observation - where mean
#' is value of spline curve.
#' @return A modified data.table with an added column named newvar.
#' @examples
#' ddef <- defData(varname = "age", formula = "0;1", dist = "uniform")
#'
#' theta1 <- c(0.1, 0.8, 0.6, 0.4, 0.6, 0.9, 0.9)
#' knots <- c(0.25, 0.5, 0.75)
#'
#' viewSplines(knots = knots, theta = theta1, degree = 3)
#'
#' set.seed(234)
#' dt <- genData(1000, ddef)
#'
#' dt <- genSpline(
#'   dt = dt, newvar = "weight",
#'   predictor = "age", theta = theta1,
#'   knots = knots, degree = 3,
#'   noise.var = .025
#' )
#'
#' dt
#' @export
#' @concept splines
#' @concept generate_data
genSpline <- function(dt, newvar, predictor, theta,
                      knots = c(0.25, 0.50, 0.75), degree = 3,
                      newrange = NULL, noise.var = 0) {

  # "declares" to avoid global NOTE
  y.spline <- NULL

  # Check arguments

  if (!exists(deparse(substitute(dt)), envir = parent.frame())) {
    stop("Data table does not exist.")
  }

  if (!predictor %in% names(dt)) {
    stop(paste0("Variable ", predictor, " not in data.table"))
  }


  if (!is.character(newvar)) {
    stop("newvar must be a string")
  }

  if (!(is.null(newrange))) {
    rangestr <- unlist(strsplit(as.character(newrange), split = ";", fixed = TRUE))
    rangenum <- as.numeric(rangestr)

    if (length(rangenum) != 2) {
      stop("Range not specified as two values")
    }

    if (!(all(!is.na(rangenum)))) {
      stop("Non-numbers entered in range")
    }

    newmin <- min(rangenum)
    newmax <- max(rangenum)
  }

  ### All checks passed

  x <- dt[, predictor, with = FALSE][[1]]

  if (!(min(x) >= 0 & max(x) <= 1)) {
    x.normalize <- (x - min(x)) / (max(x) - min(x))
  } else {
    x.normalize <- copy(x)
  }

  qknots <- stats::quantile(x = x.normalize, probs = knots)

  sdata <- .genbasisdt(x.normalize, qknots, degree, theta) # Call internal function

  if (is.null(newrange)) {
    newy <- sdata$dt[, y.spline]
  } else {
    newy <- sdata$dt[, y.spline * (newmax - newmin) + newmin] # de-normalize
  }

  newy <- stats::rnorm(length(newy), newy, sqrt(noise.var)) # add noise

  dt[, newy := newy]
  data.table::setnames(dt, "newy", newvar)

  return(dt[])
}

#' @title Generate survival data
#' @description Survival data is added to an existing data set.
#' @param dtName Name of complete data set
#' @param survDefs Definitions of survival
#' @param digits Number of digits for rounding
#' @return Original matrix with survival time
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
#' @concept generate_data
genSurv <- function(dtName, survDefs, digits = 3) {

  # 'declare
  varname <- NULL
  formula <- NULL

  dtSurv <- copy(dtName)

  for (i in (1:nrow(survDefs))) {
    shape <- dtSurv[, eval(parse(text = survDefs[i, shape]))]
    scale <- dtSurv[, eval(parse(text = survDefs[i, scale]))]
    survPred <- dtSurv[, eval(parse(text = survDefs[i, formula]))]

    u <- stats::runif(n = nrow(dtSurv))

    newColumn <- dtSurv[, list(survx = round((-(log(u) / ((1 / scale) * exp(survPred))))^(shape), digits)), ]

    dtSurv <- data.table::data.table(dtSurv, newColumn)

    data.table::setnames(dtSurv, "survx", as.character(survDefs[i, varname]))
  }

  return(dtSurv[])
}