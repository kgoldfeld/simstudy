#' Add columns to existing data set
#'
#' @param dtDefs name of definitions for added columns
#' @param dtOld name of data table that is to be updated
#' @param envir Environment the data definitions are evaluated in.
#'  Defaults to [base::parent.frame].
#' @return an updated data.table that contains the added simulated data
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
#' @concept generate_data
#' @md
#' @export
addColumns <- function(dtDefs, dtOld, envir = parent.frame()) {

  # "declares" varname to avoid global NOTE
  varname <- NULL
  formula <- NULL
  dist <- NULL

  assertNotMissing(dtDefs = missing(dtDefs), dtOld = missing(dtOld))
  assertClass(dtDefs = dtDefs, dtOld = dtOld, class = "data.table")

  for (i in seq_len(nrow(dtDefs))) {
    if (i == 1) {
      chkVars <- names(dtOld)
    } else { # check all previously defined vars

      chkVars <- c(dtDefs[1:(i - 1), varname], names(dtOld))
    }

    .evalDef(
      newvar = dtDefs[i, varname],
      newform = dtDefs[i, formula],
      newdist = dtDefs[i, dist],
      defVars = chkVars
    )
  }

  oldkey <- data.table::key(dtOld)

  iter <- nrow(dtDefs)
  n <- nrow(dtOld)
  for (i in (1:iter)) {
    dtOld <- .generate(
      args = dtDefs[i, ],
      n = n,
      dfSim = dtOld,
      idname = oldkey,
      envir = envir
    )
  }

  dtOld <- data.table::data.table(dtOld)
  data.table::setkeyv(dtOld, oldkey)

  return(dtOld[])
}

#' Add a single column to existing data set based on a condition
#'
#' @param condDefs Name of definitions for added column
#' @param dtOld Name of data table that is to be updated
#' @param newvar Name of new column to add
#' @param envir Environment the data definitions are evaluated in.
#'  Defaults to [base::parent.frame].
#' @return An updated data.table that contains the added simulated data
#' @examples
#'
#' # New data set
#'
#' def <- defData(varname = "x", dist = "categorical", formula = ".33;.33")
#' def <- defData(def, varname = "y", dist = "uniform", formula = "-5;5")
#'
#' dt <- genData(1000, def)
#'
#' # Define conditions
#'
#' defC <- defCondition(
#'   condition = "x == 1", formula = "5 + 2*y-.5*y^2",
#'   variance = 1, dist = "normal"
#' )
#' defC <- defCondition(defC,
#'   condition = "x == 2",
#'   formula = "3 - 3*y + y^2", variance = 2, dist = "normal"
#' )
#' defC <- defCondition(defC,
#'   condition = "x == 3",
#'   formula = "abs(y)", dist = "poisson"
#' )
#'
#' # Add column
#'
#' dt <- addCondition(defC, dt, "NewVar")
#'
#' # Plot data
#'
#' library(ggplot2)
#'
#' ggplot(data = dt, aes(x = y, y = NewVar, group = x)) +
#'   geom_point(aes(color = factor(x)))
#' @export
#' @md
#' @concept generate_data
#' @concept condition
addCondition <- function(condDefs, dtOld, newvar, envir = parent.frame()) {

  # 'declare' vars
  varname <- NULL
  formula <- NULL
  dist <- NULL

  assertNotMissing(
    condDefs = missing(condDefs),
    dtOld = missing(dtOld),
    newvar = missing(newvar)
  )
  assertClass(
    condDefs = condDefs,
    dtOld = dtOld,
    class = "data.table"
  )

  cDefs <- copy(condDefs)
  cDefs[, varname := newvar]

  chkVars <- names(dtOld)

  # Check to make sure both formulas are appropriate and reference valid data

  for (i in seq_len(nrow(condDefs))) {
    .evalDef(
      newvar = newvar,
      newform = cDefs[i, formula],
      newdist = cDefs[i, dist],
      defVars = chkVars
    )

    .evalDef(
      newvar = newvar,
      newform = cDefs[i, condition],
      newdist = "nonrandom",
      defVars = chkVars
    )
  }

  oldkey <- data.table::key(dtOld)

  iter <- nrow(cDefs)

  dtNew <- data.table()
  dtTemp <- data.table()

  # Loop through each condition

  for (i in (1:iter)) {
    condition <- cDefs[, condition][i]
    formula <- cDefs[, formula][i]

    dtTemp <- dtOld[eval(parse(text = condition))]
    n <- nrow(dtTemp)

    if (n > 0) {
      dtTemp <- .generate(
        args = cDefs[i, ],
        n = n,
        dfSim = dtTemp,
        idname =  oldkey,
        envir = envir
      )

      dtTemp <- data.table::data.table(dtTemp)
      dtTemp <- dtTemp[, list(get(oldkey), get(newvar))]

      dtNew <- rbind(dtNew, dtTemp)
    }
  }

  setnames(dtNew, c(oldkey, newvar))
  data.table::setkeyv(dtNew, oldkey)

  dtNew <- dtNew[dtOld]

  return(dtNew)
}

#' @title  Add Markov chain
#' @description Generate a Markov chain for n individuals or units by
#' specifying a transition matrix.
#' @param dd data.table with a unique identifier
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
#' @param start0lab Character string that represents name of the integer
#' field containing starting state (State 0) of the chain for each individual.
#' If it is NULL, starting state defaults to 1. Default is NULL.
#' @param trimvalue Integer value indicating end state. If trimvalue is not NULL,
#' all records after the first instance of state = trimvalue will be deleted.
#' @return A data table with n rows if in wide format, or n by chainLen rows
#' if in long format.
#' @examples
#' def1 <- defData(varname = "x1", formula = 0, variance = 1)
#' def1 <- defData(def1, varname = "x2", formula = 0, variance = 1)
#' def1 <- defData(def1,
#'   varname = "S0", formula = ".6;.3;.1",
#'   dist = "categorical"
#' )
#'
#' dd <- genData(20, def1)
#'
#' # Transition matrix P
#'
#' P <- t(matrix(c(
#'   0.7, 0.2, 0.1,
#'   0.5, 0.3, 0.2,
#'   0.0, 0.7, 0.3
#' ),
#' nrow = 3
#' ))
#'
#' d1 <- addMarkov(dd, P, chainLen = 3)
#' d2 <- addMarkov(dd, P, chainLen = 5, wide = TRUE)
#' d3 <- addMarkov(dd, P, chainLen = 5, wide = TRUE, start0lab = "S0")
#' d4 <- addMarkov(dd, P, chainLen = 5, start0lab = "S0", trimvalue = 3)
#' @export
#' @concept generate_data
addMarkov <- function(dd, transMat, chainLen, wide = FALSE, id = "id",
                      pername = "period", varname = "state",
                      widePrefix = "S", start0lab = NULL,
                      trimvalue = NULL) {

  # 'declare' vars created in data.table
  variable <- NULL
  .e <- NULL

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

  # verify id is in data.table dd

  if (!(id %in% names(dd))) stop(paste(id, "is not in data table"))

  ####

  n <- nrow(dd)

  if (is.null(start0lab)) {
    s0 <- rep(1, n)
  } else if (!(start0lab %in% names(dd))) {
    stop(paste("Start state field", start0lab, "does not exist"))
  } else {
    s0 <- dd[, get(start0lab)]
  }

  idlab <- id
  ids <- dd[, get(idlab)]
  xmat <- markovChains(n, transMat, chainLen, s0)

  dx <- data.table::data.table(id = ids, xmat)
  data.table::setnames(dx, "id", ".id") # changed 8/19

  defnames <- paste0("V", seq(1:chainLen))
  tempnames <- paste0(".V", seq(1:chainLen))
  data.table::setnames(dx, defnames, tempnames)

  dx <- merge(dd, dx, by.x = id, by.y = ".id")

  if (wide == TRUE) {
    defnames <- paste0(".V", seq(1:chainLen))
    newnames <- paste0(widePrefix, seq(1:chainLen))
    data.table::setnames(dx, defnames, newnames)
    setkeyv(dx, id)
  } else { # wide = FALSE, so long format

    dx <- data.table::melt(dx,
      id.vars = names(dd),
      value.name = varname, variable.factor = TRUE
    )

    dx[, variable := as.integer(variable)]
    data.table::setnames(dx, "variable", pername)
    setkeyv(dx, id)

    if (!is.null(trimvalue)) {
      dx[, .e := as.integer(get(varname) == trimvalue)]
      dx <- trimData(dx, pername, eventvar = ".e", id)
      dx[, .e := NULL]
    }
  }

  dx[]
}

#' Add multi-factorial data
#'
#' @param dtOld data.table that is to be modified
#' @param nFactors Number of factors (columns) to generate.
#' @param levels Vector or scalar. If a vector is specified, it must be
#' the same length as nFatctors. Each value of the vector represents the
#' number of levels of each corresponding factor. If a scalar is specified,
#' each factor will have the same number of levels. The default is 2 levels
#' for each factor.
#' @param coding String value to specify if "dummy" or "effect" coding is used.
#' Defaults to "dummy".
#' @param colNames A vector of strings, with a length of nFactors. The strings
#' represent the name for each factor.
#' @return A data.table that contains the added simulated data. Each new column contains
#' an integer.
#' @examples
#' defD <- defData(varname = "x", formula = 0, variance = 1)
#'
#' DT <- genData(360, defD)
#' DT <- addMultiFac(DT, nFactors = 3, levels = c(2, 3, 3), colNames = c("A", "B", "C"))
#' DT
#' DT[, .N, keyby = .(A, B, C)]
#'
#' DT <- genData(300, defD)
#' DT <- addMultiFac(DT, nFactors = 3, levels = 2)
#' DT[, .N, keyby = .(Var1, Var2, Var3)]
#' @export
#' @concept generate_data
addMultiFac <- function(dtOld, nFactors, levels = 2, coding = "dummy", colNames = NULL) {

  # 'declare' vars
  count <- NULL

  if (nFactors < 2) stop("Must specify at least 2 factors")
  if (length(levels) > 1 & (length(levels) != nFactors)) stop("Number of levels does not match factors")

  if (is.null(colNames)) {
    cn <- paste0("Var", 1:nFactors)
    if (any(cn %in% names(dtOld))) stop("Default column name(s) already in use")
  } else {
    if (any(colNames %in% names(dtOld))) stop("At least one column name already in use")
  }

  if (length(levels) == 1) {
    combos <- prod(rep(levels, nFactors))
  } else {
    combos <- prod(levels)
  }

  each <- ceiling(nrow(dtOld) / combos)
  extra <- nrow(dtOld) %% combos

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

  dnew <- data.table(as.data.frame(lapply(
    expand.grid(x),
    function(x) rep(x, each = each)
  )))
  dnew[, count := rep(c(1:each), length.out = .N)]
  neworder <- sample(1:nrow(dnew), nrow(dnew), replace = FALSE)
  dnew <- dnew[neworder]

  if (extra > 0) {
    full <- dnew[count < each]
    partial <- dnew[count == each][1:extra]

    all <- rbind(full, partial)
  } else {
    all <- copy(dnew)
  }

  all <- all[, -"count"]

  if (!is.null(colNames)) setnames(all, colNames)

  origNames <- copy(names(all))
  dreturn <- cbind(dtOld, all)

  return(dreturn[])
}
