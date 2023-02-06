#' Are arguments missing?
#'
#' @param ... any number of missing(arg) as named elements
#' e.g. x = missing(x)
#' @noRd
assertNotMissing <- function(..., call = sys.call(-1)) {
  args <- list(...)
  names <- names(args)
  args <- unlist(args)

  stopifnot(length(args) == length(names))

  if (any(args)) {
    argMissingError(names[args], call = call)
  }
}

#' Are Lengths Equal?
#'
#' @description Checks if all passed vars are of equal Length. Caveat:
#' length(matrix) = number of elements but length(data.frame) = number
#'  of columns.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertLengthEqual <- function(..., call = sys.call(-1)) {
  dots <- dots2argNames(...)
  stopifnot(length(dots$args) >= 2)

  sameLength <- length(unique(lengths(dots$args))) == 1L
  if (!sameLength) {
    lengthError(dots$names, "equal", call = call)
  }
}

#' Are arguments equal to value?
#'
#' @description Checks if all passed vars are equal to given value.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param val Value to check if variables are equal to
#' @noRd
assertEqual <- function(..., val, call = sys.call(-1)) {
  dots <- dots2argNames(...)

  notEqual <- !sapply(dots$args, function(i) {
    i == val
  })
  if (any(notEqual)) {
    notEqualError(dots$names, val, call = call)
  }
}

#' Are arguments not equal to value?
#'
#' @description Checks if all passed vars are not equal to given value.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param val Value to check if variables are equal to
#' @noRd
assertNotEqual <- function(..., val, call = sys.call(-1), msg="") {
  dots <- dots2argNames(...)
  
  Equal <- !sapply(dots$args, function(i) {
    i != val
  })
  if (any(Equal)) {
    equalError(dots$names, val, call = call, msg)
  }
}

#' Is length correct?
#'
#' @description Checks if all passed vars are of length 'length'. Caveat:
#' length(matrix) = numer of elements but length(data.frame) = number
#'  of columns.
#' @param length Length to check
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertLength <- function(..., length, call = sys.call(-1)) {
  dots <- dots2argNames(...)
  correctLength <- lengths(dots$args) == length
  if (!all(correctLength)) {
    lengthError(dots$names[!correctLength],
      prop = length,
      msg = "{ names *} should be of length { prop }!", call = call
    )
  }
}

#' Is length correct?
#'
#' @description Checks if all passed vars are of at least
#' of length 'length'. Caveat: length(matrix) = numer of elements
#' but length(data.frame) = number of columns.
#' @param length Length to check
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertAtLeastLength <- function(..., length, call = sys.call(-1)) {
  dots <- dots2argNames(...)
  correctLength <- lengths(dots$args) >= length
  if (!all(correctLength)) {
    lengthError(dots$names[!correctLength],
      prop = length,
      msg = "{ names *} should be at least length { prop }!", call = call
    )
  }
}

#' Are all elements of vector probabilities?
#'
#' @description Checks if passed vector includes only proper probabilities
#' @param vec Vector under consideration
#' @noRd
assertAtLeast <- function(..., minVal, call = sys.call(-1)) {
  
  dots <- dots2argNames(...)
  correctSize <- dots$args >= minVal
  
  if (!all(correctSize)) {
    minError(dots$names[!correctSize], minVal, call = call)
  }
}

#' Check for Class
#'
#' @description Checks if all passed vars inherit from class.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param class Class to check against.
#' @noRd
assertClass <- function(..., class, call = sys.call(-1)) {
  dots <- dots2argNames(...)
  wrongClass <- !sapply(dots$args, inherits, class)

  if (any(wrongClass)) {
    classError(dots$names[wrongClass], class = class, call = call)
  }
}

#' Check for Type
#'
#' @description Checks if all passed vars and their content are of type.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param type Type to check against.
#' @param deep Should the elements of the variable be tested?
#' @noRd
assertType <- function(..., type, deep = TRUE, call = sys.call(-1)) {
  dots <- dots2argNames(...)
  reduceType <- function(arg) {
    if (deep && length(arg) >= 1) {
      types <- sapply(arg, typeof)
      if (length(types) == 1) {
        return(types == type)
      }
      return(Reduce(`&&`, types == type))
    } else {
      typeof(arg) == type
    }
  }
  wrongType <- !sapply(dots$args, reduceType)
  if (any(wrongType)) {
    typeError(dots$names[wrongType], type = type, call = call)
  }
}

#' Check for Numeric
#'
#' @description Checks if all passed vars and their content are not null
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertNotNull <- function(..., call = sys.call(-1)) {
  dots <- dots2argNames(...)
  isNull <- sapply(dots$args, function(x) is.null(x))
  if (any(isNull)) {
    nullError(dots$names[isNull], type = "numeric", call = call)
  }
}

#' Check for Numeric
#'
#' @description Checks if all passed vars and their content are numeric.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertNumeric <- function(..., call = sys.call(-1)) {
  dots <- dots2argNames(...)
  reduceNumeric <- function(arg) {
    types <- sapply(arg, typeof)
    Reduce(`&&`, types == "integer" |
      types == "double" | types == "numeric")
  }
  notNumeric <- !sapply(dots$args, reduceNumeric)
  if (any(notNumeric)) {
    typeError(dots$names[notNumeric], type = "numeric", call = call)
  }
}

#' Check for Integer
#'
#' @description Checks if all passed vars and their content are integers or can
#' be coerced to integer without loss of information.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertInteger <- function(..., type, call = sys.call(-1)) {
  assertNumeric(..., call = call)
  dots <- dots2argNames(...)
  checkInteger <- function(arg) {
    arg <- unlist(arg)
    all(arg == as.integer(arg))
  }
  notInteger <- !sapply(dots$args, checkInteger)
  if (any(notInteger)) {
    typeError(dots$names[notInteger], type = "integer", call = call)
  }
}

#' Check for Factor
#'
#' @description Checks if all passed vars and their content are factors.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertFactor <- function(..., type, call = sys.call(-1)) {
  # assertNumeric(..., call = call)
  dots <- dots2argNames(...)
  notFactor <- !sapply(dots$args, function(fac) {
    is.factor(fac)
  })
  if (any(notFactor)) {
    typeError(dots$names[notFactor], type = "factor", call = call)
  }
}

#' Check for numeric matrix (character matrices not allowed)
#'
#' @description Checks if all passed vars and their content are numeric matrices
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertNumericMatrix <- function(..., call = sys.call(-1)) {
  dots <- dots2argNames(...)
  notNumericMatrix <- !sapply(dots$args, function(mat) {
    isMatrix <- is.matrix(mat)
    isNumeric <- is.numeric(c(mat))
    isMatrix & isNumeric
  })
  if (any(notNumericMatrix)) {
    typeError(dots$names[notNumericMatrix], type = "numeric matrix", call = call)
  }
}

#' Check for Value
#'
#' @description Checks if all passed vars have a value other than NULL and NA
#' and a length > 0.
#' Does not check if all elements contained are NULL or NA.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertValue <- function(..., call = sys.call(-1)) {
  dots <- dots2argNames(...)
  noValue <- sapply(dots$args, is.null) |
    is.na(dots$args) |
    lengths(dots$args) == 0

  if (any(noValue)) {
    noValueError(dots$names[noValue], call = call)
  }
}

#' Are Values Unique?
#'
#' @description Checks if all passed vars have only unique values.
#' @param ... Any number of list or vectors as named elements e.g. var1 = var1.
#' @noRd
assertUnique <- function(..., call = sys.call(-1)) {
  dots <- dots2argNames(...)
  stopifnot(sapply(dots$args, is, "vector") |
    sapply(dots$args, is, "list") | sapply(dots$args, is, "glue") |
    lengths(dots$args) == 1L)

  isUnique <- function(var) {
    length(var) == length(unique(var))
  }
  notUnique <- !sapply(dots$args, isUnique)
  if (any(notUnique)) {
    notUniqueError(dots$names[notUnique], call = call)
  }
}

#' Var Defined?
#'
#' @description Checks if all passed vars have been defined in dt.
#' @param vars Name of variables to check.
#' @param dt data.table to check for vars. Can also be character vector with
#' defined variables.
#' @noRd
assertInDataTable <- function(vars, dt, call = sys.call(-1)) {
  if (is.data.frame(dt)) {
    dtNames <- names(dt)
  } else {
    stopifnot(is.character(dt))
    dtNames <- dt
  }
  notDefined <- !vars %in% dtNames

  if (any(notDefined)) {
    notDefinedError(vars[notDefined], call = call)
  }
}

#' Var Not Defined?
#'
#' @description Checks if all passed vars have not been defined in dt.
#' @param ... vars Name of variables to check.
#' @param dt data.table to check for vars. Can also be character vector with
#' defined variables.
#' @noRd
assertNotInDataTable <- function(vars, dt, call = sys.call(-1)) {
  if (is.data.frame(dt)) {
    dtNames <- names(dt)
  } else {
    stopifnot(is.character(dt))
    dtNames <- dt
  }
  areDefined <- vars %in% dtNames

  if (any(areDefined)) {
    alreadyDefinedError(vars[areDefined], call = call)
  }
}

#' Check if single string not in vector of strings
#'
#' @description Checks if all passed vars have not been defined in dt.
#' @param ... vars Name of variables to check.
#' @param dt data.table to check for vars. Can also be character vector with
#' defined variables.
#' @noRd
assertNotInVector <- function(var, vec, call = sys.call(-1)) {
  areInVector <- var %in% vec
  elementOfVar <- var[areInVector]
  nameVar <- deparse(substitute(var))
  nameVec <- deparse(substitute(vec))

  if (any(areInVector)) {
    alreadyInVectorError(elementOfVar, nameVar, nameVec, call = call)
  }
}

#' Is vector in ascending order?
#'
#' @description Checks if passed vector is in ascending order
#' @param vec Vector under consideration
#' @noRd
assertAscending <- function(vec, call = sys.call(-1)) {
  name <- deparse(substitute(vec))
  ascending <- all(vec[order(vec)] == vec) & (length(unique(vec)) == length(vec))

  if (!ascending) {
    orderError(name, "ascending", call = call)
  }
}

#' Is vector in descending order?
#'
#' @description Checks if passed vector is in descending order
#' @param vec Vector under consideration
#' @noRd
assertDescending <- function(vec, call = sys.call(-1)) {
  name <- deparse(substitute(vec))
  descending <- all(vec[rev(order(vec))] == vec) & (length(unique(vec)) == length(vec))

  if (!descending) {
    orderError(name, "descending", call = call)
  }
}


#' Are all elements of vector positive?
#'
#' @description Checks if passed vector is includes only positive values
#' @param vec Vector under consideration
#' @noRd
assertPositive <- function(vec, call = sys.call(-1)) {
  name <- deparse(substitute(vec))
  positive <- all(vec > 0)

  if (!positive) {
    signError(name, "positive", call = call)
  }
}

#' Are all elements of vector probabilities?
#'
#' @description Checks if passed vector includes only proper probabilities
#' @param vec Vector under consideration
#' @noRd
assertProbability <- function(vec, call = sys.call(-1)) {
  name <- deparse(substitute(vec))
  probability <- all(vec >= 0 & vec <= 1)

  if (!probability) {
    probError(name, call = call)
  }
}



#' Ensure Length
#'
#' @description Ensures that var is of length n or 1. Repeats the value n
#' times in the second case. Throws simstudy::lengthError if other length found.
#' @param ... One variable as named element: var = var.
#' @param n Desired length.
#' @return Invisibly returns input var with length n.
#' @noRd
ensureLength <- function(..., n,
                         msg = list(
                           "{ dots$names[[1]] } should be",
                           " either length 1 or { n } but",
                           " is { length(var) }!"
                         ),
                         call = sys.call(-1)) {
  dots <- dots2argNames(...)
  stopifnot(length(dots$args) == 1, !missing(n))
  var <- dots$args[[1]]

  if (length(var) == 1) {
    invisible(rep(var, n))
  } else if (length(var) == n) {
    invisible(var)
  } else {
    lengthError(
      names = dots$names, call = call,
      msg = do.call(glue, msg)
    )
  }
}

#' Ensure Input is Matrix
#'
#' @description Checks if var is a matrix or vector, if vector converts it to 1
#' row matrix.
#' @param var Variable to check
#' @return var as matrix.
#' @importFrom methods is
#' @noRd
ensureMatrix <- function(var) {
  stopifnot(is(var, "matrix") || is(var, "vector"))

  if (is(var, "matrix")) {
    return(var)
  }

  if (is(var, "vector")) {
    return(matrix(var, nrow = 1))
  }
}

#' Check Matrix is Positve Definite
#'
#' @description Checks if Matrix is positiv definite,
#' @param ... A matrix as named element e.g. var1 = var1.
#' @noRd
assertPositiveDefinite <- function(..., call = sys.call(-1)) {
  stopifnot(...length() == 1)
  dots <- dots2argNames(...)
  matrix <- dots$args[[1]]
  isSym <- isSymmetric(round(matrix, 7))
  eigenValues <- round(unlist(eigen(matrix, only.values = TRUE)), 8)

  if (!all(eigenValues >= 0) || !isSym) {
    notPositiveDefiniteError(dots$names, call = call)
  }
}

#' Check Option Valid
#'
#' @param ... An argument as named element e.g. var1 = var1.
#' @param value Value of the argument
#' @param options Valid options for the argument
#' @param msg Additonal message to be displayed.
#' @param call sys.call to pass on to the error.
#' @noRd
assertOption <- function(..., options, msg = "", call = sys.call(-1)) {
  stopifnot(...length() == 1)
  dots <- dots2argNames(...)
  notValid <- !dots$args[[1]] %in% options

  if (notValid) {
    optionInvalidError(
      name = dots$names[[1]],
      value = dots$args[[1]],
      options = options,
      msg = msg,
      call = call
    )
  }
}

#' Check Values in Range
#' !! Slow for a larg number of elements !!
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param range Numeric vector of range as c(min,max).
#' @param minCheck Comparison that is made with the lower boundary.
#' @param maxCheck Comparison that is made with the upper boundary.
#' @noRd
assertInRange <- function(...,
                          range,
                          minCheck = ">=",
                          maxCheck = "<=",
                          call = sys.call(-1)) {
  assertLength(
    minCheck = minCheck, maxCheck = maxCheck,
    length = 1,
    call = call
  )
  assertLength(range = range, length = 2)
  assertNumeric(range = range)
  assertOption(
    minCheck = minCheck,
    options = c("<", "<=", "==", "!=", ">", ">=")
  )
  assertOption(
    maxCheck = maxCheck,
    options = c("<", "<=", "==", "!=", ">", ">=")
  )
  dots <- dots2argNames(...)
  do.call(function(...) assertNumeric(..., call = call), dots$args)

  createExpressions <- function(values) {
    glue(
      "{values} {minCheck} {range[[1]]}",
      " && {values} {maxCheck} {range[[2]]} "
    )
  }

  inRange <- function(values) {
    all(
      sapply(
        lapply(createExpressions(values), function(x) parse(text = x)),
        eval
      )
    )
  }

  notInRange <- !sapply(dots$args, inRange)

  if (any(notInRange)) {
    valueError(
      names = dots$names[notInRange], var = range,
      msg = list(
        "Some values in {names *} are not in the",
        " range from {var[[1]]} to {var[[2]]}."
      ),
      call = call
    )
  }
}

#' Ensure Option Valid
#'
#' @param ... An argument as named element e.g. var1 = var1.
#' @param value Value of the argument
#' @param options Valid options for the argument
#' @param default Value argument will default to.
#' @param call sys.call to pass on to the warn.
#' @return The argument or default.
#' @noRd
ensureOption <- function(..., options, default, call = sys.call(-1)) {
  stopifnot(...length() == 1)
  dots <- dots2argNames(...)
  notValid <- !dots$args[[1]] %in% options

  if (notValid) {
    optionInvalidWarning(
      dots$names[[1]],
      dots$args[[1]],
      options,
      default,
      call
    )
    return(default)
  }
  dots$args[[1]]
}

#' Ensure Names are Valid
#'
#' @param names A character vector of names to check.
#' @param call sys.call to pass on to the warn.
#' @return The modified names.
#' @noRd
ensureValidName <- function(names, call = sys.call(-1)) {
  notValid <- !.isValidVarName(names)

  if (any(notValid)) {
    # TODO pluralize
    valueWarning(
      msg = list(
        "Variable name(s) '{var *}' not a valid R variable name,",
        "and will be converted to: '{make.names(var)}'."
      ), var = names[notValid],
      call. = call
    )
    return(make.names(names))
  }

  names
}

#' Dots to Args & Names
#'
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @return A list containing the arguments and names.
#' @noRd
dots2argNames <- function(...) {
  stopifnot(...length() != 0)
  args <- list(...)
  names <- names(args)
  names <- names[names != ""]
  stopifnot(length(args) == length(names))

  list(args = args, names = names)
}
