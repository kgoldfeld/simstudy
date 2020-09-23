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
#' @description Checks if all passed vars are of equal Length.
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

#' Is length correct?
#'
#' @description Checks if all passed vars are of length 'length'.
#' @param length Length to check
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertLength <- function(..., length, call = sys.call(-1)) {
    dots <- dots2argNames(...)
    correctLength <- lengths(dots$args) == length
    if (!correctLength) {
        lengthError(dots$names[!correctLength],
        prop = length,
            msg = "{ names *} should be of length { prop }!", call = call
        )
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
#' @noRd
assertType <- function(..., type, call = sys.call(-1)) {
    dots <- dots2argNames(...)
    reduceType <- function(arg) {
        types <- sapply(arg, typeof)
        if (length(types) == 1) {
            return(types == type)
        }
        Reduce(`&&`, types == type)
    }
    wrongType <- !sapply(dots$args, reduceType)
    if (any(wrongType)) {
        typeError(dots$names[wrongType], type = type, call = call)
    }
}

#' Check for Integer
#'
#' @description Checks if all passed vars and their content are of integers.
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

#' Check for Value
#'
#' @description Checks if all passed vars have a value other than NULL and NA.
#' Does not check if all elements contained are NULL or NA.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertValue <- function(..., call = sys.call(-1)) {
    dots <- dots2argNames(...)
    noValue <- sapply(dots$args, is.null) | is.na(dots$args)

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
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param dt data.table to check for vars.
#' @noRd
assertInDataTable <- function(vars, dt, call = sys.call(-1)) {
    notDefined <- !vars %in% names(dt)

    if (any(notDefined)) {
        notDefinedError(vars[notDefined], call = call)
    }
}

#' Var Not Defined?
#'
#' @description Checks if all passed vars have not been defined in dt.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param dt data.table to check for vars.
#' @noRd
assertNotInDataTable <- function(vars, dt, call = sys.call(-1)) {
    areDefined <- vars %in% names(dt)

    if (any(areDefined)) {
        alreadyDefinedError(vars[areDefined], call = call)
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
    stopifnot(length(dots$args) == 1)
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

    if (is(var, "vector")) {
        return(matrix(var, nrow = 1))
    } else {
        return(var)
    }
}

#' Dots to Args & Names
#'
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @return A list containing the arguments and names.
#' @noRd
dots2argNames <- function(...) {
    args <- list(...)
    names <- names(args)
    names <- names[names != ""]

    stopifnot(length(args) == length(names))

    list(args = args, names = names)
}