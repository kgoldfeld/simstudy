#' Are arguments missing?
#'
#' @param ... any number of missing(arg) as named elements
#' e.g. x = missing(x)
#' @noRd
assertNotMissing <- function(...) {
    args <- list(...)
    names <- names(args)
    args <- unlist(args)

    stopifnot(length(args) == length(names))

    if (any(args)) {
        argMissingError(names[args], call = sys.call(-1))
    }
}

#' Are Lengths Equal?
#'
#' @description Checks if all passed vars are of equal Length.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertLengthEqual <- function(...) {
    dots <- dots2argNames(...)
    stopifnot(length(dots$args) >= 2)

    sameLength <- length(unique(lengths(dots$args))) == 1L
    if (!sameLength) {
        lengthError(dots$names, "equal", call = sys.call(-1))
    }
}

#' Check for Class
#'
#' @description Checks if all passed vars inherit from class.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param class Class to check against.
#' @noRd
assertClass <- function(..., class) {
    dots <- dots2argNames(...)
    wrongClass <- !sapply(dots$args, inherits, class)

    if (any(wrongClass)) {
        classError(dots$names[wrongClass], class = class, call = sys.call(-1))
    }
}

#' Check for Value
#'
#' @description Checks if all passed vars have a value other than NULL and NA.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @noRd
assertValue <- function(...) {
    dots <- dots2argNames(...)
    noValue <- sapply(dots$args, is.null) | is.na(dots$args)

    if (any(noValue)) {
        valueError(dots$names[noValue], call = sys.call(-1))
    }
}

#' Var Defined?
#'
#' @description Checks if all passed vars have been defined in dt.
#' @param ... Any number of variables as named elements e.g. var1 = var1.
#' @param dt data.table to check for vars.
#' @noRd
assertInDataTable <- function(vars, dt) {
    notDefined <- !vars %in% names(dt)

    if (any(notDefined)) {
        notDefinedError(vars[notDefined], call = sys.call(-1))
    }
}

#' Ensure Length
#'
#' @description Ensures that var is of length n or 1. Repeats the value n
#' times in the second case. Throws simstudy::lengthError if other length found.
#' @param ... One variable as named element: var = var.
#' @param n Desired length.
#' @return Input var of length n.
#' @noRd
ensureLength <- function(..., n,
                         msg = list(
                             "{ dots$names[[1]] } should be",
                             " either length 1 or { n } but",
                             " is { length(var) }!"
                         )) {
    dots <- dots2argNames(...)
    stopifnot(length(dots$args) == 1)
    var <- dots$args[[1]]

    if (length(var) == 1) {
        return(rep(var, n))
    } else if (length(var) == n) {
        return(var)
    } else {
        lengthError(
            names = dots$names, call = sys.call(-1),
            msg = do.call(glue, msg)
        )
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