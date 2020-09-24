#' Condition Constructor
#'
#' @description Creates custom conditions for error handling.
#' @param subclass String or character vector of class names (e.g. "error")
#' @param message Message to pass to the user as string.
#' @param call Call to display when condition is triggered.
#' @param ... additional arguments to pass to structure()
#' @return Condition inhereting from "condition" and subclass
#' @noRd
condition <- function(subclass, message, call = sys.call(-1), ...) {
    structure(
        class = c(subclass, "condition"),
        list(message = message, call = call),
        ...
    )
}

#' Argument Missing Error
#'
#' @param args Missing argument(s) as character vector.
#' @noRd
argMissingError <- function(args, call = sys.call(-1), msg = "", ...) {
    plr <- ifelse(length(args) > 1, "s are", " is")
    message <- glueCollapse(
        "The following argument{ plr }",
        " missing with no default: { args *} ",
        msg
    )
    c <- condition(c("simstudy::missingArgument", "error"), message, call, ...)

    stop(c)
}

#' Length Mismatch Error
#'
#' @param names Names of the mismatched vars.
#' @param prop Which property should hold e.g. "equal".
#' @param call sys.call to pass on to the error.
#' @param msg Error message as glueCollapse'able string.
#' @noRd
lengthError <- function(names,
                        prop = "equal",
                        call = sys.call(-1),
                        msg = "{ names *} should be of { prop } length!", ...) {
    c <- condition(
        c("simstudy::lengthMismatch", "error"),
        glueCollapse(msg), call, ...
    )
    stop(c)
}

#' Wrong Class Error
#'
#' @param names Names of the vars.
#' @param class Class the variable should inherit.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
classError <- function(names, class, call = sys.call(-1), msg = "", ...) {
    message <- glueCollapse("{ names *} should be a { class }!", msg)

    c <- condition(
        c("simstudy::wrongClass", "error"),
        message, call, ...
    )
    stop(c)
}

#' Wrong Type Error
#'
#' @param names Names of the vars.
#' @param type Type the variable should inherit.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
typeError <- function(names, type, call = sys.call(-1), msg = "", ...) {
    message <- glueCollapse("{ names *} should be/contain only { type }(s)!", msg)

    c <- condition(
        c("simstudy::wrongType", "error"),
        message, call, ...
    )
    stop(c)
}

#' No Value Error
#'
#' @param names Names of the vars.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
noValueError <- function(names, call = sys.call(-1), msg = "", ...) {
    plr <- ifelse(length(names) > 1, "", "s")
    message <- glueCollapse(
        "{ names *} need{plr} to have a value",
        " other than NULL or NA an a length > 0!",
        msg
    )

    c <- condition(
        c("simstudy::noValue", "error"),
        message, call, ...
    )
    stop(c)
}

#' Not Uniwue Error
#'
#' @param names Names of the vars.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
notUniqueError <- function(names, call = sys.call(-1), msg = "", ...) {
    plr <- ifelse(length(names) > 1, "", "s")
    message <- glueCollapse(
        "{ names *} need{plr} to have only unique values",
        msg
    )

    c <- condition(
        c("simstudy::uniqueValue", "error"),
        message, call, ...
    )
    stop(c)
}

#' Var Not Defined Error
#'
#' @param names Names of the vars.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
notDefinedError <- function(names, call = sys.call(-1), msg = "", ...) {
    plr <- ifelse(length(names) > 1, "s", "")
    message <- glueCollapse(
        "Variable{plr} { names *} not previously defined!", msg
    )

    c <- condition(
        c("simstudy::notDefined", "error"),
        message, call, ...
    )
    stop(c)
}

#' Var Already Defined Error
#'
#' @param names Names of the vars.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
alreadyDefinedError <- function(names, call = sys.call(-1), msg = "", ...) {
    plr <- ifelse(length(names) > 1, "s", "")
    message <- glueCollapse(
        "Variable{plr} { names *} previously defined!", msg
    )

    c <- condition(
        c("simstudy::alreadyDefined", "error"),
        message, call, ...
    )
    stop(c)
}

#' Not Positive Definite Error
#'
#' @param var Name of matrix.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
notPositiveDefiniteError <- function(var,
                                     call = sys.call(-1),
                                     msg = "", ...) {
  message <- glueCollapse("{ var *} is not positive definite!", msg)

  c <- condition(
    c("simstudy::notPositiveDefinite", "error"),
    message, call, ...
  )
  stop(c)
}
