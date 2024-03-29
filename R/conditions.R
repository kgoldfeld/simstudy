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

#' Value below threshold error
#'
#' @param names Names of fields below threshold
#' @param prop Which property should hold e.g. "equal".
#' @param call sys.call to pass on to the error.
#' @param msg Error message as glueCollapse'able string.
#' @noRd
minError <- function(names,
                        value = 1,
                        call = sys.call(-1),
                        msg = "{ names *} should be at least { value }!", ...) {
  c <- condition(
    c("simstudy::minError", "error"),
    glueCollapse(msg), call, ...
  )
  stop(c)
}

#' Order Error
#'
#' @param name Name of the incorrectly ordered vector
#' @param prop Which property should hold e.g. "ascending".
#' @param call sys.call to pass on to the error.
#' @param msg Error message as glueCollapse'able string.
#' @noRd
orderError <- function(name,
                       prop = "ascending",
                       call = sys.call(-1),
                       msg = "{ name } should be in { prop } order!", ...) {
  c <- condition(
    c("simstudy::wrongOrder", "error"),
    glueCollapse(msg), call, ...
  )
  stop(c)
}

#' Positive Error
#'
#' @param name Name of the vector with non-positive values
#' @param prop Which property should hold e.g. "positive".
#' @param call sys.call to pass on to the error.
#' @param msg Error message as glueCollapse'able string.
#' @noRd
signError <- function(name,
                      prop = "positive",
                      call = sys.call(-1),
                      msg = "{ name } should be { prop}!", ...) {
  c <- condition(
    c("simstudy::wrongSign", "error"),
    glueCollapse(msg), call, ...
  )
  stop(c)
}

#' Probability Error
#'
#' @param name Name of the vector with improper probabilities
#' @param call sys.call to pass on to the error.
#' @param msg Error message as glueCollapse'able string.
#' @noRd
probError <- function(name,
                      call = sys.call(-1),
                      msg = "probabilities { name } should all be between 0 and 1!", ...) {
  c <- condition(
    c("simstudy::probError", "error"),
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
  message <- glueCollapse(
    "{ names *} should be/contain only { type }(s)!",
    msg
  )

  c <- condition(
    c("simstudy::wrongType", "error"),
    message, call, ...
  )
  stop(c)
}

#' Null Error
#'
#' @param names Names of the vars.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
nullError <- function(names, call = sys.call(-1), msg = "", ...) {
  message <- glueCollapse(
    "{ names *} should not be NULL!",
    msg
  )
  
  c <- condition(
    c("simstudy::nullError", "error"),
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
noValueError <- function(names, call = sys.call(-1),
                         msg = list(
                           "{ names *} need{plr} to have a value",
                           " other than NULL or NA and a length > 0!"
                         ), ...) {
  plr <- ifelse(length(names) > 1, "", "s")

  if (is.list(msg)) {
    message <- do.call(glueCollapse, msg)
  } else {
    message <- glueCollapse(msg)
  }

  c <- condition(
    c("simstudy::noValue", "error"),
    message, call, ...
  )
  stop(c)
}

#' Generic Value Error
#'
#' @param names Names of the vars.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @param var Additional var to access in msg via glue.
#' @param call sys.call to pass on to the error.
#' @noRd
valueError <- function(names, msg, var = NULL, call = sys.call(-1), ...) {
  if (is.list(msg)) {
    message <- do.call(glueCollapse, msg)
  } else {
    message <- glueCollapse(msg)
  }

  c <- condition(
    c("simstudy::valueError", "error"),
    message, call, ...
  )
  stop(c)
}

#' Generic Value Warning
#'
#' @param names Names of the vars.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @param var Additional var to access in msg via glue.
#' @param call sys.call to pass on to the error.
#' @noRd
valueWarning <- function(names, msg, var = NULL, call = sys.call(-1), ...) {
  if (is.list(msg)) {
    message <- do.call(glueCollapse, msg)
  } else {
    message <- glueCollapse(msg)
  }

  c <- condition(
    c("simstudy::valueWarning", "warning"),
    message, call, ...
  )
  warning(c)
}

#' Not Unique Error
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

#' Element in vector
#'
#' @param nameVar Value(s) in vector.
#' @param nameVec Name of the vector.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
alreadyInVectorError <- function(elementOfVar, nameVar, nameVec,
                                 call = sys.call(-1), msg = "", ...) {
  vars <- paste(elementOfVar, collapse = ", ")
  message <- glueCollapse(
    "{ vars } (in { nameVar }) should not also be in vector { nameVec }!", msg
  )

  c <- condition(
    c("simstudy::alreadyInVector", "error"),
    message, call, ...
  )
  stop(c)

}

#' Not Positive Semi-definite Error
#'
#' @param var Name of matrix.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
notPositiveSemiDefiniteError <- function(var,
                                     call = sys.call(-1),
                                     msg = "", ...) {
  message <- glueCollapse("{ var *} is not positive semi-definite!", msg)

  c <- condition(
    c("simstudy::notPositiveSemiDefinite", "error"),
    message, call, ...
  )
  stop(c)
}

#' Option Invalid Warning
#'
#' @param name Name of invalid option
#' @param value Value of name
#' @param options Valid options for name
#' @param default Value name will default to.
#' @param call sys.call to pass on to the warning.
#' @return Warning with information which value name will take.
#' @noRd
optionInvalidWarning <- function(name,
                                 value,
                                 options,
                                 default,
                                 call = sys.call(-1),
                                 ...) {
  message <- glueCollapse(
    "Argument {name}: '{value}' invalid.",
    " Valid options: {options *}.",
    "\nDefaulting to '{default}'."
  )
  c <- condition(c("simstudy::optionInvalid", "warning"), message, call, ...)
  warning(c)
}

#' Option Invalid Error
#'
#' @param name Name of invalid option
#' @param value Value of name
#' @param options Valid options for name
#' @param msg Additional message to be displayed.
#' @param call sys.call to pass on to the error.
#' @noRd
optionInvalidError <- function(name,
                               value,
                               options,
                               call = sys.call(-1),
                               msg = "",
                               ...) {
  message <- glueCollapse(
    "Argument {name}: '{value}' invalid.",
    " Valid options: {options *}.",
    msg
  )
  c <- condition(c("simstudy::optionInvalid", "error"), message, call, ...)
  stop(c)
}

#' Mismatch of arguments Warning
#'
#' @param name_specified Name of argument specified
#' @param name_null Name of argument not specified
#' @param call sys.call to pass on to the warning.
#' @return Error message
#' @noRd
mismatchError <- function(name_specified, name_null,
                          call = sys.call(-1),
                          ...) {
  message <- glueCollapse(
    "Argument {name_specified} has been specified",
    " without argument {name_null}.",
    " If specifying one argument, ",
    " the other must be specified."
  )
  c <- condition(
    c("simstudy::mismatch", "error"),
    message, call, ...
  )
  stop(c)
}

### Added by ME 5/9/22

#' Variables not equal to value error.
#'
#' @param names Names of the variables.
#' @param val Value to be equal to.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
notEqualError <- function(names, val, call = sys.call(-1), msg = "", ...) {
  message <- glueCollapse(
    "{ names *} not equal to {val}!", msg
  )

  c <- condition(
    c("simstudy::notEqual", "error"),
    message, call, ...
  )
  stop(c)
}

#' Variables not equal to value error.
#'
#' @param names Names of the variables.
#' @param val Value to be not equal to.
#' @param call sys.call to pass on to the error.
#' @param msg Additional information for the error message as
#' glueCollapse'able string.
#' @noRd
equalError <- function(names, val, call = sys.call(-1), msg = "", ...) {
  message <- glueCollapse(
    "{ names *} equal to {val}!", msg
  )
  
  c <- condition(
    c("simstudy::equal", "error"),
    message, call, ...
  )
  stop(c)
}
