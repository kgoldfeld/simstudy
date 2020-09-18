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
argMissingError <- function(args, call = sys.call(-1), ...) {
    plr <- ifelse(length(args) > 1, "s are", " is")
    message <- glueCollapse(
        "The following argument{ plr }",
        " missing with no default: { args *} "
    )
    c <- condition(c("simstudy::MissingArgument", "error"), message, call, ...)
    stop(c)
}