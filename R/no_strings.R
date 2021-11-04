#' @import cli
#' @importFrom rlang enexpr
#' @importFrom purrr map
#' @importFrom magrittr %>%
def_data <- function(dtDefs = NULL,
                     varname,
                     formula,
                     variance = 0,
                     dist = "normal",
                     link = "identity",
                     id = "id",
                     param1 = 0,
                     param2 = "identity") {
  assertNotMissing(name = missing(varname), formula = missing(formula))

  if (!missing(variance) || !missing(link)) {
    cli_warn(c(
      "The arguments {.arg variance} and {.arg link} are deprecated",
      "i" = "Use {.arg param1} and {.arg param2} instead."
    ), .frequency = "once", .frequency_id = "new args")

    # this is only needed for backwardscompatability
    if (!missing(variance)) {
      param1 <- enexpr(variance)
    }
    if (!missing(link)) {
      param2 <- enexpr(link)
    }
  } else {
    param1 <- enexpr(param1)
    param2 <- enexpr(param2)
  }

  if (is.null(dtDefs)) {
    dtDefs <- data.table::data.table()
    attr(dtDefs, "id") <- id
  } else {
    assertClass(dtDefs = dtDefs, class = "data.table")
    dtDefs <- copy(dtDefs)
  }

  # this creates an S3 object with the class dist
  def <- new_definition(
    list(
      dtDefs = dtDefs,
      varname = enexpr(varname),
      formula = enexpr(formula),
      param1 = param1,
      param2 = param2
    ),
    dist
  )

  # so we can dispatch the correct method here
  validate(def)

  # turn the expressions into strings so we get readable output
  l <- def[-1] %>%
    map(deparse) %>%
    list(dtDefs, .)

  defNew <- rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")

  defNew
}

#' Definition Constructor
#'
#' @param x List with the elements of the definition:
#'   `dtDefs`, `varname`, `formula`, `param1`, `param2`
#' @param dist String with the distribution name.
#' @return S3 object with class dist.
#' @export
#' @md
#' @concept custom_distributions
new_definition <- function(x, dist) {
  stopifnot(is.list(x))
  stopifnot(length(x) == 5)
  structure(x, class = c(dist, "list"))
}


#' Validate a definition
#'
#' Each distribution has a validate method, that throws an error
#'  if the definition is not valid. Silent if valid.
#' @param def S3 definition object with class `dist`.
#' @export
#' @seealso [new_definition()]
#' @concept custom_distributions
validate <- function(def) UseMethod("validate")

#' @export
validate.normal <- function(def) {
  if (rlang::is_string(def$formula)) {
    .isValidArithmeticFormula(def$formula, "")
  }
}

#' @export
validate.default <- function(x) {
  cli::cli_warn(
    "No {.fun validate} function for distribution {.cls {class(x)}} found."
  )
}

#' Generate Data
#'
#' Each distribution has a generate method, that generates data.
#' The expected format is...
#' @param def Row of a definition table containing expressions with the elements of the definition:
#'  `dtDefs`, `varname`, `formula`, `param1`, `param2`, `data`
#' @export
#' @concept custom_distributions
generate <- function(def, env = parent.frame()) {
  stopifnot(is.list(def))
  stopifnot(length(def) == 5)
  stopifnot(is.environment(env))

  def$env <- env
  UseMethod("generate")
}

#' @export
generate.default <- function(x) {
  cli::cli_abort(
    "No {.fun generate} function for distribution {.cls {class(x)}} found."
  )
}

#' Function to check if the option 'simstudy.use_reference' is set
#' which signals that data.tables should not be copied in generation
#'  processes. (see issue #50)
ensureReference <- function(dt) {
  if (is.null("simstudy.use_reference")) {
    dt <- copy(dt)
  }
  dt
}

#' @export
#' @importFrom purrr pmap
gen_data <- function(n, dtDefs = NULL, id = "id", envir = parent.frame()) {
  assertNotMissing(n = missing(n))
  assertValue(n = n, id = id)
  assertType(id = id, type = "character")
  assertNumeric(n = n)

  data <- dt_with_id(n, id)

  if (!is.null(dtDefs)) {
    assertClass(dtDefs = dtDefs, class = "data.table")

    oldId <- attr(dtDefs, "id")
    if (!is.null(oldId) && id != oldId && !missing(id)) {
      if (oldId != "id") {
        valueWarning(
          var = oldId,
          names = id,
          msg = list(
            "Previously defined 'id'-column found: '{var}'. ",
            "The current specification '{names}' will override it."
          )
        )
      }
    } else {
      id <- oldId %||% id
    }

    # use dt syntax here
    dtDefs %>% pmap(~ parse(text = .x))

    # for (i in seql_len(nrows(dtDefs))) {
    #   dfSimulate <- .generate(
    #     args = dtDefs[i, ],
    #     n = n,
    #     dfSim = dfSimulate,
    #     idname = id,
    #     envir = envir
    #   )
    # }


    data.table::setkeyv(dt, id)
  }

  data
}

dt_with_id <- function(n, id = "id") {
  dt <- data.table::data.table(x = 1:n)
  data.table::setnames(dt, id)
  data.table::setkeyv(dt, id)

  dt
}