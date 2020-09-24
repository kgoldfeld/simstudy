#' Collapse Transformer
#'
#' @description Transformer for use with glue(). Collapses content of glue block
#' ending with regex.
#' @param regex Regex to mark blocks to collapse.
#' @param ... Arguments passed by the calling lue function (text, envir)
#'  and params to pass through to glue_collapse like sep, last.
#' @return The collapsed text, or identity when no marker found.
#' @noRd
collapseTransformer <- function(regex = "[*]$", ...) {
    function(text, envir) {
        collapse <- grepl(regex, text)
        if (collapse) {
            text <- sub(regex, "", text)
        }
        res <- identity_transformer(text, envir)
        if (collapse) {
            glue_collapse(res, ...)
        } else {
            res
        }
    }
}

#' Sprintf Transformer
#'
#' @description Transformer for use with glue(). Formats numbers
#' similar to sprintf. Use like: var:02d
#' @param text Text to format.
#' @param envir environment
#' @param envir environment
#' @return The formatted text.
#' @noRd
sprintfTransformer <- function(text, envir) {
  m <- regexpr(":.+$", text)
  if (m != -1) {
    format <- substring(regmatches(text, m), 2)
    regmatches(text, m) <- ""
    res <- eval(parse(text = text, keep.source = FALSE), envir)
    do.call(sprintf, list(glue("%{format}"), res))
  } else {
    eval(parse(text = text, keep.source = FALSE), envir)
  }
}

#' Sprintf Collapse Transformer
#'
#' @description Transformer for use with glue(). Formats numbers
#' similar to sprintf. Collapses vectors/lists.
#' @param ... Arguments passed by the calling lue function (text, envir)
#' @param sep Characters used to seperate items.
#' @param last Characters used to eperate last items.
#' @return The formatted text.
#' @noRd
sprintfCTransformer <- function(sep = ", ", last = " and ", ...) {
  function(text, envir) {
    m <- regexpr(":.+$", text)
    if (m != -1) {
      format <- substring(regmatches(text, m), 2)
      regmatches(text, m) <- ""
      expr <- parse(text = text, keep.source = FALSE)
      var <- all.vars(expr)
      fmtString <- glue("%{format}")
      varL <- ifelse(length(var) != 0, length(get(var, envir = envir)), 1)
      res <- eval(expr, envir)

      if (varL > 1) {
        do.call(
          sprintf,
          c(
            glue_collapse(
              rep(fmtString, varL),
              sep = sep, last = last
            ),
            as.list(res)
          )
        )
      } else {
        do.call(sprintf, list(fmtString, res))
      }
    } else {
      eval(parse(text = text, keep.source = FALSE), envir)
    }
  }
}

#' Collapse and glue text
#'
#' @inheritParams collapseTransformer
#' @inheritDotParams glue
#' @return The collapsed text.
#' @details Mark blocks to collapse with *
#' @noRd
glueCollapse <- function(..., sep = ", ", last = " and ",
                         .envir = parent.frame()) {
    glue(...,
        .transformer = collapseTransformer(sep = sep, last = last),
        .envir = .envir
    )
}

#' Format numeric vars and glue text
#'
#' @inheritParams sprintfTransformer
#' @inheritDotParams glue
#' @return The formated text.
#' @details var:.2 = %.2f
#' @noRd
glueFmt <- function(..., .envir = parent.frame()) {
    glue(...,
        .transformer = sprintfTransformer,
        .envir = .envir
    )
}

#' Format and collapse numeric vars
#'
#' @inheritParams sprintfCTransformer
#' @inheritDotParams glue
#' @return The formated and collapsed text.
#' @details var:.2 = %.2f
#' @noRd
glueFmtC <- function(..., .envir = parent.frame(), sep = ", ", last = " and ") {
    glue(...,
        .transformer = sprintfCTransformer(sep = sep, last = last),
        .envir = .envir
    )
}
