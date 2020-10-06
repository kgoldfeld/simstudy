#' @keywords internal
#' @import glue
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

# nolint start

#' Distributions for Data Definitions
#'
#' This help file describes the distributions used for data creation in
#' `simstudy`.
#'
#' @param formula Desired mean as a Number or an R expression for mean as a
#'  String. Variables defined via [defData()] and variables within the
#'  parent environment (prefixed with `..`) can be used within the formula.
#'  Functions from the parent environment can be used without a prefix.
#' @param variance Number. Default is `0`.
#' @param link String identifying the link function to be used. Default is
#' `identity`.
#' @details For details about the statistical distributions please see
#'  [stats::distributions], any non-statistical distributions will be
#'  explained below. Required variables and expected pattern for each
#'  distribution can be found in this table:
#'
#' | **name**        | **formula**            | **format**                               | **variance**     | **link**          |
#' |-----------------|------------------------|------------------------------------------|------------------|-------------------|
#' | beta            | mean                   | String or Number                         | dispersion value | identity or logit |
#' | binary          | probability for 1      | String or Number                         | NA             | identity or logit |
#' | binomial        | probability of success | String or Number                         | number of trials | identity or logit |
#' | categorical     | probabilities          | `p_1;p_2;..;p_n`                         | NA             | NA              |
#' | exponential     | mean (lambda)          | String or Number                         | NA             | identity or log   |
#' | gamma           | mean                   | String or Number                         | dispersion value | identity or log   |
#' | mixture         | formula                | `x_1 `\|` p_1 + x_2 `\|` p_2 ... x_n `\|` p_n` | NA             | NA              |
#' | negBinomial     | mean                   | String or Number                         | dispersion value | identity or log   |
#' | nonrandom       | formula                | String or Number                         | NA             | NA              |
#' | normal          | mean                   | String or Number                         | variance         | NA              |
#' | noZeroPoisson   | mean                   | String or Number                         | NA             | identity or log   |
#' | poisson         | mean                   | String or Number                         | NA             | identity or log   |
#' | uniform         | range                  | `from;to`                                | NA             | NA              |
#' | uniformInt      | range                  | `from;to`                                | NA             | NA              |
#'
#'
#' @section Mixture: The mixture distribution makes it possible to mix to
#'  previously defined distributions/variables. Each variable that should be
#'  part of the new distribution `x_1,...,X_n` is assigned a probability
#'  `p_1,...,p_n`. For more information see
#'  [rdatagen.net](https://www.rdatagen.net/post/adding-mixture-distributions-to-simstudy/).
#' @examples
#' ext_var <- 2.9
#' def <- defData(varname = "external", formula = "3 + log(..ext_var)", variance = .5)
#' def
#' genData(5, def)
#' @name distributions
#' @aliases normal poisson noZeroPoisson binary binomial uniform
#'  categorical gamma beta negBinomia nonrandom exponential mixture
#' @md
NULL

# nolint end

#' Deprecated functions in simstudy
#'
#' These functions are provided for compatibility with older versions
#' of simstudy only, and will be defunct in the future.
#'
#' * [genCorOrdCat]: This function is deprecated, and will
#'  be removed in the future. Use [genOrdCat] with `asFactor = FALSE` instead.
#' * [catProbs]: This function is deprecated, and will be removed in the future.
#'   Use [genCatFormula] with the same functionality instead.
#' @md
#' @name simstudy-deprecated
NULL
