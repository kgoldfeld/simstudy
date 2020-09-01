#' @keywords internal
#' @importFrom glue glue
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

  # nolint start

#'Distributions for Data Definitions
#'
#'This help file describes the distributions used for data creation in
#'`simstudy`.
#'
#'@param formula Desired mean as a Number or an R expression for mean as a
#'  String. Variables defined via [defData()] and variables within the
#'  parent environment (prefixed with `..`) can be used within the formula.
#'  Functions from the parent environment can be used without a prefix.
#'@param variance Number. Default is `0`.
#'@param link String identifying the link function to be used. Default is
#' `identity`.
#'@details For details about the statistical distributions please see
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
#' | mixture         | formula                | `x_1 \| p_1 + x_2 \| p_2 ... x_n \| p_n` | NA             | NA              |
#' | negBinomial     | mean                   | String or Number                         | dispersion value | identity or log   |
#' | nonrandom       | formula                | String or Number                         | NA             | NA              |
#' | normal          | mean                   | String or Number                         | variance         | NA              |
#' | noZeroPoisson   | mean                   | String or Number                         | NA             | identity or log   |
#' | poisson         | mean                   | String or Number                         | NA             | identity or log   |
#' | uniform         | range                  | `from;to`                                | NA             | NA              |
#' | uniformInt      | range                  | `from;to`                                | NA             | NA              |
#' 
#'
#'@section Mixture: The mixture distribution makes it possible to mix to
#'  previously defined distributions/variables. Each variable that should be
#'  part of the new distribution `x_1,...,X_n` is assigned a probability
#'  `p_1,...,p_n`. For more information see
#'  [rdatagen.net](https://www.rdatagen.net/post/adding-mixture-distributions-to-simstudy/).
#'@examples
#' ext_var <- 2.9
#' def <- defData(varname = "external",formula = "3 + log(..ext_var)", variance = .5)
#' def
#' genData(5,def)
#'@name distributions
#'@aliases normal, poisson, noZeroPoisson, binary, binomial, uniform,
#'  categorical, gamma, beta, negBinomial, nonrandom, exponential, mixture,
#'  pseudorandom, pseudorandomSeq
#'@md
NULL

# TODO pseudorandom dist doc
# | pseudorandom    | vector and repetitions | "vector;reps"                            | NA             | identity or fill  |
# | pseudorandomSeq | vector and repetitions | "vector;reps"                            | NA             | identity or fill  |
#@section Pseudo-random and Pseudo-random-sequence: These distributions repeat
#  the elements of a numeric vector a certain number of times in a random
#  order. With \code{pseudorandom} every element in \code{vector} will be
#  repeated \code{reps} times in a completly random order.
#  \code{pseudorandomSeq} will also repeat the elements \code{reps} times but
#  will randomize in sequences of \code{length(vector)}. E.g. \code{formula =
#  "1+2+3;2"} could look like \code{3,1,1,2,3,2} for \code{pseudorandom} but
#  something like \code{3,1,2,1,3,2} for \code{pseudorandomSeq}. Both
#  distributions expect \code{reps * length(vector) = n} where \code{n} is the
#  number of rows in the simulated data. Setting \code{link = "fill"} will
#  supress this requirement and adjust the output to length \code{n}.
# nolint end