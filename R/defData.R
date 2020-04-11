#'Distributions for Data Definitions
#'
#'This help file describes the distributions used for data creation in
#'\code{simstudy}.
#'
#'@param formula Desired mean as a Number or an R expression for mean as a
#'  String. Only variables defined via \code{\link{defData}} can be used within
#'  the formula.
#'@param variance Number. Default is \code{0}.
#'@param link String identifying the link function to be used. Default is
#'  \code{identity}.
#'@details For details about the statistical distributions please see
#'  \code{\link[stats]{distribution}}, any non-statistical distributions will be
#'  explained below. Required variables and expected pattern for each
#'  distribution can be found in this table:
#'
#'  \tabular{lllll}{ name \tab formula \tab format\tab variance \tab link \cr
#'  beta \tab mean \tab String or Number \tab dispersion value \tab  identity or
#'  logit \cr binary  \tab probability for 1 \tab String or Number \tab N.A.
#'  \tab  identity or logit \cr binomial\tab probability of success \tab String
#'  or Number \tab number of trials \tab identity or logit \cr categorical \tab
#'  probabilities \tab "p_1;p_2;..;p_n" \tab N.A. \tab N.A. \cr exponential \tab
#'  mean (lambda) \tab String or Number \tab N.A. \tab identity or log \cr gamma
#'  \tab mean \tab String or Number \tab dispersion value \tab identity or log
#'  \cr mixture \tab formula \tab "x_1 | p_1 + x_2 | p_2 ... x_n | p_n" \tab
#'  N.A. \tab N.A. \cr negBinomial \tab mean \tab String or Number \tab
#'  dispersion value \tab identity or log \cr nonrandom \tab formula \tab String
#'  or Number \tab N.A. \tab N.A. \cr normal \tab mean \tab String or Number
#'  \tab variance \tab N.A.\cr noZeroPoisson \tab mean \tab String or Number
#'  \tab N.A. \tab identity or log \cr poisson \tab mean \tab String or Number
#'  \tab N.A.  \tab identity or log \cr pseudorandom \tab vector and repetitions
#'  \tab "vector;reps" \tab N.A. \tab identity or fill \cr pseudorandomSeq \tab
#'  vector and repetitions \tab "vector;reps" \tab N.A. \tab identity or fill
#'  \cr uniform \tab range \tab "from;to" \tab N.A. \tab N.A. \cr uniformInt
#'  \tab range \tab "from;to" \tab N.A. \tab N.A. \cr }
#'
#'@section Pseudo-random and Pseudo-random-sequence: These distributions repeat
#'  the elements of a numeric vector a certain number of times in a random
#'  order. With \code{pseudorandom} every element in \code{vector} will be
#'  repeated \code{reps} times in a completly random order.
#'  \code{pseudorandomSeq} will also repeat the elements \code{reps} times but
#'  will randomize in sequences of \code{length(vector)}. E.g. \code{formula =
#'  "1,2,3;2"} could look like \code{3,1,1,2,3,2} for \code{pseudorandom} but
#'  something like \code{3,1,2,1,3,2} for \code{pseudorandomSeq}. Both
#'  distributions expect \code{reps * length(vector) = n} where \code{n} is the
#'  number of rows in the simulated data. Setting \code{link = "fill"} will
#'  supress this requirement and adjust the output to length \code{n}.
#'
#'@section Mixture: The mixture distribution makes it possible to mix to
#'  previously defined distributions/variables. Each variable that should be
#'  part of the new distribution \code{x_1,...,X_n} is assigned a probability
#'  \code{p_1,...,p_n}. For more information see
#'  \url{https://www.rdatagen.net/post/adding-mixture-distributions-to-simstudy/}.
#'
#'@name distributions
#'@aliases normal, poisson, noZeroPoisson, binary, binomial, uniform,
#'  categorical, gamma, beta, negBinomial, nonrandom, exponential, mixture,
#'  pseudorandom, pseudorandomSeq
NULL

#' Add single row to definitions table
#'
#' @useDynLib simstudy, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import data.table
#'
#' @param dtDefs Definition data.table to be modified
#' @param varname Name (string) of new variable
#' @param formula An R expression for mean (string)
#' @param variance Number
#' @param dist Distribution. For possibilities, see details
#' @param link The link function for the mean, see details
#' @param id A string indicating the field name for the unique record identifier
#' @return A data.table named dtName that is an updated data definitions table
#' @details The possible data distributions include "normal", "poisson",
#' "noZeroPoisson", "negBinomial" "binary", "binomial", "beta", "uniform",
#' "uniformInt", "categorical", "gamma", "exponential",
#' "mixture", and "nonrandom."
#'
#' @examples
#' def <- defData(varname = "xNr", dist = "nonrandom", formula=7, id = "idnum")
#' def <- defData(def, varname="xUni", dist="uniform", formula="10;20")
#' def <- defData(def, varname="xNorm", formula="xNr + xUni * 2", dist="normal", variance=8)
#' def <- defData(def, varname="xPois", dist="poisson", formula="xNr - 0.2 * xUni", link="log")
#' def <- defData(def, varname="xCat", formula = "0.3;0.2;0.5", dist="categorical")
#' def <- defData(def, varname="xGamma", dist="gamma", formula = "5+xCat", variance = 1, link = "log")
#' def <- defData(def, varname = "xBin", dist = "binary" , formula="-3 + xCat", link="logit")
#' def <- defData(def, varname = "xPseudo", dist = "pseudorandom", formula = "1,2,3,4;3" , link = "fill")
#' def
#'
#' @seealso \code{\link{distributions}}
#' @export

defData <- function(dtDefs = NULL,
                    varname,
                    formula,
                    variance = 0,
                    dist = "normal",
                    link = "identity",
                    id = "id") {
  #### Check that arguments have been passed ####
  
  if (missing(varname))
    stop("argument 'varname' is missing", call. = FALSE)
  if (missing(formula))
    stop("argument 'formula' is missing", call. = FALSE)
  
  #### No missing arguments
  
  if (is.null(dtDefs)) {
    # checking that initial formula has no variables ...
    
    # warnings are suppressed because we want to test for NAs
    
    elements <-
      unlist(strsplit(
        as.character(formula),
        split = ";",
        fixed = TRUE
      ))
    suppressWarnings(test <- as.numeric(elements))
    
    if (sum(is.na(test))) {
      stop("First defined formula must be scalar", call. = FALSE)
    }
    
    dtDefs <- data.table::data.table()
    attr(dtDefs, "id") <- id
    
  } else {
    .evalDef(varname, formula, dist, dtDefs[, varname])
  }
  
  dt.new <- data.table::data.table(varname,
                                   formula,
                                   variance,
                                   dist,
                                   link)
  
  l = list(dtDefs, dt.new)
  
  defNew <- data.table::rbindlist(l, use.names = TRUE, fill = TRUE)
  attr(defNew, "id") <- attr(dtDefs, "id")
  
  return(defNew[])
  
}
