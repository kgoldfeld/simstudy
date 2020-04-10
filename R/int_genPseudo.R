#' Pseudo-Random Sequence
#'
#' Generate a sequence of \code{n/length(vector)} randomly ordered repetitions
#' of a numeric vector.
#'
#' @param n The number of observations required in the data set
#' @param formula String that specifies the pool of numbers to be sampled from
#'   and the number of repetions. "pool;reps" e.g. "1,2,4,5;4"
#' @param link  String \code{"fill"} to indicate if a length mismatch should be
#'   tolerated, anything else will enforce \code{n = reps * length(vector)}.
#' @return A data.frame column (aka vector) with the simulated data
#' @keywords internal
.genPseudoSeq <- function(n, formula,link) {
  .formulaError <-  function() {
    stop(
      "Failed to parse formula. Format: \"1,2,3;4\". See ?simstudy::distributions",
      call. = F
    )
  }
  
  if(!is.character(link) | is.na(link))
      stop("Parameter 'link' needs to be a str.",call. = F)
  
  args <-
    unlist(strsplit(as.character(formula), split = ";", fixed = TRUE))
  
  if (length(args) != 2)
    .formulaError()
  
  # NA through coercion warning is superflous due to following checks
  suppressWarnings(reps <- as.numeric(args[2]))
  suppressWarnings(pool <-
                     as.numeric(unlist(strsplit(
                       args[1], split = ",", fixed = T
                     ))))
  
  if (!is.numeric(reps) | is.na(reps))
    .formulaError()
  
  if (!is.vector(pool) | !is.numeric(pool) | any(is.na(pool)))
    .formulaError()
  
  nOver <- n %% (length(pool) * reps)
  nTimes <- n %/% (length(pool) * reps)
  if ( (nOver != 0 | nTimes != 1) & tolower(link) != "fill")
    stop(
      "Length mismatch:\n length(pool) * reps must be the same as the length of the generated data"
    )
  reps <- nTimes + ifelse(nOver != 0,1,0)
  as.vector(replicate(reps, sample(pool)))[1:n]
  
}

#' Pseudo-Random
#'
#' Generate a random sequence of length \code{n}, that contains every number in
#' the input vector exactly \code{n / length(vector)} times.
#'
#' @inheritParams .genPseudoSeq
#'
#' @return A data.frame column (aka vector) with the simulated data
#'
#' @keywords internal
.genPseudoRandom <- function(n, formula,link) {
  sample(.genPseudoSeq(n, formula,link))
}
