#### evalDef ####

# Internal function to check new data definition
#
# @param newvar Name of new variable
# @param newfrom New formula
# @param defVars Existing column names
# @return Nothing is returned if all tests are passed. If a test fails,
# execution is halted.

evalDef <- function(newvar, newform, newdist, defVars) {

  # Check if previously defined

  if (newvar %in% defVars) {
    stop(paste("Variable name", newvar, "previously defined"), call. = FALSE)
  }

  # Check to make sure equation is valid form

  test <- unlist(strsplit(as.character(newform),split=";", fixed = TRUE))
  nparam <- length(test)

  # Check number of arguments for distrubtion

  if (newdist %in% c("uniform","uniformInt") & nparam != 2) {
      stop("Uniform (continuous & integer) requires min and max", call. = FALSE)
  }

  if (newdist == "categorical" & nparam < 2) {
    stop("Categorical distribution requires 2 or more probabilities", call. = FALSE)
  }

  if (!(newdist %in% c("uniform", "categorical", "uniformInt")) & nparam != 1) {
    stop("Only one parameter is permitted", call. = FALSE)
  }

  # check to make sure that each parameter is a valid equation

  for (i in (1:nparam)) {

    newExpress <- try(parse(text = test[i]), silent = TRUE)

    if (is.error(newExpress)) {
      stop("Equation not in proper form", call. = FALSE)
    }
    # Check to makes sure all vars have been previously defined in data.table

    equvars <- all.vars(newExpress)
    inDef <- equvars %in% defVars
    unRefVars <- equvars[!inDef]

    if (!all(inDef)) {

      stop(paste("Variable(s) referenced not previously defined:",
                 paste(unRefVars, collapse = ", ")
      ), call. = FALSE)
    }

    # Check for bad functions

    if (length(equvars) > 0) {
      for (i in 1:length(equvars)) eval(parse(text = paste(equvars[i],"<- 1")))
      formtest <- try(eval(newExpress), silent = TRUE)
      if (is.error(formtest)) {
        stop("Formula includes unrecognized function", call. = FALSE)
      }
    }
  }

  # Make sure that distribution is allowed

  if (!(newdist %in% c("normal","binary", "binomial","poisson","noZeroPoisson",
                       "uniform","categorical","gamma","nonrandom",
                       "uniformInt", "negBinomial", "exponential"
  ))) {

    stop(paste0("'",newdist,"' distribution is not a valid option"), call. = FALSE)

  }

}
