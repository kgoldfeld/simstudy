#### .evalDef ####

# Internal function to check new data definition
#
# @param newvar Name of new variable
# @param newfrom New formula
# @param defVars Existing column names
# @return Nothing is returned if all tests are passed. If a test fails,
# execution is halted.

.evalDef <- function(newvar, newform, newdist, defVars ) {

  # Check if previously defined
  if(missing(defVars)) defVars <- ""
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

    if (.iserror(newExpress)) {
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
      if (.iserror(formtest)) {
        stop("Formula includes unrecognized function", call. = FALSE)
      }
    }
  }
  
  # Check equation for mixture
  
  if (newdist == "mixture") {
    
    fcompress <- gsub(" ", "", newform, fixed = TRUE)  # compress formula
    
    if ( regexec("+", fcompress, fixed = TRUE) == -1 ) {
      stop("Formula requires `+` to separate variables")
    }
    
    fsplit <- strsplit(fcompress, "+", fixed = TRUE)[[1]] # split variables
    
    chkb <-unlist(lapply(fsplit, function(x) regexec("|", x, fixed = TRUE)))
    if (any(chkb == -1)) {
      stop("Mixture formula not in proper format")
    }
    
    flist <- lapply(fsplit, function(x) unlist(strsplit(x, "|", fixed=TRUE) ))
    ps <- cumsum(as.numeric(unlist(lapply(flist, function(x) (x[2])))))
    
    form_probs <- sum(as.numeric(unlist(regmatches(
      newform,
      gregexpr("[[:blank:]][[:digit:]]+\\.*[[:digit:]]*", str)
    ))))
    
    if (all.equal(form_probs,1)) {
      stop("Probabilities must sum to 1")
    }

  }

  # Make sure that distribution is allowed

  if (!(newdist %in% c("normal","binary", "binomial","poisson","noZeroPoisson",
                       "uniform","categorical","gamma","beta","nonrandom",
                       "uniformInt", "negBinomial", "exponential", 
                       "mixture"
  ))) {

    stop(paste0("'",newdist,"' distribution is not a valid option"), call. = FALSE)

  }

}

.evalDef2 <- function(newvar, newform, newdist, defVars) {
  
  if (!newdist %in% .getDists()) {
    stop(paste0("'", newdist, "' distribution is not a valid option. See ?distributions."), call. = FALSE)
  }

  if (missing(defVars)) {
    warning("Argument 'defVars' missing with no default. Was this intentional?")
    defVars <- ""
  }

  if (newvar %in% defVars) {
    stop(paste("Variable name '", newvar, "' previously defined."), call. = FALSE)
  }

  if (!is_valid_variable_name(newvar)) {
    warning(
      paste(
        "Variable name '",
        newvar,
        "' is not a valid R variable name,\n",
        "and will be converted to: '",
        make.names(newvar),
        "'."
      ),
      call. = FALSE
    )
    newvar <- make.names(newvar)
  }
  
  if (startsWith(newvar, ".."))
    stop(
      paste(
        "The prefix '..' is reserved to escape variables",
        "from outside the definition table in formulas."
      )
    )
  
  
  switch(newdist,
         
         binary = {
           .isValidArithmeticFormula(formula, defVars)
           .isIdLogit(link)
         }, 
         
         beta = ,
         
         binomial = {
           .isValidArithmeticFormula(formula, defVars)
           .isValidArithmeticFormula(variance, defVars)
           .isIdLogit(link)
         }, 
         
         noZeroPoisson = ,
         
         poisson = ,
         
         exponential = {
           .isValidArithmeticFormula(formula, defVars)
           .isIdLog(link)
         }, 

         gamma = , 

         negBinomial = {
           .isValidArithmeticFormula(formula, defVars)
           .isValidArithmeticFormula(variance, defVars)
           .isIdLog(link)
         },
         
         nonrandom =  .isValidArithmeticFormula(formula, defVars),
         
         normal = {
           .isValidArithmeticFormula(formula, defVars)
           .isValidArithmeticFormula(variance, defVars)
         },
         

         
         categorical = ,
   
         mixture = {
           .isValidArithmeticFormula(formula, defVars)
           .checkMixture(formula, defVars)
         },
         
         uniform = .checkUniform(formula,defVars),
         
         uniformInt = .checkUniformInt(formula,defVars),
         
         stop("Unkown distribution."))
}







.isIdLogit <- function(link) {
  if (!link %in% c("identity", "logit"))
    stop(
      paste(
        "Invalid link function: '",
        link,
        "', must be 'identity' or 'logit'. See ?distributions"
      )
    )
}

.isIdLog <- function(link) {
  if (!link %in% c("identity", "log"))
    stop(
      paste(
        "Invalid link function: '",
        link,
        "', must be 'identity' or 'logit'. See ?distributions"
      )
    )
}

.checkCategorical <- function(formula, variance, defVars){
  
}

.checkMixture <- function(formula, defVars){
  
}


.checkUniform <- function(formula,defVars){
  
  c(min,max)
}

.checkUniformInt <- function(formula, defVars) {
  range <- .checkUniform(formula, defVars)
  if (any(!is.integer(range)))
    stop(paste(
      "For 'uniformInt' min and max must be integers,",
      "did you mean to use 'uniform'?"
    ))
}


.isValidArithmeticFormula <- function(formula, defVars) {
  
  if(grepl(";",formula,fixed = T))
    stop("';' are not allowed in arithmetic formulas. See ?distribution")
  
  # This only catches gross errors like trailing operators, does not check
  # functionnames etc.
  newExpress <- try(parse(text = formula), silent = TRUE)

  if (.iserror(newExpress)) {
    stop(paste("Equation: '", formula, "' not in proper form. See ?distributions ."),
      call. = FALSE
    )}
  
  formFuncs <- all.names(newExpress,unique = T)
  formVars <- all.vars(newExpress)
  formFuncs <- formFuncs[!formFuncs %in% formVars]
  
  if (any(startsWith(formFuncs, "..")))
    warning(
      paste("Functions don't need to be escaped with '..',",
        "\nunless this is the real name this will cause an error later.",
        "\nFunctions:",formFuncs[startsWith(formFuncs, "..")]
      )
    )
  
  dotVarsBol <- startsWith(formVars, "..")
  inDef <- formVars %in% defVars
  unRefVars <- !inDef & !dotVarsBol
  
  if (!all(unRefVars)) {
    
    stop(paste("Variable(s) referenced not previously defined:",
               paste(formVars[unRefVars], collapse = ", ")
    ), call. = FALSE)
  }
  
  naFormFuncs <- is.na(mget(formFuncs,ifnotfound = NA,mode = "function",inherits = T))
  if(any(naFormFuncs))
    stop(paste("Functions(s) referenced not defined:",
               paste(formFuncs[naFormFuncs], collapse = ", ")
    ), call. = FALSE)
  
  naDotVars <- is.na(mget(sub("..","",formVars[dotVarsBol]),ifnotfound = NA,mode = "numeric",inherits = T))
  if(any(naDotVars))
    stop(paste("Escaped variables referenced not defined (or not numeric):",
               paste(names(naDotVars), collapse = ", ")
    ), call. = FALSE)
  
  invisible(formula)
}
