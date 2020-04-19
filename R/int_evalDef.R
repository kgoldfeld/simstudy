#### .evalDef ####

# Internal function to check new data definition
#
# @param newvar Name of new variable
# @param newfrom New formula
# @param defVars Existing column names
# @return Nothing is returned if all tests are passed. If a test fails,
# execution is halted.
.evalDef <- function(newvar, newform, newdist, variance = 0, link = "identity", defVars) {
  
  if(!is.character(newvar) || length(newvar) != 1 || is.na(newvar)){
    stop("Parameter 'varname' must be single string.", call. = FALSE)
  }

  if (!newdist %in% .getDists()) {
    stop(paste0("'", newdist, "' distribution is not a valid option. See ?distributions."), call. = FALSE)
  }

  if (missing(defVars)) {
    warning("Argument 'defVars' missing with no default. Was this intentional?")
    defVars <- ""
  }
  
  if (startsWith(newvar, "..")){
    stop(
      paste(
        "The prefix '..' is reserved to escape variables",
        "from outside the definition table in formulas."
      )
    )}
  
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
  
  if (newvar %in% defVars) {
    stop(paste("Variable name '", newvar, "' previously defined."), call. = FALSE)
  }
  
 
  switch(newdist,
         
         binary = {
           .isValidArithmeticFormula(newform, defVars)
           .isIdLogit(link)
         }, 
         
         beta = ,
         
         binomial = {
           .isValidArithmeticFormula(newform, defVars)
           .isValidArithmeticFormula(variance, defVars)
           .isIdLogit(link)
         }, 
         
         noZeroPoisson = ,
         
         poisson = ,
         
         exponential = {
           .isValidArithmeticFormula(newform, defVars)
           .isIdLog(link)
         }, 

         gamma = , 

         negBinomial = {
           .isValidArithmeticFormula(newform, defVars)
           .isValidArithmeticFormula(variance, defVars)
           .isIdLog(link)
         },
         
         nonrandom =  .isValidArithmeticFormula(newform, defVars),
         
         normal = {
           .isValidArithmeticFormula(newform, defVars)
           .isValidArithmeticFormula(variance, defVars)
         },
        
         categorical = .checkCategorical(newform),
   
         mixture = {
           .isValidArithmeticFormula(newform)
           .checkMixture(newform, defVars)
         },
         
         uniform = .checkUniform(newform),
         
         uniformInt = .checkUniformInt(newform),
         
         stop("Unkown distribution."))
  
  invisible(newvar)
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
  invisible(link)
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
  
  invisible(link)
}

.checkCategorical <- function(formula){
  probs <- unlist(strsplit(formula,split=";",fixed = T))
  probs <- suppressWarnings(as.numeric(probs))
  
  if(length(probs) < 2 || any(is.na(probs)))
    stop(paste0("The formula for 'categorical' must contain atleast", 
                " two numeric probabilities." ))
  
  #all.equal allows for insignificant tolerances due to floats
  if(!isTRUE(all.equal(sum(probs),1)))
    stop("Probabilities must sum to 1. See ?distributions")
  
  invisible(formula)
}

.checkMixture <- function(formula){
  
  formula <- gsub("[[:blank:]]","",formula)
  var_pr <- strsplit(formula,"+",fixed=T)
  var_dt <- strsplit(var_pr[[1]],"|",fixed=T)
  
  if (length(unlist(var_dt)) %% 2)
    stop(
      paste0(
        "Mixture formula most contain same amount",
        " of vars and probabilities!",
        " See ?distributions"
      )
    )
  
  formDT <- as.data.table(do.call(rbind,var_dt))
  names(formDT) <- c("vars","probs")
  
  dotProbs <- startsWith(formDT$probs,"..")
  dotVars <- startsWith(formDT$vars,"..")
  dotVarArrays <- grepl("\\.\\..+\\[",formDT$vars)
  dotProbArrays <- grepl("\\.\\..+\\[",formDT$probs)
  dotProbsNames <- sub("..","",formDT$probs[dotProbs])
  dotVarNames <- sub("..","",formDT$vars[dotVars & !dotVarArrays])
  notDotVarProbs <- is.na(suppressWarnings(as.numeric(formDT$probs))) 
  
  if (any(dotVarArrays) |
      any(notDotVarProbs[!dotProbs])) {
    stop(
      paste0(
        "Invalid variable(s): ",
        paste0(formDT$probs[(notDotVarProbs & !dotProbs) | dotProbArrays],collapse=", "),
        "\n",
        "Probabilities can only be numeric or numeric",
        " ..vars (not arrays). See ?distribution"
      )
    )
  }
  
  formDT$probs[dotProbs] <- mget(dotProbsNames)
  formDT$vars[dotVars & !dotVarArrays] <- mget(dotVarNames)
  formDT$probs <- suppressWarnings(as.numeric(formDT$probs))
  
  if(any(is.na(formDT$probs)))
    stop(paste0("Probabilites contain 'NA',",
                " probably through coercion from String."))
  
  if(!isTRUE(all.equal(sum(formDT$probs),1)))
    stop("Probabilities must sum to 1 and not See ?distributions")
  
  invisible(formula)
}

.checkUniform <- function(formula){
  
  range <- unlist(strsplit(formula,";",fixed = T))
  # We test for NAs so coercion warning can be suppressed.
  range <- suppressWarnings(as.numeric(range))
  
  if(length(range) != 2 || any(is.na(range)))
    stop(paste(
      "Formula for unifrom distributions must have",
      "the format: 'min;max' \nwhere min/max are numeric. See ?distributions" 
      ))
  
  r_min <- range[1]
  r_max <- range[2]
  
  if (r_min == r_max) {
    warning(
      paste0("Formula warning: ",
        "'min' and 'max' are both: '",
        r_min,
        "'.",
        "This results in all Data being the same!"
      )
    )
  }
  
  if(r_max < r_min)
    stop("Formula invalid: 'max' < 'min'")
  
  invisible(c(r_min,r_max))
}

.checkUniformInt <- function(formula) {
  range <- .checkUniform(formula)
  if (any(!is.integer(range)))
    stop(paste(
      "For 'uniformInt' min and max must be integers,",
      "did you mean to use 'uniform'?"
    ))
  
  invisible(formula)
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
  
  if (any(unRefVars)) {
    
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
