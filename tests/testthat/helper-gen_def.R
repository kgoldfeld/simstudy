
## General Generators ----
gen_abc <- gen.element(letters)
gen_ABC <- gen.element(LETTERS)
gen_123 <- gen.element(0:9)
gen_azAZ09 <- gen.choice(gen_abc, gen_ABC, gen_123)
gen_azAZ09_ <- gen.choice(gen_azAZ09,"_")

gen_prob <- function(x) gen.unif(0,1)
gen_var <- function(x) 0
gen_id <- gen.element(c("identity"))

# Shrink varnames?? 
gen_name <- function(r)  gen.and_then(gen.element(r), function(x) gen.c(of = x, gen_azAZ09))
gen_varname <- gen.map(function(x) make.names(paste0(x,collapse ="")),gen_name(1:20))
gen_fun_name <- gen.map(function(x) make.names(paste0(x,collapse ="")) , gen_name(2:6))

gen_assign_dotdot <- function(val) {gen.map(function(name){
  assign(name,val,pos = .GlobalEnv)
  paste0("..",name)
} , gen_varname)}

gen_dotdot_num <- gen.and_then(gen.unif(from=-1000,to=1000),function(x) gen_assign_dotdot(x)) 
gen_dotdot_vec <-
  gen.and_then(gen.and_then(gen.int(20), function(n) {
    gen.c(of = n, gen.unif(from=-1000,to=1000))
  }), function(val) {
    gen_assign_dotdot(val)
  })

gen_dotdot_vec <-  function(n) {
    gen.and_then(gen.c(of = n, gen.unif(from = -1000, to = 1000)), function(val) {
      gen.map(function(name) {
        paste0(name, "[", sample(1:n, 1), "]")
      } , gen_assign_dotdot(val))
    })}

gen_dotdot_vec_ele <- gen.and_then(gen.int(12),gen_dotdot_vec)    


gen_dotdot_var <- gen.choice(gen_dotdot_num,gen_dotdot_vec_ele)
gen_dotdot_chr <- gen.and_then(gen_varname,function(val) gen_assign_dotdot(val))

gen_base_func <- gen.sample(c("log(abs(%s))","exp(%s)","sin(%s)","cos(%s)","log10(abs(%s))","log2(abs(%s))"),1)

gen_arb_fun <-
  gen.and_then(gen_fun_name, function(name) {
    gen.map(function(formula) {
      assign(name, eval(parse(text = paste(
        "function(p)", formula
      ))), pos = .GlobalEnv)
      paste0(name, "(%s)")
    }, gen_formula("p"))
  })

## gen vars ala ..var and assign!!
gen_wrap_fun <-
  function(inner) {
    gen.map(function(x)
      sprintf(x, inner),
      gen.choice(gen_base_func,gen_arb_fun))
  }


distributions <- .getDists()
distributions_noMix <- distributions[distributions != "mixture"]
gen_dist <- gen.no.shrink(gen.element(distributions))
gen_dist_fst <- gen.no.shrink(gen.element(distributions_noMix))

## Formula Generators ----
#
# Generators to create an arithmetic expression with previously defined
# variables as formula argument for many distributions. 
#
# Only change gen_const range. 


gen_prev_var <- function(prev_vars) gen.element(prev_vars) 
gen_const <- gen.element(0:1000)
gen_constf <- function(x) gen.element(0:1000)

gen_factor <- function(prev_var) {
  gen.choice(
    gen_prev_var(prev_var),
    gen_dotdot_var,
    gen_const,
    
    gen_expr_br(prev_var)
  )
}
#gen_expr_fun(prev_var),
gen_factor_dt <- function(prev_var) {
  generate(for (x in list(
    op = gen.element(c(" * ", " / ", " ^ ", " %% ", " %/% ")),
    fac1 = gen_factor(prev_var),
    fac2 = gen_factor(prev_var)
  ))
    list(x$fac1, x$op, x$fac2))
}

gen_term <- function(prev_var) gen.choice(gen_factor(prev_var),gen_factor_dt(prev_var))
gen_term_pm <-
  function(prev_var)
    generate(for (x in list(
      op = gen.element(c(" + ", " - ")),
      term1 = gen_factor(prev_var),
      term2 = gen_factor(prev_var)
    ))
      list(x$term1, x$op, x$term2))

gen_expr_br <- function(prev_var) gen.map(function(x) c("(", x , ")") ,gen_expr(prev_var))
gen_expr_fun <- function(prev_var) gen.and_then(gen_formula(prev_var),function(inner) gen_wrap_fun(inner))
gen_expr <- function(prev_var) gen.choice(gen_term(prev_var),gen_term_pm(prev_var))


gen_formula <- function(prev_var) gen.map(function(x) paste(unlist(x),collapse = ""),gen_expr(prev_var) )
gen_formula_scalar <-  gen_formula(-100:100)
gen_formula_choice <-
  function(prev_var) {
    if (length(prev_var) == 0)
      prev_var <- c(-100:100)
    
    gen.choice(gen_formula(prev_var), gen_formula_scalar)
  }

## Mixture Generators ----
gen_n_norm_Probs <- function(n) gen.map(function(p) p/sum(p) ,gen.c(of = n, gen_prob()))
    # atleast 2 vars are required for a correct mixture formula
gen_mixture <-
  function(prev_var) {
    if(length(prev_var) == 0 | is.numeric(prev_var))
      return("1 | 1 + 1 | 0")
    gen.and_then(gen.element(2:max(2, length(prev_var))), function(n) {
      gen_mix_parts(prev_var = prev_var, n = n)
    })
  }

gen_mix_parts <- function(prev_var,n) generate(for (x in 
                                                    list(probs = gen_n_norm_Probs(n),
                                                         vars = gen.c(of = n, gen_prev_var(prev_var))))
  paste(x$vars,x$probs,sep = " | ", collapse = " + "))
## Categorical Generators ----
gen_cat_probs <- gen.and_then(gen.element(2:10), function(n) gen_n_norm_Probs(n))
gen_cat_formula <- function(x) gen.map(function(p) catProbs(p),gen_cat_probs)

## Uniform Generators ----
gen_uniformInt_range <- function(x) gen.and_then(gen.int(1000), function(x) gen.map(function(y) paste0(y,";",x) ,gen.int(x)))
gen_uniform_range <- function(x) gen.map(function(x)paste0(sort(unlist(x)),collapse = ";"), gen.c(of = 2,gen.unif(-100,100)))

## Link Generators ----
gen_link_log <- gen.no.shrink(gen.element(c("identity","log")))
gen_link_logit <- gen.no.shrink(gen.element(c("identity","log")))

## Lookup Table ----

reg <- data.table()
reg$name <- sort(.getDists())
reg$formula <- character()
reg$variance <- "gen_var"
reg$link <- "gen_id"
reg[!(name %in% c("binary","binomial","categorical","mixture","uniform","uniformInt")),]$formula <- rep("gen_formula_choice",8)
reg[name %in% c("binary","binomial")]$formula <- rep("gen_prob",2)
reg[name == "binomial"]$variance <- "gen_constf"
reg[name == "categorical"]$formula <- "gen_cat_formula"
reg[name == "mixture"]$formula <- "gen_mixture"
reg[name == "uniform"]$formula <- "gen_uniform_range"
reg[name == "uniformInt"]$formula <- "gen_uniformInt_range"
reg[name %in% c("normal","negBinomial","gamma","beta")]$variance <-  rep("gen_formula_choice",4)
reg[name %in% c("beta","binary","binomial")]$link <- rep("gen_link_logit",3)
reg[name %in% c("exponential","gamma","negBinomial","noZeroPoisson","poisson")]$link <- rep("gen_link_log",5)


## Generators for complete Data Definitions ----
# does it make sense to not shrink? TODO
gen_varnames <- function(n){ gen.no.shrink(gen.c(of = n, gen_varname))}

gen_dists <-
  function(n) {
    gen.map(function(dists)
      unlist(dists),
      gen.and_then(gen_dist_fst, function(dist)
        list(dist, gen.list(of = n - 1, gen_dist))))
  }

gen_def_dt <-
  function(n) {
    gen.map(function(list) {
      dt <- as.data.table(list)
      names(dt) <- c("varname", "dist")
      dt <- dt[!duplicated(dt$varname),]
      dt
    } , gen.and_then(gen_varnames(n), function(vars) {
      list(vars, gen_dists(n))
    }))
  }



gen_def_fst <- function(dt.all) {
  dt <- dt.all[1,]
  generate(for (x in list(
    varname = dt$varname,
    dist = dt$dist,
    formula = get(reg[name == dt$dist]$formula)(-100:100),
    variance = get(reg[name == dt$dist]$variance)(-100:100),
    link = get(reg[name == dt$dist]$link)
  ))
    list(def = do.call(
      defData,
      list(
        varname = x$varname,
        dist = x$dist,
        formula = ifelse(.isFormulaScalar(x$formula),eval(parse(text = x$formula)),x$formula),
        variance = eval(parse(text = x$variance)),
        link = x$link
      )
    ),dt = dt.all))
}

gen_def_nth <- function(res) {
  def <- res$def
  dt <- res$dt
  row <- nrow(def)+1
  cDist <- dt[row,dist]
  distParams <- reg[name == cDist]
  
  generate(for (x in list(
    dtDefs = def,
    varname = dt[row,varname],
    dist = cDist,
    formula = get(distParams$formula)(def$varname),
    variance = get(distParams$variance)(def$varname),
    link = get(distParams$link)
  ))
    list(def = do.call(defData,x), dt = dt) ) 
}

gen_def_nRows <- function(def, dt) {
  gen.map(function(res) {
    dt <- res$dt
    
    for (i in seq_len(nrow(dt)-2)) {
      print(i)
      print(dim(res$def))
      print(dim(dt))
      res <- gen_def_nth(res$def, dt)
      print(dim(res$def))
    } 
    
    res$def
  }, gen_def_nth(def, dt))
}

gen_def_dt_n <- gen.and_then(gen.int(50),function(n) gen_def_dt(n))
gen_def_1row <-gen.and_then(gen_def_dt_n,function(dt)gen_def_fst(dt))

gen_def_loop <- function(res) {
  dif <- nrow(res$dt) - nrow(res$def)
  
  if (dif > 1)
    gen.and_then(gen_def_nth(res), function(res)
      gen_def_loop(res))
  else if (dif == 1)
    gen.map(function(res)
      res$def, gen_def_nth(res))
  else
    stop(paste(typeof(res),"This should never be reached"))
}
# Generate def table upto 50 entries long.

gen_def <- gen.and_then(gen_def_1row,function(res) gen_def_loop(res))