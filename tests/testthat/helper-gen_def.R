library("hedgehog")
## General Generators ----
gen_abc <- gen.element(letters)
gen_ABC <- gen.element(LETTERS)
gen_123 <- gen.element(0:9)
gen_azAZ09 <- gen.choice(gen_abc, gen_ABC, gen_123)
gen_azAZ09_ <- gen.choice(gen_azAZ09,"_",prob = c(0.98,0.02)) 

gen_prob <- function(x) gen.unif(0,1)
gen_var <- function(x) 0
gen_id <- gen.element(c("identity"))

# Shrink varnames?? 
gen_name <- function(r)  gen.and_then(gen.element(r), function(x) gen.c(of = x, gen_azAZ09_))
gen_varname <- gen.map(function(x) make.names(paste0(x,collapse ="")),gen_name(3:12))
gen_fun_name <- gen.map(function(x) make.names(paste0(x,collapse ="")) , gen_name(2:6))

gen_assign_dotdot <- function(val) {gen.map(function(name){
  assign(name,val,pos = .GlobalEnv)
  paste0("..",name)
} , gen_varname)}

gen_dotdot_num <- gen.and_then(gen.unif(from=-1000,to=1000),gen_assign_dotdot) 
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

gen_dotdot_vec_ele <- gen.sized(gen_dotdot_vec) 


gen_dotdot_var <- gen.choice(gen_dotdot_num,gen_dotdot_vec_ele)
gen_dotdot_chr <- gen.and_then(gen_varname,gen_assign_dotdot)

gen_base_func <- gen.sample(c("log(abs(%s))","exp(%s)","sin(%s)","cos(%s)","log10(abs(%s))","log2(abs(%s))"),1)

gen_arb_fun <-
  gen.and_then(gen_fun_name, function(name) {
    gen.map(function(formula) {
      formula <- gsub("..","",formula, fixed = T)
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
      gen.choice(gen_base_func,gen_arb_fun,prob = c(.8,.2)))
  }


distributions <- .getDists()
gen_dist <- gen.no.shrink(gen.element(distributions))


gen_precision <- function(...) gen.int(50)
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
    gen_expr_fun(prev_var),
    gen_expr_br(prev_var),
    gen_const
  )
}

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


gen_formula <-
  function(prev_var) {
    if (missing(prev_var) ||
        length(prev_var) == 0 || !all(nchar(prev_var) > 0))
      prev_var <- c(-100:100)
    
    gen.map(function(x)
      paste(unlist(x), collapse = ""), gen_expr(prev_var))
  }

gen_formula_scalar <-  gen_formula(-100:100)


## Mixture Generators ----
gen_n_norm_Probs <- function(n) gen.map(function(p){ p/sum(p) },gen.c(of = n, gen_prob()))
   
gen_mixture <-  function(prev_var) {
  if (missing(prev_var) || length(prev_var) == 0 || !all(nchar(prev_var) > 0)) {
    gen_mix_scalar
  } else{
    gen.and_then(gen.element(1:(length(prev_var)+2)), function(n) {
      gen_mix_parts(prev_var = prev_var, n = n)
    })
  }
}

gen_mix_scalar <- gen.sized(function(n){ gen.and_then(gen.c(gen.element(-1000:1000),of = n),function(p) gen_mix_parts(p,n))})
gen_mix_parts <- function(prev_var, n) {
  generate(for (x in
                list(probs = gen_n_norm_Probs(n),
                     vars = gen.list(
                       of = n, gen.choice(gen.element(prev_var), gen.element(-1000:1000),prob = c(.7,.3))
                     )))
    paste(x$vars, x$probs, sep = " | ", collapse = " + "))
}
## Categorical Generators ----
gen_cat_probs <- gen.and_then(gen.element(2:10), function(n) gen_n_norm_Probs(n))
gen_cat_formula <- function(x) gen.map(function(p) do.call(catProbs,as.list(p)),gen.choice(gen_cat_probs,gen.map(function(x) list(n = x),gen.element(2:15) )))

## Uniform Generators ----
gen_uniformInt_range <- function(x) gen.map(function(x)paste0(sort(unlist(floor(x) ))-c(1,0),collapse = ";"), gen.c(of = 2,gen.unif(-100,100)))
gen_uniform_range <- function(x) gen.map(function(x)paste0(sort(unlist(x)),collapse = ";"), gen.c(of = 2,gen.unif(-100,100)))

## Link Generators ----
gen_link_log <- gen.no.shrink(gen.element(c("identity","log")))
gen_link_logit <- gen.no.shrink(gen.element(c("identity","logit")))

## Lookup Table ----

reg <- data.table()
reg$name <- sort(.getDists())
reg$formula <- character()
reg$variance <- "gen_var"
reg$link <- "gen_id"
reg[!(name %in% c("binary","binomial","beta","categorical","mixture","uniform","uniformInt")),]$formula <- rep("gen_formula",7)
reg[name %in% c("binary","binomial","beta")]$formula <- rep("gen_prob",3)
reg[name == "binomial"]$variance <- "gen_constf"
reg[name == "categorical"]$formula <- "gen_cat_formula"
reg[name == "mixture"]$formula <- "gen_mixture"
reg[name == "uniform"]$formula <- "gen_uniform_range"
reg[name == "uniformInt"]$formula <- "gen_uniformInt_range"
reg[name %in% c("normal","negBinomial","gamma")]$variance <-  rep("gen_formula",3)
reg[name == "beta"]$variance <- "gen_precision"
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
      dt$formula <- character()
      dt$variance <- character()
      dt$link <- character()
      dt
    } , gen.and_then(gen_varnames(n), function(vars) {
      list(vars, gen_dists(n))
    }))
  }

gen_def_dt_n <- gen.sized(gen_def_dt)


gen_defs <- function(dt,n,i = 1){
  if(i > n) return(dt)
  
  generate(for  (x in list(
    formula = get(reg[name == dt[i,dist]]$formula)(dt[seq_len(i-1),varname]),
    variance = get(reg[name == dt[i,dist]]$variance)(dt[seq_len(i-1),varname]),
    link = get(reg[name == dt[i,dist]]$link)
  )){ dt$formula[i] <- x$formula
     dt$variance[i] <- x$variance
     dt$link[i] <- x$link
     gen_defs(dt,n,i+1) })
  }

gen_def <- gen.and_then(gen.int(20),function(n) gen.and_then(gen_def_dt(n),function(dt) gen_defs(dt,n,1)))
