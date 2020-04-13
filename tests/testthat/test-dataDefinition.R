test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test <- paste("varname = ", gen_varname(3))
gen_abc <- gen.element(letters)
gen_ABC <- gen.element(LETTERS)
gen_123 <- gen.element(0:9)
gen_azAZ09 <- gen.choice(gen_abc, gen_ABC, gen_123)
gen
# gen. length of varname  and then  gen. varname of that length
gen_varname <- gen.and_then(gen.element(1:20), function(x) gen.c(of = x, gen_azAZ09))
gen_dist <- gen.no.shrink(gen.element(.getDists()))


## Formula Generator ----
#
# Generators to create an arithmetic expression with previously defined
# variables as formula argument for many distributions. 
#
# Only change gen_const range. 
gen_prev_var <- function(prev_vars) gen.no.shrink(gen.element(prev_vars)) 
gen_const <- gen.element(0:100)

gen_factor <- function(prev_var) gen.choice(gen_prev_var(prev_var),gen_const,gen_expr_br(prev_var))
gen_factor_dt <-
  function(prev_var)
    generate(for (x in list(
      op = gen.element(c(" * ", " / ")),
      fac1 = gen_factor(prev_var),
      fac2 = gen_factor(prev_var)
    ))
      list(x$fac1, x$op, x$fac2))

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
gen_expr <- function(prev_var) gen.choice(gen_term(prev_var),gen_term_pm(prev_var))

gen_formula <- function(prev_var) gen.map(function(x) paste(unlist(x),collapse = ""),gen_expr(prev_var) )

## Mixture Generator ----
gen_n_norm_Probs <- function(n) gen.map(function(p) p/sum(p) ,gen.c(of = n, gen.unif(0, 1)))
gen_mixture <- function(prev_var) gen.and_then(gen.element(2:length(prev_var)),function(n) gen_mix_parts(prev_var = prev_var,n = n))
gen_mix_parts <- function(prev_var,n) generate(for (x in 
                                                list(probs = gen_n_norm_Probs(n),
                                                     vars = gen.c(of = n, gen_prev_var(prev_var))))
                                                paste(x$vars,x$probs,sep = " | ", collapse = " + "))
## Categorical Generator ----
gen_cat_probs <- gen.and_then(gen.element(2:10), function(n) gen_n_norm_Probs(n))
gen_cat_formula <- gen.map(function(p) catProbs(p),gen_cat_probs)

## Uniform Generator ----
gen_uniformInt_range <- gen.and_then(gen.int(1000), function(x) gen.map(function(y) paste0(y,";",x) ,gen.int(x)))
gen_uniform_range <- gen.map(function(x)paste0(sort(unlist(x)),collapse = ";"), gen.c(of = 2,gen.unif(-100,100)))


gen_cat_args <- function()
  generate(for (x in
                list(names = gen_varname,
                     probs = gen_cat_args))
    
    list(
      varname = paste(x$names, sep = "", collapse = ""),
      formula = catProbs(x$probs),
      dist = "categorical"
    ))

test_that("Catprobs are handeled", {
  forall(gen_cat_args(), function(x) expect_type(   do.call(defData,x),"list") )
})



