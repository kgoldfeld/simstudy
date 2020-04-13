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
gen_cat_probs <- gen.and_then(gen.element(2:10), function(x) gen.c(of = x, gen.unif(0, 1)))


gen_prev_var <- function(prev_vars) gen.no.shrink(gen.element(prev_vars)) 
gen_const <- gen.element(0:100)
gen_factor <- function(prev_var) gen.choice(gen_prev_var(prev_var),gen_const,gen_expr_br(prev_var))

gen_factor_dt <- function(prev_var) generate( for (x in list(op = gen.element(c(" * "," / ")),
                                                    fac1 = gen_factor(prev_var),
                                                    fac2 = gen_factor(prev_var)
                                                    )) list(x$fac1,x$op,x$fac2))

gen_term <- function(prev_var) gen.choice(gen_factor(prev_var),gen_factor_dt(prev_var))

gen_term_pm <- function(prev_var) generate( for (x in list(op = gen.element(c(" + "," - ")),
                                                           term1 = gen_factor(prev_var),
                                                           term2 = gen_factor(prev_var)
)) list(x$term1,x$op,x$term2))

gen_expr_br <- function(prev_var) gen.map(function(x) c("( ", x , " )") ,gen_expr(prev_var))
gen_expr <- function(prev_var) gen.choice(gen_term(prev_var),gen_term_pm(prev_var))


gen_cat_args <- function()
  generate(for (x in
                list(names = gen_varname,
                     probs = gen_cat_args))
 
      list(
        varname = paste(x$names, sep = "", collapse = ""),
        formula = catProbs(x$probs),
        dist = "categorical"
      )
    )

test_that("Catprobs are handeled", {
  forall(gen_cat_args(), function(x) expect_type(   do.call(defData,x),"list") )
})



