

ge_before <- names(.GlobalEnv)
ge_before <- names(.GlobalEnv)

test_that("Faulty distribution names are detected", {
 
  gen_gae <- gen.and_then(gen_varnames(8), function(ns) gen.map(function(y) list( defVars = ns, formula = y),gen_formula(ns) ))
  forall(gen_gae,function(x) expect_silent(.isValidArithmeticFormula(x$formula,x$defVars)))
  
})

ge_after <- names(.GlobalEnv)
ge_after <- names(.GlobalEnv)
rm(list = ge_after[!ge_after %in% ge_before])
