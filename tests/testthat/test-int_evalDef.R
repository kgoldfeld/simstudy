 ge_before <- names(.GlobalEnv)
 ge_before <- names(.GlobalEnv)

test_that("valid formula causes no errors.", {
  
  gen_gae <- gen.and_then(gen_varnames(8), function(ns) gen.map(function(y) list( defVars = ns, formula = y),gen_formula(ns) ))
  forall(gen_gae,function(x) expect_silent(.isValidArithmeticFormula(x$formula,x$defVars)))
  
})

 ge_after <- names(.GlobalEnv)
 ge_after <- names(.GlobalEnv)
 rm(list = ge_after[!ge_after %in% ge_before])

test_that("'link' checked as expected",{
  expect_silent(.isIdLog("identity"))
  expect_silent(.isIdLog("log"))
  expect_silent(.isIdLogit("identity"))
  expect_silent(.isIdLogit("logit"))
  
  expect_error(.isIdLog("what"),"Invalid link")
  expect_error(.isIdLogit("no"),"Invalid link")
}) 

test_that("utility functions",{
  names <- c("..as","..bs","..cs[4]","..ds[x]")
  res <- c("as","bs","cs[4]","ds[x]")
  
  expect_equal(.isDotArr(names),c(F,F,T,T))
  expect_equal(.rmDots(names),res)
  expect_equal(.rmWS(" ab  c      d \n\t e "),"abcde")
  expect_equal(.splitFormula("nosplit"),"nosplit")
  expect_equal(.splitFormula("a;split"),c("a","split"))
  expect_equal(.splitFormula(";split"),c("","split"))
})

