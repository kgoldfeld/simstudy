freeze_eval <- names(.GlobalEnv)

test_that("g.a.e. formula checked correctly.", {
  gen_gae <-
    gen.and_then(gen_varnames(8), function(ns)
      gen.map(function(y)
        list(
          defVars = ns, formula = y
        ), gen_formula(ns)))
  forall(gen_gae, function(x)
    expect_silent(.isValidArithmeticFormula(x$formula, x$defVars)))
  
  expect_error(.isValidArithmeticFormula("a;3"), "';' are not allowed")
  expect_error(.isValidArithmeticFormula("1+3-"), "not in proper form")
  expect_error(.isValidArithmeticFormula("..log(3)", ""),
               "Functions don't need")
  expect_error(.isValidArithmeticFormula("a + 3", ""),
               "not previously defined")
  expect_error(.isValidArithmeticFormula("noAfunc() + 3", ""),
               "referenced not defined")
  expect_error(.isValidArithmeticFormula("..notAvar + 3", ""),
               "Escaped variables referenced")
})

rm(list = setdiff(names(.GlobalEnv),freeze_eval),pos = .GlobalEnv)

test_that("'mixture' formula checked correctly", {
  gen_mix_vars <-
    gen.choice(gen_dotdot_num, gen_varname, gen.element(-100:100))
  gen_mix_vForm <-
    gen.sized(function(n) {
      gen.and_then(gen.c(gen_mix_vars, of = n), function(p) {
        gen_mixture(p)
      })
    })
  gen_mix_form <- gen.choice(gen_mix_vForm, gen_mix_scalar)
  
  forall(gen_mix_form, function(f)
    expect_silent(.checkMixture(f)))
  
  expect_error(.checkMixture("nr | .5 + a "), "same amount")
  expect_error(.checkMixture("nr | be"), "Probabilities can only be")
  expect_error(.checkMixture("nr | ..be[3]"), "Probabilities can only be")
  expect_error(.checkMixture("..nr | .2"), "Variables contain")
  expect_error(.checkMixture("1 | .1 + 2 | 2"), "sum to 1")
  notANumber <- "string"
  expect_error(.checkMixture("1 | ..notANumber + 2 | 2"),
               "Probabilites contain 'NA'")
  expect_error(.checkMixture("..notANumber | 1 + 2 | 2"),
               "Variables contain 'NA'")
})

test_that("'categorical' formula checked correctly", {
  forall(gen_cat_probs, function(f)
    expect_silent(.checkCategorical(catProbs(f))))
  
  expect_error(.checkCategorical("1;a;as;2"), "two numeric")
  expect_error(.checkCategorical("1;3;2"), "sum to 1")
  expect_error(.checkCategorical("1"), "two numeric")
})

test_that("'uniform' formula checked correctly", {
  forall(gen_uniform_range(), function(r)
    expect_silent(.checkUniform(r)))
  
  expect_warning(.checkUniform("1;1"), "the same")
  expect_error(.checkUniform(""), "format")
  expect_error(.checkUniform("1;2;3"), "format")
  expect_error(.checkUniform("2;1"), "'max' < 'min'")
})

test_that("'uniformInt' formula checked correctly", {
  forall(gen_uniformInt_range(), function(r)
    expect_silent(.checkUniformInt(r)))
  
  expect_warning(.checkUniformInt("1;1"), "the same")
  expect_error(.checkUniformInt(""), "format")
  expect_error(.checkUniformInt("1;2;3"), "format")
  expect_error(.checkUniformInt("2;1"), "'max' < 'min'")
  expect_error(.checkUniformInt("1.1;2.4"), "must be integer")
})

test_that("'link' checked as expected", {
  expect_silent(.isIdLog("identity"))
  expect_silent(.isIdLog("log"))
  expect_silent(.isIdLogit("identity"))
  expect_silent(.isIdLogit("logit"))
  
  expect_error(.isIdLog("what"), "Invalid link")
  expect_error(.isIdLogit("no"), "Invalid link")
  expect_error(.isIdLog(""), "Invalid link")
  expect_error(.isIdLogit(""), "Invalid link")
})

test_that("utility functions work", {
  names <- c("..as", "..bs", "..cs[4]", "..ds[x]")
  res <- c("as", "bs", "cs[4]", "ds[x]")
  
  expect_equal(.isDotArr(names), c(FALSE, FALSE, TRUE, TRUE))
  
  expect_equal(.rmDots(names), res)
  
  expect_equal(.rmWS(" ab  c      d \n\t e "), "abcde")
  
  expect_equal(.splitFormula("nosplit"), "nosplit")
  expect_vector(.splitFormula("a;split"))
  expect_equal(.splitFormula("a;split"), c("a", "split"))
  expect_equal(.splitFormula(";split"), c("", "split"))
})
