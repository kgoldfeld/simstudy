library(hedgehog)
freeze_eval <- names(.GlobalEnv)

# defCondition ----

# .evalDef ----

test_that("checks combine in .evalDef correctly", {
  skip_on_cran()

  # this generates 20 previously defined varnames.
  gen_defVars <- gen.and_then(gen.int(20), gen_varnames)

  gen_evalDef_call <-
    gen.and_then(gen_defVars, function(defVars) {
      generate(for (i in gen_dist) {
        list(
          newvar = defVars[1],
          newform = get(reg[name == i]$formula)(defVars[-1]),
          newdist = i,
          variance = get(reg[name == i]$variance)(defVars[-1]),
          link = get(reg[name == i]$link),
          defVars = defVars[-1]
        )
      })
    })

  forall(gen_evalDef_call, function(args) expect_silent(do.call(.evalDef, args)))
})

test_that(".evalDef throws errors correctly.", {
  expect_error(.evalDef(newvar = 1, "1 + 2", "normal", 0, "identiy", ""), class = "simstudy::wrongType")
  expect_error(.evalDef(newvar = c("a", "b"), "1 + 2", "normal", 0, "identiy", ""), class = "simstudy::lengthMismatch")
  expect_error(.evalDef(newvar = "varname", "1 + 2", "not valid", 0, "identiy", ""), class = "simstudy::optionInvalid")
  expect_error(.evalDef("..varname", 3, "normal", 0, "identity", ""), class = "simstudy::valueError")
  expect_error(.evalDef("varname", 3, "normal", 0, "identity", "varname"), class = "simstudy::alreadyDefined")
  expect_error(.evalDef("varname", 3, "normal", 0, "identity"), class = "simstudy::missingArgument")
  expect_warning(.evalDef("2", 3, "normal", 0, "identity", ""), class = "simstudy::valueError")
})

# .isValidArithmeticFormula ----

test_that("g.a.e. formula checked correctly.", {
  skip_on_cran()
  gen_gae <-
    gen.and_then(gen_varnames(8), function(ns) {
      gen.map(function(y) {
        list(
          defVars = ns, formula = y
        )
      }, gen_formula(ns))
    })

  forall(gen_gae, function(x) {
    expect_silent(.isValidArithmeticFormula(x$formula, x$defVars))
  })
})

test_that(".isValidArithmeticFormula throws errors correctly.", {
  expect_error(.isValidArithmeticFormula(""), class = "simstudy::noValue")
  expect_error(.isValidArithmeticFormula("a;3"), class = "simstudy::valueError")
  expect_error(.isValidArithmeticFormula("1+3-"), class = "simstudy::valueError")
  expect_error(.isValidArithmeticFormula("..log(3)", ""), class = "simstudy::valueError")
  expect_error(.isValidArithmeticFormula("a + 3", ""), class = "simstudy::notDefined")
})

# .checkMixture ----

test_that("'mixture' formula checked correctly", {
  skip_on_cran()
  gen_mix_vars <- gen.choice(gen_dotdot_num, gen_varname, gen.element(-100:100))
  gen_mix_vForm <- gen.sized(function(n) {
    gen.and_then(gen.c(gen_mix_vars, of = n), function(p) {
      gen_mixture(p)
    })
  })

  gen_mix_form <- gen.choice(gen_mix_vForm, gen_mix_scalar)

  forall(gen_mix_form, function(f) {
    expect_silent(.checkMixture(f))
  })
})

test_that(".checkMixture throws errors.", {
  expect_error(.checkMixture("nr | .5 + a "), "same amount")
  expect_error(.checkMixture("nr | be"), "Probabilities can only be")
})

# .checkCategorical ----

test_that("'categorical' formula checked correctly", {
  skip_on_cran()
  forall(gen_cat_probs, function(f) {
    expect_silent(.checkCategorical(genCatFormula(f)))
  })
})

test_that(".checkCategorical throws errors.", {
  expect_error(.checkCategorical("1"), "two numeric")
})

# .checkUniform ----

test_that("'uniform' formula checked correctly", {
  skip_on_cran()
  forall(
    gen.and_then(gen_varnames(10), function(names) {
      generate(for (x in list(
        min = gen_formula(names),
        max = gen_formula(names)
      )) {
        paste0(x$min, ";", x$max)
      })
    }),
    function(r) {
      expect_silent(.checkUniform(r))
    }
  )
})

test_that(".checkUniform throws errors.", {
  expect_error(.checkUniform(""), "format")
  expect_error(.checkUniform("1;2;3"), "format")
})

# .isLink ----
# .isIdLog ----
# .isIdLogit ----
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

# .rmDots ----
# .rmWS ----
# .isDotArr ----

# .splitFormula ----

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

# defData ----

test_that("defData throws off errors.", {
  expect_error(defData(formula = .5, dist = "binary"),
    class = "simstudy::missingArgument")
  expect_error(defData(varname = "x", dist = "binary"),
               class = "simstudy::missingArgument")
  
  d <- defData(varname = "x", formula = .5, dist = "binary")
  expect_error(defData(d, varname = "y", dist = "binary"),
    class = "simstudy::missingArgument")
  expect_error(defData(d, formula = .4, dist = "binary"),
               class = "simstudy::missingArgument")
  
  expect_error(defData(varname = "b", formula = "5+", dist = "gamma"))
  expect_error(defData(varname = "b", formula = .5, variance = "1+", dist = "gamma"))
  expect_error(defData(varname = "b", formula = .5, variance = 10, 
                       dist = "gamma", link="logit"))
  
  expect_error(defData(varname = "b", formula = "5+", dist = "negBinomial"))
  expect_error(defData(varname = "b", formula = .5, variance = "1+", 
                       dist = "negBinomial"))
  expect_error(defData(varname = "b", formula = .5, variance = 10, 
                       dist = "negBinomial", link="logit"))
  
  expect_error(defData(varname = "b", formula = "5+", dist = "exponential"))
  expect_error(defData(varname = "b", formula = 5, variance = 10, 
                       dist = "exponential", link="logit"))
  
  expect_error(defData(varname = "b", formula = "5+", dist = "binomial"))
  expect_error(defData(varname = "b", formula = .5, variance = "1+", dist = "binomial"))
  expect_error(defData(varname = "b", formula = .5, variance = 10, dist = "binomial", link="log"))
  
  expect_error(defData(varname = "b", formula = "5", dist = "expotial"))
  
})

# defSurv ----

test_that("defSurv works.", {
  def <- defData(varname = "x1", formula = .5, dist = "binary")
  def <- defData(def, varname = "x2", formula = .5, dist = "binary")
  def <- defData(def, varname = "grp", formula = .5, dist = "binary")

  expect_silent(
    defSurv(
      varname = "s", formula = "1.5*x1",
      scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5"
    )
  )
})

# defRepeat ----

test_that("defRepeat works.", {
  expect_silent(
    defRepeat(nVars = 4, prefix = "g", formula = "1/3;1/3;1/3", variance = 0, dist = "categorical")
  )
  
  def <- defData(varname = "a", formula = "1;1", dist = "trtAssign")
  expect_silent(
    defRepeat(def, 8, "b", formula = "5 + a", variance = 3, dist = "normal")
  )
})

test_that("defRepeat throws errors correctly.", {
  expect_error(defRepeat(prefix = "b", formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument")
  expect_error(defRepeat(nVars = 8, formula = 5, variance = 3, dist = "normal"), 
    class = "simstudy::missingArgument")
  expect_error(defRepeat(nVars = 8, prefix = "b", variance = 3, dist = "normal"),
    class = "simstudy::missingArgument")
  expect_error(defRepeat(nVars = 4, prefix = "b", formula = "5 + a", variance = 3, dist = "normal"))
})

# defRead ----

test_that("defRead works.", {
  oldSeed <- .Random.seed
  
  def1 <- defData(varname = "x1", formula = 0, var = 1, dist = "normal")
  def1 <- defData(def1, varname = "x2", formula = 0.2, dist = "binary")
  
  filen <- tempfile("test", fileext = ".csv")
  data.table::fwrite(x=def1, file=filen)
  
  def2 <- defRead(file = filen)
  
  seed <-sample(1:1e8, 1)
  set.seed(seed)
  d1 <- genData(10, def1)
  
  set.seed(seed)
  d2 <- genData(10, def2)
  
  expect_equal(d1, d2)
  set.seed(oldSeed)
  
})

test_that("defRead throws off errors",{
  
  filen <- tempfile("test", fileext = ".csv")
  
  def1 <- defData(varname = "x1", formula = 0, var = 1, dist = "normal")
  def1 <- defData(def1, varname = "x2", formula = 0.2, dist = "binary")
  def1[varname == "x2", varname := "x1"]
   
  data.table::fwrite(x=def1, file=filen)
  expect_error(defRead(file = filen))
   
  filen <- tempfile("test", fileext = ".csv")
  expect_error(defRead(file = filen))
  
  filen <- tempfile("test", fileext = ".csv")
  def1 <- defData(varname = "x1", formula = 0, var = 1, dist = "normal")
  def1$error <- 0
  
  data.table::fwrite(x=def1, file=filen)
  expect_error(defRead(file = filen))
  
  filen <- tempfile("test", fileext = ".csv")
  def1 <- defData(varname = "x1", formula = 1, var = 1, dist = "normal")
  def1$formula <- "a"
    
  data.table::fwrite(x=def1, file=filen)
  expect_error(defRead(file = filen))
  
})

# defReadAdd ----

test_that("defReadAdd works.", {
  oldSeed <- .Random.seed
  
  def1 <- defData(varname = "x1", formula = 0, var = 1, dist = "normal")
  def1 <- defData(def1, varname = "x2", formula = 0.2, dist = "binary")
  
  def2 <- defDataAdd(varname = "x3", formula = 0, var = 1, dist = "normal")
  
  filen <- tempfile("test", fileext = ".csv")
  data.table::fwrite(x=def2, file=filen)
  
  def3 <- defReadAdd(file = filen)
  
  seed <-sample(1:1e8, 1)
  
  set.seed(seed)
  d1 <- genData(10, def1)
  d1 <- addColumns(def2, d1)
  
  set.seed(seed)
  d2 <- genData(10, def1)
  d2 <- addColumns(def3, d2)
  
  expect_equal(d1, d2)
  set.seed(oldSeed)
  
})

test_that("defReadAdd throws off errors",{
  
  filen <- tempfile("test", fileext = ".csv")
  expect_error(defReadAdd(file = filen))
  
  filen <- tempfile("test", fileext = ".csv")
  def1 <- defDataAdd(varname = "x1", formula = 0, var = 1, dist = "normal")
  def1$error <- 0
  
  data.table::fwrite(x=def1, file=filen)
  expect_error(defReadAdd(file = filen))
  
})

# defReadCond ----

test_that("defReadCond works.", {

  defC <- defCondition(
    condition = "x == 1", formula = "5 + 2*y",
    variance = 1, dist = "normal"
  )
  defC <- defCondition(defC,
    condition = "x <= 5 & x >= 2", formula = "3 - 2*y",
    variance = 1, dist = "normal"
  )
  defC <- defCondition(defC,
    condition = "x >= 6", formula = 1,
    variance = 1, dist = "normal"
  )
  
  filen <- tempfile("test", fileext = ".csv")
  data.table::fwrite(x=defC, file=filen)
  
  defCr <- defReadCond(file = filen)
  expect_equal(defC, defCr)

})

test_that("defReadCond throws off errors",{
  
  defC <- defCondition(
    condition = "x == 1", formula = "5 + 2*y",
    variance = 1, dist = "normal"
  )
  defC <- defCondition(defC,
                       condition = "x <= 5 & x >= 2", formula = "3 - 2*y",
                       variance = 1, dist = "normal"
  )
  defC <- defCondition(defC,
                       condition = "x >= 6", formula = 1,
                       variance = 1, dist = "normal"
  )
  
  defC$error <- 0
  
  filen <- tempfile("test", fileext = ".csv")
  data.table::fwrite(x=defC, file=filen)
  expect_error(defReadCond(file = filen))
  
  
  filen <- tempfile("test", fileext = ".csv")
  expect_error(defReadCond(file = filen))
  
  
  
})

# done ----

rm(list = setdiff(names(.GlobalEnv), freeze_eval), pos = .GlobalEnv)
