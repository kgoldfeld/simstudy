library(testthat)
library(simstudy)
library(data.table)

library(hedgehog)
freeze_eval <- names(.GlobalEnv)

# defCondition ----

# defData ----
test_that("defData throws errors", {
  skip_on_cran()
  
  expect_error(defData(dist = "nonrandom", formula = 7, id = "idnum"), class = "simstudy::missingArgument")
  expect_error(defData(varname = "xnr", dist = "nonrandom", id = "idnum"), class = "simstudy::missingArgument")
})

# defRead ----


# .evalDef ----

test_that("Valid binary definition passes silently", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_bin",
    newform = "0.5 + y",
    newdist = "binary",
    variance = 0,
    link = "logit",
    defVars = c("y")
  ))
})

test_that("Valid binomial definition passes silently", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_binom",
    newform = "0.5 + y",
    newdist = "binomial",
    variance = 5,
    link = "logit",
    defVars = c("y")
  ))
})

test_that("Valid poisson definition passes silently", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_pois",
    newform = "0.5 + y",
    newdist = "poisson",
    variance = 0,
    link = "log",
    defVars = c("y")
  ))
})

test_that("Valid no zero poisson definition passes silently", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_no0pois",
    newform = "0.5 + y",
    newdist = "noZeroPoisson",
    variance = 0,
    link = "log",
    defVars = c("y")
  ))
})

test_that("Valid no zero poisson definition passes silently", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_nonrandom",
    newform = "0.5 + y",
    newdist = "nonrandom",
    variance = 0,
    link = "log",
    defVars = c("y")
  ))
})

test_that("Valid normal distribution passes with variance", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_norm",
    newform = "3 + y",
    newdist = "normal",
    variance = 1,
    defVars = c("y")
  ))
})

test_that("Valid gamma distribution passes with variance", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_gamma",
    newform = "3 + y",
    newdist = "gamma",
    variance = 1,
    defVars = c("y")
  ))
})

test_that("Valid negBinomial distribution passes with variance", {
  skip_on_cran()
  expect_invisible(simstudy:::.evalDef(
    newvar = "x_negBinom",
    newform = "3 + y",
    newdist = "negBinomial",
    variance = 1,
    defVars = c("y")
  ))
})

test_that("Fails when variable already exists in defVars", {
  skip_on_cran()
  expect_error(
    simstudy:::.evalDef(
      newvar = "y",
      newform = "1",
      newdist = "normal",
      variance = 0,
      defVars = c("y", "z")
    ),
    regexp = "Variable y previously defined"
  )
})

test_that("Fails when newvar starts with '..'", {
  skip_on_cran()
  
  expect_error(
    simstudy:::.evalDef(
      newvar = "..badname",
      newform = "1",
      newdist = "normal",
      variance = 0,
      defVars = character()
    ),
    regexp = "The prefix '\\.\\.' is reserved"
  )
})

test_that("Unknown distribution throws error", {
  skip_on_cran()
  
  expect_error(
    simstudy:::.evalDef(
      newvar = "x",
      newform = "1",
      newdist = "madeUpDist",
      variance = 0,
      defVars = character()
    ),
    regexp = "Argument dist: 'madeUpDist' invalid."
  )
})

test_that("Valid categorical definition passes", {
  skip_on_cran()
  
  expect_invisible(
    simstudy:::.evalDef(
      newvar = "catVar",
      newform = "1;1;1",
      newdist = "categorical",
      defVars = character()
    )
  )
})

test_that("Valid mixture definition passes", {
  skip_on_cran()
  
  expect_invisible(
    simstudy:::.evalDef(
      newvar = "mixVar",
      newform = "x1|.5 + x2|.5",
      newdist = "mixture",
      defVars = c("x1", "x2")
    )
  )
})

test_that("Valid uniformInt distribution passes", {
  skip_on_cran()
  
  expect_invisible(
    simstudy:::.evalDef(
      newvar = "uInt",
      newform = "5;10",
      newdist = "uniformInt",
      defVars = character()
    )
  )
})

test_that("Valid clusterSize distribution passes", {
  skip_on_cran()
  
  expect_invisible(
    simstudy:::.evalDef(
      newvar = "cs",
      newform = "5",
      newdist = "clusterSize",
      variance = 2,
      defVars = character()
    )
  )
})

test_that("Valid beta distribution passes with logit link", {
  skip_on_cran()
  
  expect_invisible(
    simstudy:::.evalDef(
      newvar = "betaVar",
      newform = "2 + y",
      newdist = "beta",
      variance = 1,
      link = "logit",
      defVars = c("y")
    )
  )
})

test_that(".evalDef throws errors correctly.", {
  skip_on_cran()
  
  expect_error(simstudy:::.evalDef(newvar = 1, "1 + 2", "normal", 0, "identiy", ""), class = "simstudy::wrongType")
  expect_error(simstudy:::.evalDef(newvar = c("a", "b"), "1 + 2", "normal", 0, "identiy", ""), class = "simstudy::lengthMismatch")
  expect_error(simstudy:::.evalDef(newvar = "varname", "1 + 2", "not valid", 0, "identiy", ""), class = "simstudy::optionInvalid")
  expect_error(simstudy:::.evalDef("..varname", 3, "normal", 0, "identity", ""), class = "simstudy::valueError")
  expect_error(simstudy:::.evalDef("varname", 3, "normal", 0, "identity", "varname"), class = "simstudy::alreadyDefined")
  expect_error(simstudy:::.evalDef("varname", 3, "normal", 0, "identity"), class = "simstudy::missingArgument")
  expect_warning(simstudy:::.evalDef("2", 3, "normal", 0, "identity", ""), class = "simstudy::valueError")
})

#.isValidArithmeticFormula ----

test_that("g.a.e. formula checked correctly.", {

  skip_on_cran()

  gen_gae <-
    gen.and_then(g = gen_varnames(1), f = function(ns) {
      gen.map(function(y) {
        list(
          defVars = ns, formula = y
        )
      }, gen_formula(ns))
    })

  forall(gen_gae, function(x) {
    expect_silent(simstudy:::.isValidArithmeticFormula(x$formula, x$defVars))
  })
})

test_that("Invalid arithmetic expression triggers stop()", {
  skip_on_cran()
  expect_error(
    simstudy:::.isValidArithmeticFormula("5 + * 2", defVars = character()),
    regexp = "Equation: '5 \\+ \\* 2' not in proper form"
  )
})

test_that(".isValidArithmeticFormula throws errors correctly.", {
  skip_on_cran()
  
  expect_error(simstudy:::.isValidArithmeticFormula(""), class = "simstudy::noValue")
  expect_error(simstudy:::.isValidArithmeticFormula("a;3"), class = "simstudy::valueError")
  expect_error(simstudy:::.isValidArithmeticFormula("1+3-"), class = "simstudy::valueError")
  expect_error(simstudy:::.isValidArithmeticFormula("..log(3)", ""), class = "simstudy::valueError")
  expect_error(simstudy:::.isValidArithmeticFormula("a + 3", ""), class = "simstudy::notDefined")
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
    expect_silent(simstudy:::.checkMixture(f))
  })
})

test_that(".checkMixture throws errors.", {
  skip_on_cran()
  
  expect_error(simstudy:::.checkMixture("nr | .5 + a "), "same amount")
  expect_error(simstudy:::.checkMixture("nr | be"), "Probabilities can only be")
})

# .checkCategorical ----
test_that("'categorical' formula checked correctly", {
  skip_on_cran()
  
  forall(gen_cat_probs, function(f) {
    expect_silent(simstudy:::.checkCategorical(genCatFormula(f)))
  })
})

test_that(".checkCategorical throws errors.", {
  skip_on_cran()
  
  expect_error(simstudy:::.checkCategorical("1"), "two numeric")
})

# .checkUniform ----
# test_that("'uniform' formula checked correctly", {
#   skip_on_cran()
#   
#   forall(
#     gen.and_then(gen_varnames(10), function(names) {
#       generate(for (x in list(
#         min = gen_formula(names),
#         max = gen_formula(names)
#       )) {
#         paste0(x$min, ";", x$max)
#       })
#     }),
#     function(r) {
#       expect_silent(.checkUniform(r))
#     }
#   )
# })

test_that(".checkUniform throws errors.", {
  skip_on_cran()
  
  expect_error(simstudy:::.checkUniform(""), "format")
  expect_error(simstudy:::.checkUniform("1;2;3"), "format")
})

# .isLink ----
# .isIdLog ----
# .isIdLogit ----
test_that("'link' checked as expected", {
  skip_on_cran()
  
  expect_silent(simstudy:::.isIdLog("identity"))
  expect_silent(simstudy:::.isIdLog("log"))
  expect_silent(simstudy:::.isIdLogit("identity"))
  expect_silent(simstudy:::.isIdLogit("logit"))

  expect_error(simstudy:::.isIdLog("what"), "Invalid link")
  expect_error(simstudy:::.isIdLogit("no"), "Invalid link")
  expect_error(simstudy:::.isIdLog(""), "Invalid link")
  expect_error(simstudy:::.isIdLogit(""), "Invalid link")
})

# .rmDots ----
# .rmWS ----
# .isDotArr ----
# .splitFormula ----
test_that("utility functions work", {
  skip_on_cran()
  
  names <- c("..as", "..bs", "..cs[4]", "..ds[x]")
  res <- c("as", "bs", "cs[4]", "ds[x]")

  expect_equal(simstudy:::.isDotArr(names), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(simstudy:::.rmDots(names), res)
  expect_equal(simstudy:::.rmWS(" ab  c      d \n\t e "), "abcde")
  expect_equal(simstudy:::.splitFormula("nosplit"), "nosplit")
  expect_vector(simstudy:::.splitFormula("a;split"))
  expect_equal(simstudy:::.splitFormula("a;split"), c("a", "split"))
  expect_equal(simstudy:::.splitFormula(";split"), c("", "split"))
})

test_that("defRepeat works.", {
  skip_on_cran()
  
  expect_silent(
    defRepeat(nVars = 4, prefix = "g", formula = "1/3;1/3;1/3", variance = 0, dist = "categorical")
  )

  def <- defData(varname = "a", formula = "1;1", dist = "trtAssign")
  expect_silent(
    defRepeat(def, 8, "b", formula = "5 + a", variance = 3, dist = "normal")
  )
})

test_that("defRepeat throws errors correctly.", {
  skip_on_cran()
  
  expect_error(defRepeat(prefix = "b", formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeat(nVars = 8, formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeat(nVars = 8, prefix = "b", variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeat(nVars = 4, prefix = "b", formula = "5 + a", variance = 3, dist = "normal"))
})

# defRead <-----

test_that("Correctly reads valid CSV file and returns data.table with id attribute", {
  
  skip_on_cran()
  
  lines <- c(
    "varname,formula,variance,dist,link",
    "a,5,0,nonrandom,identity",
    "b,a + 1,1,normal,identity"
  )
  tf <- tempfile(fileext = ".csv")
  writeLines(lines, tf)

  def <- defRead(tf, id = "customID")

  expect_s3_class(def, "data.table")
  expect_equal(attr(def, "id"), "customID")
  expect_equal(nrow(def), 2)
  expect_equal(def$varname[1], "a")
  expect_equal(def$formula[2], "a + 1")

  unlink(tf)
})

test_that("Throws error if file does not exist", {
  skip_on_cran()
  
  expect_error(defRead("nonexistent.csv"), "No such file")
})


test_that("Throws error if field names do not match", {
  skip_on_cran()
  
  lines <- c(
    "wrongname,formula,variance,dist,link",
    "a,5,0,nonrandom,identity"
  )
  tf <- tempfile(fileext = ".csv")
  writeLines(lines, tf)

  expect_error(defRead(tf), "Field names do not match")

  unlink(tf)
})

test_that("Throws error if first formula is not scalar", {
  skip_on_cran()
  
  lines <- c(
    "varname,formula,variance,dist,link",
    "a,a + 1,0,nonrandom,identity"
  )
  tf <- tempfile(fileext = ".csv")
  writeLines(lines, tf)

  expect_error(defRead(tf), "First defined formula must be scalar")

  unlink(tf)
})

test_that("Returns definition that can be used in genData()", {
  skip_on_cran()
  
  lines <- c(
    "varname,formula,variance,dist,link",
    "a,3,0,nonrandom,identity",
    "b,a + 2,5,normal,identity"
  )
  tf <- tempfile(fileext = ".csv")
  writeLines(lines, tf)

  def <- defRead(tf)
  data <- genData(5, def)

  expect_true(all(c("a", "b") %in% names(data)))
  expect_equal(nrow(data), 5)

  unlink(tf)
})

# defCondition <-----

test_that("Creates a new definition table when dtDefs is NULL", {
  skip_on_cran()
  
  def <- defCondition(
    dtDefs = NULL,
    condition = "x > 1",
    formula = "3 + y",
    variance = 2,
    dist = "normal",
    link = "identity"
  )
  
  expect_s3_class(def, "data.table")
  expect_equal(nrow(def), 1)
  expect_named(def, c("condition", "formula", "variance", "dist", "link"))
  expect_equal(def$condition[1], "x > 1")
  expect_equal(def$formula[1], "3 + y")
  expect_equal(def$variance[1], 2)
  expect_equal(def$dist[1], "normal")
  expect_equal(def$link[1], "identity")
})

test_that("Adds a row to an existing definition table", {
  skip_on_cran()
  
  dtDefs <- defCondition(
    dtDefs = NULL,
    condition = "x > 1",
    formula = "3 + y",
    variance = 2,
    dist = "normal",
    link = "identity"
  )
  
  dtDefs <- defCondition(
    dtDefs = dtDefs,
    condition = "x <= 1",
    formula = "2 * y",
    variance = 1,
    dist = "normal",
    link = "log"
  )
  
  expect_equal(nrow(dtDefs), 2)
  expect_equal(dtDefs$condition[2], "x <= 1")
  expect_equal(dtDefs$formula[2], "2 * y")
  expect_equal(dtDefs$variance[2], 1)
  expect_equal(dtDefs$dist[2], "normal")
  expect_equal(dtDefs$link[2], "log")
})

test_that("Defaults are correctly applied", {
  skip_on_cran()
  
  def <- defCondition(
    dtDefs = NULL,
    condition = "x == 0",
    formula = "1"
  )
  
  expect_equal(def$variance[1], 0)
  expect_equal(def$dist[1], "normal")
  expect_equal(def$link[1], "identity")
})

test_that("Handles unusual but valid inputs", {
  skip_on_cran()
  
  def <- defCondition(
    dtDefs = NULL,
    condition = "TRUE",
    formula = "mean(x)",
    variance = 0.5,
    dist = "beta",
    link = "logit"
  )
  
  expect_equal(def$condition[1], "TRUE")
  expect_equal(def$formula[1], "mean(x)")
  expect_equal(def$dist[1], "beta")
  expect_equal(def$link[1], "logit")
})

test_that("Produces consistent column types", {
  skip_on_cran()
  
  def <- defCondition(
    dtDefs = NULL,
    condition = "x > 0",
    formula = "3 * y",
    variance = 1,
    dist = "poisson",
    link = "log"
  )
  
  expect_type(def$condition, "character")
  expect_type(def$formula, "character")
  expect_type(def$variance, "double")
  expect_type(def$dist, "character")
  expect_type(def$link, "character")
})

# defDataAdd <---

test_that("Creates a new definition table when dtDefs is NULL", {
  skip_on_cran()
  
  def <- defDataAdd(
    dtDefs = NULL,
    varname = "x",
    formula = "5 + y",
    variance = 2,
    dist = "normal",
    link = "identity"
  )
  
  expect_s3_class(def, "data.table")
  expect_equal(nrow(def), 1)
  expect_named(def, c("varname", "formula", "variance", "dist", "link"))
  expect_equal(def$varname[1], "x")
  expect_equal(def$formula[1], "5 + y")
  expect_equal(def$variance[1], 2)
  expect_equal(def$dist[1], "normal")
  expect_equal(def$link[1], "identity")
})

test_that("Adds a row to an existing definition table", {
  skip_on_cran()
  
  dtDefs <- defDataAdd(
    dtDefs = NULL,
    varname = "x",
    formula = "5",
    variance = 1,
    dist = "normal",
    link = "identity"
  )
  
  dtDefs <- defDataAdd(
    dtDefs = dtDefs,
    varname = "y",
    formula = "x * 2",
    variance = 0,
    dist = "poisson",
    link = "log"
  )
  
  expect_equal(nrow(dtDefs), 2)
  expect_equal(dtDefs$varname[2], "y")
  expect_equal(dtDefs$formula[2], "x * 2")
  expect_equal(dtDefs$variance[2], 0)
  expect_equal(dtDefs$dist[2], "poisson")
  expect_equal(dtDefs$link[2], "log")
})

test_that("Defaults are correctly applied", {
  skip_on_cran()
  
  def <- defDataAdd(
    dtDefs = NULL,
    varname = "x",
    formula = "3"
  )
  
  expect_equal(def$variance[1], 0)
  expect_equal(def$dist[1], "normal")
  expect_equal(def$link[1], "identity")
})

test_that("Handles vector inputs for multiple calls", {
  skip_on_cran()
  
  def <- defDataAdd(NULL, varname = "a", formula = "1")
  def <- defDataAdd(def, varname = "b", formula = "2")
  def <- defDataAdd(def, varname = "c", formula = "3")
  
  expect_equal(nrow(def), 3)
  expect_equal(def$varname, c("a", "b", "c"))
})

test_that("Column types are consistent", {
  skip_on_cran()
  
  def <- defDataAdd(
    dtDefs = NULL,
    varname = "x",
    formula = "4",
    variance = 0.5,
    dist = "gamma",
    link = "log"
  )
  
  expect_type(def$varname, "character")
  expect_type(def$formula, "character")
  expect_type(def$variance, "double")
  expect_type(def$dist, "character")
  expect_type(def$link, "character")
})

# defRepeat -----

test_that("Creates new definition table with correct number of rows", {
  
  skip_on_cran()
  
  def <- defRepeat(
    dtDefs = NULL,
    nVars = 3,
    prefix = "test",
    formula = "0.5",
    variance = 1,
    dist = "normal"
  )
  
  expect_s3_class(def, "data.table")
  expect_equal(nrow(def), 3)
  expect_equal(def$varname, c("test1", "test2", "test3"))
})

test_that("Appends new variables to existing definition table", {
  
  skip_on_cran()
  
  def0 <- defData(varname = "x", formula = "1", dist = "nonrandom")
  def <- defRepeat(
    dtDefs = def0,
    nVars = 3,
    prefix = "newVar",
    formula = "x + 1",
    variance = 2,
    dist = "normal"
  )
  
  expect_equal(nrow(def), 4)
  expect_true(all(c("x", "newVar1", "newVar2", "newVar3") %in% def$varname))
})

test_that("Uses correct default values for optional arguments", {
  
  skip_on_cran()
  
  def <- defRepeat(
    nVars = 2,
    prefix = "dflt",
    formula = "1"
  )
  
  expect_equal(nrow(def), 2)
  expect_equal(def$dist, rep("normal", 2))
  expect_equal(def$link, rep("identity", 2))
  expect_equal(def$variance, rep(0, 2))
})

test_that("Different prefixes generate distinct variable names", {
  
  skip_on_cran()
  
  defA <- defRepeat(nVars = 2, prefix = "a", formula = "1")
  defB <- defRepeat(nVars = 2, prefix = "b", formula = "1")
  
  expect_equal(defA$varname, c("a1", "a2"))
  expect_equal(defB$varname, c("b1", "b2"))
})

test_that("Fails when required arguments are missing", {
  
  skip_on_cran()
  
  expect_error(defRepeat(prefix = "x", formula = "1"), "argument is missing.*nVars")
  expect_error(defRepeat(nVars = 2, formula = "1"), "argument is missing.*prefix")
  expect_error(defRepeat(nVars = 2, prefix = "x"), "argument is missing.*formula")
})

test_that("Supports complex formula strings", {
  
  skip_on_cran()
  
  def <- defRepeat(nVars = 2, prefix = "comp", formula = "0.25;0.75", dist = "categorical")
  expect_equal(nrow(def), 2)
  expect_equal(def$dist, rep("categorical", 2))
  expect_equal(def$formula, rep("0.25;0.75", 2))
})

# defRepeatAdd -----

test_that("Creates new definition table with correct number of variables", {
  
  skip_on_cran()
  
  def <- defRepeatAdd(
    dtDefs = NULL,
    nVars = 3,
    prefix = "var",
    formula = "5",
    variance = 2,
    dist = "normal"
  )
  
  expect_s3_class(def, "data.table")
  expect_equal(nrow(def), 3)
  expect_equal(def$varname, c("var1", "var2", "var3"))
  expect_true(all(def$dist == "normal"))
  expect_true(all(def$formula == "5"))
})

test_that("Appends new variables to an existing definition table", {
  
  skip_on_cran()
  
  def0 <- defDataAdd(varname = "x", formula = "1", dist = "nonrandom")
  def <- defRepeatAdd(
    dtDefs = def0,
    nVars = 2,
    prefix = "add",
    formula = "x + 2",
    variance = 1,
    dist = "normal"
  )
  
  expect_equal(nrow(def), 3)
  expect_true(all(c("x", "add1", "add2") %in% def$varname))
})

test_that("Default arguments are used when not specified", {
  
  skip_on_cran()
  
  def <- defRepeatAdd(
    nVars = 2,
    prefix = "d",
    formula = "3"
  )
  
  expect_equal(nrow(def), 2)
  expect_equal(def$dist, rep("normal", 2))
  expect_equal(def$link, rep("identity", 2))
  expect_equal(def$variance, rep(0, 2))
})

test_that("Fails when required arguments are missing", {
  
  skip_on_cran()
  
  expect_error(defRepeatAdd(prefix = "x", formula = "1"), 
               regexp = "argument is missing.*nVars")
  expect_error(defRepeatAdd(nVars = 2, formula = "1"), 
               regexp = "argument is missing.*prefix")
  expect_error(defRepeatAdd(nVars = 2, prefix = "x"), 
               regexp = "argument is missing.*formula")
})

test_that("Supports non-default distributions and formulas", {
  
  skip_on_cran()
  
  def <- defRepeatAdd(
    nVars = 2,
    prefix = "cat",
    formula = "0.3;0.7",
    dist = "categorical"
  )
  
  expect_equal(def$dist, rep("categorical", 2))
  expect_equal(def$formula, rep("0.3;0.7", 2))
})

test_that("Correct varnames are generated for multiple calls", {
  
  skip_on_cran()
  
  defA <- defRepeatAdd(nVars = 2, prefix = "a", formula = "1")
  defB <- defRepeatAdd(defA, nVars = 2, prefix = "b", formula = "2")
  
  expect_equal(defB$varname, c("a1", "a2", "b1", "b2"))
})

# defSurv -----

test_that("defSurv adds a single row to an empty data.table", {
  
  skip_on_cran()
  
  result <- defSurv(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5")
  expect_equal(nrow(result), 1)
  expect_equal(result$varname, "survTime")
  expect_equal(result$formula, "1.5*x1")
  expect_equal(result$scale, "grp*50 + (1-grp)*25")
  expect_equal(result$shape, "grp*1 + (1-grp)*1.5")
  expect_equal(result$transition, 0)
})

test_that("defSurv adds a single row to a non-empty data.table", {
  
  skip_on_cran()
  
  result <- defSurv(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5")
  result <- defSurv(result, varname = "censor", formula = 1, scale = 1, shape = 1)
  expect_equal(nrow(result), 2)
  expect_equal(result$varname[2], "censor")
  expect_equal(result$formula[2], "1")
  expect_equal(result$scale[2], "1")
  expect_equal(result$shape[2], "1")
  expect_equal(result$transition[2], 0)
})

test_that("defSurv throws an error if transition is not  for the first instance of a varname", {
  
  skip_on_cran()
  
  expect_error(defSurv(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5", transition = 1), 
               "first transition time must be set to ")
})

test_that("defSurv handles multiple transitions correctly", {
  
  skip_on_cran()
  
  dtDefs <- data.table(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5", transition = 0)
  result <- defSurv(dtDefs, varname = "survTime", formula = "2.*x2", scale = "grp*60 + (1-grp)*30", shape = "grp*1.2 + (1-grp)*1.8", transition = 1)
  expect_equal(nrow(result), 2)
  expect_equal(result$varname[2], "survTime")
  expect_equal(result$formula[2], "2.*x2")
  expect_equal(result$scale[2], "grp*60 + (1-grp)*30")
  expect_equal(result$shape[2], "grp*1.2 + (1-grp)*1.8")
  expect_equal(result$transition[2], 1)
})

test_that("defSurv throws an error if transitions are not in ascending order", {
  
  skip_on_cran()
  
  dtDefs <- data.table(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25", shape = "grp*1 + (1-grp)*1.5", transition = 0)
  dtDefs <- defSurv(dtDefs, varname = "survTime", formula = "2.*x2", scale = "grp*60 + (1-grp)*30", shape = "grp*1.2 + (1-grp)*1.8", transition = 1)
  expect_error(defSurv(dtDefs, varname = "survTime", formula = "2.5*x3", scale = "grp*70 + (1-grp)*35", shape = "grp*1.4 + (1-grp)*2.", transition = 0), 
               "transition should be in ascending order")
})

test_that("defSurv works with default parameters", {
  
  skip_on_cran()
  
  result <- defSurv(varname = "survTime", formula = 1)
  expect_equal(nrow(result), 1)
  expect_equal(result$varname, "survTime")
  expect_equal(result$scale, 1)
  expect_equal(result$shape, 1)
  expect_equal(result$transition, 0 )
})

test_that("defSurv kicks out transition error", {
  skip_on_cran()
  expect_error(defSurv(varname = "censor", formula = "-7", shape = 0.55, transition = 150))
})

# defReadAdd -----

test_that("defReadAdd reads a valid CSV file correctly", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines(c(
    "varname,formula,variance,dist,link",
    "x1,.4,,binary,identity",
    "y1,nr + x1 * 2,8,normal,identity",
    "y2,nr - .2 * x1,,poisson,log"
  ), test_csv)
  
  result <- defReadAdd(test_csv)
  
  expect_equal(nrow(result), 3)
  expect_equal(result$varname, c("x1", "y1", "y2"))
  expect_equal(result$formula, c(".4", "nr + x1 * 2", "nr - .2 * x1"))
  expect_equal(result$variance, c(NA, 8, NA))
  expect_equal(result$dist, c("binary", "normal", "poisson"))
  expect_equal(result$link, c("identity", "identity", "log"))
  
  unlink(test_csv)
})

test_that("defReadAdd throws an error if the file does not exist", {
  expect_error(defReadAdd("nonexistent_file.csv"), "No such file")
})

test_that("defReadAdd throws an error if the CSV file has incorrect field names", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines(c(
    "varname,formula,variance,distribution,link",
    "x1,.4,,binary,identity"
  ), test_csv)
  
  expect_error(defReadAdd(test_csv), "Field names do not match")
  
  unlink(test_csv)
})

test_that("defReadAdd throws an error if the CSV file is empty", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines("", test_csv)
  
  expect_error(defReadAdd(test_csv), "no lines available in input")
  
  unlink(test_csv)
})

test_that("defReadAdd handles a CSV file with a single row correctly", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines(c(
    "varname,formula,variance,dist,link",
    "x1,0.4*x2,,binary,identity"
  ), test_csv)
  
  result <- defReadAdd(test_csv)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$varname, "x1")
  expect_equal(result$formula, "0.4*x2")
  expect_equal(result$variance, NA)
  expect_equal(result$dist, "binary")
  expect_equal(result$link, "identity")
  
  unlink(test_csv)
})

# defReadCond -----

test_that("defReadCond reads a valid CSV file correctly", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines(c(
    "condition,formula,variance,dist,link",
    "x == 1, .4,,binary,identity",
    "x == 2, .6,,binary,identity",
    "x >= 3, .8,,binary,identity"
  ), test_csv)
  
  result <- defReadCond(test_csv)
  
  expect_equal(nrow(result), 3)
  expect_equal(result$condition, c("x == 1", "x == 2", "x >= 3"))
  expect_equal(result$formula, c(0.4, 0.6, 0.8))
  expect_equal(result$variance, c(NA, NA, NA))
  expect_equal(result$dist, c("binary", "binary", "binary"))
  expect_equal(result$link, c("identity", "identity", "identity"))
  
  unlink(test_csv)
})

test_that("defReadCond throws an error if the file does not exist", {
  
  skip_on_cran()
  
  expect_error(defReadCond("nonexistent_file.csv"), "No such file")
})

test_that("defReadCond throws an error if the CSV file has incorrect field names", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines(c(
    "condition,formula,variance,distribution,link",
    "x == 1, .4,,binary,identity"
  ), test_csv)
  
  expect_error(defReadCond(test_csv), "field names do not match")
  
  unlink(test_csv)
})

test_that("defReadCond throws an error if the CSV file is empty", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines("", test_csv)
  
  expect_error(defReadCond(test_csv), "no lines available in input")
  
  unlink(test_csv)
})

test_that("defReadCond handles a CSV file with a single row correctly", {
  
  skip_on_cran()
  
  test_csv <- tempfile()
  writeLines(c(
    "condition,formula,variance,dist,link",
    "x == 1, .4,,binary,identity"
  ), test_csv)
  
  result <- defReadCond(test_csv)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$condition, "x == 1")
  expect_equal(result$formula, 0.4)
  expect_equal(result$variance, NA)
  expect_equal(result$dist, "binary")
  expect_equal(result$link, "identity")
  
  unlink(test_csv)
})

rm(list = setdiff(names(.GlobalEnv), freeze_eval), pos = .GlobalEnv)
