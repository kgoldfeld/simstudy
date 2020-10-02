# .parseDotVars ----
test_that("dotVars are parsed correctly.", {
  extVar1 <- 23
  extVar2 <- 42
  res <- list(..extVar1 = 23, ..extVar2 = 42)

  expect_equal(.parseDotVars("a + ..extVar1 | b + ..extVar2"), res)
  expect_equal(.parseDotVars(c("a + ..extVar1", "b + ..extVar2")), res)
  expect_equal(length(.parseDotVars("a + b")), 0)

  expect_error(.parseDotVars("..extVar12"))
})

test_that("variables from different environments are parsed correctly.", {
  extVar3 <- 7
  env1 <- new.env()
  env2 <- new.env(parent = env1)
  env1$extVar1 <- 23
  env2$extVar2 <- 42
  res <- list(..extVar1 = 23, ..extVar2 = 42, ..extVar3 = 7)

  with(env2, {
    expect_equal(.parseDotVars("a + ..extVar1 | b + ..extVar2 * ..extVar3"), res)
    expect_equal(.parseDotVars(c("a + ..extVar1 * ..extVar2", "b + ..extVar3")), res)
  })
})

# .evalWith ----
test_that("evalWith throws errors.", {
  df <- data.frame()
  ext <- list(formula2parse = 2)

  expect_error(.evalWith("", ext), "reserved variable")
  expect_error(.evalWith("", list(), df, 10), "different length")
})

test_that("evalWith output length is correct.", {
  df <- data.frame(a = rep.int(5, 5))
  ext <- list(..ev = 2)

  expect_equal(length(.evalWith("a + ..ev", ext, df, 5)), 5)
  expect_equal(length(.evalWith("a + ..ev", ext, dtSim = df)), 5)
})

test_that("evalWith output is Matrix.", {
  df <- data.frame(a = rep.int(5, 5))
  ext <- list(..ev = 2)

  expect_is(.evalWith("a + ..ev", ext, df, 5), "matrix")
  expect_is(.evalWith("a + ..ev", ext, df), "matrix")
  expect_is(.evalWith(c("a + ..ev", "..ev * 2"), ext, df), "matrix")
  expect_is(.evalWith("..ev * 2", ext), "matrix")
})

# .adjustProbs ----
test_that("probabilities (matrix) are adjusted as documented.", {
  skip_on_cran()
  forall(gen.and_then(gen.c(gen.element(2:6), of = 2), function(n) { 
    gen.with(gen.list(gen_n_norm_Probs(n[2]), of = n[1]), function(ps) {
      do.call("rbind", ps)
    })
  }), function(p) {
    over <- p / .9
    under <- p / 1.1
    expect_warning(.adjustProbs(over), class = "simstudy::valueWarning")
    expect_warning(.adjustProbs(under), class = "simstudy::valueWarning")
    expect_error(.adjustProbs(under * -1), class = "simstudy::valueError")
    expect_equal(mean(rowSums(.adjustProbs(under))), 1)
    expect_equal(mean(rowSums(.adjustProbs(over))), 1)
    expect_equal(dim(.adjustProbs(over)), dim(over))
    expect_equal(dim(.adjustProbs(under)), dim(under) + c(0, 1))
  })
})

# .getDists ----
test_that("number of Dists is up to date.", {
  expect_length(.getDists(), 14)
})

# .isFormulaScalar ----
test_that("isFormularScalar works correctly.", {
  expect_true(.isFormulaScalar("5 + 3"))
  expect_true(.isFormulaScalar(5 + 3))

  expect_false(.isFormulaScalar("a + 3"))
  expect_false(.isFormulaScalar("a;3"))
  expect_false(.isFormulaScalar(data.frame(a = "asd")))
})

# .isValidVarName ----
test_that("var names are validated correctly.", {
  validNames <- c("var1", "name", "name2", "var1")
  wrongNames <- c("...", "..1", "..5")

  expect_true(all(.isValidVarName(validNames)))
  expect_true(all(.isValidVarName(validNames[1:3], unique = TRUE)))
  expect_true(all(.isValidVarName(wrongNames, allowReserved = TRUE, unique = TRUE)))

  expect_false(all(.isValidVarName(wrongNames)))
  expect_false(all(.isValidVarName(c(validNames, wrongNames))))
  expect_false(all(.isValidVarName(validNames, unique = TRUE)))
})

# .isError ----
test_that("errors are detected correctly.", {
  err <- try(nonVar + 4, silent = TRUE)
  noErr <- try(3 + 5, silent = TRUE)

  expect_true(.isError(err))
  expect_false(.isError(noErr))
  expect_false(.isError(5))
  expect_false(.isError("ab"))
})

# .hasValue ----
test_that("hasValue works.", {
  expect_true(.hasValue("value"))
  expect_true((function(x) .hasValue(x))(5))
  expect_true((function(x) .hasValue(x))(NA))
  expect_false(.hasValue())
  expect_false((function(x) .hasValue(x))())
  expect_false((function(x) .hasValue(x))(NULL))
  expect_false(.hasValue(NULL))
})

# .log2Prob ----
test_that("log odds are converted correctly.", {
  prob <- 0.2
  logOdds <- log(0.25)

  expect_equal(.log2Prob(logOdds), prob)
  expect_equal(.log2Prob(rep(logOdds, 5)), rep(prob, 5))
})
