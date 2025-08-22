# .parseDotVars ----
test_that("dotVars are parsed correctly.", {
  skip_on_cran()
  extVar1 <- 23
  extVar2 <- 42
  res <- list(..extVar1 = 23, ..extVar2 = 42)

  expect_equal(.parseDotVars("a + ..extVar1 | b + ..extVar2"), res)
  expect_equal(.parseDotVars(c("a + ..extVar1", "b + ..extVar2")), res)
  expect_equal(length(.parseDotVars("a + b")), 0)

  expect_error(.parseDotVars("..extVar12"))
})

test_that("variables from different environments are parsed correctly.", {
  skip_on_cran()
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
  skip_on_cran()
  df <- data.frame()
  ext <- list(formula2parse = 2)

  expect_error(.evalWith("", ext), "reserved variable")
  expect_error(.evalWith("", list(), df, 10), "different length")
})

test_that("evalWith output length is correct.", {
  skip_on_cran()
  df <- data.frame(a = rep.int(5, 5))
  ext <- list(..ev = 2)

  expect_equal(length(.evalWith("a + ..ev", ext, df, 5)), 5)
  expect_equal(length(.evalWith("a + ..ev", ext, dtSim = df)), 5)
})

test_that("evalWith output is Matrix.", {
  skip_on_cran()
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
  skip_on_cran()
  expect_length(.getDists(), 17)
})

# .isFormulaScalar ----
test_that("isFormularScalar works correctly.", {
  skip_on_cran()
  expect_true(.isFormulaScalar("5 + 3"))
  expect_true(.isFormulaScalar(5 + 3))

  expect_false(.isFormulaScalar("a + 3"))
  expect_false(.isFormulaScalar("a;3"))
  expect_false(.isFormulaScalar(data.frame(a = "asd")))
})

# .isValidVarName ----
test_that("var names are validated correctly.", {
  skip_on_cran()
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
  skip_on_cran()
  err <- try(nonVar + 4, silent = TRUE)
  noErr <- try(3 + 5, silent = TRUE)

  expect_true(.isError(err))
  expect_false(.isError(noErr))
  expect_false(.isError(5))
  expect_false(.isError("ab"))
})

# .hasValue ----
test_that("hasValue works.", {
  skip_on_cran()
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
  skip_on_cran()
  prob <- 0.2
  logOdds <- log(0.25)

  expect_equal(.log2Prob(logOdds), prob)
  expect_equal(.log2Prob(rep(logOdds, 5)), rep(prob, 5))
})

# .buildCorMat ----

test_that(".buildCorMat handles dimension mismatch and asymmetric matrices", {
  skip_on_cran()
  
  # Test 1: Dimension mismatch - nvars doesn't match correlation matrix size
  # Create a 3x3 correlation matrix but specify nvars = 4
  corMatrix_3x3 <- matrix(c(1, 0.5, 0.3,
                            0.5, 1, 0.4,
                            0.3, 0.4, 1), nrow = 3)
  
  expect_error(
    simstudy:::.buildCorMat(nvars = 4, corMatrix = corMatrix_3x3, corstr = "cs", rho = 0.5),
    "Length of mean vector mismatched with correlation matrix"
  )
  
  # Test another dimension mismatch case
  corMatrix_2x2 <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
  
  expect_error(
    simstudy:::.buildCorMat(nvars = 3, corMatrix = corMatrix_2x2, corstr = "ar1", rho = 0.7),
    "Length of mean vector mismatched with correlation matrix"
  )
  
  # Test 2: Non-symmetric correlation matrix
  # Create an asymmetric matrix
  asymmetric_matrix <- matrix(c(1, 0.5, 0.3,
                                0.6, 1, 0.4,  # Note: 0.6 != 0.5
                                0.3, 0.4, 1), nrow = 3)
  
  expect_error(
    simstudy:::.buildCorMat(nvars = 3, corMatrix = asymmetric_matrix, corstr = "cs", rho = 0.5),
    "Correlation matrix not symmetric"
  )
  
  # Test another asymmetric case
  asymmetric_matrix2 <- matrix(c(1, 0.2, 0.7, 1), nrow = 2)  # 0.7 in [1,2] but 0.2 in [2,1]
  
  expect_error(
    simstudy:::.buildCorMat(nvars = 2, corMatrix = asymmetric_matrix2, corstr = "ar1", rho = 0.3),
    "Correlation matrix not symmetric"
  )
})

test_that(".buildCorMat works correctly with valid inputs", {
  skip_on_cran()
  
  # Test that valid correlation matrices work correctly
  valid_corMatrix <- matrix(c(1, 0.5, 0.3,
                              0.5, 1, 0.4,
                              0.3, 0.4, 1), nrow = 3)
  
  expect_silent(result1 <- simstudy:::.buildCorMat(
    nvars = 3, 
    corMatrix = valid_corMatrix, 
    corstr = "cs", 
    rho = 0.5
  ))
  expect_identical(result1, valid_corMatrix)
  
  # Test with NULL correlation matrix - should generate based on corstr
  expect_silent(result2 <- simstudy:::.buildCorMat(
    nvars = 3, 
    corMatrix = NULL, 
    corstr = "cs", 
    rho = 0.6
  ))
  expect_true(is.matrix(result2))
  expect_equal(dim(result2), c(3, 3))
  
  # Test ar1 structure generation
  expect_silent(result3 <- simstudy:::.buildCorMat(
    nvars = 4, 
    corMatrix = NULL, 
    corstr = "ar1", 
    rho = 0.7
  ))
  expect_true(is.matrix(result3))
  expect_equal(dim(result3), c(4, 4))
})
