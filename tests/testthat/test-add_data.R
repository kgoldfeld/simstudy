# addCondition ----
test_that("addCondition throws errors.", {
  skip_on_cran()
  expect_error(addCondition(), class = "simstudy::missingArgument")
  expect_error(addCondition("a"), class = "simstudy::missingArgument")
  expect_error(addCondition(data.frame(), data.frame(), "a"), class = "simstudy::wrongClass")
})

test_that("addCondition works.", {
  skip_on_cran()
  def <- defData(varname = "x", formula = "1;10", dist = "uniformInt")
  defC <- defCondition(condition = "x >= 5", formula = "x + 5", dist = "nonrandom")
  defC <- defCondition(defC, condition = "x < 5", formula = "10", dist = "nonrandom")
  defC2 <- defCondition(condition = "x >= 5", formula = "x + 6", dist = "nonrandom")
  defC2 <- defCondition(defC2, condition = "x < 5", formula = "11", dist = "nonrandom")
  dt <- genData(1000, def)
  defs <- list(defC = defC, defC2 = defC2)

  expect_equal(range(addCondition(defC, dt, "x2")$x2), c(10, 15))
  expect_equal(range(addCondition(defs[["defC2"]], dt, "x2")$x2), c(11, 16))
})

# addColumns ----
test_that("addColumns throws errors.", {
  skip_on_cran()
  expect_error(addColumns(), class = "simstudy::missingArgument")
  expect_error(addColumns("a"), class = "simstudy::missingArgument")
  expect_error(addColumns(data.frame(), data.frame()), class = "simstudy::wrongClass")
})

test_that("addColumns works.", {
  skip_on_cran()
  def <- defData(varname = "x", formula = "1;10", dist = "uniformInt")
  dt <- genData(100, def)
  def2 <- defDataAdd(varname = "y", formula = "2.3 * (1/x)", dist = "normal")

  expect_silent(addColumns(def2, dt))
})

test_that("defRepeatAdd works", {
  skip_on_cran()
  expect_silent(
    defRepeatAdd(nVars = 4, prefix = "g", formula = "1/3;1/3;1/3", variance = 0, dist = "categorical")
  )

  def <- defDataAdd(varname = "a", formula = "1;1", dist = "trtAssign")
  expect_silent(
    defRepeatAdd(def, 8, "b", formula = "5 + a", variance = 3, dist = "normal")
  )

  expect_silent(defRepeatAdd(nVars = 4, prefix = "b", formula = "5 + a", variance = 3, dist = "normal"))
})

test_that("defRepeatAdd throws errors correctly.", {
  skip_on_cran()
  expect_error(defRepeatAdd(prefix = "b", formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeatAdd(nVars = 8, formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeatAdd(nVars = 8, prefix = "b", variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
})

# addMarkov ----
test_that("addMarkov throws errors.", {
  skip_on_cran()
  d0 <- defData(varname = "xx", formula = 2)
  d0 <- defData(d0, varname = "xy", formula = 5)
  dd <- genData(n = 10, dt = d0)

  # check transMat is matrix
  mat1 <- c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9)
  expect_error(addMarkov(dd, transMat = mat1, chainLen = 5, wide = TRUE), class = "simstudy::typeMatrix")

  # check transMat is square matrix
  mat2 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9, 0.3, 0.4, 0.3), nrow = 4, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat2, chainLen = 5, wide = TRUE), class = "simstudy::squareMatrix")

  # check transMat row sums = 1
  mat3 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.8), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat3, chainLen = 5, wide = TRUE), class = "simstudy::rowSums1")

  # check chainLen is > 1
  mat4 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat4, chainLen = 0, wide = TRUE), class = "simstudy::chainLen")

  # if start0lab defined, check that it is defined in dd
  mat5 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat5, chainLen = 5, wide = TRUE, start0lab = "yy"), class = "simstudy::notDefined")


  # if start0lab defined, check that it exists in the transition matrix
  mat6 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat6, chainLen = 5, wide = TRUE, start0lab = "xy"), class = "simstudy::start0probNotInTransMat")

})

# addSynthetic ----

test_that("addSynthetic throws errors.", {
  skip_on_cran()

  ### Create fake "real" data set

  d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
  d <- defData(d, varname = "b", formula = 5, dist = "poisson")
  d <- defData(d, varname = "c", formula = 0.3, dist = "binary")
  d <- defData(d, varname = "d", formula = "a + b + 3*c", variance = 2, dist = "normal")

  A <- genData(1000, d, id = "index")

  def <- defData(varname = "x", formula = 0, variance = 5)

  S <- genData(120, def)

  expect_error(addSynthetic(dtFrom = A), class = "simstudy::missingArgument")

  x <- c(1, 2, 3)
  expect_error(addSynthetic(dtOld = x, dtFrom = A), class = "simstudy::wrongClass")
  expect_error(addSynthetic(dtOld = S, dtFrom = x), class = "simstudy::wrongClass")
  expect_error(addSynthetic(dtOld = S, dtFrom = A, id = "index"), class = "simstudy::notDefined")
  expect_error(addSynthetic(dtOld = S, dtFrom = A, id = "id"), class = "simstudy::notDefined")

  d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
  d <- defData(d, varname = "x", formula = 5, dist = "poisson")

  A <- genData(1000, d)
  S <- genData(120, def)
  expect_error(addSynthetic(dtOld = S, dtFrom = A), class = "simstudy::alreadyDefined")
})

test_that("addSynthetic works.", {
  skip_on_cran()

  ### Create fake 'external' data set 'A'

  d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
  d <- defData(d, varname = "b", formula = 5, dist = "poisson")
  d <- defData(d, varname = "c", formula = 0.3, dist = "binary")
  d <- defData(d, varname = "d", formula = "a + b + 3*c", variance = 2, dist = "normal")

  A <- genData(1000, d)

  ### Create synthetic data set from "observed" data set A
  ### and add it to other data set S:

  def <- defData(varname = "x", formula = 0, variance = 5)

  n <- rpois(1, 100)
  vars <- c("d", "b")

  S <- genData(n, def)
  Snew <- addSynthetic(dtOld = S, dtFrom = A, vars = vars)

  expect_true(all(c(names(S), vars) == names(Snew)))
  expect_equal(nrow(Snew), nrow(S))

  mu_a <- rnorm(1, 25, 4)
  n <- rpois(1, 3500)

  d <- defData(varname = "a", formula = "..mu_a", variance = 1, dist = "normal")
  A <- genData(n, d)

  S <- genData(n, def)
  Snew <- addSynthetic(S, A)

  expect_lt(Snew[, abs(mean(a) - mu_a)], 0.15)

})
