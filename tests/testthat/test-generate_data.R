# genData ----
test_that("data is generated as expected", {
  n <- 20

  null_def <- defData(varname = "test", formula = .3, dist = "nonrandom", id = NULL)
  def <- defData(varname = "test", formula = .3, dist = "nonrandom", id = "some_id")
  def <- defData(def, varname = "test2", formula = .7, dist = "nonrandom")
  def2 <- defData(def, varname = "cat", formula = "test2;.4", dist = "categorical")
  def3 <- defData(def, varname = "cat", formula = "test2;.2", dist = "categorical")
  def <- defData(def, varname = "cat", formula = "test;test2", dist = "categorical")

  expect_silent(genData(n, def))
  expect_silent(genData(n, null_def))

  expect_warning(genData(n, def, "not-id"), class = "simstudy::valueWarning")
  expect_equal(
    {
      data <- suppressWarnings(genData(n, def, "not-id"))
      names(data)[1]
    },
    "not-id"
  )
  expect_silent(genData(n, def, "some_id"))

  expect_warning(genData(n, def2), "will be normalized")
  expect_warning(genData(n, def3), "Adding category")
  # TODO expand test with hedgehog
})

test_that("vectorized variables work in formulas", {
  d <- defData(varname = "a", formula = 0.6, dist = "binary")
  d <- defData(d, varname = "b", formula = 0.4, dist = "binary")
  d <- defData(d, varname = "c", formula = 0.3, dist = "binary")
  d <- defData(d, varname = "theta", formula = "t(..tau) %*% c(a, b, c)", dist = "nonrandom")
  tau <- rnorm(3, 0, 1)

  expect_silent(genData(10, d))
})

# genOrdCat ----
test_that("genOrdCat throws errors.", {
  oldSeed <- .Random.seed
  set.seed(87920)

  expect_error(genOrdCat("not a data table", NULL, c(.1, .1)), class = "simstudy::wrongClass")
  expect_error(genOrdCat(adjVar = NULL, rho = 1), class = "simstudy::missingArgument")
  expect_error(genOrdCat(NULL, NULL, NULL), class = "simstudy::noValue")
  expect_warning(genOrdCat(genData(1), baseprobs = c(0.5, 0.5), corstr = "notValid"), class = "simstudy::invalidOption")

  d1 <- defData(varname = "rx", formula = "1;1", dist = "trtAssign")
  d1 <- defData(d1, varname = "male", formula = .4, dist = "binary")
  d1 <- defData(d1, varname = "z", formula = "0 - 1.2*rx - 1*male", dist = "nonrandom")

  dd <- genData(5, d1)

  baseprobs <- c(0.4, 0.3, 0.2, 0.1)
  npAdj <- matrix(c(
    0, 1, 0, 0,
    1, 0, 1, 0
  ), nrow = T, byrow = T)

  expect_error(genOrdCat(dtName = dd, baseprobs = baseprobs, npVar = "rx"), class = "simstudy::mismatch")
  expect_error(genOrdCat(dtName = dd, baseprobs = baseprobs, npAdj = npAdj), class = "simstudy::mismatch")
  expect_error(genOrdCat(dd, "z", baseprobs, npVar = "rx", npAdj = npAdj))
  expect_error(genOrdCat(dd, "z", baseprobs, npVar = c("rx"), npAdj = c(0, 1, 1)))

  n <- 100000

  d1 <- defData(varname = "rx", formula = "1;1", dist = "trtAssign")
  d1 <- defData(d1, varname = "male", formula = .4, dist = "binary")
  d1 <- defData(d1, varname = "z", formula = "0 - 1.2*rx - 1*male", dist = "nonrandom")

  dd <- genData(n, d1)
  baseprobs <- c(.4, .3, .2, .1)

  expect_error({
    dn <- genOrdCat(
      dtName = dd, adjVar = "z",
      baseprobs = baseprobs,
      npVar = "rx", npAdj = c(0, 2, 0, 0)
    )
  })

  expect_error({
    dn <- genOrdCat(
      dtName = dd, adjVar = "z",
      baseprobs = baseprobs,
      npVar = c("rx", "male"), npAdj = c(0, 1, 0, 0)
    )
  })

  expect_error({
    dn <- genOrdCat(
      dtName = dd, adjVar = "z",
      baseprobs = baseprobs,
      npVar = c("rx"),
      npAdj = matrix(c(
        0, .2, 0, 0,
        0, 0, -.2, 0
      ), nrow = 2, byrow = T)
    )
  })

  set.seed(oldSeed)
})

library(magrittr)
library(dplyr)
test_that("ordinal categorical data is generated correctly.", {
  oldSeed <- .Random.seed
  set.seed(230920)
  n <- 10000
  probs_short <- c(.2, .4, .2)
  probs <- c(.2, .2, .6)
  data <- genData(n)
  # TODO more test variable parameter combinations
  expect_equal(
    {
      data %>%
        genOrdCat(baseprobs = probs, asFactor = FALSE) %>%
        select(cat) %>%
        range()
    },
    c(1, 3)
  )

  expect_equal(
    {
      data %>%
        genOrdCat(baseprobs = probs, asFactor = FALSE) %>%
        select(cat) %>%
        table() %>%
        as.numeric() / n
    },
    probs,
    tolerance = 0.01
  )

  expect_equal(
    {
      set.seed(123)
      genOrdCat(genData(1), baseprobs = c(0.5, 0.5), corstr = "ind")
    },
    {
      set.seed(123)
      suppressWarnings(genOrdCat(genData(1), baseprobs = c(0.5, 0.5), corstr = "notValid"),
        classes = "simstudy::optionInvalid"
      )
    }
  )
  set.seed(oldSeed)
  expect_silent(genOrdCat(dtName = data, adjVar = "id", baseprobs = rbind(probs, probs), asFactor = FALSE))
})


test_that("non-proportional ordinal categorical data are generated correctly.", {
  oldSeed <- .Random.seed
  set.seed(87920)
  n <- 100000

  d1 <- defData(varname = "rx", formula = "1;1", dist = "trtAssign")
  d1 <- defData(d1, varname = "male", formula = .4, dist = "binary")
  d1 <- defData(d1, varname = "z", formula = "0 - 1.2*rx - 1*male", dist = "nonrandom")

  dd <- genData(n, d1)
  baseprobs <- c(.4, .3, .2, .1)

  # Assumes proportional odds

  expect_lte(
    {
      dn <- genOrdCat(
        dtName = dd, adjVar = "z",
        baseprobs = baseprobs
      )

      dc <- dn[, .(.N), keyby = .(rx, cat)]
      dc[, cprop := cumsum(N) / sum(N), keyby = .(rx)]
      dc[, codds := cprop / (1 - cprop)]

      dcc <- dcast(dc[codds != Inf], cat ~ rx, value.var = "codds")
      dcc[, cOR := `1` / `0`]

      dcc[, abs(max(cOR) - min(cOR))]
    },
    0.5
  )

  # Assumes non-proportional data generation

  expect_gt(
    {
      dn <- genOrdCat(
        dtName = dd, adjVar = "z",
        baseprobs = baseprobs,
        npVar = "rx", npAdj = c(0, 1, 0, 0)
      )

      dc <- dn[, .(.N), keyby = .(rx, cat)]
      dc[, cprop := cumsum(N) / sum(N), keyby = .(rx)]
      dc[, codds := cprop / (1 - cprop)]

      dcc <- dcast(dc[codds != Inf], cat ~ rx, value.var = "codds")
      dcc[, cOR := `1` / `0`]

      dcc[, abs(max(cOR) - min(cOR))]
    },
    1.5,
  )

  set.seed(oldSeed)
})



test_that("deprecation warning shows up.", {
  expect_warning(genCorOrdCat(genData(5), baseprobs = c(.2, .3, .5), rho = 0, corstr = "cs"), "deprecated")
})

test_that("correlated ordinal categorical data is generated correctly.", {
  library(pracma)
  oldSeed <- .Random.seed
  set.seed(230920)
  probs <- matrix(0.25, 5, 4)
  rownames(probs) <- letters[1:5]
  n <- 10000

  dT <- genData(n)
  dX <- genOrdCat(dT, baseprobs = probs, prefix = "q", rho = 0.2, corstr = "cs", asFactor = FALSE)
  cdX <- cor(dX[, -1])
  truMat <- genCorMat(nvars = 5, rep(0.2, 10))
  distSum <- sum(diag(distmat(cdX, truMat)))
  expect_lte(distSum, 1)

  set.seed(oldSeed)
})

# genFactor ----
test_that("genFactor throws erros", {
  expect_error(genFactor(),
    regexp = "dtName and varname", class = "simstudy::missingArgument"
  )
  expect_error(genFactor(NULL, NA),
    regexp = "dtName and varname", class = "simstudy::noValue"
  )
  expect_error(genFactor(data.frame(a = 3), "a"),
    regexp = "dtName", class = "simstudy::wrongClass"
  )
  expect_error(genFactor(data.table(a = 3), 5),
    regexp = "varname", class = "simstudy::wrongType"
  )
  expect_error(genFactor(data.table(a = 3), c("a", "a")),
    regexp = "varname", class = "simstudy::uniqueValue"
  )
  expect_error(genFactor(data.table(a = 3), "b"),
    regexp = "b", class = "simstudy::notDefined"
  )
  expect_error(genFactor(data.table(a = "c"), "a"),
    regexp = "columns2Convert", class = "simstudy::wrongType"
  )
  expect_error(genFactor(data.table(a = 5, fa = 5), "a"),
    regexp = "fa", class = "simstudy::alreadyDefined"
  )
})

test_that("genFactor works.", {
  dt <- data.table(
    id = 1:100, q1 = sample(1:5, 100, replace = TRUE),
    q2 = sample(1:5, 100, replace = TRUE),
    q3 = sample(1:5, 100, replace = TRUE)
  )
  labels <- list(q1 = letters[1:5], q2 = letters[6:10])
  dt_res <- copy(dt)
  dt_res$fq1 <- factor(dt$q1, labels = labels$q1)
  dt_res$fq2 <- factor(dt$q2, labels = labels$q2)

  expect_true(is.factor(genFactor(copy(dt), "q2")$fq2))
  expect_equal(genFactor(copy(dt), "q2")$fq2, factor(dt$q2))
  expect_length(genFactor(copy(dt), c("q1", "q2")), 6)
  expect_length(genFactor(copy(dt), c("q1", "q2"), replace = TRUE), 4)
  expect_equal(genFactor(copy(dt), c("q1", "q2"), labels = labels), dt_res)
})

# genDummy ----
test_that("genDummy throws errors.", {
  oldSeed <- .Random.seed
  set.seed(10076)

  d1 <- defData(varname = "rx", formula = "1;1", dist = "trtAssign")
  d1 <- defData(d1, varname = "male", formula = .4, dist = "binary")
  d1 <- defData(d1, varname = "z", formula = "0 - 1.2*rx - 1*male", dist = "nonrandom")

  dd <- genData(5, d1)

  # Initial data checks
  expect_error(genDummy(varname = "rx", sep = ".", replace = FALSE), class = "simstudy::missingArgument")
  expect_error(genDummy(dd, sep = ".", replace = FALSE), class = "simstudy::missingArgument")

  # Check if data table exists
  expect_error(genDummy(d, varname = "rx", sep = ".", replace = FALSE), class = "simstudy::dtNotExist")
  # expect_error(genDummy(d, varname = "rx", sep = ".", replace = FALSE), class = "simstudy::dtDoesNotExist")

  # Check if varname exists
  expect_error(genDummy(dd, varname = "xx", sep = ".", replace = FALSE), class = "simstudy::notDefined")

  # Check if field is integer or factor

  d2 <- defData(varname = "tx", formula = 5, dist = "normal", variance = 2)

  dd2 <- genData(6, d2)

  expect_error(genDummy(dd2, varname = "tx", sep = ".", replace = FALSE), class = "simstudy::notIntegerOrFactor")

  d1 <- defData(d1, varname = "rx.1", formula = 5, dist = "nonrandom")

  dd3 <- genData(5, d1)

  expect_error(genDummy(dd3, varname = "rx", sep = ".", replace = FALSE), class = "simstudy::alreadyDefined")

  set.seed(oldSeed)
})

test_that("genDummy works.", {
  d4 <- defData(varname = "a", formula = ".2;.3;.5", dist = "trtAssign")
  dd4 <- genData(10, d4)

  expect_true(is.data.frame(genDummy(dd4, varname = "a")))

  dd4dum <- genDummy(dd4, varname = "a")

  expect_equal(ncol(dd4dum), 5)
  for (i in seq_along(dd4dum)) {
    if (dd4dum$a[i] == 1) {
      expect_equal(dd4dum$a.1[i], 1)
      expect_equal(dd4dum$a.2[i], 0)
      expect_equal(dd4dum$a.3[i], 0)
    }

    if (dd4dum$a[i] == 2) {
      expect_equal(dd4dum$a.1[i], 0)
      expect_equal(dd4dum$a.2[i], 1)
      expect_equal(dd4dum$a.3[i], 0)
    }

    if (dd4dum$a[i] == 3) {
      expect_equal(dd4dum$a.1[i], 0)
      expect_equal(dd4dum$a.2[i], 0)
      expect_equal(dd4dum$a.3[i], 1)
    }
  }

  dd5dum <- genDummy(dd4, varname = "a", replace = TRUE)

  expect_equal(ncol(dd5dum), 4)
  for (i in seq_along(dd5dum)) {
    if (dd4dum$a[i] == 1) {
      expect_equal(dd5dum$a.1[i], 1)
      expect_equal(dd5dum$a.2[i], 0)
      expect_equal(dd5dum$a.3[i], 0)
    }

    if (dd4dum$a[i] == 2) {
      expect_equal(dd5dum$a.1[i], 0)
      expect_equal(dd5dum$a.2[i], 1)
      expect_equal(dd5dum$a.3[i], 0)
    }

    if (dd4dum$a[i] == 3) {
      expect_equal(dd5dum$a.1[i], 0)
      expect_equal(dd5dum$a.2[i], 0)
      expect_equal(dd5dum$a.3[i], 1)
    }
  }
})

# genFormula ----
test_that("genFormula throws errors.", {
  oldSeed <- .Random.seed
  set.seed(24761)

  # Check coefficients and variables properly specified
  expect_error(genFormula(c(1, 2), c("a", "b", "c", "d")), class = "simstudy::coeffVar")

  # Check non-numeric coefficients are properly specified
  expect_error(genFormula(c(1, ".b", 3), c("xx", "yy", "zz")), class = "simstudy::doubleDot")

  # Check vars are type character
  expect_error(genFormula(c(1, 2, 3), c(12, 23, 34)), class = "simstudy::wrongType")

  set.seed(oldSeed)
})

test_that("genFormula works.", {
  oldSeed <- .Random.seed
  set.seed(23456)

  # intercept
  expect_equal(genFormula(c(42, 54, 32, 2), c("A", "B", "C")), "42 + 54 * A + 32 * B + 2 * C")

  # no intercept
  expect_equal(genFormula(c(2.1, 3.1, 4.1), c("a", "b", "c")), "2.1 * a + 3.1 * b + 4.1 * c")

  # intercept, double dot
  expect_equal(genFormula(c(42, "..y", "..z", 2), c("A", "B", "C")), "42 + ..y * A + ..z * B + 2 * C")

  # no intercept, double dot
  expect_equal(genFormula(c("..x", "..y", 4.1), c("a", "b", "..z")), "..x * a + ..y * b + 4.1 * ..z")

  set.seed(oldSeed)
})

# genMarkov ----
test_that("genMarkov throws errors.", {
  oldSeed <- .Random.seed
  set.seed(24761)

  # check transMat is matrix
  mat1 <- c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9)
  expect_error(genMarkov(n = 10, transMat = mat1, chainLen = 5, wide = TRUE), class = "simstudy::typeMatrix")

  # check transMat is square matrix
  mat2 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9, 0.3, 0.4, 0.3), nrow = 4, ncol = 3))
  expect_error(genMarkov(n = 10, transMat = mat2, chainLen = 5, wide = TRUE), class = "simstudy::squareMatrix")

  # check transMat row sums = 1
  mat3 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.8), nrow = 3, ncol = 3))
  expect_error(genMarkov(n = 10, transMat = mat3, chainLen = 5, wide = TRUE), class = "simstudy::rowSums1")

  # check chainLen is > 1
  mat4 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(genMarkov(n = 10, transMat = mat4, chainLen = 0, wide = TRUE), class = "simstudy::chainLen")

  # if startProb defined, check it sums to 1
  mat5 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(genMarkov(n = 10, transMat = mat5, chainLen = 5, wide = TRUE, startProb = ".3;.3;.1"), class = "simstudy::notEqual")


  # if startProb defined, check it has length == number of matrix rows
  mat6 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(genMarkov(n = 10, transMat = mat6, chainLen = 5, wide = TRUE, startProb = ".7;.3"), class = "simstudy::lengthMismatch")

  set.seed(oldSeed)
})

test_that("genMarkov works.", {
  oldSeed <- .Random.seed
  set.seed(23456)


  # not startProb
  ## pk
  mat_pow <- function(x, k) {
    x.k <- x
    for (i in 2:k) {
      x.k <- x.k %*% x
    }
    x.k
  }

  matr <- t(matrix(c(
    0.5, 0.5, 0.0, 0.0,
    0.15, 0.5, 0.35, 0.0,
    0.0, 0.35, 0.5, 0.15,
    0.0, 0.0, 0.5, 0.5
  ), nrow = 4, ncol = 4))

  theoretical_p <- mat_pow(matr, 100)[1, ]

  nind <- 10
  nchain <- 5000

  gm1 <- genMarkov(n = nind, transMat = matr, chainLen = nchain)

  prop <- gm1[, .N, keyby = .(id, state)]
  prop[, p := N / sum(N), keyby = id]
  prop[, t.p := rep(theoretical_p, max(id))]
  prop[, dif := abs(p - t.p)]

  expect_true(all(prop[, dif < 0.02]))

  ## correct number of events gen
  expect_equal(nchain, gm1[, max(period)])

  ## number of categories == dimensions of transistion matrix
  expect_true((length(table(gm1$state)) == dim(matr)[1]) & (gm1[, max(state)] == dim(matr)[1]))

  # startProb
  ## pk
  set.seed(23456)
  gm2 <- genMarkov(n = nind, transMat = matr, chainLen = nchain, startProb = "0.65;0.25;0.05;0.05")

  prop <- gm1[, .N, keyby = .(id, state)]
  prop[, p := N / sum(N), keyby = id]
  prop[, t.p := rep(theoretical_p, max(id))]
  prop[, dif := abs(p - t.p)]

  expect_true(all(prop[, dif < 0.02]))

  ## correct number of events gen
  expect_equal(nchain, gm2[, max(period)])

  ## number of categories == dimensions of transistion matrix
  expect_true((length(table(gm2$state)) == dim(matr)[1]) & (gm2[, max(state)] == dim(matr)[1]))


  # not wide == wide
  set.seed(23456)
  gm1_w <- genMarkov(n = nind, transMat = matr, chainLen = nchain, wide = TRUE)

  set.seed(23456)
  gm2_w <- genMarkov(n = nind, transMat = matr, chainLen = nchain, wide = TRUE, startProb = "0.65;0.25;0.05;0.05")

  check_equal <- function(gm_not_wide, gm_wide) {
    rand_id <- sample(nind, 1)
    rand_state <- sample(nchain, 1)
    gmnw <- gm_not_wide[id == rand_id & period == rand_state, state]
    gmw <- gm_wide[rand_id, rand_state + 1, with = FALSE]

    expect_equal(as.numeric(gmnw), as.numeric(gmw))
  }

  for (i in 1:5) {
    check_equal(gm1, gm1_w)
    check_equal(gm2, gm2_w)
  }

  set.seed(oldSeed)
})

# genMultiFac ----
test_that("genMultiFac throws errors.", {
  oldSeed <- .Random.seed
  set.seed(98765)

  # check nFactors are integers
  expect_error(genMultiFac(1.4, each = 4), class = "simstudy::wrongType")

  # check length nFactors greater than 2
  expect_error(genMultiFac(1, each = 4), class = "simstudy::greaterThan")

  # check number of levels matches factors
  expect_error(genMultiFac(3, levels = c(2, 3)), class = "simstudy::lengthMismatch")

  # check coding == 'effect' or 'dummy'
  expect_error(genMultiFac(2, each = 3, coding = "trtAssign"), class = "simstudy::codingVal")

  set.seed(oldSeed)
})

test_that("genMultiFac works.", {
  oldSeed <- .Random.seed

  # coding == dummy, levels == 2
  nFac <- sample(2:5, size = 1)
  nEach <- sample(2:5, size = 1)
  g1 <- genMultiFac(nFac, each = nEach)

  # checks each column sums to correct amount
  for (i in sample(2:(nFac + 1))) {
    expect_equal(sum(g1[, i, with = FALSE]), nrow(g1) / 2)
  }

  # checks all values are 0s or 1s
  expect_true(all(g1[, 2:(nFac + 1)] == 0 | g1[, 2:(nFac + 1)] == 1))

  # checks all rows are unique
  rowStrings <- unlist(lapply(split(g1[, -1], seq(nrow(g1))), function(x) paste0(x, collapse = "")))
  expect_true(length(rowStrings) == (length(unique(rowStrings)) * nEach))

  # checks there is right number of rows
  expect_true(length(rowStrings) == nEach * 2^nFac)


  ## coding == effect, levels == 2
  nFac <- sample(2:5, size = 1)
  nEach <- sample(2:5, size = 1)
  g2 <- genMultiFac(nFac, each = nEach, coding = "effect")

  # checks all columns sum to 0
  for (i in sample(2:(nFac + 1))) {
    expect_equal(sum(g2[, i, with = FALSE]), 0)
  }

  # checks all values are 1s or -1s
  expect_true(all(g2[, 2:(nFac + 1)] == 1 | g2[, 2:(nFac + 1)] == -1))

  # checks all rows are unique
  rowStrings <- unlist(lapply(split(g2[, -1], seq(nrow(g2))), function(x) paste0(x, collapse = "")))
  expect_true(length(rowStrings) == (length(unique(rowStrings)) * nEach))

  # checks there is right number of rows
  expect_true(length(rowStrings) == nEach * 2^nFac)


  ## levels == other, len(levels) == 1
  nFac <- sample(2:5, size = 1)
  nEach <- sample(2:5, size = 1)
  nLev <- sample(2:5, size = 1)
  g3 <- genMultiFac(nFac, each = nEach, levels = nLev)

  # checks all rows are unique
  rowStrings <- unlist(lapply(split(g3[, -1], seq(nrow(g3))), function(x) paste0(x, collapse = "")))
  expect_true(length(rowStrings) == (length(unique(rowStrings)) * nEach))

  # checks there is right number of rows
  expect_true(length(rowStrings) == nEach * nLev^nFac)

  # checks all values are in correct range
  expect_true(all(g3[, 2:(nFac + 1), with = FALSE] <= nLev))
  expect_true(all(g3[, 2:(nFac + 1), with = FALSE] > 0))


  ## levels == other, len(levels) != 1
  nFac <- sample(2:5, size = 1)
  nEach <- sample(2:5, size = 1)
  nLev <- NULL
  for (i in 1:nFac) {
    nLev <- c(nLev, sample(2:5, size = 1))
  }
  g4 <- genMultiFac(nFac, each = nEach, levels = nLev)

  # checks all rows are unique
  rowStrings <- unlist(lapply(split(g4[, -1], seq(nrow(g4))), function(x) paste0(x, collapse = "")))
  expect_true(length(rowStrings) == (length(unique(rowStrings)) * nEach))

  # checks there is right number of rows
  expect_true(length(rowStrings) == nEach * prod(nLev))

  # checks all values are in correct range
  for (i in 2:(nFac + 1)) {
    expect_true(all(g4[, i, with = FALSE] <= nLev[i - 1]))
    expect_true(all(g4[, i, with = FALSE] > 0))
  }

  ## check colNames works
  nFac <- sample(2:5, size = 1)
  nEach <- sample(2:5, size = 1)
  colNames <- NULL
  for (i in 1:nFac) {
    colNames <- c(colNames, paste0("test", i))
  }
  g5 <- genMultiFac(nFac, nEach, colNames = colNames)

  columnNames <- colnames(g5[, 2:(nFac + 1)])
  expect_equal(colNames, columnNames)


  set.seed(oldSeed)
})


# genSyntheticc ----
test_that("genSynthetic throws errors.", {
  mu_a <- rnorm(1)
  v_a <- rgamma(1, 9)
  mu_b <- rgamma(1, 4)
  mu_c <- rbeta(1, 2, 2)

  n <- rpois(1, 3500)

  d <- defData(varname = "a", formula = "..mu_a", variance = "..v_a", dist = "normal")
  d <- defData(d, varname = "b", formula = "..mu_b", dist = "poisson")
  d <- defData(d, varname = "c", formula = "..mu_c", dist = "binary")

  A <- genData(n, d)
  B <- c(2, 3, 4)

  ### Errors

  expect_error(genSynthetic(id = "cid"), class = "simstudy::missingArgument")
  expect_error(genSynthetic(B), class = "simstudy::wrongClass")
  expect_error(genSynthetic(A, vars = c(1, 2), id = "id"), class = "simstudy::wrongClass")
  expect_error(genSynthetic(A, n = "4"), class = "simstudy::wrongType")
  expect_error(genSynthetic(A, vars = c("z")), class = "simstudy::notDefined")
  expect_error(genSynthetic(A, id = "cid"), class = "simstudy::notDefined")
  expect_error(genSynthetic(A, vars = c("a", "b", "id")), class = "simstudy::alreadyInVector")
})

test_that("genSynthetic works.", {
  mu_a <- rnorm(1)
  v_a <- rgamma(1, 9)
  mu_b <- rgamma(1, 4)
  mu_c <- rbeta(1, 2, 2)

  n <- rpois(1, 3500)

  d <- defData(varname = "a", formula = "..mu_a", variance = "..v_a", dist = "normal")
  d <- defData(d, varname = "b", formula = "..mu_b", dist = "poisson")
  d <- defData(d, varname = "c", formula = "..mu_c", dist = "binary")

  A <- genData(n, d)
  B <- c(2, 3, 4)

  S <- genSynthetic(A)
  expect_true(all(names(S) == names(A)))
  expect_true(nrow(S) == n)

  expect_lt(abs(S[, mean(a)] - mu_a), 0.4)
  expect_lt(abs(S[, mean(b)] - mu_b), 0.25)
  expect_lt(abs(S[, mean(c)] - mu_c), 0.05)

  n <- rpois(1, 100)
  S <- genSynthetic(A, n = n, vars = c("a", "b"))
  expect_true(all(names(S) == c("id", "a", "b")))
  expect_true(nrow(S) == n)
})

