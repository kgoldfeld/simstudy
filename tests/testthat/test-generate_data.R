# genData ----
test_that("data is generated as expected", {
  n <- 20
  def <- defData(varname = "test", formula = .3, dist = "nonrandom")
  def <- defData(def, varname = "test2", formula = .7, dist = "nonrandom")
  def2 <- defData(def, varname = "cat", formula = "test2;.4", dist = "categorical")
  def3 <- defData(def, varname = "cat", formula = "test2;.2", dist = "categorical")
  def <- defData(def, varname = "cat", formula = "test;test2", dist = "categorical")

  expect_silent(genData(n, def))
  expect_warning(genData(n, def2), "will be normalized")
  expect_warning(genData(n, def3), "Adding category")
})

# genOrdCat ----
test_that("genOrdCat throws errors.", {
  expect_error(genOrdCat("not a data table", NULL, c(.1, .1)), class = "simstudy::wrongClass")
  expect_error(genOrdCat(adjVar = NULL, rho = 1), class = "simstudy::missingArgument")
  expect_error(genOrdCat(NULL, NULL, NULL), class = "simstudy::noValue")
  expect_warning(genOrdCat(genData(1), baseprobs = c(0.5, 0.5), corstr = "notValid"), class = "simstudy::invalidOption")
  
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

  expect_equal({
    set.seed(123)
    genOrdCat(genData(1), baseprobs = c(0.5, 0.5), corstr = "ind")
  }, {
    set.seed(123)
    suppressWarnings(genOrdCat(genData(1), baseprobs = c(0.5, 0.5), corstr = "notValid"),
    classes = "simstudy::optionInvalid")
  })
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