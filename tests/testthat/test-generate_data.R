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
  expect_error(genOrdCat2("not a data table", NULL, c(.1, .1)), class = "simstudy::wrongClass")
  expect_error(genOrdCat2(adjVar = NULL, rho = 1), class = "simstudy::missingArgument")
  expect_error(genOrdCat2(NULL, NULL, NULL), class = "simstudy::noValue")
})

library(magrittr)
library(dplyr)
test_that("ordinal categorical data is generated correctly.", {
  n <- 10000
  probs_short <- c(.2, .4, .2)
  probs <- c(.2, .2, .6)
  data <- genData(n)

  expect_equal(
    {
      data %>%
        genOrdCat2(baseprobs = probs, asFactor = FALSE) %>%
        select(cat) %>%
        range()
    },
    c(1, 3)
  )

  expect_equal({
      data %>%
        genOrdCat2(baseprobs = probs, asFactor = FALSE) %>%
        select(cat) %>%
        table() %>%
        as.numeric() / n
    },
    probs, tolerance = 0.01
  )

})
