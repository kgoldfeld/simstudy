# genCatFormula ----
roundTrip <- function(args) {
  as.numeric(.splitFormula(do.call(genCatFormula, as.list(args))))
}

test_that("probabilities stay the same after genCatFormula", {
  skip_on_cran()
  forall(gen_cat_probs, function(ps) {
    rt <- roundTrip(ps)
    expect_equal(sum(ps), 1)
    expect_equal(length(ps), length(rt))
    expect_equal(ps, rt)
  })
})

test_that("cat probs are generated correctly", {
  skip_on_cran()
  forall(gen.element(2:15), function(n) {
    rt <- roundTrip(list(n = n))
    expect_equal(length(rt), n)
    expect_equal(sum(rt), 1)
  })

  forall(gen.unif(0, 1), function(p) {
    expect_equal(sum(roundTrip(p)), sum(p, 1 - p))
    expect_length(roundTrip(p), 2)
  })
})

test_that("probabilities (vector) are adjusted as documented.", {
  skip_on_cran()
  forall(gen.and_then(gen.element(2:15), function(n) {
    gen_n_norm_Probs(n)
  }), function(p) {
    over <- p / .9
    under <- p / 1.1
    expect_warning(genCatFormula(over), "will be normalized")
    expect_warning(genCatFormula(under), "Adding category")
    expect_equal(sum(roundTrip(over)), 1)
    expect_equal(sum(roundTrip(under)), 1)
    expect_length(roundTrip(over), length(over))
    expect_length(roundTrip(under), length(under) + 1)
  })
})

test_that("genCatFormula throws errors.", {
  expect_error(genCatFormula(), "Need to specify")
  expect_error(genCatFormula(1, 2, 3, n = 5), "or n, not both")
  expect_error(genCatFormula(1.1), "must be less than 1")
  expect_error(genCatFormula(n = 1.1), "must be a whole number")
  expect_error(genCatFormula(n = -3), "Negative values")
})

# betaGetShapes ----
test_that("betaGetShapes throws errors.", {
  expect_error(betaGetShapes(1, 12), class = "simstudy::valueError")
  expect_error(betaGetShapes(.5, -5), class = "simstudy::valueError")
})

test_that("betaGetShapes works.", {
  expect_equal(betaGetShapes(.4, 5), list(shape1 = .4 * 5, shape2 = (1 - .4) * 5))
})

# genMixFormula ----
test_that("genMixFormula throws errors.", {
  expect_error(genMixFormula(), class = "simstudy::missingArgument")
  expect_error(genMixFormula("a", varLength = 3), class = "simstudy::valueError")
  expect_error(genMixFormula("..a", varLength = "b"), class = "simstudy::wrongType")
  expect_error(genMixFormula(3, varLength = "b"), class = "simstudy::wrongType")
  expect_error(genMixFormula(c("a", "b"), probs = "b"), class = "simstudy::wrongType")
  expect_warning(genMixFormula(c("a", "b"), probs = c(.3)), class = "simstudy::valueWarning")
})

test_that("genMixFormula works.", {
  expect_equal(genMixFormula("a"), "a | 1")
  expect_equal(genMixFormula(c("a", "..b"), c(.3, .7)), "a | 0.3 + ..b | 0.7")
  expect_equal(
    genMixFormula("..a", varLength = 3),
    "..a[[1]] | 0.333333333333333 + ..a[[2]] | 0.333333333333333 + ..a[[3]] | 0.333333333333333"
  )
})

# survGetParams ----

test_that("survGetParams throws errors.", {
  expect_error(survGetParams(), class = "simstudy::missingArgument")
  expect_error(survGetParams(c(100, .5)), class = "simstudy::wrongClass")
  points <- list(c(280, 0.85), c(165, .45))
  expect_error(survGetParams(points), class = "simstudy::wrongOrder")
  points <- list(c(80, 0.45), c(165, .55))
  expect_error(survGetParams(points), class = "simstudy::wrongOrder")
  points <- list(c(-280, 0.85), c(165, .45))
  expect_error(survGetParams(points), class = "simstudy::wrongSign")
  points <- list(c(28, 1.85), c(365, .45))
  expect_error(survGetParams(points), class = "simstudy::probError")
})

test_that("survGetParam works.", {
  points <- list(c(50, 0.90), c(100, 0.10))
  expect_equal(survGetParams(points), c(-19.658, 0.225), tolerance = .001)
  points <- list(c(60, 0.90), c(100, .75), c(200, .25), c(250, .10))
  expect_equal(survGetParams(points), c(-11.206, 0.459), tolerance = .001)
})

# plotSurv ----

test_that("survParamPlot throws errors.", {
  expect_error(survParamPlot(), class = "simstudy::missingArgument")
  expect_error(survParamPlot(formula = -10), class = "simstudy::missingArgument")
  expect_error(survParamPlot(formula = 4, shape = -1), class = "simstudy::wrongSign")
})

test_that("survParamPlot works.", {
  expect_is(survParamPlot(formula = -4, shape = 1), class = "ggplot")

  points <- list(c(100, .8), c(200, .5))
  r <- survGetParams(points)
  expect_is(survParamPlot(formula = r[1], shape = r[2], points = points),
    class = "ggplot"
  )
})
