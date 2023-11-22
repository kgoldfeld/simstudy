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
  skip_on_cran()
  expect_error(genCatFormula(), "Need to specify")
  expect_error(genCatFormula(1, 2, 3, n = 5), "or n, not both")
  expect_error(genCatFormula(1.1), "must be less than 1")
  expect_error(genCatFormula(n = 1.1), "must be a whole number")
  expect_error(genCatFormula(n = -3), "Negative values")
})

# betaGetShapes ----
test_that("betaGetShapes throws errors.", {
  skip_on_cran()
  expect_error(betaGetShapes(1, 12), class = "simstudy::valueError")
  expect_error(betaGetShapes(.5, -5), class = "simstudy::valueError")
})

test_that("betaGetShapes works.", {
  skip_on_cran()
  expect_equal(betaGetShapes(.4, 5), list(shape1 = .4 * 5, shape2 = (1 - .4) * 5))
})

# genMixFormula ----
test_that("genMixFormula throws errors.", {
  skip_on_cran()
  expect_error(genMixFormula(), class = "simstudy::missingArgument")
  expect_error(genMixFormula("a", varLength = 3), class = "simstudy::valueError")
  expect_error(genMixFormula("..a", varLength = "b"), class = "simstudy::wrongType")
  expect_error(genMixFormula(3, varLength = "b"), class = "simstudy::wrongType")
  expect_error(genMixFormula(c("a", "b"), probs = "b"), class = "simstudy::wrongType")
  expect_warning(genMixFormula(c("a", "b"), probs = c(.3)), class = "simstudy::valueWarning")
})

test_that("genMixFormula works.", {
  skip_on_cran()
  expect_equal(genMixFormula("a"), "a | 1")
  expect_equal(genMixFormula(c("a", "..b"), c(.3, .7)), "a | 0.3 + ..b | 0.7")
  expect_equal(
    genMixFormula("..a", varLength = 3),
    "..a[[1]] | 0.333333333333333 + ..a[[2]] | 0.333333333333333 + ..a[[3]] | 0.333333333333333"
  )
})

# survGetParams ----

test_that("survGetParams throws errors.", {
  skip_on_cran()
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
  skip_on_cran()
  points <- list(c(50, 0.90), c(100, 0.10))
  expect_equal(survGetParams(points), c(-19.658, 0.225), tolerance = .001)
  points <- list(c(60, 0.90), c(100, .75), c(200, .25), c(250, .10))
  expect_equal(survGetParams(points), c(-11.206, 0.459), tolerance = .001)
})

# plotSurv ----

test_that("survParamPlot throws errors.", {
  skip_on_cran()
  expect_error(survParamPlot(), class = "simstudy::missingArgument")
  expect_error(survParamPlot(formula = -10), class = "simstudy::missingArgument")
  expect_error(survParamPlot(formula = 4, shape = -1), class = "simstudy::wrongSign")
})

test_that("survParamPlot works.", {
  skip_on_cran()
  expect_is(survParamPlot(formula = -4, shape = 1), class = "ggplot")

  points <- list(c(100, .8), c(200, .5))
  r <- survGetParams(points)
  expect_is(survParamPlot(formula = r[1], shape = r[2], points = points),
    class = "ggplot"
  )
  expect_is(survParamPlot(formula = r[1], shape = r[2], points = points, limits=c(0, 220)),
            class = "ggplot"
  )
})

# logisticCoefs

# test_that("logisticCoefs works.", {
#   
#   skip_on_cran()
#   
#   d1 <- defData(varname = "x1", formula = 0, variance = 1)
#   d1 <- defData(d1, varname = "b1", formula = 0.5, dist = "binary")
#   
#   coefs <- log(runif(2, min = .8, max = 1.2))
#   
#   ### Prevalence
#   
#   d1a <- defData(d1, varname = "y",
#                  formula = "t(..B) %*% c(1, x1, b1)",
#                  dist = "binary", link = "logit"
#   )
#   
#   tPop <- round(runif(1, .2, .5), 2)
#   B <- logisticCoefs(defCovar = d1, coefs = coefs, popPrev = tPop)
#   
#   dd <- genData(100000, d1a)
#   expect_equal(dd[, mean(y)], tPop, tolerance = .025)
#   
#   #### Comparisons
#   
#   d1a <- defData(d1, varname = "rx", formula = "1;1", dist = "trtAssign")
#   d1a <- defData(d1a, varname = "y",
#                  formula = "t(..B) %*% c(1, rx, x1, b1)",
#                  dist = "binary", link = "logit"
#   )
#   
#   ### Risk ratio
#   
#   rr <- runif(1, .1, 1/tPop)
#   B <- logisticCoefs(d1, coefs, popPrev = tPop, rr = rr, trtName = "rx")
#   
#   dd <- genData(100000, d1a)
#   expect_equal(dd[rx==0, mean(y)], tPop, tolerance = .025)
#   expect_equal(dd[rx==1, mean(y)]/dd[rx==0, mean(y)], rr, tolerance = 0.025)
#   
#   ### risk difference
#   
#   rd <- runif(1, -tPop, 1 - tPop)
#   B <- logisticCoefs(d1, coefs, popPrev = tPop, rd = rd, trtName = "rx")
#   
#   dd <- genData(100000, d1a)
#   expect_equal(dd[rx==0, mean(y)], tPop, tolerance = .025)
#   expect_equal(dd[rx==1, mean(y)] - dd[rx==0, mean(y)], rd, tolerance = 0.025)
#   
#   ### AUC 
#   
#   d1a <- defData(d1, varname = "y",
#                  formula = "t(..B) %*% c(1, x1, b1)",
#                  dist = "binary", link = "logit"
#   )
#   
#   auc <- runif(1, 0.6, 0.95)
#   B <- logisticCoefs(d1, coefs, popPrev = tPop, auc = auc)
#   
#   dx <- genData(500000, d1a)
#   expect_equal(dx[, mean(y)], tPop, tolerance = .025)
# 
#   form <- paste("y ~", paste(d1[, varname], collapse = " + "))
#   
#   fit <- stats::glm(stats::as.formula(form), data = dx)
#   dx[, py := stats::predict(fit)]
#   
#   Y1 <- dx[y == 1, sample(py, 1000000, replace = TRUE)]
#   Y0 <- dx[y == 0, sample(py, 1000000, replace = TRUE)]
#   aStat <-  mean(Y1 > Y0) 
#   
#   expect_equal(aStat, auc, tolerance = 0.025)
#  
# })

test_that("logisticCoefs throws errors.", {
  
  skip_on_cran()
  
  d1 <- defData(varname = "x1", formula = 0, variance = 1)
  d1 <- defData(d1, varname = "b1", formula = 0.5, dist = "binary")
  
  coefs <- log(runif(2, min = .8, max = 1.2))
  coefs2 <- log(runif(1, min = .8, max = 1.2))
  coefs3 <- c("a", "b")
  
  expect_error(logisticCoefs(d1, coefs), class = "simstudy::missingArgument")
  expect_error(logisticCoefs(coef = coefs, popPrev = .5), class = "simstudy::missingArgument")
  expect_error(logisticCoefs(defCovar = d1, popPrev = .5), class = "simstudy::missingArgument")
  expect_error(logisticCoefs(d1, coefs, popPrev = .5, rr = 1.1, rd = .4), class = "simpleError")
  expect_error(logisticCoefs(d1, coefs=coefs2, popPrev = .5), class = "simstudy::lengthMismatch" )
  expect_error(logisticCoefs(d1, coefs=coefs, popPrev = .5, rr = -1), class = "simstudy::minError" )
  expect_error(logisticCoefs(d1, coefs=coefs, popPrev = .5, rr = 2.1), class = "simpleError" )
  expect_error(logisticCoefs(d1, coefs=coefs, popPrev = .5, rd = .6), class = "simstudy::valueError" )
  expect_error(logisticCoefs(d1, coefs=coefs, popPrev = .5, rd = -.7), class = "simstudy::valueError" )
  expect_error(logisticCoefs(d1, coefs=coefs, popPrev = .5, auc = .4), class = "simstudy::valueError" )
  expect_error(logisticCoefs(d1, coefs=coefs, popPrev = 1.2), class = "simstudy::valueError" )
  expect_error(logisticCoefs(d1, coefs=coefs3, popPrev = .4), class = "simstudy::wrongType" )
  
})
