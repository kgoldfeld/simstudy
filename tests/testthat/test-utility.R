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

test_that("catProbs returns same result as genCatFormula and throws warning", {
  expect_warning(
    result1 <- catProbs(0.2, 0.3, 0.5, n = 0),
    regexp = "genCatFormula"
  )
  
  result2 <- genCatFormula(0.2, 0.3, 0.5, n = 0)
  
  expect_identical(result1, result2)
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

test_that("logisticCoefs works.", {

  skip_on_cran()

  d1 <- defData(varname = "x1", formula = 0, variance = 1)
  d1 <- defData(d1, varname = "b1", formula = 0.5, dist = "binary")

  coefs <- log(runif(2, min = .8, max = 1.2))

  ### Prevalence

  d1a <- defData(d1, varname = "y",
                 formula = "t(..B) %*% c(1, x1, b1)",
                 dist = "binary", link = "logit"
  )

  tPop <- round(runif(1, .2, .5), 2)
  B <- logisticCoefs(defCovar = d1, coefs = coefs, popPrev = tPop)

  dd <- genData(100000, d1a)
  expect_equal(dd[, mean(y)], tPop, tolerance = .025)

  #### Comparisons

  d1a <- defData(d1, varname = "rx", formula = "1;1", dist = "trtAssign")
  d1a <- defData(d1a, varname = "y",
                 formula = "t(..B) %*% c(1, rx, x1, b1)",
                 dist = "binary", link = "logit"
  )

  ### Risk ratio

  rr <- runif(1, .1, 1/tPop)
  B <- logisticCoefs(d1, coefs, popPrev = tPop, rr = rr, trtName = "rx")

  dd <- genData(100000, d1a)
  expect_equal(dd[rx==0, mean(y)], tPop, tolerance = .025)
  expect_equal(dd[rx==1, mean(y)]/dd[rx==0, mean(y)], rr, tolerance = 0.025)

  ### risk difference

  rd <- runif(1, -tPop, 1 - tPop)
  B <- logisticCoefs(d1, coefs, popPrev = tPop, rd = rd, trtName = "rx")

  dd <- genData(100000, d1a)
  expect_equal(dd[rx==0, mean(y)], tPop, tolerance = .025)
  expect_equal(dd[rx==1, mean(y)] - dd[rx==0, mean(y)], rd, tolerance = 0.025)

  ### AUC

  d1a <- defData(d1, varname = "y",
                 formula = "t(..B) %*% c(1, x1, b1)",
                 dist = "binary", link = "logit"
  )

  auc <- runif(1, 0.6, 0.95)
  B <- logisticCoefs(d1, coefs, popPrev = tPop, auc = auc)

  dx <- genData(500000, d1a)
  expect_equal(dx[, mean(y)], tPop, tolerance = .025)

  form <- paste("y ~", paste(d1[, varname], collapse = " + "))

  fit <- stats::glm(stats::as.formula(form), data = dx)
  dx[, py := stats::predict(fit)]

  Y1 <- dx[y == 1, sample(py, 1000000, replace = TRUE)]
  Y0 <- dx[y == 0, sample(py, 1000000, replace = TRUE)]
  aStat <-  mean(Y1 > Y0)

  expect_equal(aStat, auc, tolerance = 0.025)

})

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

# delColumns -----

test_that("delColumns removes specified columns", {
  skip_on_cran()
  dt <- data.table(x = 1:5, y = 6:10, z = 11:15)
  dt_new <- delColumns(dt, c("x", "y"))
  expect_equal(names(dt_new), "z")
})

test_that("delColumns throws an error if dtOld is missing", {
  skip_on_cran()
  expect_error(delColumns(vars = c("x", "y")), 
               "argument is missing with no default: dtOld")
})

test_that("delColumns throws an error if vars is missing", {
  skip_on_cran()
  dt <- data.table(x = 1:5, y = 6:10, z = 11:15)
  expect_error(delColumns(dtOld = dt), 
    "following argument is missing with no default: vars")
})

### Need to include a check that for a data.table

# test_that("delColumns throws an error if dtOld is not a data.table", {
#   skip_on_cran()
#   df <- data.frame(x = 1:5, y = 6:10, z = 11:15)
#   expect_error(delColumns(df, c("x", "y")), "dtOld must be a data.table")
# })

### Not clear why this test doesn't work
# test_that("delColumns throws an error if vars is not a character vector", {
#.  skip_on_cran()
#   dt <- data.table(x = 1:5, y = 6:10, z = 11:15)
#   expect_error(delColumns(dt, c(1, 2)), 
#     "vars should be/contain only character(s)!")
# })

test_that("delColumns throws an error if vars contains non-existent columns", {
  skip_on_cran()
  dt <- data.table(x = 1:5, y = 6:10, z = 11:15)
  expect_error(delColumns(dt, c("a", "b")), "Variables a and b not previously defined!")
})

test_that("delColumns throws an error if trying to delete the index key", {
  skip_on_cran()
  dt <- data.table(x = 1:5, y = 6:10, z = 11:15)
  setkey(dt, x)
  expect_error(delColumns(dt, "x"), "Cannot delete the index key")
})

# gammaGetShapeRate -----

test_that("gammaGetShapeRate returns correct shape and rate for positive mean and dispersion", {
  skip_on_cran()
  mean <- 5
  dispersion <- 1.5
  result <- gammaGetShapeRate(mean, dispersion)
  expected_shape <- (mean^2) / (dispersion * (mean^2))
  expected_rate <- mean / (dispersion * (mean^2))
  expect_equal(result$shape, expected_shape)
  expect_equal(result$rate, expected_rate)
})

test_that("gammaGetShapeRate handles mean of 1 and dispersion of 1", {
  mean <- 1
  dispersion <- 1
  result <- gammaGetShapeRate(mean, dispersion)
  expected_shape <- (mean^2) / (dispersion * (mean^2))
  expected_rate <- mean / (dispersion * (mean^2))
  expect_equal(result$shape, expected_shape)
  expect_equal(result$rate, expected_rate)
})

test_that("gammaGetShapeRate handles mean of 0", {
  mean <- 0
  dispersion <- 1.5
  result <- gammaGetShapeRate(mean, dispersion)
  expect_equal(result$shape, NaN)
  expect_equal(result$rate, NaN)
})

test_that("gammaGetShapeRate handles dispersion of 0", {
  mean <- 5
  dispersion <- 0
    result <- gammaGetShapeRate(mean, dispersion)
  expect_equal(result$shape, Inf)
  expect_equal(result$rate, Inf)
})

test_that("gammaGetShapeRate handles negative mean", {
  mean <- -5
  dispersion <- 1.5
  result <- gammaGetShapeRate(mean, dispersion)
  expected_shape <- (mean^2) / (dispersion * (mean^2))
  expected_rate <- mean / (dispersion * (mean^2))
  expect_equal(result$shape, expected_shape)
  expect_equal(result$rate, expected_rate)
})

test_that("gammaGetShapeRate handles negative dispersion", {
  mean <- 5
  dispersion <- -1.5
  result <- gammaGetShapeRate(mean, dispersion)
  expected_shape <- (mean^2) / (dispersion * (mean^2))
  expected_rate <- mean / (dispersion * (mean^2))
  expect_equal(result$shape, expected_shape)
  expect_equal(result$rate, expected_rate)
})

# iccRE -----

test_that("iccRE returns correct variances for Poisson distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  lambda <- 30
  result <- iccRE(targetICC, "poisson", lambda = lambda)
  expected <- unlist(lapply(targetICC, function(x) .findPoisVar(x, lambda)))
  expect_equal(result, expected)
})

test_that("iccRE returns correct variances for binary distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  result <- iccRE(targetICC, "binary")
  expected <- unlist(lapply(targetICC, function(x) (x / (1 - x) * (pi^2) / 3)))
  expect_equal(result, expected)
})

test_that("iccRE returns correct variances for normal distribution with varTotal", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  varTotal <- 100
  result <- iccRE(targetICC, "normal", varTotal = varTotal)
  expected <- unlist(lapply(targetICC, function(x) x * varTotal))
  expect_equal(result, expected)
})

test_that("iccRE returns correct variances for normal distribution with varWithin", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  varWithin <- 100
  result <- iccRE(targetICC, "normal", varWithin = varWithin)
  expected <- unlist(lapply(targetICC, function(x) (x / (1 - x)) * varWithin))
  expect_equal(result, expected)
})

test_that("iccRE returns correct variances for gamma distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  disp <- .5
  result <- iccRE(targetICC, "gamma", disp = disp)
  expected <- unlist(lapply(targetICC, function(x) (x / (1 - x)) * trigamma(1 / disp)))
  expect_equal(result, expected)
})

test_that("iccRE returns correct variances for negative binomial distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  lambda <- 40
  disp <- .5
  result <- iccRE(targetICC, "negBinomial", lambda = lambda, disp = disp)
  expected <- unlist(lapply(targetICC, function(x) (x / (1 - x)) * trigamma((1 / lambda + disp)^(-1))))
  expect_equal(result, expected)
})

test_that("iccRE throws an error if lambda is not specified for Poisson distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  expect_error(iccRE(targetICC, "poisson"), "Specify a value for lambda")
})

test_that("iccRE throws an error if varTotal and varWithin are both specified for normal distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  expect_error(iccRE(targetICC, "normal", varTotal = 100, varWithin = 100), "Do not specify total and within variance simultaneously")
})

test_that("iccRE throws an error if neither varTotal nor varWithin is specified for normal distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  expect_error(iccRE(targetICC, "normal"), "Specify either total or within variance")
})

test_that("iccRE throws an error if dispersion is not specified for gamma distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  expect_error(iccRE(targetICC, "gamma"), "Specify dispersion")
})

test_that("iccRE throws an error if lambda or dispersion is not specified for negative binomial distribution", {
  skip_on_cran()
  targetICC <- seq(.05, .20, by = .01)
  expect_error(iccRE(targetICC, "negBinomial", lambda = 40), "Specify dispersion")
  expect_error(iccRE(targetICC, "negBinomial", disp = .5), "Specify lambda")
})

skip_on_cran()
test_that("iccRE throws an error for unsupported distribution", {
  targetICC <- seq(.05, .20, by = .01)
  expect_error(iccRE(targetICC, "unsupported"), "Specify appropriate distribution")
})

# trimData -----

# Define a helper function to create sample data
create_sample_data <- function() {
  skip_on_cran()
  eDef <- defDataAdd(varname = "e", formula = "as.integer(u==4)", dist = "nonrandom")
  
  P <- t(matrix(c(
    0.4, 0.3, 0.2, 0.1,
    0.0, 0.4, 0.3, 0.3,
    0.0, 0.0, 0.5, 0.5,
    0.0, 0.0, 0.0, 1.0
  ),
  nrow = 4
  ))
  
  dp <- genMarkov(
    n = 5, transMat = P,
    chainLen = 8, id = "id",
    pername = "period",
    varname = "u"
  )
  
  dp <- addColumns(eDef, dp)
  return(dp[])
}

# Test that the function correctly trims data after the first event
test_that("trimData correctly trims data after the first event", {
  skip_on_cran()
  dd <- create_sample_data()
  trimmed_dt <- trimData(dd, seqvar = "period", eventvar = "e", idvar = "id")
  trimmed_dt[e == 1]
  
  expected_dt <- dd[e==1, .SD[1], keyby = id]
  
  expect_equal(trimmed_dt[e==1], expected_dt)
})

# Test that the function works when all events occur at the first period
test_that("trimData works when all events occur at the first period", {
  skip_on_cran()
  dd <- data.table(
    id = rep(1:3, each = 5),
    period = rep(1:5, times = 3),
    e = rep(1, 15)
  )
  setkey(dd, "id")
  trimmed_dt <- trimData(dd, seqvar = "period", eventvar = "e", idvar = "id")
  
  expected_dt <- data.table(
    id = rep(1:3, each = 1),
    period = rep(1, 3),
    e = rep(1, 3)
  )
  
  expect_equal(as.data.frame(trimmed_dt), as.data.frame(expected_dt))
})

# viewSpline -----

# Define a helper function to create a simple set of knots and theta
create_test_data <- function() {
  knots <- c(.25, .5, .75)
  theta <- c(.1, .8, .4, .9, .2, 1.)
  list(knots = knots, theta = theta)
}

# Test that the function runs without errors for valid inputs
test_that("viewSplines runs without errors for valid inputs", {
  test_data <- create_test_data()
  expect_error(viewSplines(test_data$knots, degree = 2, test_data$theta), NA)
})


# Test that the function returns a ggplot object
test_that("viewSplines returns a ggplot object", {
  test_data <- create_test_data()
  p <- viewSplines(test_data$knots, degree = 2, test_data$theta)
  expect_s3_class(p, "ggplot")
})

# Test that the plot data is as expected
test_that("viewSplines generates correct plot data", {
  test_data <- create_test_data()
  p <- viewSplines(test_data$knots, degree = 2, test_data$theta)
  
  # Extract the data used in the plot
  plot_data <- ggplot2::ggplot_build(p)$data[[1]]
  
  # Check that the data contains the expected columns
  expect_true(all(c("x", "y", "colour") %in% names(plot_data)))
  
  # Check that the x values are within the expected range
  expect_true(all(plot_data$x >= 0  & plot_data$x <= 1))
  
  # Check that the y values are within the expected range
  expect_true(all(plot_data$y >= 0 & plot_data$y <= 1))
})

