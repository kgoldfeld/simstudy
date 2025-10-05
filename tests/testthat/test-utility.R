library(testthat)
library(simstudy)
library(data.table)

# genCatFormula ----
roundTrip <- function(args) {
  as.numeric(simstudy:::.splitFormula(do.call(genCatFormula, as.list(args))))
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
  expected <- unlist(lapply(targetICC, function(x) simstudy:::.findPoisVar(x, lambda)))
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
  skip_on_cran()
  
  test_data <- create_test_data()
  expect_error(viewSplines(test_data$knots, degree = 2, test_data$theta), NA)
})


# Test that the function returns a ggplot object
test_that("viewSplines returns a ggplot object", {
  skip_on_cran()
  
  test_data <- create_test_data()
  p <- viewSplines(test_data$knots, degree = 2, test_data$theta)
  expect_s3_class(p, "ggplot")
})

# Test that the plot data is as expected
test_that("viewSplines generates correct plot data", {
  skip_on_cran()
  
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

# mergeData -----

test_that("mergeData basic functionality works", {
  
  skip_on_cran()
  
  # Setup test data
  dt1 <- data.table(
    id = 1:5,
    x = letters[1:5],
    value1 = 10:14
  )
  
  dt2 <- data.table(
    id = c(1, 3, 5, 6),
    y = LETTERS[1:4],
    value2 = 20:23
  )
  
  # Test inner join (na.rm = TRUE, default)
  result_inner <- mergeData(dt1, dt2, "id", na.rm = TRUE)
  
  expect_equal(nrow(result_inner), 3)  # Only matching rows
  expect_equal(result_inner$id, c(1, 3, 5))
  expect_true(all(c("x", "y", "value1", "value2") %in% names(result_inner)))
  expect_false(any(is.na(result_inner$value1)))
  expect_false(any(is.na(result_inner$value2)))
})

test_that("mergeData outer join works", {
  
  skip_on_cran()
  
  # Setup test data
  dt1 <- data.table(
    id = 1:3,
    x = letters[1:3]
  )
  
  dt2 <- data.table(
    id = c(2, 3, 4),
    y = LETTERS[1:3]
  )
  
  # Test outer join (na.rm = FALSE)
  result_outer <- mergeData(dt1, dt2, "id", na.rm = FALSE)
  
  expect_equal(nrow(result_outer), 4)  # All unique ids: 1,2,3,4
  expect_equal(sort(result_outer$id), 1:4)
  expect_true(is.na(result_outer[id == 1, y]))  # id=1 only in dt1
  expect_true(is.na(result_outer[id == 4, x]))  # id=4 only in dt2
})

test_that("mergeData preserves original keys", {
  # Setup test data with keys
  dt1 <- data.table(
    id = 1:3,
    category = c("A", "B", "A"),
    value1 = 10:12
  )
  setkey(dt1, category, id)
  original_key <- key(dt1)
  
  dt2 <- data.table(
    category = c("A", "B", "C"),
    value2 = 20:22
  )
  
  result <- mergeData(dt1, dt2, "category")
  
  # Check that original key is preserved
  expect_equal(key(result), original_key)
  
  # Check column order - key columns should come first
  expected_order <- c("category", "id", "value1", "value2")
  expect_equal(names(result), expected_order)
})

test_that("mergeData doesn't modify input data.tables", {
  
  skip_on_cran()
  # Setup test data
  dt1 <- data.table(
    id = 1:3,
    x = letters[1:3]
  )
  setkey(dt1, x)
  
  dt2 <- data.table(
    id = 2:4,
    y = LETTERS[1:3]
  )
  setkey(dt2, y)
  
  # Store original states
  dt1_original <- copy(dt1)
  dt2_original <- copy(dt2)
  dt1_key_original <- key(dt1)
  dt2_key_original <- key(dt2)
  
  # Perform merge
  result <- mergeData(dt1, dt2, "id")
  
  # Check that originals are unchanged
  expect_equal(dt1, dt1_original)
  expect_equal(dt2, dt2_original)
  expect_equal(key(dt1), dt1_key_original)
  expect_equal(key(dt2), dt2_key_original)
})

# negbinomGetSizeProb -----

test_that("negbinomGetSizeProb basic functionality works", {
  mean <- 5
  dispersion <- 0.5
  
  result <- negbinomGetSizeProb(mean, dispersion)
  
  # Check return type and structure
  expect_type(result, "list")
  expect_named(result, c("size", "prob"))
  expect_type(result$size, "double")
  expect_type(result$prob, "double")
  expect_length(result$size, 1)
  expect_length(result$prob, 1)
})

test_that("negbinomGetSizeProb mathematical relationships are correct", {
  mean <- 10
  dispersion <- 0.3
  
  result <- negbinomGetSizeProb(mean, dispersion)
  
  # Expected variance based on simstudy parameterization
  expected_variance <- mean + (mean^2) * dispersion
  
  # Check size calculation: size = mean^2 / (variance - mean)
  expected_size <- (mean^2) / (expected_variance - mean)
  expect_equal(result$size, expected_size, tolerance = 1e-10)
  
  # Check prob calculation: prob = mean / variance
  expected_prob <- mean / expected_variance
  expect_equal(result$prob, expected_prob, tolerance = 1e-10)
  
  # Verify negative binomial parameterization relationships
  # For neg binom with size and prob: mean = size * (1-prob) / prob
  nb_mean <- result$size * (1 - result$prob) / result$prob
  expect_equal(nb_mean, mean, tolerance = 1e-10)
  
  # For neg binom with size and prob: variance = size * (1-prob) / prob^2
  nb_variance <- result$size * (1 - result$prob) / (result$prob^2)
  expect_equal(nb_variance, expected_variance, tolerance = 1e-10)
})

# updateDef -----

test_that("updateDef basic parameter updates work", {
  skip_on_cran()
  
  # Create test definition table
  defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
  defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
  
  # Test updating formula
  updated <- updateDef(dtDefs = defs, changevar = "y", newformula = "x + 5")
  expect_equal(updated[varname == "y", formula], "x + 5")
  expect_equal(updated[varname == "x", formula], "0")  # Unchanged
  
  # Test updating variance
  updated <- updateDef(dtDefs = defs, changevar = "x", newvariance = 5)
  expect_equal(updated[varname == "x", variance], "5")
  
  # Test updating distribution
  updated <- updateDef(dtDefs = defs, changevar = "y", newdist = "poisson")
  expect_equal(updated[varname == "y", dist], "poisson")
  
  # Test updating link
  updated <- updateDef(dtDefs = defs, changevar = "y", newlink = "log")
  expect_equal(updated[varname == "y", link], "log")
})

test_that("updateDef multiple parameter updates work", {
  skip_on_cran()
  
  # Create test definition table
  defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
  defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
  
  # Test updating multiple parameters at once
  updated <- updateDef(dtDefs = defs, changevar = "y", 
                       newformula = "x + 10", newvariance = 2, 
                       newdist = "poisson", newlink = "log")
  
  expect_equal(updated[varname == "y", formula], "x + 10")
  expect_equal(updated[varname == "y", variance], "2")
  expect_equal(updated[varname == "y", dist], "poisson")
  expect_equal(updated[varname == "y", link], "log")
})

test_that("updateDef remove functionality works", {
  skip_on_cran()
  
  # Create test definition table with 3 variables
  defs <- defData(varname = "a", formula = 0, variance = 1, dist = "normal")
  defs <- defData(defs, varname = "w", formula = 0, variance = 3, dist = "normal")
  defs <- defData(defs, varname = "x", formula = "1 + w", variance = 1, dist = "normal")
  defs <- defData(defs, varname = "z", formula = 4, variance = 1, dist = "normal")
  
  original_nrow <- nrow(defs)
  
  # Test removing first variable
  updated <- updateDef(dtDefs = defs, changevar = "a", remove = TRUE)
  
  expect_equal(nrow(updated), original_nrow - 1)
  expect_false("a" %in% updated$varname)
  expect_true(all(c("w", "x", "z") %in% updated$varname))
  
  # Test removing middle variable
  updated <- updateDef(dtDefs = defs, changevar = "x", remove = TRUE)
  
  expect_equal(nrow(updated), original_nrow - 1)
  expect_false("x" %in% updated$varname)
  expect_true(all(c("w", "z") %in% updated$varname))
  
  # Test removing last variable
  updated <- updateDef(dtDefs = defs, changevar = "z", remove = TRUE)
  expect_equal(nrow(updated), original_nrow - 1)
  expect_false("z" %in% updated$varname)
  expect_true(all(c("w", "x") %in% updated$varname))
})

test_that("updateDef doesn't modify original table", {
  skip_on_cran()
  
  # Create test definition table
  defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
  defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
  
  # Store original state
  original_defs <- copy(defs)
  
  # Update definition
  updated <- updateDef(dtDefs = defs, changevar = "y", newformula = "x + 5")
  
  # Check that original is unchanged
  expect_equal(defs, original_defs)
  expect_false(updated[varname == "y", formula] == defs[varname == "y", formula])
})

test_that("updateDef error handling works", {
  skip_on_cran()
  
  # Create test definition table
  defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
  defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
  
  # Test error when variable doesn't exist
  expect_error(
    updateDef(dtDefs = defs, changevar = "nonexistent", newformula = "0"),
    "Variable nonexistent not in definition table"
  )
  
  # Test error when definition table doesn't exist
  expect_error(
    updateDef(dtDefs = nonexistent_table, changevar = "x", newformula = "0"),
    "Data definition does not exist"
  )
})

test_that("updateDef handles single variable table", {
  skip_on_cran()
  
  # Create single variable definition
  defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
  
  # Test updating the single variable
  updated <- updateDef(dtDefs = defs, changevar = "x", newformula = 5, newvariance = 2)
  
  expect_equal(nrow(updated), 1)
  expect_equal(updated[varname == "x", formula], "5")
  expect_equal(updated[varname == "x", variance], "2")
  
  # Test removing the single variable (should result in empty table)
  updated <- updateDef(dtDefs = defs, changevar = "x", remove = TRUE)
  expect_equal(nrow(updated), 0)
})

test_that("updateDef handles updating first variable", {
  skip_on_cran()
  
  # Create test definition table
  defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
  defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
  
  # Update the first variable (rowvar == 1, triggers prevVars <- "")
  updated <- updateDef(dtDefs = defs, changevar = "x", newformula = 5, newvariance = 2)
  
  expect_equal(updated[varname == "x", formula], "5")
  expect_equal(updated[varname == "x", variance], "2")
  expect_equal(updated[varname == "y", formula], "2 + 3*x")  # Unchanged
})

# updateDefAdd -----

test_that("updateDefAdd basic parameter updates work", {
  skip_on_cran()
  
  # Create test definition table
  defsA <- defDataAdd(varname = "a", formula = "w + x + z", variance = 2, dist = "normal")
  defsA <- defDataAdd(defsA, varname = "b", formula = "5", variance = 1, dist = "poisson")
  
  # Test updating formula
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", newformula = "w + z")
  expect_equal(updated[varname == "a", formula], "w + z")
  expect_equal(updated[varname == "b", formula], "5")  # Unchanged
  
  # Test updating variance
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", newvariance = 3)
  expect_equal(updated[varname == "a", variance], 3)
  
  # Test updating distribution
  updated <- updateDefAdd(dtDefs = defsA, changevar = "b", newdist = "normal")
  expect_equal(updated[varname == "b", dist], "normal")
  
  # Test updating link
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", newlink = "log")
  expect_equal(updated[varname == "a", link], "log")
})

test_that("updateDefAdd multiple parameter updates work", {
  skip_on_cran()
  
  # Create test definition table
  defsA <- defDataAdd(varname = "a", formula = "w + x", variance = 2, dist = "normal")
  
  # Test updating multiple parameters at once
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", 
                          newformula = "w + x + z", newvariance = 1, 
                          newdist = "poisson", newlink = "log")
  
  expect_equal(updated[varname == "a", formula], "w + x + z")
  expect_equal(updated[varname == "a", variance], 1)
  expect_equal(updated[varname == "a", dist], "poisson")
  expect_equal(updated[varname == "a", link], "log")
})

test_that("updateDefAdd remove functionality works", {
  skip_on_cran()
  
  # Create test definition table with multiple variables
  defsA <- defDataAdd(varname = "a", formula = "w + x", variance = 2, dist = "normal")
  defsA <- defDataAdd(defsA, varname = "b", formula = "5", variance = 1, dist = "poisson")
  defsA <- defDataAdd(defsA, varname = "c", formula = "a + b", variance = 1, dist = "normal")
  
  original_nrow <- nrow(defsA)
  
  # Test removing middle variable
  updated <- updateDefAdd(dtDefs = defsA, changevar = "b", remove = TRUE)
  
  expect_equal(nrow(updated), original_nrow - 1)
  expect_false("b" %in% updated$varname)
  expect_true(all(c("a", "c") %in% updated$varname))
  
  # Test removing last variable
  updated <- updateDefAdd(dtDefs = defsA, changevar = "c", remove = TRUE)
  expect_equal(nrow(updated), original_nrow - 1)
  expect_false("c" %in% updated$varname)
  expect_true(all(c("a", "b") %in% updated$varname))
})

test_that("updateDefAdd doesn't modify original table", {
  skip_on_cran()
  
  # Create test definition table
  defsA <- defDataAdd(varname = "a", formula = "w + x", variance = 2, dist = "normal")
  
  # Store original state
  original_defsA <- copy(defsA)
  
  # Update definition
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", newformula = "w + x + z")
  
  # Check that original is unchanged
  expect_equal(defsA, original_defsA)
  expect_false(updated[varname == "a", formula] == defsA[varname == "a", formula])
})

test_that("updateDefAdd error handling works", {
  skip_on_cran()
  
  # Create test definition table
  defsA <- defDataAdd(varname = "a", formula = "w + x", variance = 2, dist = "normal")
  
  # Test error when variable doesn't exist
  expect_error(
    updateDefAdd(dtDefs = defsA, changevar = "nonexistent", newformula = "0"),
    "Variable nonexistent not in definition table"
  )
  
  # Test error when definition table doesn't exist
  expect_error(
    updateDefAdd(dtDefs = nonexistent_table, changevar = "a", newformula = "0"),
    "Data definition does not exist"
  )
})

test_that("updateDefAdd handles single variable table", {
  skip_on_cran()
  
  # Create single variable definition
  defsA <- defDataAdd(varname = "a", formula = "w + x", variance = 2, dist = "normal")
  
  # Test updating the single variable
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", newformula = "w", newvariance = 1)
  
  expect_equal(nrow(updated), 1)
  expect_equal(updated[varname == "a", formula], "w")
  expect_equal(updated[varname == "a", variance], 1)
  
  # Test removing the single variable (should result in empty table)
  updated <- updateDefAdd(dtDefs = defsA, changevar = "a", remove = TRUE)
  expect_equal(nrow(updated), 0)
})

# viewBasis -----

test_that("viewBasis returns ggplot object", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  knots <- c(0.25, 0.50, 0.75)
  
  # Test with degree 1
  plot1 <- viewBasis(knots, degree = 1)
  expect_s3_class(plot1, "ggplot")
  
  # Test with degree 2
  plot2 <- viewBasis(knots, degree = 2)
  expect_s3_class(plot2, "ggplot")
  
  # Test with degree 3
  plot3 <- viewBasis(knots, degree = 3)
  expect_s3_class(plot3, "ggplot")
})

test_that("viewBasis handles different knot configurations", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  # Test with single knot
  plot1 <- viewBasis(knots = 0.5, degree = 1)
  expect_s3_class(plot1, "ggplot")
  
  # Test with multiple knots
  plot2 <- viewBasis(knots = c(0.2, 0.4, 0.6, 0.8), degree = 2)
  expect_s3_class(plot2, "ggplot")
  
  # Test with knots at boundaries
  plot3 <- viewBasis(knots = c(0.1, 0.9), degree = 1)
  expect_s3_class(plot3, "ggplot")
})

test_that("viewBasis plot has expected structure", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  knots <- c(0.25, 0.75)
  degree <- 2
  plot <- viewBasis(knots, degree)
  
  # Check that plot has data
  expect_true(nrow(plot$data) > 0)
  
  # Check expected number of basis functions
  # Should be length(knots) + degree + 1 = 2 + 2 + 1 = 5
  expected_basis_count <- length(knots) + degree + 1
  actual_basis_count <- length(unique(plot$data$basis))
  expect_equal(actual_basis_count, expected_basis_count)
  
  # Check x values are in [0,1] range
  expect_true(all(plot$data$x >= 0))
  expect_true(all(plot$data$x <= 1))
  
  # Check that all basis functions have same number of x points
  basis_counts <- table(plot$data$basis)
  expect_true(all(basis_counts == basis_counts[1]))
})

test_that("viewBasis scale breaks include knots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  knots <- c(0.3, 0.7)
  plot <- viewBasis(knots, degree = 1)
  
  # Extract x-axis breaks
  x_breaks <- plot$scales$scales[[1]]$breaks
  
  # Should include 0, all knots, and 1
  expected_breaks <- c(0, knots, 1)
  expect_setequal(x_breaks, expected_breaks)
})


test_that("viewBasis error handling for missing ggplot2", {
  skip_on_cran()
  
  # Mock the requireNamespace function to return FALSE
  with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base",
    {
      expect_error(
        viewBasis(knots = 0.5, degree = 1),
        "Package \"ggplot2\" must be installed to use this function"
      )
    }
  )
})

test_that("viewBasis handles edge cases", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  # Test with minimum valid degree
  plot1 <- viewBasis(knots = 0.5, degree = 1)
  expect_s3_class(plot1, "ggplot")
  
  # Test with many knots
  many_knots <- seq(0.1, 0.9, by = 0.1)
  plot2 <- viewBasis(knots = many_knots, degree = 1)
  expect_s3_class(plot2, "ggplot")
  
  # Verify correct number of basis functions for many knots case
  expected_count <- length(many_knots) + 1 + 1  # length(knots) + degree + 1
  actual_count <- length(unique(plot2$data$basis))
  expect_equal(actual_count, expected_count)
  
  # Test with no internal knots (empty knots vector)
  plot3 <- viewBasis(knots = numeric(0), degree = 2)
  expect_s3_class(plot3, "ggplot")
  expected_count_empty <- 0 + 2 + 1  # length(knots) + degree + 1
  actual_count_empty <- length(unique(plot3$data$basis))
  expect_equal(actual_count_empty, expected_count_empty)
})

test_that("viewBasis plot theme settings", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  plot <- viewBasis(knots = c(0.3, 0.7), degree = 2)
  
  # Check that theme exists and has legend position setting
  expect_true(!is.null(plot$theme))
  expect_true("legend.position" %in% names(plot$theme))
  
  # Check that x-scale limits are set to [0,1]
  x_scale <- NULL
  for (scale in plot$scales$scales) {
    if ("x" %in% scale$aesthetics) {
      x_scale <- scale
      break
    }
  }
  expect_true(!is.null(x_scale))
  expect_equal(x_scale$limits, c(0, 1))
})

test_that("viewBasis consistency across calls", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  knots <- c(0.2, 0.8)
  degree <- 2
  
  # Generate same plot twice
  plot1 <- viewBasis(knots, degree)
  plot2 <- viewBasis(knots, degree)
  
  # Should have identical data structure
  expect_equal(dim(plot1$data), dim(plot2$data))
  expect_equal(names(plot1$data), names(plot2$data))
  expect_equal(plot1$data$x, plot2$data$x)
  expect_equal(plot1$data$basis, plot2$data$basis)
  # Values should be identical (deterministic function)
  expect_equal(plot1$data$value, plot2$data$value)
})

# addCompRisk -----

test_that("addCompRisk basic functionality works", {
  skip_on_cran()
  
  # Create test data
  dt <- data.table(
    id = 1:5,
    event1 = c(10, 5, 15, 8, 12),
    event2 = c(8, 12, 10, 15, 6),
    event3 = c(20, 18, 5, 25, 30)
  )
  
  result <- addCompRisk(dt, events = c("event1", "event2", "event3"), 
                        timeName = "time")
  
  # Check basic structure
  expect_true(is.data.table(result))
  expect_true("time" %in% names(result))
  expect_true("event" %in% names(result))
  expect_true("type" %in% names(result))
  expect_equal(nrow(result), 5)
  
  # Check that time is minimum of all events
  expect_equal(result$time, c(8, 5, 5, 8, 6))  # min for each row
  
  # Check that event indicates which event occurred first (1-indexed)
  expect_equal(result$event, c(2, 1, 3, 1, 2))  # which.min for each row
  
  # Check that type matches the event names
  expect_equal(result$type, c("event2", "event1", "event3", "event1", "event2"))
})

test_that("addCompRisk handles censoring correctly", {
  skip_on_cran()
  
  # Create test data
  dt <- data.table(
    id = 1:4,
    event1 = c(10, 5, 15, 8),
    event2 = c(8, 12, 10, 15),
    censor = c(20, 3, 5, 25)  # censor occurs first for id=2 and id=3
  )
  
  result <- addCompRisk(dt, events = c("event1", "event2", "censor"), 
                        timeName = "time", censorName = "censor")
  
  # Check that censored events have event = 0
  expect_equal(result[id == 2, event], 0)  # censor time = 3 is minimum
  expect_equal(result[id == 3, event], 0)  # censor time = 5 equals event2
  
  # Check that non-censored events have event > 0
  expect_true(result[id == 1, event] > 0)
  expect_true(result[id == 4, event] > 0)
  
  # Check that type is correct for censored observations
  expect_equal(result[id == 2, type], "censor")
  expect_equal(result[id == 3, type], "censor")
})

test_that("addCompRisk keepEvents parameter works", {
  skip_on_cran()
  
  # Create test data
  dt <- data.table(
    id = 1:3,
    event1 = c(10, 5, 15),
    event2 = c(8, 12, 10)
  )
  
  # Test keepEvents = FALSE (default)
  result1 <- addCompRisk(dt, events = c("event1", "event2"), 
                         timeName = "time", keepEvents = FALSE)
  expect_false("event1" %in% names(result1))
  expect_false("event2" %in% names(result1))
  
  # Test keepEvents = TRUE
  result2 <- addCompRisk(dt, events = c("event1", "event2"), 
                         timeName = "time", keepEvents = TRUE)
  expect_true("event1" %in% names(result2))
  expect_true("event2" %in% names(result2))
  expect_true("time" %in% names(result2))
})

test_that("addCompRisk custom column names work", {
  skip_on_cran()
  
  # Create test data
  dt <- data.table(
    id = 1:3,
    event1 = c(10, 5, 15),
    event2 = c(8, 12, 10)
  )
  
  result <- addCompRisk(dt, events = c("event1", "event2"), 
                        timeName = "survival_time", 
                        eventName = "outcome", 
                        typeName = "outcome_type")
  
  expect_true("survival_time" %in% names(result))
  expect_true("outcome" %in% names(result))
  expect_true("outcome_type" %in% names(result))
  expect_false("time" %in% names(result))
  expect_false("event" %in% names(result))
  expect_false("type" %in% names(result))
})

test_that("addCompRisk custom idName works", {
  skip_on_cran()
  
  # Create test data with different id column name
  dt <- data.table(
    subject_id = 1:3,
    event1 = c(10, 5, 15),
    event2 = c(8, 12, 10)
  )
  
  result <- addCompRisk(dt, events = c("event1", "event2"), 
                        timeName = "time", idName = "subject_id")
  
  expect_true("subject_id" %in% names(result))
  expect_false("id" %in% names(result))
  expect_equal(result$subject_id, c(1, 2, 3))
})

test_that("addCompRisk doesn't modify original data", {
  skip_on_cran()
  
  # Create test data
  dt <- data.table(
    id = 1:3,
    event1 = c(10, 5, 15),
    event2 = c(8, 12, 10)
  )
  
  original_dt <- copy(dt)
  
  result <- addCompRisk(dt, events = c("event1", "event2"), 
                        timeName = "time")
  
  # Original data should be unchanged
  expect_equal(dt, original_dt)
  expect_true(nrow(result) == nrow(dt))
  expect_false("time" %in% names(dt))
})

test_that("addCompRisk handles ties correctly", {
  skip_on_cran()
  
  # Create test data with ties
  dt <- data.table(
    id = 1:3,
    event1 = c(10, 5, 8),
    event2 = c(10, 12, 8),  # Ties with event1 for id=1 and id=3
    event3 = c(15, 5, 10)   # Tie with event1 for id=2
  )
  
  result <- addCompRisk(dt, events = c("event1", "event2", "event3"), 
                        timeName = "time")
  
  # Check that minimum time is correct
  expect_equal(result$time, c(10, 5, 8))
  
  # Check that first occurring event is selected in case of ties
  # which.min returns the first occurrence
  expect_equal(result$event, c(1, 1, 1))  # event1 comes first in ties
  expect_equal(result$type, c("event1", "event1", "event1"))
})

test_that("addCompRisk error handling works", {
  skip_on_cran()
  
  # Create test data
  dt <- data.table(
    id = 1:3,
    event1 = c(10, 5, 15),
    event2 = c(8, 12, 10)
  )
  
  # Test missing required arguments
  expect_error(addCompRisk(events = c("event1", "event2"), timeName = "time"))
  expect_error(addCompRisk(dt, timeName = "time"))
  expect_error(addCompRisk(dt, events = c("event1", "event2")))
  
  # Test insufficient events
  expect_error(addCompRisk(dt, events = "event1", timeName = "time"))
  
  # Test non-existent event columns
  expect_error(addCompRisk(dt, events = c("event1", "nonexistent"), timeName = "time"))
  
  # Test non-existent id column
  expect_error(addCompRisk(dt, events = c("event1", "event2"), timeName = "time", idName = "nonexistent"))
  
  # Test conflicting column names
  dt_conflict <- copy(dt)
  dt_conflict[, event := 1]  # Add conflicting column
  expect_error(addCompRisk(dt_conflict, events = c("event1", "event2"), timeName = "time"))
})

test_that("addCompRisk handles single row data", {
  skip_on_cran()
  
  # Create single row test data
  dt <- data.table(
    id = 1,
    event1 = 10,
    event2 = 8,
    event3 = 12
  )
  
  result <- addCompRisk(dt, events = c("event1", "event2", "event3"), 
                        timeName = "time")
  
  expect_equal(nrow(result), 1)
  expect_equal(result$time, 8)
  expect_equal(result$event, 2)
  expect_equal(result$type, "event2")
})

# Test grouped() function
test_that("grouped() creates proper structure with names", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  
  result <- grouped(x, y)
  
  expect_s3_class(result, "grouped_params")
  expect_named(result, c("x", "y"))
  expect_equal(result$x, x)
  expect_equal(result$y, y)
})

test_that("grouped() errors when lengths don't match", {
  x <- c(1, 2, 3)
  y <- c(4, 5)  # Different length
  
  expect_error(
    grouped(x, y),
    "x and y should be of equal length!"
  )
})

test_that("grouped() works with single argument", {
  x <- c(1, 2, 3)
  
  result <- grouped(x)
  
  expect_s3_class(result, "grouped_params")
  expect_named(result, "x")
  expect_equal(result$x, x)
})

test_that("grouped() preserves variable names correctly", {
  my_var <- c(1, 2, 3)
  another_var <- c(4, 5, 6)
  
  result <- grouped(my_var, another_var)
  
  expect_named(result, c("my_var", "another_var"))
})

test_that("grouped() preserves variable names correctly when specified in call", {
  
  another_var <- c(2, 3, 4)
  
  result1 <- grouped(my_var = c(1,2,3), another_var = c(2,3,4))
  result2 <- grouped(my_var = c(1,2,3), another_var)
  
  expect_named(result1, c("my_var", "another_var"))
  expect_named(result2, c("my_var", "another_var"))
  
  
})

test_that("grouped() works with numeric vectors of length 1", {
  x <- 5
  y <- 10
  
  result <- grouped(x, y)
  
  expect_s3_class(result, "grouped_params")
  expect_equal(length(result$x), 1)
  expect_equal(length(result$y), 1)
})

# Test scenario_list() function
test_that("scenario_list() works with only regular parameters", {
  a <- c(1, 2)
  b <- c(10, 20)
  
  result <- scenario_list(a, b)
  
  expect_equal(length(result), 4)  # 2 * 2 = 4 scenarios
  expect_named(result[[1]], c("a", "b", "scenario"))
})

test_that("scenario_list() works with only grouped parameters", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  
  result <- scenario_list(grouped(x, y))
  
  expect_equal(length(result), 3)  # 3 rows (not expanded)
  expect_named(result[[1]], c("x", "y", "scenario"))
})

test_that("scenario_list() works with both regular and grouped parameters", {
  a <- c(1, 2)
  b <- c(10, 20)
  x <- c(3, 4, 5)
  y <- c(6, 7, 8)
  
  result <- scenario_list(a, b, grouped(x, y))
  
  expect_equal(length(result), 12)  # (2 * 2) * 3 = 12 scenarios
  expect_named(result[[1]], c("a", "b", "x", "y", "scenario"))
})

test_that("scenario_list() works with multiple grouped() calls", {
  a <- c(1, 2)
  x <- c(3, 4, 5)
  y <- c(6, 7, 8)
  q1 <- c(3.4, 2.3)
  q2 <- c(3.1, 4.3)
  
  result <- scenario_list(a, grouped(x, y), grouped(q1, q2))
  
  expect_equal(length(result), 12)  # 2 * 3 * 2 = 12 scenarios
  expect_named(result[[1]], c("a", "x", "y", "q1", "q2", "scenario"))
})

test_that("scenario_list() errors on duplicate variable names", {
  a <- c(1, 2)
  x <- c(3, 4)
  
  expect_error(
    scenario_list(a, x, grouped(x)),
    "Variable\\(s\\) included more than once: x"
  )
})

test_that("scenario_list() errors on duplicates across groups", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)
  
  expect_error(
    scenario_list(grouped(x, y), grouped(x)),
    "Variable\\(s\\) included more than once: x"
  )
})

test_that("scenario_list() errors on multiple duplicates", {
  a <- c(1, 2)
  x <- c(3, 4)
  
  expect_error(
    scenario_list(a, x, grouped(a, x)),
    "Variable\\(s\\) included more than once: a, x"
  )
})

test_that("scenario_list() preserves correct values in scenarios", {
  a <- c(1, 2)
  x <- c(10, 20)
  y <- c(100, 200)
  
  result <- scenario_list(a, grouped(x, y))
  
  # Check first scenario
  expect_equal(unname(result[[1]][1]), 1)
  expect_equal(unname(result[[1]][2]), 10)
  expect_equal(unname(result[[1]][[3]]), 100)
  
  # Check last scenario
  expect_equal(unname(result[[4]][1]), 2)
  expect_equal(unname(result[[4]][2]), 20)
  expect_equal(unname(result[[4]][[3]]), 200)
})

test_that("scenario_list() assigns sequential scenario numbers", {
  a <- c(1, 2, 3)
  b <- c(10, 20)
  
  result <- scenario_list(a, b)
  
  scenarios <- sapply(result, function(x) x["scenario"])
  expect_equal(unname(scenarios), 1:6)
})

test_that("scenario_list() returns list of named vectors", {
  a <- c(1, 2)
  
  result <- scenario_list(a)
  
  expect_type(result, "list")
  expect_named(result[[1]], c("a", "scenario"))
})


test_that("scenario_list() handles edge case of single value", {
  a <- 1
  
  result <- scenario_list(a)
  
  expect_equal(length(result), 1)
  expect_equal(unname(result[[1]]["a"]), 1)
  expect_equal(unname(result[[1]]["scenario"]), 1)
})

test_that("scenario_list() preserves variable names correctly when specified in call", {
  
  result <- scenario_list(a = c(1,2,3), b = c(3,4), grouped(x = c(3,4), y = c(5,6)))
  expect_true(all(sapply(result, function(x) all(c("a", "b", "x", "y") %in% names(x)))))
  
  y <- c(5, 6)
  b <- c(3, 4)
  result <- scenario_list(a = c(1,2,3), b, grouped(x = c(3,4), y))
  expect_true(all(sapply(result, function(x) all(c("a", "b", "x", "y") %in% names(x)))))
  
  
  
})

