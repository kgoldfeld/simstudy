library(testthat)
library(simstudy)
library(data.table)

# checkBoundsBin ----

test_that("Correlation boundaries for binary variables are correct", {
  skip_on_cran()

  p1 <- .5
  p2 <- .8

  expect_error(simstudy:::checkBoundsBin(p1, p2, d = .9))
  expect_error(simstudy:::checkBoundsBin(p1, p2, d = -.6))

  expect_silent(simstudy:::checkBoundsBin(p1, p2, -0.4))
  expect_silent(simstudy:::checkBoundsBin(p1, p2, 0.3))
  expect_silent(simstudy:::checkBoundsBin(p1, p2, 0.2))
})

# .genBinEP ----

test_that(".genBinEP handles non-positive definite correlation matrices", {
  skip_on_cran()

  # Test with tcorr as a correlation matrix that could become non-PD
  n <- 20
  p <- rep(0.3, 5)  # 5 periods

  # Create a correlation matrix that might cause issues
  tcorr <- matrix(0.9, nrow = 5, ncol = 5)
  diag(tcorr) <- 1

  expect_silent(result1 <- simstudy:::.genBinEP(n, p, tcorr))
  expect_true(is.data.table(result1))
  expect_equal(nrow(result1), n * length(p))

  # Test with a correlation matrix that's more likely to become non-PD
  # Create a matrix with very high correlations that might not be PD
  
  p2 <- rep(0.4, 6)  # 6 periods
  tcorr2 <- matrix(0.95, nrow = 6, ncol = 6)
  diag(tcorr2) <- 1

  expect_silent(result2 <- simstudy:::.genBinEP(n, p2, tcorr2))
  expect_true(is.data.table(result2))
  expect_equal(nrow(result2), n * length(p2))

  # Test with a correlation matrix designed to potentially need nearPD correction
  p3 <- rep(0.2, 8)  # 8 periods
  tcorr3 <- matrix(0.98, nrow = 8, ncol = 8)
  diag(tcorr3) <- 1

  expect_silent(result3 <- simstudy:::.genBinEP(n, p3, tcorr3))
  expect_true(is.data.table(result3))
  expect_equal(nrow(result3), n * length(p3))
  expect_true(all(c("id", "seq", "X", "period", "seqid") %in% names(result3)))
  expect_true(all(result3$X %in% c(0, 1)))  # Should be binary outcomes
})

# Test with correlation matrices that are more likely to need nearPD correction
test_that(".genBinEP works with correlation matrices that may become non-PD", {
  skip_on_cran()

  # Create a correlation matrix that's close to being singular
  # This is more likely to trigger the nearPD correction
  n <- 15
  p <- rep(0.25, 10)  # 10 periods

  # Create a correlation matrix with a pattern that might cause numerical issues
  tcorr <- matrix(0.97, nrow = 10, ncol = 10)
  diag(tcorr) <- 1
  # Add some variation to make it more realistic but potentially problematic
  tcorr[1:5, 6:10] <- 0.99
  tcorr[6:10, 1:5] <- 0.99

  expect_silent(result <- simstudy:::.genBinEP(n, p, tcorr))
  expect_true(is.data.table(result))
  expect_equal(nrow(result), n * length(p))

  # Test with an even more problematic correlation structure
  # Create a matrix where some correlations are very close to perfect
  p2 <- rep(0.3, 12)  # 12 periods
  tcorr2 <- matrix(0.85, nrow = 12, ncol = 12)
  diag(tcorr2) <- 1
  # Make some correlations extremely high
  for(i in 1:11) {
    tcorr2[i, i+1] <- 0.995
    tcorr2[i+1, i] <- 0.995
  }

  expect_silent(result2 <- simstudy:::.genBinEP(n, p2, tcorr2))
  expect_true(is.data.table(result2))
  expect_equal(nrow(result2), n * length(p2))
})

# blockExchangeMat ----

test_that("blockExchangeMat works", {
  skip_on_cran()
  
  
  x <- runif(1, .6, .8)
  x2 <- x - runif(1, 0, .1)
  y <- sample(1:10, 1)
  z <- sample(2:10, 1)
  n <- sample(2:5, 1)
  w <- runif(n, .6, .8)
  
  expect_silent(blockExchangeMat(ninds = y, nperiods = z, rho_w = x))
  expect_silent(blockExchangeMat(ninds = y, nperiods = z, rho_w = x, rho_b = x*.5))
  expect_silent(blockExchangeMat(ninds = y, nperiods = z, rho_w = x, rho_b = x*.8, rho_a = x2, pattern = "cohort"))
  
  expect_silent(blockExchangeMat(ninds = y, nperiods = z, rho_w = x, rho_b = x*.8, rho_a = x2, pattern = "cohort", nclusters = n))
  expect_silent(blockExchangeMat(ninds = y, nperiods = z, rho_w = w, rho_b = w*.8, nclusters = n))
  
})


test_that("blockExchangeMat errors correctly.", {
  skip_on_cran()
  
  expect_error(blockExchangeMat(ninds = 2, nperiods = 3), class="simstudy::missingArgument")
  expect_error(blockExchangeMat(nperiods = 3, rho_w = .8), class="simstudy::missingArgument")
  expect_error(blockExchangeMat(ninds = 3, rho_w = .8), class="simstudy::missingArgument")
  
  expect_error(blockExchangeMat(ninds = 3.5, nperiods = 3, rho_w = .8), class = "simstudy::wrongType")
  expect_error(blockExchangeMat(ninds = 3, nperiods = 3.2, rho_w = .8), class = "simstudy::wrongType")
  expect_error(blockExchangeMat(ninds = 3, nperiods = 3, rho_w = .8, nclusters = 5.1), class = "simstudy::wrongType")
  
  expect_error(blockExchangeMat(ninds = 3, nperiods =  1, rho_w = .8), class = "simstudy::minError")
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 1.4), class = "simstudy::valueError")
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 0.8, rho_b = 1.4), class = "simstudy::valueError")
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 0.8, rho_b = 0.5, rho_a = -3.2), class = "simstudy::valueError")
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 0.8, rho_b = c(.7, .6, 1.4), nclusters = 3), class = "simstudy::valueError")
  

  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 0.8, pattern = "closed"), class = "simstudy::optionInvalid")
  
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 0.8, rho_a = .6))
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = 0.8, pattern = "cohort"))
  expect_error(blockExchangeMat(ninds = c(3, 2, 1, 4, 5, 1), nperiods =  2, rho_w = 0.8, rho_a = .7, pattern = "cohort", nclusters = 3))
  
  expect_error(blockExchangeMat(ninds = c(3, 2, 4, 3), nperiods =  2, rho_w = 0.8, nclusters = 3))
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = c(0.8, .7), nclusters = 3), class = "simstudy::lengthMismatch")
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = c(0.8, .7,.6), rho_b = c(0.7,.6), nclusters = 3), class = "simstudy::lengthMismatch")
  expect_error(blockExchangeMat(ninds = 3, nperiods =  2, rho_w = c(0.8, .7,.6), rho_b = 0.7, rho_a = c(.4, .3), pattern = "cohort", nclusters = 3), class = "simstudy::lengthMismatch")
})

test_that("blockExchangeMat handles different ninds configurations", {
  skip_on_cran()
  
  # Test case 1: length(ninds) == nclusters
  # Each cluster has a different sample size, but constant across periods within cluster
  expect_silent(blockExchangeMat(
    ninds = c(3, 5), 
    nperiods = 4, 
    rho_w = 0.7, 
    rho_b = 0.4, 
    nclusters = 2
  ))
  
  # Test case 1 with cohort pattern
  expect_silent(blockExchangeMat(
    ninds = c(3, 5, 4), 
    nperiods = 3, 
    rho_w = 0.6, 
    rho_b = 0.3, 
    rho_a = 0.5,
    pattern = "cohort",
    nclusters = 3
  ))

  # Test case 2: length(ninds) == nclusters * nperiods (cross-sectional only)
  # Sample size varies by both cluster and period
  expect_silent(blockExchangeMat(
    ninds = c(2, 3, 4, 5, 1, 6), # 2 clusters, 3 periods each = 6 values
    nperiods = 3, 
    rho_w = 0.8, 
    rho_b = 0.5, 
    nclusters = 2
  ))
  
  # Test case 2 with varying correlation parameters
  expect_silent(blockExchangeMat(
    ninds = c(3, 4, 2, 1, 5, 3, 2, 4), # 2 clusters, 4 periods each = 8 values
    nperiods = 4, 
    rho_w = c(0.7, 0.6), 
    rho_b = c(0.4, 0.3), 
    nclusters = 2
  ))
  
  # Verify the error for cohort pattern with varying ninds across periods
  expect_error(
    blockExchangeMat(
      ninds = c(3, 4, 2, 1), # 2 clusters, 2 periods each = 4 values
      nperiods = 2, 
      rho_w = 0.7, 
      rho_b = 0.4,
      rho_a = 0.5,
      pattern = "cohort",
      nclusters = 2
    ),
    "The number of individuals per period must be constant across periods with a cohort design"
  )
})

###

test_that("blockDecayMat works", {
  skip_on_cran()
  
  x <- runif(1, .6, .8)
  y <- sample(1:10, 1)
  z <- sample(2:10, 1)
  n <- sample(2:5, 1)
  w <- runif(n, .6, .8)
  
  expect_silent(blockDecayMat(ninds = y, nperiods = z, rho_w = x, r = x))
  expect_silent(blockDecayMat(ninds = y, nperiods = z, rho_w = x, r = x))
  expect_silent(blockDecayMat(ninds = y, nperiods = z, rho_w = x, r = x, pattern = "cohort"))
  
  expect_silent(blockDecayMat(ninds = y, nperiods = z, rho_w = x, r = x, pattern = "cohort", nclusters = n))
  expect_silent(blockDecayMat(ninds = y, nperiods = z, rho_w = w, r = w, nclusters = n))
  
})


test_that("blockDecayMat errors correctly.", {
  skip_on_cran()
  
  expect_error(blockDecayMat(ninds = 2, nperiods = 3, r = .8), class="simstudy::missingArgument")
  expect_error(blockDecayMat(nperiods = 3, rho_w = .8, r = .7), class="simstudy::missingArgument")
  expect_error(blockDecayMat(ninds = 3, rho_w = .8, r - .3), class="simstudy::missingArgument")
  expect_error(blockDecayMat(ninds = 3, nperiods = 4, rho_w = .8), class="simstudy::missingArgument")
  
  expect_error(blockDecayMat(ninds = 3.5, nperiods = 3, rho_w = .8, r=.7), class = "simstudy::wrongType")
  expect_error(blockDecayMat(ninds = 3, nperiods = 3.2, rho_w = .8, r=.7), class = "simstudy::wrongType")
  expect_error(blockDecayMat(ninds = 3, nperiods = 3, rho_w = .8, r = .7, nclusters = 5.1), class = "simstudy::wrongType")
  
  expect_error(blockDecayMat(ninds = 3, nperiods =  1, rho_w = .8, r = .7), class = "simstudy::minError")
  expect_error(blockDecayMat(ninds = 3, nperiods =  2, rho_w = 1.4, r = .7), class = "simstudy::valueError")
  expect_error(blockDecayMat(ninds = 3, nperiods =  2, rho_w = 0.8, r = 1.4), class = "simstudy::valueError")
  expect_error(blockDecayMat(ninds = 3, nperiods =  2, rho_w = 0.8, r = c(.7, .6, 1.4), nclusters = 3), class = "simstudy::valueError")
  
  expect_error(blockDecayMat(ninds = 3, nperiods =  2, rho_w = 0.8, r= .7, pattern = "closed"), class = "simstudy::optionInvalid")
  expect_error(blockDecayMat(ninds = c(3, 2, 1, 4, 5, 1), nperiods =  2, rho_w = 0.8, r= .7, pattern = "cohort", nclusters = 3))
  
  expect_error(blockDecayMat(ninds = c(3, 2, 4, 3), nperiods =  2, rho_w = 0.8, r=.5, nclusters = 3))
  expect_error(blockDecayMat(ninds = 3, nperiods =  2, rho_w = c(0.8, .7), r=.3, nclusters = 3), class = "simstudy::lengthMismatch")
  expect_error(blockDecayMat(ninds = 3, nperiods =  2, rho_w = c(0.8, .7,.6), r = c(0.7,.6), nclusters = 3), class = "simstudy::lengthMismatch")
})

test_that("blockDecayMat handles different ninds configurations", {
  skip_on_cran()
  
  # Test case 1: length(ninds) == nclusters
  # Each cluster has a different sample size, but constant across periods within cluster
  expect_silent(blockDecayMat(
    ninds = c(3, 5), 
    nperiods = 4, 
    rho_w = 0.7, 
    r = 0.8,
    nclusters = 2
  ))
  
  # Test case 1 with cohort pattern
  expect_silent(blockDecayMat(
    ninds = c(3, 5, 4), 
    nperiods = 3, 
    rho_w = 0.6, 
    r = 0.7,
    pattern = "cohort",
    nclusters = 3
  ))
  
  # Test case 1 with varying correlation parameters per cluster
  expect_silent(blockDecayMat(
    ninds = c(4, 6), 
    nperiods = 3, 
    rho_w = c(0.7, 0.5), 
    r = c(0.8, 0.6),
    nclusters = 2
  ))
  
  # Test case 2: length(ninds) == nclusters * nperiods (cross-sectional only)
  # Sample size varies by both cluster and period
  expect_silent(blockDecayMat(
    ninds = c(2, 3, 4, 5, 1, 6), # 2 clusters, 3 periods each = 6 values
    nperiods = 3, 
    rho_w = 0.8, 
    r = 0.7,
    nclusters = 2
  ))
  
  # Test case 2 with varying correlation parameters
  expect_silent(blockDecayMat(
    ninds = c(3, 4, 2, 1, 5, 3, 2, 4), # 2 clusters, 4 periods each = 8 values
    nperiods = 4, 
    rho_w = c(0.7, 0.6), 
    r = c(0.8, 0.5),
    nclusters = 2
  ))
  
  # Test case 2 with single correlation parameters applied to all clusters
  expect_silent(blockDecayMat(
    ninds = c(2, 4, 3, 1, 2, 5), # 2 clusters, 3 periods each = 6 values
    nperiods = 3, 
    rho_w = 0.6, 
    r = 0.9,
    nclusters = 2
  ))
  
  # Verify the error for cohort pattern with varying ninds across periods
  expect_error(
    blockDecayMat(
      ninds = c(3, 4, 2, 1), # 2 clusters, 2 periods each = 4 values
      nperiods = 2, 
      rho_w = 0.7, 
      r = 0.8,
      pattern = "cohort",
      nclusters = 2
    ),
    "The number of individuals per period must be constant across periods with a cohort design"
  )
})

###

test_that("genCorMat works", {

  skip_on_cran()

  expect_silent(genCorMat(nvars = 4, cors = c(.3, .2, .1), corstr = "structured"))
  expect_silent(genCorMat(nvars = 4, cors = c(0.6, 0.5, 0.4, .3, .2, .1)))
  expect_silent(genCorMat(nvars = 4, corstr = "arx"))
  expect_silent(genCorMat(nvars = 4))
  expect_silent(genCorMat(nvars = 4, rho = .4))

  expect_silent(genCorMat(nvars = 4, nclusters = 3))
  expect_silent(genCorMat(nvars = c(4, 2, 5), rho = c(0.6, .3, .2), corst = "ar1", nclusters = 3))
  expect_silent(genCorMat(nvars = c(4, 2, 5), rho = 0.6, corstr = "ar1", nclusters = 3))

  expect_silent(genCorMat(nvars = 3, corstr = "arx", nclusters=5))
  expect_silent(genCorMat(nvars = 3, rho = .4, nclusters=5))


})

test_that("genCorMat generates errors correctly.", {

  skip_on_cran()

  expect_error(genCorMat(cors = c(.3, .2, .1), corstr = "structured"), class="simstudy::missingArgument")
  expect_error(genCorMat(nvars = 4.5), class="simstudy::wrongType")
  expect_error(genCorMat(nvars = 4, nclusters = 4.3), class="simstudy::wrongType")
  expect_error(genCorMat(nvars = 4, corstr = c("ar1", "arx"), nclusters = 4), class="simstudy::lengthMismatch")
  expect_error(genCorMat(nvars = 4, corstr = "ar2", nclusters = 4), class="simstudy::optionInvalid")
  expect_error(genCorMat(nvars = 4, rho = "0.5"), class="simstudy::wrongType")
  expect_error(genCorMat(nvars = 4, rho = 4.5), class="simstudy::valueError")

  expect_error(genCorMat(nvars = 3, cors = c(.4, .3)), class="simstudy::lengthMismatch")
  expect_error(genCorMat(nvars = 3, cors = c(.4, .3), corstr = "structured", nclusters=2),
          class="simstudy::notEqual")
  expect_error(genCorMat(nvars = c(3, 2), cors = c(.4, .3), corstr = "structured", nclusters=1),
               class="simstudy::lengthMismatch")

  expect_error(genCorMat(nvars = c(3), cors = c(.4, .3), nclusters=2),
               class="simstudy::notEqual")
  expect_error(genCorMat(nvars = c(3, 2), cors = c(.4, .3), nclusters=1),
               class="simstudy::lengthMismatch")

  expect_error(genCorMat(nvars = c(3, 2), corstr = "arx", nclusters=1),
               class="simstudy::lengthMismatch")
  expect_error(genCorMat(nvars = c(3, 2), corstr = "arx", nclusters=3),
               class="simstudy::lengthMismatch")

  expect_error(genCorMat(nvars = c(3, 2), nclusters=1),
               class="simstudy::lengthMismatch")
  expect_error(genCorMat(nvars = c(3, 2), nclusters=3),
               class="simstudy::lengthMismatch")

  expect_error(genCorMat(nvars = c(3, 2), rho = .5, nclusters=1),
               class="simstudy::lengthMismatch")
  expect_error(genCorMat(nvars = c(3), rho = c(.5, .3), nclusters=1),
               class="simstudy::lengthMismatch")

  expect_error(genCorMat(nvars = c(3, 2, 2), rho = c(.5, .3), nclusters=2),
               class="simstudy::lengthMismatch")
  expect_error(genCorMat(nvars = c(3, 2), rho = c(.5, .3, .2), nclusters=2),
               class="simstudy::lengthMismatch")


  expect_error(genCorMat(nvars = c(3, 2), nclusters=3),
               class="simstudy::lengthMismatch")

})

# addCorData ----

test_that("addCorData emits warning if `idname` appears multiple times", {
  tdef <- defData(varname = "grp", dist = "binary", formula = 0.5, id = "id")

  dt <- genData(500, tdef)
  dt <- rbind(dt, dt)

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(3, 1.5, 1)
  corMat <- genCorMat(3)

  expect_warning(
    addCorData(dt, "id", mu = mu, sigma = sigma, corMat = corMat),
    "id appears multiple times in data table. Please check results."
  )
})

test_that("addCorData adds correlated data with compound symmetry structure", {

  skip_on_cran()

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(3, 1.5, 1)
  rho <- rbeta(1, 20, 10)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(500, def)



  dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, rho = rho, corstr = "cs")

  expect_true(all(c("V1", "V2", "V3") %in% colnames(dtAdd)))
  expect_equal(nrow(dtAdd), 500)
  expect_equal(
    round(cor(dtAdd[, .(V1, V2, V3)]), 2),
    matrix(c(1, rho, rho, rho, 1, rho, rho, rho, 1), nrow = 3),
    tolerance = .15,
    check.attributes = FALSE
  )
})

test_that("addCorData adds correlated data with AR1 structure", {

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(3, 1.5, 1)
  rho <- rbeta(1, 20, 10)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(500, def)

  dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, rho = rho, corstr = "ar1")

  expect_true(all(c("V1", "V2", "V3") %in% colnames(dtAdd)))
  expect_equal(nrow(dtAdd), 500)
  expect_equal(
    round(cor(dtAdd[, .(V1, V2, V3)]), 2),
    matrix(c(1, rho, rho^2, rho, 1, rho, rho^2, rho, 1), nrow = 3),
    tolerance = .15,
    check.attributes = FALSE
  )
})

test_that("addCorData adds correlated data with custom correlation matrix", {

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(3, 1.5, 1)
  corMat <- genCorMat(3)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(500, def)

  dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, corMatrix = corMat)

  expect_true(all(c("V1", "V2", "V3") %in% colnames(dtAdd)))
  expect_equal(nrow(dtAdd), 500)
  expect_equal(
    round(cor(dtAdd[, .(V1, V2, V3)]), 2),
    corMat,
    tolerance = .15,
    check.attributes = FALSE
  )
})

test_that("addCorData handles different sigma values", {

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(3, 1.5, 1)
  rho <- rbeta(1, 20, 10)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(500, def)

  dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, rho = rho, corstr = "cs",
                      cnames = c("X1", "X2", "X3"))

  expect_true(all(c("X1", "X2", "X3") %in% colnames(dtAdd)))
  expect_equal(nrow(dtAdd), 500)
  expect_equal(round(sd(dtAdd$X1), 1), sigma[1], tolerance = 0.2)
  expect_equal(round(sd(dtAdd$X2), 1), sigma[2], tolerance = 0.2)
  expect_equal(round(sd(dtAdd$X3), 1), sigma[3], tolerance = 0.2)
})

test_that("addCorData handles constant sigma value", {

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(1, 1.5, 1)
  rho <- rbeta(1, 20, 10)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(250, def)

  dtAdd <- addCorData(dt, "myID", mu = mu, sigma = sigma, rho = .7, corstr = "cs")

  expect_true(all(c("V1", "V2", "V3") %in% colnames(dtAdd)))
  expect_equal(nrow(dtAdd), 250)
  expect_equal(round(sd(dtAdd$V1), 1), sigma, tolerance = 0.2)
  expect_equal(round(sd(dtAdd$V2), 1), sigma, tolerance = 0.2)
  expect_equal(round(sd(dtAdd$V3), 1), sigma, tolerance = 0.2)
})

test_that("addCorData handles constant sigma value", {

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(2, 1.5, 1)
  rho <- rbeta(1, 20, 10)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(250, def)

  expect_error(addCorData(dt, "myID", mu = mu, sigma = sigma, rho = rho, corstr = "cs"),
               "Improper number of standard deviations")

})

test_that("addCorData throws error for mismatched cnames length", {

  mu <- rnorm(3, mean = c(3, 8, 15))
  sigma <- rgamma(1, 1.5, 1)
  rho <- rbeta(1, 20, 10)

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(250, def)

  expect_error(addCorData(dt, "myID", mu = mu, sigma = sigma, rho = .7, corstr = "cs",
                          cnames = c("X1", "X2")), "Invalid number of variable names")

})

test_that("addCorData throws error for invalid correlation matrix", {
  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  dt <- genData(250, def)

  mu <- c(3, 8, 15)
  sigma <- c(1, 2, 3)
  invalid_corMat <- matrix(c(1, .2, .8, .2, 1, .6, .8, .6, .5), nrow = 3)  # Not positive semi-definite

  expect_error(addCorData(dt, "myID", mu = mu, sigma = sigma, corMatrix = invalid_corMat),
               "Correlation matrix not positive definite")
})

test_that("addCorData throws error for invalid correlation coefficient", {
  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  dt <- genData(250, def)

  mu <- c(3, 8, 15)
  sigma <- c(1, 2, 3)

  expect_error(addCorData(dt, "myID", mu = mu, sigma = sigma, rho = 1.5, corstr = "cs"),
               "corMatrix is not positive semi-definite!")
  expect_error(addCorData(dt, "myID", mu = mu, sigma = sigma, rho = -1.5, corstr = "cs"),
               "corMatrix is not positive semi-definite!")
})

# genCorFlex  ----

test_that("Basic Functionality Test", {

  def <- defData(varname = "xNorm", formula = 3 , variance = 4, dist = "normal")
  def <- defData(def, varname = "xGamma1", formula = 15, variance = 2, dist = "gamma")
  def <- defData(def, varname = "xBin", formula = .5, dist = "binary")

  dt <- genCorFlex(100, def, rho = .3, corstr = "cs")

  expect_equal(nrow(dt), 100)
  expect_equal(ncol(dt), 4) # 3 variables + 1 id column
})

test_that("Distribution Test", {
  def <- defData(varname = "xNorm", formula = 8, variance = 4, dist = "normal")
  def <- defData(def, varname = "xGamma1", formula = 15, variance = 2, dist = "gamma")
  def <- defData(def, varname = "xUniform1", formula = "3;8", dist = "uniform")
  def <- defData(def, varname = "xPois1", formula = 8, dist = "poisson")
  def <- defData(def, varname = "xNB1", formula = 4, variance = 1, dist = "negBinomial")

  dt <- genCorFlex(500, def, rho = .3, corstr = "cs")

  expect_equal(mean(dt$xNorm), 8, tolerance = 0.5)
  expect_equal(var(dt$xNorm),  4, tolerance = 0.5)
  expect_equal(mean(dt$xGamma1), 15, tolerance = 0.5)
  expect_equal(mean(dt$xUniform1), 5.5, tolerance = 0.5)
  expect_equal(mean(dt$xPois1), 8, tolerance = 0.5)
  expect_equal(mean(dt$xNB1), 4, tolerance = 0.5)


})

test_that("Correlation Structure Test", {
  def <- defData(varname = "xNorm", formula = 7, variance = 4, dist = "normal")
  def <- defData(def, varname = "xGamma1", formula = 15, variance = 2, dist = "gamma")

  dt <- genCorFlex(500, def, rho = .3, corstr = "cs")

  cor_matrix <- cor(dt[, -"id"])
  expect_equal(cor_matrix, matrix(c(1, .3, .3, 1), 2, 2),
               tolerance = .15, check.attributes = FALSE)
})

test_that("Correlation Structure Test for tau", {
  def <- defData(varname = "xPois1", formula = 7, dist = "poisson")
  def <- defData(def, varname = "xPois2", formula = 15, dist = "poisson")

  dt <- genCorFlex(1000, def, tau = .3, corstr = "cs")
  obs_cor <- cor(dt[, -"id"])[1,2]

  expect_equal(obs_cor, sin(.3 * pi/2), tolerance = .1)
})

test_that("Character error for formula", {
  # Create a definition with a non-numeric formula to trigger the warning
  def <- data.table::data.table(
    varname = c("xNorm", "xInvalid"),
    formula = c(3, "invalid"),
    variance = c(4, 2),
    dist = c("normal", "gamma")
  )

  expect_error(genCorFlex(100, def, rho = .3, corstr = "cs"), "Non-scalar values in definitions")
})

test_that("NA error for formula", {
  # Create a definition with a non-numeric formula to trigger the warning
  def <- data.table::data.table(
    varname = c("xNorm", "xInvalid"),
    formula = c(3, NA),
    variance = c(4, 2),
    dist = c("normal", "gamma")
  )

  expect_error(genCorFlex(100, def, rho = .3, corstr = "cs"), "Non-scalar values in definitions")
})

test_that("Distribution type error for dist", {
  # Create a definition with a non-numeric formula to trigger the warning
  def <- data.table::data.table(
    varname = c("xNorm", "xInvalid"),
    formula = c(3, 8),
    variance = c(4, 2),
    dist = c("normal", "invalid")
  )

  expect_error(genCorFlex(100, def, rho = .3, corstr = "cs"),
    "Only implemented for the following distributions: binary, uniform, normal, poisson, gamma, and negative binomial")
})

# addCorFlex <-----

create_test_defs <- function(distributions = c("normal"), varnames = c("X1")) {
  defs <- data.table(
    varname = varnames,
    formula = rep("1", length(varnames)),
    dist = distributions,
    variance = rep(1, length(varnames)),
    link = rep("identity", length(varnames))
  )
  return(defs)
}

# Test suite for addCorFlex function
test_that("addCorFlex basic functionality works", {
  # Create test data
  dt <- genData(10)
  defs <- create_test_defs(varnames = c("X1", "X2"))

  # Test basic execution
  result <- addCorFlex(dt, defs, rho = 0.5)

  # Basic checks
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 10)
  expect_true("X1" %in% names(result))
  expect_true("id" %in% names(result))
})

test_that("addCorFlex parameter validation works", {
  dt <- data.table(5)

  # Test unsupported distribution error
  invalid_defs <- data.table(
    varname = "X1",
    formula = "0",
    dist = "beta",  # unsupported distribution
    variance = 1,
    link = "identity"
  )

  expect_error(
    addCorFlex(dt, invalid_defs),
    "Only implemented for the following distributions"
  )
})

test_that("addCorFlex works with different correlation structures", {
  dt <- genData(5)
  defs <- create_test_defs(c("normal", "normal"), c("X1", "X2"))

  # Test compound symmetry structure
  result_cs <- addCorFlex(dt, defs, rho = 0.3, corstr = "cs")
  expect_s3_class(result_cs, "data.table")
  expect_equal(ncol(result_cs), 3)  # id + X1 + X2

  # Test autoregressive structure
  result_ar1 <- addCorFlex(dt, defs, rho = 0.3, corstr = "ar1")
  expect_s3_class(result_ar1, "data.table")
  expect_equal(ncol(result_ar1), 3)
})

test_that("addCorFlex works with tau parameter", {
  dt <- genData(5)
  defs <- create_test_defs()

  # Test with tau parameter (should override rho)
  result <- addCorFlex(dt, defs, rho = 0.5, tau = 0.3)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5)
})

test_that("addCorFlex works with correlation matrix", {
  dt <- genData(5)
  defs <- create_test_defs(c("normal", "normal"), c("X1", "X2"))

  # Create a valid correlation matrix
  corMatrix <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)

  result <- addCorFlex(dt, defs, corMatrix = corMatrix)

  expect_s3_class(result, "data.table")
  expect_equal(ncol(result), 3)
})

test_that("addCorFlex works with normal distribution", {
  dt <- genData(10)
  defs <- create_test_defs("normal", "norm_var")

  result <- addCorFlex(dt, defs, rho = 0.2)

  expect_s3_class(result, "data.table")
  expect_true("norm_var" %in% names(result))
  expect_equal(nrow(result), 10)
  expect_true(is.numeric(result$norm_var))
})

test_that("addCorFlex works with binary distribution", {
  dt <- genData(10)
  defs <- create_test_defs("binary", "bin_var")

  result <- addCorFlex(dt, defs, rho = 0.3)

  expect_s3_class(result, "data.table")
  expect_true("bin_var" %in% names(result))
  expect_true(all(result$bin_var %in% c(0, 1)))
})

test_that("addCorFlex works with poisson distribution", {
  dt <- genData(10)
  defs <- create_test_defs("poisson", "pois_var")

  result <- addCorFlex(dt, defs, rho = 0.4)

  expect_s3_class(result, "data.table")
  expect_true("pois_var" %in% names(result))
  expect_true(all(result$pois_var >= 0))
  expect_true(all(result$pois_var == floor(result$pois_var)))  # integers
})

test_that("addCorFlex works with gamma distribution", {
  dt <- genData(10)
  defs <- create_test_defs(distributions = "gamma", varnames = "gamma_var")

  result <- addCorFlex(dt, defs, rho = 0.1)

  expect_s3_class(result, "data.table")
  expect_true("gamma_var" %in% names(result))
  expect_true(all(result$gamma_var > 0))  # gamma values are positive
})

test_that("addCorFlex works with negative binomial distribution", {
  dt <- genData(10)
  defs <- create_test_defs("negBinomial", "nb_var")

  result <- addCorFlex(dt, defs, rho = 0.2)

  expect_s3_class(result, "data.table")
  expect_true("nb_var" %in% names(result))
  expect_true(all(result$nb_var >= 0))
  expect_true(all(result$nb_var == floor(result$nb_var)))  # integers
})

## Old tests

test_that("addCorFlex handles invalid distribution", {
  dt <- data.table(id = 1:10)
  defs <- data.table(varname = "A", formula = "1", dist = "invalid", variance = 1)
  expect_error(addCorFlex(dt, defs, rho = .4, corstr = "cs"),
    "Only implemented for the following distributions: binary, normal, poisson, gamma, and negative binomial")
})

test_that("addCorFlex generates data with correct dimensions", {

  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(500, def)

  defs <- data.table(varname = c("A", "B"), formula = c("1", "2"),
          dist = c("normal", "poisson"), variance = c(1, 1), link = c("identity", "identity"))
  result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
  expect_equal(nrow(result), 500)
  expect_equal(ncol(result), 5)
})

test_that("addCorFlex generates data with specified correlation structure", {
  def <- defData(varname = "xUni", dist = "uniform", formula = "10;20", id = "myID")
  def <- defData(def, varname = "xNorm", formula = "xUni * 2", dist = "normal", variance = 8)
  dt <- genData(1000, def)

  defs <- data.table(varname = c("A", "B", "C"), formula = c(1, 2, 0),
    dist = c("normal", "normal", "normal"), variance = c(1, 1, 3),
    link = c("identity", "identity", "idendity"))
  result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
  obs_matrix <- cor(result[, .SD, .SDcols = c("A", "B", "C")])
  cor_matrix <- genCorMat(3, rho = .4)
  expect_equal(cor_matrix, obs_matrix, tolerance = .1, check.attributes = FALSE)
})

test_that("addCorFlex works with multiple variables of different distributions", {
  dt <- genData(10)
  defs <- data.table(
    varname = c("norm_var", "bin_var", "pois_var"),
    formula = c("0", ".5", "1"),
    dist = c("normal", "binary", "poisson"),
    variance = c(1, 0, 0),
    link = c("identity", "identity", "identity")
  )

  result <- addCorFlex(dt, defs, rho = 0.3)

  expect_s3_class(result, "data.table")
  expect_equal(ncol(result), 4)  # id + 3 variables
  expect_true(all(c("norm_var", "bin_var", "pois_var") %in% names(result)))
})

test_that("addCorFlex preserves original data columns", {

  samp_letter <- sample(letters, 5, replace = TRUE)
  samp_num <- sample(1:1000, 5, replace = TRUE)


  defs <-
    defData(varname = "existing_var", formula = "..samp_letter", dist = "nonrandom") |>
    defData(varname = "numeric_var", formula = "..samp_num", dist = "nonrandom")

  dt <- genData(5, defs)

  defs <- create_test_defs("normal", "new_var")

  result <- addCorFlex(dt, defs, rho = 0.1)

  expect_true(all(c("id", "existing_var", "numeric_var") %in% names(result)))
  expect_equal(result$existing_var, samp_letter)
  expect_equal(result$numeric_var, samp_num)
})

test_that("addCorFlex handles edge cases for correlation coefficients", {
  dt <- genData(5)
  defs <- create_test_defs()

  # Test rho = 0 (no correlation)
  result_zero <- addCorFlex(dt, defs, rho = 0)
  expect_s3_class(result_zero, "data.table")

  # Test rho = 1 (perfect positive correlation)
  result_one <- addCorFlex(dt, defs, rho = 1)
  expect_s3_class(result_one, "data.table")

  # Test rho = -1 (perfect negative correlation)
  result_neg_one <- addCorFlex(dt, defs, rho = -1)
  expect_s3_class(result_neg_one, "data.table")
})

test_that("addCorFlex works with custom environment", {
  dt <- genData(5)
  defs <- create_test_defs()

  # Create custom environment
  custom_env <- new.env()

  result <- addCorFlex(dt, defs, rho = 0.2, envir = custom_env)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5)
})

test_that("addCorFlex with single variable", {
  dt <- genData(10)
  defs <- create_test_defs("normal", "single_var")

  result <- addCorFlex(dt, defs, rho = 0.5)

  expect_s3_class(result, "data.table")
  expect_equal(ncol(result), 2)  # id + single_var
  expect_true("single_var" %in% names(result))
})

# test_that("addCorFlex generates data with specified correlation matrix", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   cor_matrix <- matrix(c(1, .5, .5, 1), nrow = 2)
#   result <- addCorFlex(dt, defs, corMatrix = cor_matrix)
#   generated_cor_matrix <- cor(result[, .SD, .SDcols = -1])
#   expect_true(all(abs(generated_cor_matrix[upper.tri(generated_cor_matrix)]) > .4))
# })

# test_that("addCorFlex handles tau parameter correctly", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, tau = .3, corstr = "cs")
#   cor_matrix <- cor(result[, .SD, .SDcols = -1])
#   expect_true(all(abs(cor_matrix[upper.tri(cor_matrix)]) > .2))
# })

# test_that("addCorFlex generates data with correct column names", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_true(all(c("A", "B") %in% names(result)))
# })

# test_that("addCorFlex maintains original data columns", {
#   dt <- data.table(id = 1:10, original_col = rnorm(10))
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_true("original_col" %in% names(result))
# })

# test_that("addCorFlex handles empty data table", {
#   dt <- data.table()
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_equal(nrow(result), )
#   expect_equal(ncol(result), 2)
# })

# test_that("addCorFlex handles single row data table", {
#   dt <- data.table(id = 1)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_equal(nrow(result), 1)
#   expect_equal(ncol(result), 3)
# })

# test_that("addCorFlex handles single variable definition", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = "A", formula = "1", dist = "normal", variance = 1)
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_equal(nrow(result), 10)
#   expect_equal(ncol(result), 2)
#   expect_true("A" %in% names(result))
# })

# genCorGen <-----

test_that("genCorGen handles invalid distribution", {
  expect_error(genCorGen(
      100, nvars = 3, params1 = 5, dist = "invalid", rho = .7,
      corstr = "cs"),
    "Distribution not properly specified.")
})

test_that("genCorGen handles non-numeric params1", {
  expect_error(genCorGen(100, nvars = 3, params1 = "non-numeric",
                         dist = "poisson", rho = .7, corstr = "cs"),
               "Parameters must be numeric")
})

test_that("genCorGen handles non-numeric params2", {
  expect_error(genCorGen(100, nvars = 3, params1 = 5, params2 = "non-numeric", dist = "gamma", rho = .7, corstr = "cs"),
               "Parameters must be numeric")
})

test_that("genCorGen handles too many parameter vectors for poisson", {
  expect_error(genCorGen(100, nvars = 3, params1 = 5, params2 = 2, dist = "poisson", rho = .7, corstr = "cs"),
               "Too many parameter vectors")
})

test_that("genCorGen handles too few parameter vectors for gamma", {
  expect_error(genCorGen(100, nvars = 3, params1 = 5, dist = "gamma", rho = .7, corstr = "cs"),
               "Too few parameter vectors")
})

test_that("genCorGen handles mismatched length of params1", {
  expect_error(genCorGen(100, nvars = 3, params1 = c(5, 6), dist = "poisson", rho = .7, corstr = "cs"),
               "Length of vector 1 = 2, not equal to number of correlated variables: 3")
})

test_that("genCorGen handles mismatched length of params2", {
  expect_error(genCorGen(100, nvars = 3, params1 = 5, params2 = c(2, 3), dist = "gamma", rho = .7, corstr = "cs"),
               "Length of vector 2 = 2, not equal to number of correlated variables: 3")
})

test_that("genCorGen handles invalid method", {
  expect_error(genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", method = "invalid"),
               "invalid is not a valid method")
})

test_that("genCorGen handles method ep for non-binary data", {
  expect_error(genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", method = "ep"),
               "Method `ep` applies only to binary data generation")
})

test_that("genCorGen generates data with correct dimensions", {
  result <- genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs")
  expect_equal(nrow(result), 300)
  expect_equal(ncol(result), 3)
})

test_that("genCorGen generates data in wide format", {
  result <- genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
  expect_equal(nrow(result), 100)
  expect_equal(ncol(result), 4)
})

test_that("genCorGen assigns custom column names", {
  result <- genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE, cnames = "a, b, c")
  expect_equal(names(result), c("id", "a", "b", "c"))
})

test_that("genCorGen assigns custom column names when wide is not TRUE", {
  result <- genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", cnames = "x")
  expect_equal(names(result), c("id", "period", "x"))
})

test_that("genCorGen assigns custom id name", {
  result <- genCorGen(100, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", idname = "custom_id")
  expect_true("custom_id" %in% names(result))
})

test_that("genCorGen generates data with specified correlation structure", {
  result <- genCorGen(1000, nvars = 3, params1 = 5, dist = "poisson", rho = .7, corstr = "cs", wide = TRUE)
  cor_matrix <- cor(result[, .SD, .SDcols = -1])
  expect_equal(cor_matrix, genCorMat(3, rep(.7, 3)), tolerance = .1, check.attributes = FALSE)
})

test_that("genCorGen generates data with specified correlation matrix", {
  cor_matrix <- genCorMat(3)
  result <- genCorGen(1000, nvars = 3, params1 = 5, dist = "poisson", corMatrix = cor_matrix, wide = TRUE)
  obs_cor <- cor(result[, .SD, .SDcols = -1])
  expect_equal(cor_matrix, obs_cor, tolerance = .1, check.attributes = FALSE)
})

test_that("number of parameters adjusted", {
  cor_matrix <- genCorMat(3)
  result <- genCorGen(1000, nvars = 3, params1 = c(5, 2, 1), params2 = 4, dist = "normal",
      corMatrix = cor_matrix, wide = TRUE)
  obs_cor <- cor(result[, .SD, .SDcols = -1])
  expect_equal(cor_matrix, obs_cor, tolerance = .15, check.attributes = FALSE)
})

test_that("All distributions work", {
  means <- c(.3, .2, .1)
  dd <- genCorGen(1000, nvars = 3, params1 = means, dist = "binary",
            rho = .3, corstr = "cs", wide = TRUE)
  obs_mean <- apply(dd[,-1], 2, mean)
  expect_equal(obs_mean, means, tolerance = .1, check.attributes = FALSE)

  lower <- c(4, 2, 6)
  upper <- c(11, 11, 11)
  averages <- (lower + upper) / 2

  dd <- genCorGen(1000, nvars = 3, params1 = lower, params2= upper,
                  dist = "uniform",
                  rho = .3, corstr = "cs", wide = TRUE)
  obs_mean <- apply(dd[,-1], 2, mean)
  expect_equal(obs_mean, averages, tolerance = .1, check.attributes = FALSE)

  means <- c(4, 6, 1)
  dd <- genCorGen(1000, nvars = 3, params1 = means, params2= 1,
                  dist = "negBinomial",
                  rho = .3, corstr = "cs", wide = TRUE)
  obs_mean <- apply(dd[,-1], 2, mean)
  expect_equal(obs_mean, means, tolerance = .1, check.attributes = FALSE)

  means <- c(4, 6, 1)
  dd <- genCorGen(1000, nvars = 3, params1 = means, params2= 1,
                  dist = "gamma",
                  rho = .3, corstr = "cs", wide = TRUE)
  obs_mean <- apply(dd[,-1], 2, mean)
  expect_equal(obs_mean, means, tolerance = .1, check.attributes = FALSE)

})

test_that("ep method work", {
  means <- c(.3, .2, .1)
  rho <- .3
  dd <- genCorGen(1000, nvars = 3, params1 = means, dist = "binary",
                  rho = rho, corstr = "cs", wide = TRUE, method = "ep")
  obs_mean <- apply(dd[,-1], 2, mean)
  obs_cor <- cor(dd[, .SD, .SDcols = -1])
  cor_matrix <- genCorMat(3, rho = rho)

  expect_equal(obs_mean, means, tolerance = .1, check.attributes = FALSE)
  expect_equal(obs_cor, cor_matrix, tolerance = .1, check.attributes = FALSE)

})

#### addCorGen

# Test suite for addCorGen function
# This assumes you're using testthat framework

# Helper function to create sample data
create_sample_data <- function(n = 10, grouped = FALSE) {
  if (!grouped) {
    # Ungrouped data (one row per id)
    dt <- data.table(
      id = 1:n,
      mu = runif(n, 1, 5),
      sigma = runif(n, 0.5, 2),
      prob = runif(n, 0.2, 0.8),
      lambda = runif(n, 1, 3),
      min_val = runif(n, 0, 1),
      max_val = runif(n, 2, 5)
    )
  } else {
    # Grouped data (multiple rows per id)
    cluster_sizes <- sample(2:5, n, replace = TRUE)
    dt <- data.table(
      id = rep(1:n, cluster_sizes),
      mu = rep(runif(n, 1, 5), cluster_sizes),
      sigma = rep(runif(n, 0.5, 2), cluster_sizes),
      prob = rep(runif(n, 0.2, 0.8), cluster_sizes),
      lambda = rep(runif(n, 1, 3), cluster_sizes)
    )
  }
  return(dt)
}

# Test 1: Basic functionality tests
test_that("addCorGen basic functionality works", {
  dt <- create_sample_data(5)

  # Test normal distribution
  result <- addCorGen(
    dtOld = dt,
    idvar = "id",
    nvars = 3,
    rho = 0.5,
    corstr = "cs",
    dist = "normal",
    param1 = "mu",
    param2 = "sigma"
  )

  expect_true(is.data.table(result))
  expect_equal(nrow(result), nrow(dt))
  expect_equal(ncol(result), ncol(dt) + 3)
  expect_true(all(c("V1", "V2", "V3") %in% names(result)))
})

# Test 2: Argument validation tests
test_that("addCorGen validates arguments correctly", {
  dt <- create_sample_data(5)

  # Missing required arguments
  expect_error(addCorGen(idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu"))
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", param1 = "mu"))
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal"))

  # Invalid distribution
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "invalid", param1 = "mu"))

  # Invalid method
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma", method = "invalid"))

  # Non-data.table input
  expect_error(addCorGen(dtOld = data.frame(id = 1:5, mu = 1:5), idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu"))

  # Missing columns
  expect_error(addCorGen(dtOld = dt, idvar = "missing_col", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma"))
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "missing_param", param2 = "sigma"))
})

# Test 3: Parameter validation for different distributions
test_that("addCorGen validates parameters for different distributions", {
  dt <- create_sample_data(5)

  # Too many parameters for poisson
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "poisson", param1 = "lambda", param2 = "sigma"))

  # Too many parameters for binary
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "binary", param1 = "prob", param2 = "sigma"))

  # Too few parameters for gamma
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "gamma", param1 = "mu"))

  # Too few parameters for normal
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu"))

  # EP method only for binary
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma", method = "ep"))
})

# Test 4: Wide format (ungrouped data) tests
test_that("addCorGen works with wide format (ungrouped data)", {
  dt <- create_sample_data(5)

  # Test all distributions with wide format
  # Poisson
  result_pois <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "poisson", param1 = "lambda")
  expect_equal(nrow(result_pois), nrow(dt))
  expect_equal(ncol(result_pois), ncol(dt) + 3)

  # Binary
  result_bin <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "binary", param1 = "prob")
  expect_equal(nrow(result_bin), nrow(dt))
  expect_true(all(result_bin$V1 %in% c(0, 1)))

  # Gamma
  result_gamma <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "gamma", param1 = "mu", param2 = "sigma")
  expect_equal(nrow(result_gamma), nrow(dt))
  expect_true(all(result_gamma$V1 > 0))

  # Uniform
  result_unif <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "uniform", param1 = "min_val", param2 = "max_val")
  expect_equal(nrow(result_unif), nrow(dt))

  # Normal
  result_norm <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(nrow(result_norm), nrow(dt))
})

# Test 5: Long format (grouped data) tests
test_that("addCorGen works with long format (grouped data)", {
  dt <- create_sample_data(5, grouped = TRUE)

  # Test with grouped data
  result <- addCorGen(dtOld = dt, idvar = "id", rho = 0.6, corstr = "ar1", dist = "poisson", param1 = "lambda")

  expect_equal(nrow(result), nrow(dt))
  expect_equal(ncol(result), ncol(dt) + 1)
  expect_true("X" %in% names(result))

  # Check that each group has correlated values
  cluster_counts <- dt[, .N, by = id]
  result_counts <- result[, .N, by = id]
  expect_equal(cluster_counts$N, result_counts$N)
})

# Test 6: Correlation structure tests
test_that("addCorGen works with different correlation structures", {
  dt <- create_sample_data(5)

  # Compound symmetry
  result_cs <- addCorGen(dtOld = dt, idvar = "id", nvars = 4, rho = 0.3, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(ncol(result_cs), ncol(dt) + 4)

  # Autoregressive
  result_ar1 <- addCorGen(dtOld = dt, idvar = "id", nvars = 4, rho = 0.3, corstr = "ar1", dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(ncol(result_ar1), ncol(dt) + 4)
})

# Test 7: Custom correlation matrix tests
test_that("addCorGen works with custom correlation matrices", {
  dt <- create_sample_data(5)

  # Single correlation matrix for wide format
  corMat <- matrix(c(1, 0.5, 0.3,
                     0.5, 1, 0.2,
                     0.3, 0.2, 1), nrow = 3)

  result <- addCorGen(dtOld = dt, idvar = "id", corMatrix = corMat, dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(ncol(result), ncol(dt) + 3)

  # # Test with grouped data and single correlation matrix
  dt_grouped <- create_sample_data(3, grouped = TRUE)
  # Convert to grouped format with same cluster size


  # corMat_grouped <- matrix(c(1, 0.5, 0.3,
  #                            0.5, 1, 0.2,
  #                            0.3, 0.2, 1), nrow = 3)
  # 
  # result <- addCorGen(dtOld = dt_grouped, idvar = "id", corMatrix = corMat_grouped,
  #                     dist = "normal", param1 = "mu", param2 = "sigma")
  # expect_equal(ncol(result), ncol(dt_grouped) + 1)

  # This should work for grouped data with equal cluster sizes
  # Note: You might need to adjust this test based on your actual data structure
})

# Test 7a: List of correlation matrices for grouped data with varying cluster sizes
test_that("addCorGen works with list of correlation matrices for varying cluster sizes", {
  # Create grouped data with different cluster sizes
  cluster_sizes <- c(2, 3, 4)  # Different sizes for each cluster
  dt_varying <- data.table(
    id = rep(1:3, cluster_sizes),
    mu = rep(runif(3, 1, 5), cluster_sizes),
    sigma = rep(runif(3, 0.5, 2), cluster_sizes),
    lambda = rep(runif(3, 1, 3), cluster_sizes),
    prob = rep(runif(3, 0.2, 0.8), cluster_sizes)
  )

  # Create list of correlation matrices matching cluster sizes
  corMat_list <- list(
    # 2x2 matrix for cluster 1 (size 2)
    matrix(c(1, 0.6, 0.6, 1), nrow = 2),
    # 3x3 matrix for cluster 2 (size 3)
    matrix(c(1, 0.5, 0.3,
             0.5, 1, 0.4,
             0.3, 0.4, 1), nrow = 3),
    # 4x4 matrix for cluster 3 (size 4)
    matrix(c(1, 0.4, 0.3, 0.2,
             0.4, 1, 0.5, 0.3,
             0.3, 0.5, 1, 0.4,
             0.2, 0.3, 0.4, 1), nrow = 4)
  )

  # Test with list of correlation matrices
  result_list <- addCorGen(
    dtOld = dt_varying,
    idvar = "id",
    corMatrix = corMat_list,
    dist = "poisson",
    param1 = "lambda"
  )

  expect_equal(nrow(result_list), nrow(dt_varying))
  expect_equal(ncol(result_list), ncol(dt_varying) + 1)
  expect_true("X" %in% names(result_list))

  # Check that each cluster has the expected number of observations
  cluster_counts <- dt_varying[, .N, by = id]
  result_counts <- result_list[, .N, by = id]
  expect_equal(cluster_counts$N, result_counts$N)
  expect_equal(cluster_counts$N, cluster_sizes)
})

# Test 8: Custom column names tests
test_that("addCorGen works with custom column names", {
  dt <- create_sample_data(5)

  # Wide format with custom names
  result_wide <- addCorGen(
    dtOld = dt,
    idvar = "id",
    nvars = 3,
    rho = 0.4,
    corstr = "cs",
    dist = "normal",
    param1 = "mu",
    param2 = "sigma",
    cnames = "var1, var2, var3"
  )

  expect_true(all(c("var1", "var2", "var3") %in% names(result_wide)))

  # Long format with custom name
  dt_grouped <- create_sample_data(5, grouped = TRUE)
  result_long <- addCorGen(
    dtOld = dt_grouped,
    idvar = "id",
    rho = 0.4,
    corstr = "cs",
    dist = "poisson",
    param1 = "lambda",
    cnames = "custom_var"
  )

  expect_true("custom_var" %in% names(result_long))
})

# Test 9: Custom column names validation
test_that("addCorGen validates custom column names correctly", {
  dt <- create_sample_data(5)
  dt_grouped <- create_sample_data(5, grouped = TRUE)

  # Wrong number of names for wide format
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.4, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma", cnames = "var1, var2"))

  # Too many names for long format
  expect_error(addCorGen(dtOld = dt_grouped, idvar = "id", rho = 0.4, corstr = "cs", dist = "poisson", param1 = "lambda", cnames = "var1, var2"))
})

# Test 10: Method-specific tests
test_that("addCorGen EP method works correctly", {
  dt <- create_sample_data(5)

  # EP method only works with binary
  result_ep <- addCorGen(
    dtOld = dt,
    idvar = "id",
    nvars = 3,
    rho = 0.4,
    corstr = "cs",
    dist = "binary",
    param1 = "prob",
    method = "ep"
  )

  expect_equal(nrow(result_ep), nrow(dt))
  expect_true(all(result_ep$V1 %in% c(0, 1)))
  expect_true(all(result_ep$V2 %in% c(0, 1)))
  expect_true(all(result_ep$V3 %in% c(0, 1)))
})

# Test 11: Edge cases and boundary conditions
test_that("addCorGen handles edge cases", {
  dt <- create_sample_data(2)

  # Minimum nvars
  result_min <- addCorGen(dtOld = dt, idvar = "id", nvars = 2, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(ncol(result_min), ncol(dt) + 2)

  # High correlation
  result_high_cor <- addCorGen(dtOld = dt, idvar = "id", nvars = 2, rho = 0.99, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(nrow(result_high_cor), nrow(dt))

  # Negative correlation
  result_neg_cor <- addCorGen(dtOld = dt, idvar = "id", nvars = 2, rho = -0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")
  expect_equal(nrow(result_neg_cor), nrow(dt))
})

# Test 12: Missing correlation parameters for wide format
test_that("addCorGen requires correlation parameters for wide format", {
  dt <- create_sample_data(5)

  # Missing nvars, rho, corstr and no corMatrix
  expect_error(addCorGen(dtOld = dt, idvar = "id", dist = "normal", param1 = "mu", param2 = "sigma"))

  # Missing some correlation parameters
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, dist = "normal", param1 = "mu", param2 = "sigma"))
  expect_error(addCorGen(dtOld = dt, idvar = "id", nvars = 3, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma"))
  expect_error(addCorGen(dtOld = dt, idvar = "id", rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma"))
})

# Test 13: Missing correlation parameters for long format
test_that("addCorGen requires correlation parameters for long format", {
  dt <- create_sample_data(5, grouped = TRUE)

  # Missing rho, corstr and no corMatrix
  expect_error(addCorGen(dtOld = dt, idvar = "id", dist = "poisson", param1 = "lambda"))

  # Missing some correlation parameters
  expect_error(addCorGen(dtOld = dt, idvar = "id", rho = 0.5, dist = "poisson", param1 = "lambda"))
  expect_error(addCorGen(dtOld = dt, idvar = "id", corstr = "cs", dist = "poisson", param1 = "lambda"))
})

# Test 14: Data integrity tests
test_that("addCorGen preserves original data", {
  dt <- create_sample_data(5)
  original_cols <- names(dt)

  result <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")

  # Original columns should be preserved
  expect_true(all(original_cols %in% names(result)))

  # Original values should be unchanged
  for (col in original_cols) {
    expect_equal(dt[[col]], result[[col]])
  }
})

# Test 15: Return type and structure tests
test_that("addCorGen returns correct data structure", {
  dt <- create_sample_data(5)

  result <- addCorGen(dtOld = dt, idvar = "id", nvars = 3, rho = 0.5, corstr = "cs", dist = "normal", param1 = "mu", param2 = "sigma")

  # Should return data.table
  expect_s3_class(result, "data.table")

  # Should have correct number of rows
  expect_equal(nrow(result), nrow(dt))

  # Should have additional columns
  expect_gt(ncol(result), ncol(dt))
})

# Test 16: negBinomial distribution test
test_that("addCorGen works with negBinomial distribution", {
  dt <- create_sample_data(5)

  result <- addCorGen(
    dtOld = dt,
    idvar = "id",
    nvars = 2,
    rho = 0.5,
    corstr = "cs",
    dist = "negBinomial",
    param1 = "mu",
    param2 = "sigma"
  )

  expect_equal(nrow(result), nrow(dt))
  expect_equal(ncol(result), ncol(dt) + 2)
  expect_true(all(result$V1 >= 0))
  expect_true(all(result$V2 >= 0))
})

####

# Some extra

test_that("addCorGen correlation matrix dimension validation errors", {
  skip_on_cran()

  # Test 1: List of correlation matrices with wrong dimensions
  # Create grouped data with specific cluster sizes
  cluster_sizes <- c(3, 2, 4)
  dt_varying <- data.table(
    id = rep(1:3, cluster_sizes),
    lambda = rep(runif(3, 1, 3), cluster_sizes),
    mu = rep(runif(3, 1, 5), cluster_sizes),
    sigma = rep(runif(3, 0.5, 2), cluster_sizes)
  )

  # Create list of correlation matrices with WRONG dimensions
  corMat_list_wrong <- list(
    # Should be 3x3 for cluster 1 (size 3), but provide 2x2
    matrix(c(1, 0.6, 0.6, 1), nrow = 2),
    # Should be 2x2 for cluster 2 (size 2), but provide 3x3
    matrix(c(1, 0.5, 0.3,
             0.5, 1, 0.4,
             0.3, 0.4, 1), nrow = 3),
    # Should be 4x4 for cluster 3 (size 4), but provide 2x2
    matrix(c(1, 0.4, 0.4, 1), nrow = 2)
  )

  # This should trigger: "Dimensions of correlation matrices in corMatrix not equal to cluster sizes!"
  expect_error(
    addCorGen(
      dtOld = dt_varying,
      idvar = "id",
      corMatrix = corMat_list_wrong,
      dist = "poisson",
      param1 = "lambda"
    ),
    "Dimensions of correlation matrices in corMatrix not equal to cluster sizes!"
  )

  # Test 2: Single correlation matrix with wrong dimensions for grouped data
  # Create grouped data where all clusters have the same size
  dt_same_size <- data.table(
    id = rep(1:3, each = 3),  # 3 clusters, each with 3 observations
    lambda = rep(runif(3, 1, 3), each = 3),
    mu = rep(runif(3, 1, 5), each = 3)
  )

  # Provide a correlation matrix with wrong dimensions (2x2 instead of 3x3)
  corMat_wrong <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

  # This should trigger: "Dimensions of corMatrix not equal to cluster sizes!"
  expect_error(
    addCorGen(
      dtOld = dt_same_size,
      idvar = "id",
      corMatrix = corMat_wrong,
      dist = "poisson",
      param1 = "lambda"
    ),
    "Dimensions of corMatrix not equal to cluster sizes!"
  )
})

#
test_that("addCorGen grouped data with different nvars scenarios", {
  skip_on_cran()

  # Test the case where grouped data has different cluster sizes (same_nvar = FALSE)
  # This should trigger the "else" branch in the genCorMat call

  cluster_sizes <- c(2, 3, 4, 2)  # Different sizes
  dt_different <- data.table(
    id = rep(1:4, cluster_sizes),
    lambda = rep(runif(4, 1, 3), cluster_sizes),
    mu = rep(runif(4, 1, 5), cluster_sizes),
    sigma = rep(runif(4, 0.5, 2), cluster_sizes)
  )

  # Test with rho and corstr (no corMatrix provided)
  # This should use the different cluster sizes path
  expect_silent(result1 <- addCorGen(
    dtOld = dt_different,
    idvar = "id",
    rho = 0.6,
    corstr = "cs",
    dist = "poisson",
    param1 = "lambda"
  ))

  expect_true(is.data.table(result1))
  expect_equal(nrow(result1), nrow(dt_different))
  expect_true("X" %in% names(result1))

  # Verify each cluster has the expected number of observations
  cluster_counts <- dt_different[, .N, by = id]
  result_counts <- result1[, .N, by = id]
  expect_equal(cluster_counts$N, result_counts$N)
  expect_equal(cluster_counts$N, cluster_sizes)

  # Test the same scenario with ar1 correlation structure
  expect_silent(result2 <- addCorGen(
    dtOld = dt_different,
    idvar = "id",
    rho = 0.4,
    corstr = "ar1",
    dist = "normal",
    param1 = "mu",
    param2 = "sigma"
  ))

  expect_true(is.data.table(result2))
  expect_equal(nrow(result2), nrow(dt_different))
})
#
test_that("addCorGen grouped data with same cluster sizes", {
  skip_on_cran()

  # Test the case where grouped data has same cluster sizes (same_nvar = TRUE)
  # This should trigger the "if (same_nvar)" branch

  dt_same <- data.table(
    id = rep(1:4, each = 3),  # 4 clusters, each with exactly 3 observations
    lambda = rep(runif(4, 1, 3), each = 3),
    prob = rep(runif(4, 0.2, 0.8), each = 3)
  )

  # Test with rho and corstr (no corMatrix provided)
  # This should use the same cluster size path: genCorMat(nvars = counts[1], ...)
  expect_silent(result <- addCorGen(
    dtOld = dt_same,
    idvar = "id",
    rho = 0.7,
    corstr = "cs",
    dist = "binary",
    param1 = "prob"
  ))

  expect_true(is.data.table(result))
  expect_equal(nrow(result), nrow(dt_same))
  expect_true("X" %in% names(result))
  expect_true(all(result$X %in% c(0, 1)))

  # Verify all clusters have the same size
  cluster_counts <- dt_same[, .N, by = id]
  expect_true(all(cluster_counts$N == 3))
  result_counts <- result[, .N, by = id]
  expect_equal(cluster_counts$N, result_counts$N)
})

#
#
test_that("addCorGen can generate clustered data with list of cor matrices usin ep", {
  skip_on_cran()

  def <-
    defData(varname = "xbase", formula = .3, variance = .5, dist = "beta") |>
    defData(varname = "n", formula = 3, dist = "noZeroPoisson")

  dd <- genData(5, def, id = "cid")

  cMats <- genCorMat(nvars = dd$n, rho = .5, corstr = "cs", nclusters = nrow(dd))

  dx <- genCluster(dd, "cid", "n", "id")

  ## Specify with nvars, rho, and corstr

  expect_silent(
    addCorGen(
      dtOld = dx, idvar = "cid", corMatrix = cMats,
      dist = "binary", param1 = "xbase", method = "ep"
    )
  )
})
