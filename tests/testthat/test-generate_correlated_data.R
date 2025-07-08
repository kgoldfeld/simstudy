library(testthat)
library(simstudy)
library(data.table)

# .checkBoundsBin ----

test_that("Correlation boundaries for binary variables are correct", {
  skip_on_cran()
  
  p1 <- .5
  p2 <- .8

  expect_error(checkBoundsBin(p1, p2, d = .9))
  expect_error(checkBoundsBin(p1, p2, d = -.6))

  expect_silent(checkBoundsBin(p1, p2, -0.4))
  expect_silent(checkBoundsBin(p1, p2, 0.3))
  expect_silent(checkBoundsBin(p1, p2, 0.2))
})

# .findRhoBin ----

# .genBinEP ----


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

# test_that("addCorFlex generates data with specified correlation matrix", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   cor_matrix <- matrix(c(1, .5, .5, 1), nrow = 2)
#   result <- addCorFlex(dt, defs, corMatrix = cor_matrix)
#   generated_cor_matrix <- cor(result[, .SD, .SDcols = -1])
#   expect_true(all(abs(generated_cor_matrix[upper.tri(generated_cor_matrix)]) > .4))
# })
# 
# test_that("addCorFlex handles tau parameter correctly", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, tau = .3, corstr = "cs")
#   cor_matrix <- cor(result[, .SD, .SDcols = -1])
#   expect_true(all(abs(cor_matrix[upper.tri(cor_matrix)]) > .2))
# })
# 
# test_that("addCorFlex generates data with correct column names", {
#   dt <- data.table(id = 1:10)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_true(all(c("A", "B") %in% names(result)))
# })
# 
# test_that("addCorFlex maintains original data columns", {
#   dt <- data.table(id = 1:10, original_col = rnorm(10))
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_true("original_col" %in% names(result))
# })
# 
# test_that("addCorFlex handles empty data table", {
#   dt <- data.table()
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_equal(nrow(result), )
#   expect_equal(ncol(result), 2)
# })
# 
# test_that("addCorFlex handles single row data table", {
#   dt <- data.table(id = 1)
#   defs <- data.table(varname = c("A", "B"), formula = c("1", "2"), dist = c("normal", "poisson"), variance = c(1, 1))
#   result <- addCorFlex(dt, defs, rho = .4, corstr = "cs")
#   expect_equal(nrow(result), 1)
#   expect_equal(ncol(result), 3)
# })
# 
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
  expect_equal(cor_matrix, obs_cor, tolerance = .1, check.attributes = FALSE)
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


