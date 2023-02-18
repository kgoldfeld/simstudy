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

