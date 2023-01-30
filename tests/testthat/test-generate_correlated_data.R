# .checkBoundsBin ----
test_that("Correlation boundaries for binary variables are correct", {
  
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

test_that("genBlockMat works", {
  
  x <- runif(1, -1, 1)
  y <- sample(1:10, 1)
  z <- sample(2:10, 1)
  c <- sample(c("ind","cs", "ar1"), 3)
  x2 <- runif(1, -1, 1)
  c2 <- sample(c("cs", "ar1"), 2)
  
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z))
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z, corstr = c[1]))
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z, corstr = c[2]))
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z, corstr = c[3]))
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z, corstr = c2[1], iRho = x2))
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z, corstr = c2[2], iRho = x2))
  
  x <- runif(z, -1, 1)
  x2 <- runif((z-1), -1, 1)
  
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z))
  expect_silent(genBlockMat(rho = x, nInds = y, nPeriods = z, iRho = x2))
  
})

test_that("genBlockMat errors correctly.", {
  expect_error(genBlockMat(nInds = 2, nPeriods = 3), class="simstudy::missingArgument")
  expect_error(genBlockMat(rho = 0.6, nPeriods = 3), class="simstudy::missingArgument")
  expect_error(genBlockMat(rho = 0.6, nInds = 3), class="simstudy::missingArgument")
  
  expect_error(genBlockMat(rho = 0.6, nInds = 3.5, nPeriods = 3), class = "simstudy::wrongType")

  expect_error(genBlockMat(rho = 0.6, nInds = 3, nPeriods =  1), class = "simstudy::minError")
  expect_error(genBlockMat(rho = 1.4, nInds = 3, nPeriods =  2), class = "simstudy::valueError")
  
  expect_error(genBlockMat(rho = 0.4, nInds = 3, nPeriods =  2, iRho = 1.4), class = "simstudy::valueError")
  expect_error(genBlockMat(rho = 0.4, nInds = 3, nPeriods =  2, iRho = .5, corstr = "ind"), class = "simstudy::equal")
  expect_error(genBlockMat(rho = c(0.6, 1.4, .2), nInds = 2, nPeriods = 3, iRho = c(0.8, 0.5)), class = "simstudy::valueError")
  
  expect_error(genBlockMat(rho = 0.6, nInds = 2, nPeriods = 2, corstr = "ar2"), class = "simstudy::optionInvalid")
})


test_that("genCorMat works", {
  
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

