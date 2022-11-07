# .checkBoundsBin ----
test_that("Correlation boundaries for binary variables are correct", {
  x1 <- c(0, 0, 0, 1, 0)
  x2 <- c(1, 1, 1, 0, 0)

  p1 <- mean(x1)
  p2 <- mean(x2)

  expect_error(.checkBoundsBin(p1, p2, -1))

  expect_silent(.checkBoundsBin(p1, p2, -0.6))
  expect_silent(.checkBoundsBin(p1, p2, 0.4))
  expect_silent(.checkBoundsBin(p1, p2, cor(x1, x2)))
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


