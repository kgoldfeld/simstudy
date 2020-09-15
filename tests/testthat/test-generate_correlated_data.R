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
