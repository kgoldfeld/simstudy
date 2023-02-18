context("ms()")

test_that("Checking ms()", {
  
  ##########
  ###### d = 1 case
  ##########  
  
  set.seed(7617)
  mu <- 1
  sigma <- 1
  N <- 10000
  X <- rmvn(N, mu, sigma)
  
  start <- 0
  
  # Sequential
  out1 <- ms(X, init = start, H = 0.1 * sigma, store = TRUE)
  expect_lt(sum(abs(out1$final - mu)), 2e-1)
  expect_equal(drop(head(out1$traj, 1)), start)
  expect_equal(unname(drop(tail(out1$traj, 1))), drop(out1$final))
  
  # Parallel
  out2 <- ms(X, init = start, H = 0.1 * sigma, ncores = 2, store = TRUE)
  expect_equal(out1, out2)
  
  ##########
  ###### d = 2 case
  ##########  
  
  mu <- c(1, 2)
  sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  N <- 10000
  X <- rmvn(N, mu, sigma)
  
  start <- c(0, 1)
  
  # Sequential
  out1 <- ms(X, init = start, H = 0.1 * sigma, store = TRUE)
  expect_lt(sum(abs(out1$final - mu)), 4e-1)
  expect_equal(drop(head(out1$traj, 1)), start)
  expect_equal(unname(drop(tail(out1$traj, 1))), drop(out1$final))
  
  # Parallel
  out2 <- ms(X, init = start, H = 0.1 * sigma, ncores = 2, store = TRUE)
  expect_equal(out1, out2)

})

