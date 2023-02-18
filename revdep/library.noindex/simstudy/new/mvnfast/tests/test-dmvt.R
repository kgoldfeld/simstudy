context("dmvt()")

test_that("Checking dmvt() against mvtnorm::dmvt()", {
  library("mvtnorm")
  
  set.seed(666)
  N <- c(1, 100, 1, 100)
  d <- c(1, 1,   10, 10) 
  df = 4
  
  message("Testing dmvn() and maha()")
  for(ii in 1:length(N))
  {
    mu <- 1:d[ii]
    tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
    mcov <- tcrossprod(tmp, tmp) + diag(0.5, d[ii])
    myChol <- chol(mcov)
    X <- rmvnorm(N[ii], mu, mcov)
    
    ##### dmvt()
    bench <- mvtnorm::dmvt(X, delta = mu, sigma = mcov, df = df, log = T)
    # Sequential
    expect_lt(sum(abs(mvnfast::dmvt(X, mu, mcov, df = df, log = T) - bench)), 1e-6)
    expect_lt(sum(abs(mvnfast::dmvt(X, mu, myChol, df = df, isChol = TRUE, log = T) - bench)), 1e-6)
    # Parallel
    expect_lt(sum(abs(mvnfast::dmvt(X, mu, mcov, df = df, ncores = 2, log = T) - bench)), 1e-6)
    expect_lt(sum(abs(mvnfast::dmvt(X, mu, myChol, df = df, ncores = 2, isChol = TRUE, log = T) - bench)), 1e-6)
    
    message(paste("Test", ii, "passed."))
  }
  
  detach("package:mvtnorm", unload=TRUE)
  
})