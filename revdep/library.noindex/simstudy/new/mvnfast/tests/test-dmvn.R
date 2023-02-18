context("dmvn() and maha()")

test_that("Checking dmvn() and maha() against dmvnorm() and mahalanobis", {
  library("mvtnorm")
  
  ##########
  ###### d = 1, n = 1 case
  ##########
  set.seed(4616)
  N <- c(1, 100, 1, 100)
  d <- c(1, 1,   10, 10) 
  
  message("Testing dmvn() and maha()")
  for(ii in 1:length(N))
  {
    mu <- 1:d[ii]
    tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
    mcov <- tcrossprod(tmp, tmp)
    myChol <- chol(mcov)
    X <- rmvnorm(N[ii], mu, mcov)
    
    ##### dmvn()
    bench <- dmvnorm(X, mu, mcov, log = T)
    # Sequential
    expect_lt(sum(abs(dmvn(X, mu, mcov, log = T) - bench)), 1e-6)
    expect_lt(sum(abs(dmvn(X, mu, myChol, isChol = TRUE, log = T) - bench)), 1e-6)
    # Parallel
    expect_lt(sum(abs(dmvn(X, mu, mcov, ncores = 2, log = T) - bench)), 1e-6)
    expect_lt(sum(abs(dmvn(X, mu, myChol, ncores = 2, isChol = TRUE, log = T) - bench)), 1e-6)
    
    ##### maha()
    bench <- mahalanobis(X, mu, mcov)
    # Sequential
    expect_lt(sum(abs(maha(X, mu, mcov) - bench)), 1e-6)
    expect_lt(sum(abs(maha(X, mu, myChol, isChol = TRUE) - bench)), 1e-6)
    # Parallel
    expect_lt(sum(abs(maha(X, mu, mcov, ncores = 2) - bench)), 1e-6)
    expect_lt(sum(abs(maha(X, mu, myChol, ncores = 2, isChol = TRUE) - bench)), 1e-6)
    
    message(paste("Test", ii, "passed."))
  }
  
  detach("package:mvtnorm", unload=TRUE)
  
})