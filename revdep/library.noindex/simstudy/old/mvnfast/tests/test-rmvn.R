context("rmvn()")

test_that("Checking rmvn()", {
  
  set.seed(6899)
  N <- c(1e6, 1e6, 1e6)
  d <- c(1, 2, 3) 
  
  message("Testing rmvn()")
  for(ii in 1:length(N))
  {
    mu <- 1:d[ii]
    X <- t(t(matrix(rnorm(N[ii]*d[ii]), N[ii], d[ii])) + mu)
    tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
    mcov <- tcrossprod(tmp, tmp) + diag(0.5, d[ii])
    myChol <- chol(mcov)

    tolMu <- 0.01 * sum(abs(mu))
    tolCov <- 0.01 * sum(sum(abs(mcov)))
        
    ####### Sequential
    # Using covariance
    X <- rmvn(N[ii], mu, mcov)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - mcov))), tolCov)
    # Using Cholesky
    X <- rmvn(N[ii], mu, myChol, isChol = TRUE)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - mcov))), tolCov)
    
    ####### Parallel
    # Using covariance
    X <- rmvn(N[ii], mu, mcov, ncores = 2)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - mcov))), tolCov)
    # Using Cholesky
    X <- rmvn(N[ii], mu, myChol, ncores = 2, isChol = TRUE)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - mcov))), tolCov)
 
    message(paste("Test", ii, "passed."))
  }
  
})


test_that("Checking rmvn() with pre-allocated storage", {
  
  set.seed(6899)
  N <- c(1e4, 1e4, 1e4)
  d <- c(1, 2, 3) 
  
  message("Testing rmvn()")
  for(ii in 1:length(N))
  {
    mu <- 1:d[ii]
    X <- t(t(matrix(rnorm(N[ii]*d[ii]), N[ii], d[ii])) + mu)
    tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
    mcov <- tcrossprod(tmp, tmp)  + diag(0.5, d[ii])
    myChol <- chol(mcov)
    A <- matrix(NA, N[ii], d[ii])
    class(A) <- "numeric"
    
    ####### Sequential
    # Using covariance
    set.seed(5151)
    X <- rmvn(N[ii], mu, mcov)
    set.seed(5151)
    rmvn(N[ii], mu, mcov, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    # Using Cholesky
    set.seed(5151)
    X <- rmvn(N[ii], mu, myChol, isChol = TRUE)
    set.seed(5151)
    rmvn(N[ii], mu, myChol, isChol = TRUE, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    ####### Parallel
    # Using covariance
    set.seed(5151)
    X <- rmvn(N[ii], mu, mcov, ncores = 2)
    set.seed(5151)
    rmvn(N[ii], mu, mcov, ncores = 2, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    # Using Cholesky
    set.seed(5151)
    X <- rmvn(N[ii], mu, myChol, ncores = 2, isChol = TRUE)
    set.seed(5151)
    rmvn(N[ii], mu, myChol, ncores = 2, isChol = TRUE, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    message(paste("Test", ii, "passed."))
  }
  
})


