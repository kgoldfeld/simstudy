context("rmvt()")

test_that("Checking rmvt()", {
  
  set.seed(78970)
  N <- c(1e6, 1e6, 1e6)
  d <- c(1, 2, 3) 
  df <- 20
  
  message("Testing rmvt()")
  for(ii in 1:length(N))
  {
    mu <- 1:d[ii]
    X <- t(t(matrix(rnorm(N[ii]*d[ii]), N[ii], d[ii])) + mu)
    tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
    mcov <- tcrossprod(tmp, tmp) + diag(0.5, d[ii])
    myChol <- chol(mcov)
    trueSIG <- mcov * (df / (df-2))
    
    tolMu <- 0.05 * sum(abs(mu))
    tolCov <- 0.05 * sum(sum(abs(mcov)))
    
    ####### Sequential
    # Using covariance
    X <- rmvt(N[ii], mu, mcov, df = df)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - trueSIG))), tolCov)
    # Using Cholesky
    X <- rmvt(N[ii], mu, myChol, df = df, isChol = TRUE)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - trueSIG))), tolCov)
    
    ####### Parallel
    # Using covariance
    X <- rmvt(N[ii], mu, mcov, df = df, ncores = 2)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - trueSIG))), tolCov)
    # Using Cholesky
    X <- rmvt(N[ii], mu, myChol, df = df, ncores = 2, isChol = TRUE)
    expect_lt(sum(sum(abs(colMeans(X) - mu))), tolMu)
    expect_lt(sum(sum(abs(cov(X) - trueSIG))), tolCov)
    
    message(paste("Test", ii, "passed."))
  }
  
})


test_that("Checking rmvt() with pre-allocated storage", {
  
  set.seed(41244)
  N <- c(1e4, 1e4, 1e4)
  d <- c(1, 2, 3) 
  df <- 20
  
  message("Testing rmvn()")
  for(ii in 1:length(N))
  {
    mu <- 1:d[ii]
    X <- t(t(matrix(rnorm(N[ii]*d[ii]), N[ii], d[ii])) + mu)
    tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
    mcov <- tcrossprod(tmp, tmp) + diag(0.5, d[ii])
    myChol <- chol(mcov)
    A <- matrix(NA, N[ii], d[ii])
    class(A) <- "numeric"
    
    ####### Sequential
    # Using covariance
    set.seed(5151)
    X <- rmvt(N[ii], mu, mcov, df = df)
    set.seed(5151)
    rmvt(N[ii], mu, mcov, df = df, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    # Using Cholesky
    set.seed(5151)
    X <- rmvt(N[ii], mu, myChol, df = df, isChol = TRUE)
    set.seed(5151)
    rmvt(N[ii], mu, myChol, df = df, isChol = TRUE, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    ####### Parallel
    # Using covariance
    set.seed(5151)
    X <- rmvt(N[ii], mu, mcov, df = df, ncores = 2)
    set.seed(5151)
    rmvt(N[ii], mu, mcov, df = df, ncores = 2, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    # Using Cholesky
    set.seed(5151)
    X <- rmvt(N[ii], mu, myChol, df = df, ncores = 2, isChol = TRUE)
    set.seed(5151)
    rmvt(N[ii], mu, myChol, df = df, ncores = 2, isChol = TRUE, A = A)
    
    expect_lt(max(drop(abs(X - A)/abs(X))), 1e-6)
    
    message(paste("Test", ii, "passed."))
  }
  
})


