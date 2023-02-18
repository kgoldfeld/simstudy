context("rmixt()")

test_that("Checking rmixt()", {
  
  set.seed(689968)
  N <- c(1e6, 1e6, 1e6)
  d <- c(1, 2, 3) 
  df <- 20
  
  message("Testing rmixt()")
  for(nMix in 1:3)
  {
    w = rep(1, nMix)
    
    for(ii in 1:length(N))
    {
      mom <- lapply(1:nMix, 
                    function(nouse){
                      mu <- 1:d[ii]
                      X <- t(t(matrix(rnorm(N[ii]*d[ii]), N[ii], d[ii])) + mu)
                      tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
                      mcov <- tcrossprod(tmp, tmp) + diag(0.5, d[ii])
                      myChol <- chol(mcov)
                      return( list("mu" = mu, "mcov" = mcov, "myChol" = myChol) )
                    })
      
      mu <- do.call("rbind", lapply(mom, "[[", "mu"))
      mcov <- lapply(mom, "[[", "mcov")
      myChol <- lapply(mom, "[[", "myChol")
      
      X <- rmixt(N[ii], mu, mcov, df = df, w, retInd = TRUE)
      XC <- rmixt(N[ii], mu, myChol, df = df, w, isChol = TRUE, retInd = TRUE)
      XP <- rmixt(N[ii], mu, mcov, df = df, w, retInd = TRUE, ncores = 2)
      XPC <- rmixt(N[ii], mu, myChol, df, w, isChol = TRUE, retInd = TRUE, ncores = 2)
      
      for(im in 1:nMix){
        
        tolMu <- 0.05 * sum(abs(mu[im, ]))
        tolCov <- 0.05 * sum(sum(abs(mcov[[im]])))
        
        ####### Sequential
        # Using covariance
        tmp <- X[attr(X, "index") == im, , drop = F]
        expect_lt(sum(sum(abs(colMeans(tmp) - mu[im, ]))), tolMu)
        expect_lt(sum(sum(abs(cov(tmp) - mcov[[im]] * (df / (df-2))))), tolCov)
        # Using Cholesky
        tmp <- XC[attr(XC, "index") == im, , drop = F]
        expect_lt(sum(sum(abs(colMeans(tmp) - mu[im, ]))), tolMu)
        expect_lt(sum(sum(abs(cov(tmp) - mcov[[im]] * (df / (df-2))))), tolCov)
        
        ####### Parallel
        # Using covariance
        tmp <- XP[attr(XP, "index") == im, , drop = F]
        expect_lt(sum(sum(abs(colMeans(tmp) - mu[im, ]))), tolMu)
        expect_lt(sum(sum(abs(cov(tmp) - mcov[[im]] * (df / (df-2))))), tolCov)
        # Using Cholesky
        tmp <- XPC[attr(XPC, "index") == im, , drop = F]
        expect_lt(sum(sum(abs(colMeans(tmp) - mu[im, ]))), tolMu)
        expect_lt(sum(sum(abs(cov(tmp) - mcov[[im]] * (df / (df-2))))), tolCov)
        
      }
      
      message(paste("Test with d = ", ii, " and ", nMix, " mixture components passed.", sep = ""))
    }
  }
  
})


test_that("Checking rmixt() with pre-allocated storage", {
  
  set.seed(689968)
  N <- c(1e6, 1e6, 1e6)
  d <- c(1, 2, 3) 
  df <- 20
  
  message("Testing rmixt() with pre-allocated storage")
  for(nMix in 1:3)
  {
    w = rep(1, nMix)
    
    for(ii in 1:length(N))
    {
      mom <- lapply(1:nMix, 
                    function(nouse){
                      mu <- 1:d[ii]
                      X <- t(t(matrix(rnorm(N[ii]*d[ii]), N[ii], d[ii])) + mu)
                      tmp <- matrix(rnorm(d[ii]^2), d[ii], d[ii])
                      mcov <- tcrossprod(tmp, tmp) + diag(0.5, d[ii])
                      myChol <- chol(mcov)
                      return( list("mu" = mu, "mcov" = mcov, "myChol" = myChol) )
                    })
      
      mu <- do.call("rbind", lapply(mom, "[[", "mu"))
      mcov <- lapply(mom, "[[", "mcov")
      myChol <- lapply(mom, "[[", "myChol")
      
      X <- matrix(NA, N[ii], d[ii])
      class(X) <- "numeric"
      
      # Cov + Sequential
      set.seed(5151)
      rmixt(N[ii], mu, mcov, df = df, w, A = X, retInd = TRUE)
      set.seed(5151)
      XBenc <- rmixt(N[ii], mu, mcov, df = df, w, retInd = TRUE)
      
      expect_lt(max(drop(abs(XBenc - X)/abs(XBenc))), 1e-6)
      
      # Chol + sequential
      set.seed(5151)
      rmixt(N[ii], mu, myChol, df, w, A = X, isChol = TRUE, retInd = TRUE)
      set.seed(5151)
      XBenc <- rmixt(N[ii], mu, myChol, df, w, isChol = TRUE, retInd = TRUE)
      
      expect_lt(max(drop(abs(XBenc - X)/abs(XBenc))), 1e-6)
      
      # Cov + parallel
      set.seed(5151)
      rmixt(N[ii], mu, mcov, df = df, w, A = X, retInd = TRUE, ncores = 2)
      set.seed(5151)
      XBenc <- rmixt(N[ii], mu, mcov, df = df, w, retInd = TRUE, ncores = 2)
      
      expect_lt(max(drop(abs(XBenc - X)/abs(XBenc))), 1e-6)
      
      # Chol + parallel
      set.seed(5151)
      rmixt(N[ii], mu, myChol, df, w, A = X, isChol = TRUE, retInd = TRUE, ncores = 2)
      set.seed(5151)
      XBenc <- rmixt(N[ii], mu, myChol, df, w, isChol = TRUE, retInd = TRUE, ncores = 2)
      
      expect_lt(max(drop(abs(XBenc - X)/abs(XBenc))), 1e-6)
      
      message(paste("Preallocated memory: test with d = ", ii, " and ", nMix, " mixture components passed.", sep = ""))
    }
  }
  
})