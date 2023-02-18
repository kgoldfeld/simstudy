context("dmixt()")

test_that("Checking dmixt()", {
  
  bDens <- function(X, mu, sigma, df, w, log)
  {
    n <- nrow(X)
    m <- length(sigma)
    
    out <- numeric(n)
    for( ii in 1:m ){
      out <- out + w[ii] * dmvt(X, drop(mu[ii, ]), sigma[[ii]], df = df, log = FALSE)
    }
    
    if(log) out <- log(out)
    
    return(out)
  }
  
  set.seed(4141)
  N <- c(1e4, 1e4, 1e4)
  d <- c(1, 2, 3) 
  df = 5

  message("Testing dmixt()")
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
      
      X <- rmixn(N[ii], mu, mcov, w)
      
      bench <- bDens(X, mu, mcov, df = df, w = w, log = TRUE)
      
      # Seq + cov
      tmp <- dmixt(X, mu, mcov, df = df, w = w, log = TRUE)
      expect_lt(sum(abs(tmp - bench)), 1e-6)
      
      # Seq + Chol
      tmp <- dmixt(X, mu, myChol, df = df, w = w, log = TRUE, isChol = TRUE)
      expect_lt(sum(abs(tmp - bench)), 1e-6)
      
      # Par + cov
      tmp <- dmixt(X, mu, mcov, df = df, w = w, log = TRUE, ncores = 2)
      expect_lt(sum(abs(tmp - bench)), 1e-6)
      
      # Par + chol
      tmp <- dmixt(X, mu, myChol, df = df, w = w, log = TRUE, ncores = 2, isChol = TRUE)
      expect_lt(sum(abs(tmp - bench)), 1e-6)

      message(paste("Test with d = ", ii, " and ", nMix, " mixture components passed.", sep = ""))
    }
  }
  
})