#### Implement Emrich and Piedmonte algorithm for correlated binary data ####

# Internal functions called by genCorGen and addCorGen - returns matrix
#
# @param nvars Number of new variables to generate
# @param corMatrix Correlation matrix
# @param rho Correlation coefficient
# @param corstr Correlation structure
# @return A correlation matrix

.checkBoundsBin <- function(p1, p2, d) {

      l <- (p1*p2) / ((1-p1)*(1-p2))
      L <- max(-sqrt(l), -sqrt(1/l))
      
      u <- (p1*(1-p2)) / (p2*(1-p1))
      U <- min(sqrt(u), sqrt(1/u))  
      
      if ((d < L & isTRUE(all.equal(d, L)) == FALSE) | 
          (d > U & isTRUE(all.equal(d, U)) == FALSE))   {
        LU <- paste0("(", round(L,2) , " ... ", round(U, 2), ")")
        stopText <- paste("Specified correlation", d, "out of range" , LU)
        stop(stopText)
      }
}
  
  
.findRhoBin <- function(p1, p2, d) {
  
  .checkBoundsBin(p1, p2, d)
  
  target <- d*sqrt(p1*p2*(1-p1)*(1-p2)) + p1*p2
  
  # given p1, p2 & d, bisection search for corresponding rho
  
  Max <- 1
  Min <- -1
  test <- 0
  found <- FALSE
  
  while (!found) {
    
    corr <- diag(2)
    corr[1,2] <-  corr[2,1] <- test
    
    est <- mvtnorm::pmvnorm(lower = rep(-Inf, 2), upper = c(stats::qnorm(p1), stats::qnorm(p2)), mean = c(0,0), corr = corr)
    
    if (round(est, 5) == round(target, 5)) {
      found <- TRUE
      rho <- test
    } else if (est < target) {
      Min <- test
      test <- (Min + Max) / 2
    } else {
      Max <- test
      test <- (Min + Max) / 2
    }  
  }
  
  return(rho)  
  
}

.genBinEP <- function(n, p, tcorr) {
  
  # "Declare" vars to avoid R CMD warning
  
  id <- NULL
  period <- NULL
  seqid <- NULL
  
  ###
  
  np <- length(p)  
  phicorr <- diag(length(p))
  
  for (i in (1: (np - 1))) {
    
    for (j in ((i+1):np)) {
      
      p1 <- p[i]
      p2 <- p[j]
      
      phicorr[j, i] <- phicorr[i, j] <- .findRhoBin(p1, p2, tcorr[i, j])
    }
    
  }
  
  # check that phicorr is positive definite (PD), if not adjust to nearest PD matrix
  if (!all(eigen(phicorr)$values > 0)) {
    phicorr <- Matrix::nearPD(phicorr)$mat
    
  }
  
  normvars <- mvnfast::rmvn(n, mu = rep(0, length(p)), sigma = phicorr)
  z <- matrix(rep(stats::qnorm(p), nrow(normvars)), nrow = nrow(normvars), byrow = TRUE)
  binvars <- matrix(as.integer(normvars < z), nrow = nrow(z))
  
  dtX <- data.table(binvars)
  dtX[, id := .I]
  
  dtM <- melt(dtX, id.vars = "id", variable.factor = TRUE, value.name = "X", variable.name = "seq")
  
  dtM[, period := as.integer(seq) - 1]
  setkey(dtM, "id")
  dtM[, seqid := .I]

  return(dtM[])
}
