#### Check or create correlation matrix ####

# Internal function called by genCorData and addCorGen - returns matrix
#
# @param nvars Number of new variables to generate
# @param corMatrix Correlation matrix
# @param rho Correlation coefficient
# @param corstr Correlation structure
# @return A correlation matrix

.buildCorMat <- function(nvars, corMatrix, corstr, rho) {
  
  if (is.null(corMatrix)) {
    
    corMatrix <- diag(nvars) # do not modify if indepdendent
    
    if (corstr == "cs") {
      corMatrix <- rho^(row(corMatrix) != col(corMatrix))
    } else if (corstr == "ar1") {
      corMatrix <- rho^abs(row(corMatrix)-col(corMatrix))
    }
    
  } else if (! is.null(corMatrix)) { # check if positive definite/symmetric
    
    if (nvars != length(diag(corMatrix))) {
      stop("Length of mean vector mismatched with correlation matrix")
    }
    
    if (! isSymmetric(corMatrix)) {
      stop("Correlation matrix not symmetric")
    }
    
    if (! all(eigen(corMatrix)$values > 0)) {
      stop("Correlation matrix not positive definite")
    }
    
  }
  
  return(corMatrix)
  
}
