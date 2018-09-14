#' Create a correlation matrix
#'
#' @param nvars number of rows and columns (i.e. number of variables) for correlation matrix
#' @param cors vector of correlations. 
#' @return correlation matrix of size nvars x nvars
#' @details If the vector cors is not specified, a random correlation matrix is generated with no assumptions.
#' If the vector is provided, it should be interpreted as the lower triangle of the correlation
#' matrix, and is specified by reading down the columns. For example, if CM is the correlation matrix and 
#' nvars = 3, then CM[2,1] = cors[1],  CM[3,1] = cors[2], and CM[3,2] = cors[3].
#' @examples
#' genCorMat(3, c(.3, -.2, .1))
#' genCorMat(3)
#' 
#' genCorMat(5, c(.3, -.2, .1, .2, .5, .2, -.1, .3, .1, .2))
#' genCorMat(5)
#' @export
#'

genCorMat <- function(nvars, cors = NULL) {
  
  if (is.null(cors)) {
    
    ev = stats::runif(nvars, 0, 10)
    
    Z <- matrix(ncol=nvars, stats::rnorm(nvars^2))
    
    decomp <- qr(Z)
    Q <- qr.Q(decomp) 
    R <- qr.R(decomp)
    d <- diag(R)
    ph <- d / abs(d)
    O <- Q %*% diag(ph)
    Z <- t(O) %*% diag(ev) %*% O
    
    cm <- stats::cov2cor(Z)
    
  } else {
    
    if (choose(nvars, 2) != length(cors)) stop("Correlations improperly specified")
    
    cmLower <- matrix(0, nrow = nvars, ncol = nvars)
    cmLower[lower.tri(cmLower)] <- cors
    cmUpper <- t(cmLower)
    
    cm <- cmLower + cmUpper
    
    diag(cm) <- 1
    
    if (!all(eigen(cm)$values > 0)) stop("Not a positive definite matrix")  
    
  }
  
  return(cm)
  
}
