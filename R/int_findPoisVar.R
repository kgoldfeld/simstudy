#### .findPoisVar ####

# Internal function to find variance associated with ICC
#
# @param j Value of function
# @return inverse of Poisson ICC function

.findPoisVar <- function(j) {
  
  # 'declare' var
  
  y <- NULL
  
  ##
  
  a <- seq(0, 20, by  = 0.01)
  dx <- data.table::data.table(a = a, y = exp(3*a/2) - exp(a/2))
  
  amin <- dx[y <= j][.N, a]
  
  a <- seq(amin, amin + 1e-02, length = 101)
  dx <- data.table::data.table(a = a, y = exp(3*a/2) - exp(a/2))
  
  amin <- dx[y <= j][.N, a]
  
  a <- seq(amin, amin + 1e-04, length = 1001)
  dx <- data.table::data.table(a = a, y = exp(3*a/2) - exp(a/2))
  
  
  return(dx[y <= j][.N, a])
  
}
