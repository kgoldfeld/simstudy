#### Cluster group assignment #### Remove this - replaced by genIndfromCluster

#' Internal function called by generate - returns cluster level
#'
#' @param cMethod Method of allocating subjects to treatment group. Can be
#'        equal assingment or variable assignment based on covariate or 
#'        probability. Variable assignment currently works for binary only.
#' @param nIperC Number subjects per cluster
#' @param nClust Number of clusters
#' @return A data.frame column with cluster assignments


genclust <- function(cMethod,nIperC,nClust) {
  
  if (cMethod=="Equal" | is.na(cMethod)) {
    new <- rep(1 : nClust, each = nIperC)  
  } else {
    p = rep(1 / nClust, nClust)
    new <- rep(1 : nClust,
               times = 
                 table(c(t(rmultinom(nClust * nIperC, 1, p = p)) %*% 
                           c(1:nClust))))
  }
  
  return(new)
  
}
