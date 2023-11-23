library(testthat)
library(hedgehog)
library(simstudy)

data.table::setDTthreads(2) # added to solve CRAN issue
test_check("simstudy")
