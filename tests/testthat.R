library(testthat)
library(hedgehog)
library(simstudy)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

# Set single thread for CI stability. # Added 2025.08.26 to solve CRAN issue
data.table::setDTthreads(1)

test_check("simstudy")
