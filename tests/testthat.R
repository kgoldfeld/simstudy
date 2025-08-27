library(testthat)
library(simstudy)
library(hedgehog)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

# Set single thread for CI stability. # Added 2025.08.26 to solve CRAN issue
# data.table::setDTthreads(1) # if 2 gives an error.

# Also set it after loading
data.table::setDTthreads(2)

test_check("simstudy") # original 

