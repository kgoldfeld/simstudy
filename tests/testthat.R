data.table::setDTthreads(1)  # Added 2025.08.26 to solve CRAN issue

library(testthat)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

test_check("simstudy") # original 

