library(testthat)
library(hedgehog)
library(simstudy)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

# Set single thread for CI stability. # Added 2025.08.26 to solve CRAN issue
data.table::setDTthreads(1)
# Or detect if in CI and adjust accordingly
if (Sys.getenv("CI") != "") {
  data.table::setDTthreads(1)
} else {
  data.table::setDTthreads(2)
}

test_check("simstudy")
