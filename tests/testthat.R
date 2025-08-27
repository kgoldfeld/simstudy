# data.table::setDTthreads(1)  # Added 2025.08.26 to solve CRAN issue

# library(testthat)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

# test_check("simstudy") # original 

# Set threading before any package loading
data.table::setDTthreads(1)

# Load packages individually with error catching
cat("Loading testthat...\n")
library(testthat)

cat("About to load simstudy...\n")
tryCatch({
  library(simstudy)
  cat("simstudy loaded successfully\n")
}, error = function(e) {
  cat("Error loading simstudy:", e$message, "\n")
  quit(status = 1)
})

cat("About to run test_check...\n")
test_check("simstudy")



