library(testthat)
library(hedgehog)
library(simstudy)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

# Set single thread for CI stability. # Added 2025.08.26 to solve CRAN issue

# data.table::setDTthreads(1)
# test_check("simstudy")

## Debugging

# Set single thread for CI stability
data.table::setDTthreads(1)

# Get all test files
test_files <- list.files("testthat", pattern = "^test.*\\.[rR]$", full.names = TRUE)

cat("Found test files:", paste(test_files, collapse = ", "), "\n")

# Test each file individually
for (test_file in test_files) {
  cat("Running test file:", test_file, "\n")
  tryCatch({
    testthat::test_file(test_file)
    cat("Completed:", test_file, "\n")
  }, error = function(e) {
    cat("Error in", test_file, ":", e$message, "\n")
    stop(e)
  })
}
