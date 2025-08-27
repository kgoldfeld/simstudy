library(testthat)
library(hedgehog)

# data.table::setDTthreads(2) # added before 2025.08.26 to solve CRAN issue

# Set single thread for CI stability. # Added 2025.08.26 to solve CRAN issue
# data.table::setDTthreads(1) # if 2 gives an error.

# Set threading based on environment ... Added 2025.08.26 to solve CRAN issue

# if (identical(Sys.getenv("CI"), "true") || 
#     identical(Sys.getenv("GITHUB_ACTIONS"), "true") ||
#     Sys.info()["sysname"] == "Darwin") {
#   # Single thread for CI or macOS to avoid C++ initialization issues
#   data.table::setDTthreads(1)
# } else {
#   # Multi-threading for local development
#   data.table::setDTthreads(2)
# }

# Set environment variables BEFORE loading simstudy
Sys.setenv("R_DATATABLE_NUM_THREADS" = "1")
Sys.setenv("OMP_NUM_THREADS" = "1")
Sys.setenv("MKL_NUM_THREADS" = "1")

# Set options before loading simstudy
options(datatable.num.threads = 1)

library(simstudy)

# Also set it after loading
data.table::setDTthreads(1)

test_check("simstudy")

## Debugging

# # Set single thread for CI stability
# data.table::setDTthreads(1)
# 
# # Get all test files
# test_files <- list.files("testthat", pattern = "^test.*\\.[rR]$", full.names = TRUE)
# 
# cat("Found test files:", paste(test_files, collapse = ", "), "\n")
# 
# # Test each file individually
# for (test_file in test_files) {
#   cat("Running test file:", test_file, "\n")
#   tryCatch({
#     testthat::test_file(test_file)
#     cat("Completed:", test_file, "\n")
#   }, error = function(e) {
#     cat("Error in", test_file, ":", e$message, "\n")
#     stop(e)
#   })
# }
