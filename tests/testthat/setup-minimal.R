# # Set all threading to 1 before any package loading
# Sys.setenv("R_DATATABLE_NUM_THREADS" = "1")
# Sys.setenv("OMP_NUM_THREADS" = "1")
# Sys.setenv("MKL_NUM_THREADS" = "1")
# 
# # Load packages one by one to identify the problematic one
# cat("Loading testthat...\n")
# library(testthat)
# 
# cat("Loading hedgehog...\n") 
# library(hedgehog)
# 
# # Try loading simstudy last and see if this is where it fails
# cat("Loading simstudy...\n")
# library(simstudy)
# 
# cat("Setting data.table threads...\n")
# if (requireNamespace("data.table", quietly = TRUE)) {
#   data.table::setDTthreads(1)
# }

# cat("Setup complete.\n")
