library(testthat)
library(simstudy)
library(data.table)

# addCondition ----
test_that("addCondition throws errors.", {
  skip_on_cran()
  expect_error(addCondition(), class = "simstudy::missingArgument")
  expect_error(addCondition("a"), class = "simstudy::missingArgument")
  expect_error(addCondition(data.frame(), data.frame(), "a"), class = "simstudy::wrongClass")
})

test_that("addCondition works.", {
  skip_on_cran()
  def <- defData(varname = "x", formula = "1;10", dist = "uniformInt")
  defC <- defCondition(condition = "x >= 5", formula = "x + 5", dist = "nonrandom")
  defC <- defCondition(defC, condition = "x < 5", formula = "10", dist = "nonrandom")
  defC2 <- defCondition(condition = "x >= 5", formula = "x + 6", dist = "nonrandom")
  defC2 <- defCondition(defC2, condition = "x < 5", formula = "11", dist = "nonrandom")
  dt <- genData(1000, def)
  defs <- list(defC = defC, defC2 = defC2)

  expect_equal(range(addCondition(defC, dt, "x2")$x2), c(10, 15))
  expect_equal(range(addCondition(defs[["defC2"]], dt, "x2")$x2), c(11, 16))
})

# addColumns ----
test_that("addColumns throws errors.", {
  skip_on_cran()
  expect_error(addColumns(), class = "simstudy::missingArgument")
  expect_error(addColumns("a"), class = "simstudy::missingArgument")
  expect_error(addColumns(data.frame(), data.frame()), class = "simstudy::wrongClass")
})

test_that("addColumns works.", {
  skip_on_cran()
  def <- defData(varname = "x", formula = "1;10", dist = "uniformInt")
  dt <- genData(100, def)
  def2 <- defDataAdd(varname = "y", formula = "2.3 * (1/x)", dist = "normal")

  expect_silent(addColumns(def2, dt))
})

test_that("defRepeatAdd works", {
  skip_on_cran()
  expect_silent(
    defRepeatAdd(nVars = 4, prefix = "g", formula = "1/3;1/3;1/3", variance = 0, dist = "categorical")
  )

  def <- defDataAdd(varname = "a", formula = "1;1", dist = "trtAssign")
  expect_silent(
    defRepeatAdd(def, 8, "b", formula = "5 + a", variance = 3, dist = "normal")
  )

  expect_silent(defRepeatAdd(nVars = 4, prefix = "b", formula = "5 + a", variance = 3, dist = "normal"))
})

test_that("defRepeatAdd throws errors correctly.", {
  skip_on_cran()
  expect_error(defRepeatAdd(prefix = "b", formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeatAdd(nVars = 8, formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
  expect_error(defRepeatAdd(nVars = 8, prefix = "b", variance = 3, dist = "normal"),
    class = "simstudy::missingArgument"
  )
})

# addMarkov ----
test_that("addMarkov throws errors.", {
  skip_on_cran()
  d0 <- defData(varname = "xx", formula = 2)
  d0 <- defData(d0, varname = "xy", formula = 5)
  dd <- genData(n = 10, dt = d0)

  # check transMat is matrix
  mat1 <- c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9)
  expect_error(addMarkov(dd, transMat = mat1, chainLen = 5, wide = TRUE), class = "simstudy::typeMatrix")

  # check transMat is square matrix
  mat2 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9, 0.3, 0.4, 0.3), nrow = 4, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat2, chainLen = 5, wide = TRUE), class = "simstudy::squareMatrix")

  # check transMat row sums = 1
  mat3 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.8), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat3, chainLen = 5, wide = TRUE), class = "simstudy::rowSums1")

  # check chainLen is > 1
  mat4 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat4, chainLen = 0, wide = TRUE), class = "simstudy::chainLen")

  # if start0lab defined, check that it is defined in dd
  mat5 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat5, chainLen = 5, wide = TRUE, start0lab = "yy"), class = "simstudy::notDefined")


  # if start0lab defined, check that it exists in the transition matrix
  mat6 <- t(matrix(c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2, 0.0, 0.1, 0.9), nrow = 3, ncol = 3))
  expect_error(addMarkov(dd, transMat = mat6, chainLen = 5, wide = TRUE, start0lab = "xy"), class = "simstudy::start0probNotInTransMat")

})

test_that("addMarkov handles trimvalue correctly", {
  skip_on_cran()
  
  # Set up test data
  d0 <- defData(varname = "xx", formula = 2)
  dd <- genData(n = 5, dt = d0)
  
  # Create a transition matrix that makes it likely to reach state 3
  # (which we'll use as trimvalue)
  transMat <- t(matrix(c(
    0.3, 0.3, 0.4,  # From state 1: high prob to go to state 3
    0.2, 0.3, 0.5,  # From state 2: high prob to go to state 3  
    0.0, 0.0, 1.0   # From state 3: stay in state 3
  ), nrow = 3, ncol = 3))
  
  # Test trimvalue with long format (wide = FALSE)
  result <- addMarkov(dd, transMat = transMat, chainLen = 10, 
                      wide = FALSE, trimvalue = 3)
  
  # Check that the result is a data.table
  expect_s3_class(result, "data.table")
  
  # Check that no records exist after the first occurrence of state 3
  # for each individual
  for (i in unique(result$id)) {
    individual_data <- result[id == i]
    state_values <- individual_data$state
    
    # Find first occurrence of trimvalue (3)
    first_trim_idx <- which(state_values == 3)[1]
    
    if (!is.na(first_trim_idx)) {
      # If trimvalue was found, check no records exist after it
      expect_equal(nrow(individual_data), first_trim_idx)
    }
  }
  
  # Verify that the temporary column .e was properly removed
  expect_false(".e" %in% names(result))
})

# addSynthetic ----

test_that("addSynthetic throws errors.", {
  skip_on_cran()

  ### Create fake "real" data set

  d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
  d <- defData(d, varname = "b", formula = 5, dist = "poisson")
  d <- defData(d, varname = "c", formula = 0.3, dist = "binary")
  d <- defData(d, varname = "d", formula = "a + b + 3*c", variance = 2, dist = "normal")

  A <- genData(1000, d, id = "index")

  def <- defData(varname = "x", formula = 0, variance = 5)

  S <- genData(120, def)

  expect_error(addSynthetic(dtFrom = A), class = "simstudy::missingArgument")

  x <- c(1, 2, 3)
  expect_error(addSynthetic(dtOld = x, dtFrom = A), class = "simstudy::wrongClass")
  expect_error(addSynthetic(dtOld = S, dtFrom = x), class = "simstudy::wrongClass")
  expect_error(addSynthetic(dtOld = S, dtFrom = A, id = "index"), class = "simstudy::notDefined")
  expect_error(addSynthetic(dtOld = S, dtFrom = A, id = "id"), class = "simstudy::notDefined")

  d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
  d <- defData(d, varname = "x", formula = 5, dist = "poisson")

  A <- genData(1000, d)
  S <- genData(120, def)
  expect_error(addSynthetic(dtOld = S, dtFrom = A), class = "simstudy::alreadyDefined")
})

test_that("addSynthetic works.", {
  skip_on_cran()

  ### Create fake 'external' data set 'A'

  d <- defData(varname = "a", formula = 3, variance = 1, dist = "normal")
  d <- defData(d, varname = "b", formula = 5, dist = "poisson")
  d <- defData(d, varname = "c", formula = 0.3, dist = "binary")
  d <- defData(d, varname = "d", formula = "a + b + 3*c", variance = 2, dist = "normal")

  A <- genData(1000, d)

  ### Create synthetic data set from "observed" data set A
  ### and add it to other data set S:

  def <- defData(varname = "x", formula = 0, variance = 5)

  n <- rpois(1, 100)
  vars <- c("d", "b")

  S <- genData(n, def)
  Snew <- addSynthetic(dtOld = S, dtFrom = A, vars = vars)

  expect_true(all(c(names(S), vars) == names(Snew)))
  expect_equal(nrow(Snew), nrow(S))

  mu_a <- rnorm(1, 25, 4)
  n <- rpois(1, 3500)

  d <- defData(varname = "a", formula = "..mu_a", variance = 1, dist = "normal")
  A <- genData(n, d)

  S <- genData(n, def)
  Snew <- addSynthetic(S, A)

  expect_lt(Snew[, abs(mean(a) - mu_a)], 0.15)

})


# addMultiFac ----


test_that("addMultiFac basic functionality works", {
  skip_on_cran()
  
  # Set up basic test data
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(12, defD)
  
  # Test basic functionality with default parameters
  result <- addMultiFac(DT, nFactors = 2)
  
  # Check structure
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 12)
  expect_equal(ncol(result), 4) # original 2 + 2 new factors
  expect_true(all(c("Var1", "Var2") %in% names(result)))
  
  # Check that all combinations are present
  combinations <- result[, .N, keyby = .(Var1, Var2)]
  expect_equal(nrow(combinations), 4) # 2 levels × 2 factors = 4 combinations
})

test_that("addMultiFac handles different levels correctly", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(18, defD)
  
  # Test with vector of different levels
  result <- addMultiFac(DT, nFactors = 3, levels = c(2, 3, 3), colNames = c("A", "B", "C"))
  
  expect_equal(ncol(result), 5) # original 2 + 3 new factors
  expect_true(all(c("A", "B", "C") %in% names(result)))
  
  # Check factor levels
  expect_true(all(result$A %in% c(1, 2)))
  expect_true(all(result$B %in% c(1, 2, 3)))
  expect_true(all(result$C %in% c(1, 2, 3)))
  
  # Check all combinations exist
  combinations <- result[, .N, keyby = .(A, B, C)]
  expect_equal(nrow(combinations), 18) # 2 × 3 × 3 = 18 combinations
})

test_that("addMultiFac handles scalar levels correctly", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(24, defD)
  
  # Test with scalar level (same for all factors)
  result <- addMultiFac(DT, nFactors = 3, levels = 2)
  
  expect_equal(ncol(result), 5) # original 2 + 3 new factors
  expect_true(all(c("Var1", "Var2", "Var3") %in% names(result)))
  
  # All factors should have 2 levels
  expect_true(all(result$Var1 %in% c(0, 1))) # dummy coding by default
  expect_true(all(result$Var2 %in% c(0, 1)))
  expect_true(all(result$Var3 %in% c(0, 1)))
})

test_that("addMultiFac effect coding works", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(8, defD)
  
  # Test effect coding
  result <- addMultiFac(DT, nFactors = 2, levels = 2, coding = "effect")
  
  # Effect coding should use -1 and 1
  expect_true(all(result$Var1 %in% c(-1, 1)))
  expect_true(all(result$Var2 %in% c(-1, 1)))
})

test_that("addMultiFac dummy coding works", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(8, defD)
  
  # Test dummy coding (default)
  result <- addMultiFac(DT, nFactors = 2, levels = 2, coding = "dummy")
  
  # Dummy coding should use 0 and 1
  expect_true(all(result$Var1 %in% c(0, 1)))
  expect_true(all(result$Var2 %in% c(0, 1)))
})

test_that("addMultiFac handles custom column names", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(12, defD)
  
  # Test custom column names
  result <- addMultiFac(DT, nFactors = 3, levels = 2, colNames = c("Treatment", "Gender", "Age_Group"))
  
  expect_true(all(c("Treatment", "Gender", "Age_Group") %in% names(result)))
  expect_false(any(c("Var1", "Var2", "Var3") %in% names(result)))
})

test_that("addMultiFac handles uneven sample sizes", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(10, defD) # 10 doesn't divide evenly by 4 (2×2)
  
  result <- addMultiFac(DT, nFactors = 2, levels = 2)
  
  expect_equal(nrow(result), 10)
  
  # Check that all combinations are represented
  combinations <- result[, .N, keyby = .(Var1, Var2)]
  expect_equal(nrow(combinations), 4)
  
  # Some combinations should have 3 observations, others 2 (10 = 2*4 + 2)
  counts <- combinations$N
  expect_true(all(counts %in% c(2, 3)))
  expect_equal(sum(counts), 10)
})

test_that("addMultiFac throws appropriate errors", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(12, defD)
  
  # Test error: less than 2 factors
  expect_error(addMultiFac(DT, nFactors = 1), "Must specify at least 2 factors")
  
  # Test error: mismatched levels and factors
  expect_error(addMultiFac(DT, nFactors = 3, levels = c(2, 3)), 
               "Number of levels does not match factors")
  
  # Test error: default column names already exist
  DT_with_var1 <- data.table::copy(DT)
  DT_with_var1[, Var1 := 1]
  expect_error(addMultiFac(DT_with_var1, nFactors = 2), 
               "Default column name\\(s\\) already in use")
  
  # Test error: custom column names already exist
  expect_error(addMultiFac(DT, nFactors = 2, colNames = c("id", "x")), 
               "At least one column name already in use")
  
  # Test error: invalid coding
  expect_error(addMultiFac(DT, nFactors = 2, levels = 2, coding = "invalid"), 
               "Need to specify 'effect' or 'dummy' coding")
})

test_that("addMultiFac handles mixed level factors with non-binary", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(30, defD)
  
  # Test with factors that have more than 2 levels (should use 1:n coding)
  result <- addMultiFac(DT, nFactors = 2, levels = c(3, 5), colNames = c("A", "B"))
  
  expect_true(all(result$A %in% c(1, 2, 3)))
  expect_true(all(result$B %in% c(1, 2, 3, 4, 5)))
  
  # Check all combinations are present
  combinations <- result[, .N, keyby = .(A, B)]
  expect_equal(nrow(combinations), 15) # 3 × 5 = 15 combinations
})

test_that("addMultiFac handles scalar levels with non-binary factors", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  DT <- genData(27, defD)
  
  # Test with scalar levels > 2 (this should trigger the uncovered line)
  # This tests: if (length(levels) == 1) levels <- rep(levels, nFactors)
  result <- addMultiFac(DT, nFactors = 3, levels = 3, colNames = c("A", "B", "C"))
  
  # All factors should have 3 levels (1, 2, 3)
  expect_true(all(result$A %in% c(1, 2, 3)))
  expect_true(all(result$B %in% c(1, 2, 3)))
  expect_true(all(result$C %in% c(1, 2, 3)))
  
  # Check all combinations are present
  combinations <- result[, .N, keyby = .(A, B, C)]
  expect_equal(nrow(combinations), 27) # 3 × 3 × 3 = 27 combinations
})

test_that("addMultiFac preserves original data", {
  skip_on_cran()
  
  defD <- defData(varname = "x", formula = 0, variance = 1)
  defD <- defData(defD, varname = "y", formula = 5, variance = 2)
  DT <- genData(12, defD)
  original_names <- names(DT)
  original_nrow <- nrow(DT)
  
  result <- addMultiFac(DT, nFactors = 2)
  
  # Check original columns are preserved
  expect_true(all(original_names %in% names(result)))
  expect_equal(nrow(result), original_nrow)
  
  # Check original data values are unchanged
  for (col in original_names) {
    expect_equal(result[[col]], DT[[col]])
  }
})
