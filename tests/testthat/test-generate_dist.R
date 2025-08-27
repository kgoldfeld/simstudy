library(testthat)
library(simstudy)
library(data.table)

# .gencat ----
test_that(".gencat throws errors", {
  skip_on_cran()
  expect_error(simstudy:::.gencat(
    n = n,
    formula = "1;",
    variance = NULL,
    link = "identity",
    envir = emptyenv()
  ), "two probabilities")
  expect_error(simstudy:::.gencat(
    n = n,
    formula = "1; ",
    variance = NULL,
    link = "identity",
    envir = emptyenv()
  ), "two probabilities")
  expect_error(simstudy:::.gencat(
    n = 10,
    formula = ".5;.5",
    variance = "a;",
    link = "identity",
    envir = emptyenv()
  ), class = "simstudy::lengthMismatch")
})

test_that("categorical data is generated as expected.", {
  skip_on_cran()
  expect_type(simstudy:::.gencat(
    n = 10,
    formula = genCatFormula(n = 3),
    variance = "a;b;c",
    link = "identity",
    envir = emptyenv()
  ), "character")
  expect_type(simstudy:::.gencat(
    n = 10,
    formula = genCatFormula(n = 3),
    variance = "a;2;c",
    link = "identity",
    envir = emptyenv()
  ), "character")
  expect_true(is.numeric(simstudy:::.gencat(
    n = 10,
    formula = genCatFormula(n = 3),
    variance = "1;2;3",
    link = "identity",
    envir = emptyenv()
  )))
})
# .genunif ----
test_that("unif data is generated as expected.", {
  skip_on_cran()
  n <- 20
  def <- defData(varname = "test", formula = 5, dist = "nonrandom")
  def <- defData(def, varname = "test2", formula = "test + 3", dist = "normal")

  dt <- genData(n, def)
  dterr <- genData(n - 5, def)
  expect_error(simstudy:::.genunif(n, "test;test2", dterr, environment()), "Length mismatch")
  expect_length(simstudy:::.genunif(n, "test;test2", dt, environment()), n)
  expect_true(all(!is.na(simstudy:::.genunif(n, "test;test2", dt, environment()))))
  expect_length(simstudy:::.genunif(n, "1.3;100.2", dt, environment()), n)
})

test_that("'uniform' formula checked correctly", {
  skip_on_cran()
  forall(
    generate(for (x in list(
      range = gen_uniform_range(),
      n = gen.int(40)
    )) {
      x
    }),
    function(x) {
      expect_silent(defData(varname = "z", formula = x$range, dist = "uniform"))
    }
  )

  expect_error(defData(varname = "z", formula = NULL, dist = "uniform", "format"))
  expect_error(defData(varname = "z", formula = "1;2;3", dist = "uniform"), "format")
  
  def <- defData(varname = "z", formula = "2;1", dist = "uniform")
  expect_error(genData(5, def), "Formula invalid: 'max' < 'min'")
  
  def <- defData(varname = "z", formula = "2;2", dist = "uniform")
  expect_warning(genData(5, def), "'min' and 'max' are equal")
  
})

# .genUnifInt ----
test_that("unifInt data is generated as expected.", {
  skip_on_cran()
  n <- 20
  def <- defData(varname = "test", formula = 5, dist = "nonrandom")
  def <- defData(def, varname = "test2", formula = "test + 3", dist = "normal")

  dt <- genData(n, def)
  dt$test2 <- ceiling(dt$test2)

  expect_length(simstudy:::.genUnifInt(n, "test;test2", dt, environment()), n)
  expect_true(all(!is.na(simstudy:::.genUnifInt(n, "test;test2", dt, environment()))))
  expect_length(simstudy:::.genUnifInt(n, "1;100", dt, environment()), n)
})

test_that("'uniformInt' formula checked correctly", {
  skip_on_cran()
  forall(
    generate(for (x in list(
      range = gen_uniformInt_range(),
      n = gen.int(40)
    )) {
      x
    }),
    function(x) {
      expect_silent(simstudy:::.genUnifInt(x$n, x$range, NULL, environment()))
    }
  )

  expect_error(simstudy:::.genUnifInt(3, "1.1;2.4", NULL, environment()), "must be integer")
})

# .genmixture ----
test_that("mixtures are generated correctly", {
  skip_on_cran()
  def <- defData(varname = "a", formula = 10)
  def <- defData(def,
    varname = "blksize",
    formula = "..sizes[1] | .5 + ..sizes[2] * a/10 | .5", 
    dist = "mixture"
  )
  sizes <- c(2, 4)
  env <- environment()
  expect_silent(genData(1000, def, envir = env))
})



test_that("runif throws errors", {
  skip_on_cran()
  expect_error(defData(varname = "u", formula = "5", dist = "uniform"))
})

test_that("treatment assignment data is generated as expected.",{
  skip_on_cran()
  n <- 100
  def <- defData(varname = "grp", formula = .5, dist = "binary")
  dt <- genData(n, def)
  
  expect_silent(simstudy:::.genAssign(dt, balanced = "identity", strata = 0, grpName = "rx", ratio = "1;1"))
  expect_silent(simstudy:::.genAssign(dt, balanced = "identity", strata = "grp", grpName = "rx", ratio = "1;1"))
  expect_silent(simstudy:::.genAssign(dt, balanced = "identity", strata = "grp", grpName = "rx", ratio = 4))
  }
)

# .genclustsize ----
test_that("clusterSize data is generated as expected.", {
  skip_on_cran()
 
  n <- sample(10:50, 1)
  tot <- sample(100:1000, 1)
  
  def <- defData(varname = "test", formula = tot, dist = "clusterSize")
  dt1 <- genData(n, def)
  
  def <- defData(varname = "test", formula = tot, variance = .05, dist = "clusterSize")
  dt2 <- genData(n, def)
  
  expect_equal(dt1[, sum(test)], tot)
  expect_equal(dt2[, sum(test)], tot)
  expect_true(dt2[, var(test)] > dt1[, var(test)])
  
})


# .gencustom ----
test_that("custom data is generated as expected.", {
  skip_on_cran()
  
  trunc_norm <- function(n, lower, upper, mu = 0, s = 1.5) {
    
    F.a <- pnorm(lower, mean = mu, sd = s)
    F.b <- pnorm(upper, mean = mu, sd = s)
    
    u <- runif(n, min = F.a, max = F.b)
    qnorm(u, mean = mu, sd = s)
    
  }

  def <-
    defData(varname = "x", formula = 5, dist = "poisson") |>
    defData(varname = "y", formula = "trunc_norm",
            variance = "s = 100, lower = x - 1, upper = x + 1",
            dist = "custom"
    )

  dd <- genData(10000, def)

  expect_true( dd[, min(y)] > dd[, min(x-1)])
  expect_true( dd[, max(y)] < dd[, max(x+1)])

})

### related to .generate

test_that(".generate handles NULL dfSim with different distributions", {
  skip_on_cran()
  
  # Test multiple distribution types with single variable definitions
  # to ensure the NULL dfSim path works across different distributions
  
  # Binary distribution
  def_binary <- defData(varname = "x_binary", formula = 0.3, dist = "binary")
  result_binary <- genData(10, def_binary)
  expect_true(all(result_binary$x_binary %in% c(0, 1)))
  
  # Poisson distribution
  def_pois <- defData(varname = "x_pois", formula = 2, dist = "poisson")
  result_pois <- genData(10, def_pois)
  expect_true(all(result_pois$x_pois >= 0))
  
  # Exponential distribution
  def_exp <- defData(varname = "x_exp", formula = 1, dist = "exponential")
  result_exp <- genData(10, def_exp)
  expect_true(all(result_exp$x_exp > 0))
})

test_that(".generate handles NULL dfSim correctly", {
  skip_on_cran()
  
  # This test is designed to trigger the uncovered line:
  # if (is.null(dfSim)) { dfNew <- data.table(newColumn) }
  
  # The .generate function is internal, so we need to test it through
  # public functions. The dfSim parameter is NULL when generating
  # the very first variable in a dataset.
  
  # Create a minimal data definition for a single variable
  # This should result in .generate being called with dfSim = NULL
  def1 <- defData(varname = "x1", formula = 0, variance = 1, dist = "normal")
  
  # Generate data with just this single variable
  # When processing the first (and only) variable, dfSim should be NULL
  result <- genData(10, def1)
  
  # Verify the result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 2) # id column + x1 column
  expect_true("x1" %in% names(result))
  expect_true("id" %in% names(result))
  
  # Test with different distribution types to ensure the path works
  # for various distributions
  def2 <- defData(varname = "x_uniform", formula = "0;1", dist = "uniform")
  result2 <- genData(5, def2)
  
  expect_s3_class(result2, "data.table")
  expect_equal(nrow(result2), 5)
  expect_equal(ncol(result2), 2)
  expect_true("x_uniform" %in% names(result2))
  
  # Test with categorical distribution
  def3 <- defData(varname = "x_cat", formula = "0.3;0.4;0.3", dist = "categorical")
  result3 <- genData(8, def3)
  
  expect_s3_class(result3, "data.table")
  expect_equal(nrow(result3), 8)
  expect_equal(ncol(result3), 2)
  expect_true("x_cat" %in% names(result3))
  expect_true(all(result3$x_cat %in% c(1, 2, 3)))
})

test_that(".generate function handles NULL dfSim directly", {
  skip_on_cran()
  
  # Test the internal .generate function directly with dfSim = NULL
  # This should trigger the uncovered line:
  # if (is.null(dfSim)) { dfNew <- data.table(newColumn) }
  
  # Create arguments for .generate function
  args <- list(
    varname = "test_var",
    formula = "0", 
    variance = 1,
    dist = "normal",
    link = "identity"
  )
  
  # Convert to the format expected by .generate (like a row from defData)
  args_dt <- data.table(
    varname = "test_var",
    formula = "0",
    variance = 1,
    dist = "normal", 
    link = "identity"
  )
  
  # Call .generate directly with dfSim = NULL
  # Access the internal function using :::
  result <- simstudy:::.generate(
    args = args_dt[1,], 
    n = 10, 
    dfSim = NULL, 
    idname = "id"
  )
  
  # Verify the result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 1) # Should have only the new column
  expect_equal(names(result), "test_var")
  expect_true(is.numeric(result$test_var))
})

test_that(".generate function with NULL dfSim works for different distributions", {
  skip_on_cran()
  
  # Test with uniform distribution
  args_uniform <- data.table(
    varname = "uniform_var",
    formula = "0;1",
    variance = NA,
    dist = "uniform",
    link = "identity"
  )
  
  result_uniform <- simstudy:::.generate(
    args = args_uniform[1,],
    n = 5,
    dfSim = NULL,
    idname = "id"
  )
  
  expect_equal(names(result_uniform), "uniform_var")
  expect_equal(nrow(result_uniform), 5)
  expect_true(all(result_uniform$uniform_var >= 0 & result_uniform$uniform_var <= 1))
  
  # Test with categorical distribution  
  args_cat <- data.table(
    varname = "cat_var",
    formula = "0.5;0.3;0.2",
    variance = "1;2;3",
    dist = "categorical",
    link = "identity"
  )
  
  result_cat <- simstudy:::.generate(
    args = args_cat[1,],
    n = 6,
    dfSim = NULL,
    idname = "id"
  )
  
  expect_equal(names(result_cat), "cat_var")
  expect_equal(nrow(result_cat), 6)
  expect_true(all(result_cat$cat_var %in% c(1, 2, 3)))
})

# ---- .genbasisdt

test_that(".genbasisdt basic functionality works", {
  skip_on_cran()
  
  # Test basic functionality
  x <- seq(0, 1, length.out = 100)
  knots <- c(0.3, 0.7)
  degree <- 3
  theta <- c(0.2, 0.5, 0.8, 0.3, 0.6, 0.1)  # length = knots + degree + 1 = 2 + 3 + 1 = 6
  
  result <- simstudy:::.genbasisdt(x, knots, degree, theta)
  
  # Check structure of returned list
  expect_type(result, "list")
  expect_true(all(c("dt", "basis", "knots", "degree") %in% names(result)))
  
  # Check data.table structure
  expect_s3_class(result$dt, "data.table")
  expect_equal(nrow(result$dt), length(x))
  expect_equal(ncol(result$dt), 2)
  expect_true(all(c("x", "y.spline") %in% names(result$dt)))
  
  # Check that x values match input
  expect_equal(result$dt$x, x)
  
  # Check basis matrix dimensions
  expect_true(is.matrix(result$basis))
  expect_equal(nrow(result$basis), length(x))
  expect_equal(ncol(result$basis), length(theta))
  
  # Check that knots and degree are preserved
  expect_equal(result$knots, knots)
  expect_equal(result$degree, degree)
  
  # Check that y.spline values are numeric
  expect_true(is.numeric(result$dt$y.spline))
})

test_that(".genbasisdt works with no interior knots", {
  skip_on_cran()
  
  x <- seq(0, 1, length.out = 50)
  knots <- NULL
  degree <- 2
  theta <- c(0.1, 0.5, 0.8)  # length = 0 + 2 + 1 = 3
  
  result <- simstudy:::.genbasisdt(x, knots, degree, theta)
  
  expect_s3_class(result$dt, "data.table")
  expect_equal(nrow(result$dt), 50)
  expect_equal(ncol(result$basis), 3)
  expect_null(result$knots)
  expect_equal(result$degree, 2)
})

test_that(".genbasisdt works with single knot", {
  skip_on_cran()
  
  x <- seq(0, 1, length.out = 30)
  knots <- 0.5
  degree <- 1
  theta <- c(0.2, 0.7, 0.3)  # length = 1 + 1 + 1 = 3
  
  result <- simstudy:::.genbasisdt(x, knots, degree, theta)
  
  expect_s3_class(result$dt, "data.table")
  expect_equal(result$knots, 0.5)
  expect_equal(ncol(result$basis), 3)
})

test_that(".genbasisdt works with theta values equal to 1", {
  skip_on_cran()
  
  # Test boundary case where theta values can be exactly 1
  x <- seq(0, 1, length.out = 20)
  knots <- 0.5
  degree <- 2
  theta <- c(1, 0.5, 1, .5)  # Some values exactly equal to 1
  
  result <- simstudy:::.genbasisdt(x, knots, degree, theta)
  
  expect_s3_class(result$dt, "data.table")
  expect_equal(nrow(result$dt), 20)
})

test_that(".genbasisdt throws error for incorrect theta length", {
  skip_on_cran()
  
  x <- seq(0, 1, length.out = 50)
  knots <- c(0.3, 0.7)
  degree <- 3
  
  # Required length should be 2 + 3 + 1 = 6, but provide 5
  theta_short <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  expect_error(
    simstudy:::.genbasisdt(x, knots, degree, theta_short),
    "Number of specified paramaters \\(theta\\) not correct. Needs to be 6\\."
  )
  
  # Provide 7 instead of 6
  theta_long <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  expect_error(
    simstudy:::.genbasisdt(x, knots, degree, theta_long),
    "Number of specified paramaters \\(theta\\) not correct. Needs to be 6\\."
  )
})

test_that(".genbasisdt throws error for theta values > 1", {
  skip_on_cran()
  
  x <- seq(0, 1, length.out = 50)
  knots <- 0.5
  degree <- 2
  
  # Test single value > 1
  theta_single <- c(0.3, 1.5, 0.7, .3)
  expect_error(
    simstudy:::.genbasisdt(x, knots, degree, theta_single),
    "1 value of theta exceeds 1\\.00"
  )
  
  # Test multiple values > 1
  theta_multiple <- c(1.2, 0.5, 1.8, .4)
  expect_error(
    simstudy:::.genbasisdt(x, knots, degree, theta_multiple),
    "2 values of theta exceed 1\\.00"
  )
})

test_that(".genbasisdt throws error for invalid knots", {
  skip_on_cran()

  x <- seq(0, 1, length.out = 50)
  degree <- 2
  theta <- c(0.2, 0.5, 0.8, 0.3)  # length = 1 + 2 + 1 = 4

  # Test knot < 0
  knots_negative <- -0.1
  expect_error(
    simstudy:::.genbasisdt(x, knots_negative, degree, theta),
    "All knots must be between 0 and 1"
  )

  # Test knot > 1
  knots_over_one <- 1.1
  expect_error(
    simstudy:::.genbasisdt(x, knots_over_one, degree, theta),
    "All knots must be between 0 and 1"
  )

  # Test knot = 0 (boundary case)
  knots_zero <- 0
  expect_error(
    simstudy:::.genbasisdt(x, knots_zero, degree, theta),
    "All knots must be between 0 and 1"
  )

  # Test knot = 1 (boundary case)
  knots_one <- 1
  expect_error(
    simstudy:::.genbasisdt(x, knots_one, degree, theta),
    "All knots must be between 0 and 1"
  )

  # Test mixed valid and invalid knots
  knots_mixed <- c(0.3, 1.2, 0.7)
  theta_mixed <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)  # length = 3 + 2 + 1 = 6
  expect_error(
    simstudy:::.genbasisdt(x, knots_mixed, degree, theta_mixed),
    "All knots must be between 0 and 1"
  )
})

test_that(".genbasisdt mathematical properties", {
  skip_on_cran()
  
  x <- seq(0, 1, length.out = 100)
  knots <- c(0.25, 0.5, 0.75)
  degree <- 3
  theta <- c(0.1, 0.3, 0.7, 0.9, 0.5, 0.2, 0.8)
  
  result <- simstudy:::.genbasisdt(x, knots, degree, theta)
  
  # Check that basis functions sum to 1 at each x (property of B-splines with intercept=TRUE)
  basis_row_sums <- rowSums(result$basis)
  expect_true(all(abs(basis_row_sums - 1) < 1e-10))  # Should sum to 1 (within numerical precision)
  
  # Check that y.spline is computed correctly as basis %*% theta
  expected_y <- as.vector(result$basis %*% theta)
  expect_equal(result$dt$y.spline, expected_y, tolerance = 1e-10)
  
  # Check that all basis values are non-negative (property of B-splines)
  expect_true(all(result$basis >= 0))
})

test_that(".genbasisdt edge cases", {
  skip_on_cran()
  
  # Test with minimal x values
  x_minimal <- c(0, 0.5, 1)
  knots <- .3
  degree <- 1
  theta <- c(0.2, 0.8, .2)
  
  result_minimal <- simstudy:::.genbasisdt(x_minimal, knots, degree, theta)
  expect_equal(nrow(result_minimal$dt), 3)
  expect_equal(nrow(result_minimal$basis), 3)
  
  # Test with many knots
  x_many <- seq(0, 1, length.out = 200)
  knots_many <- seq(0.1, 0.9, by = 0.1)  # 9 knots
  degree_many <- 2
  theta_many <- rep(0.5, 9 + 2 + 1)  # 12 parameters
  
  result_many <- simstudy:::.genbasisdt(x_many, knots_many, degree_many, theta_many)
  expect_equal(ncol(result_many$basis), 12)
  expect_equal(length(result_many$knots), 9)
})

### .getBinaryMean

test_that(".getBinaryMean basic functionality works", {
  skip_on_cran()
  
  # Create test data
  dtSim <- data.table(
    id = 1:10,
    x1 = rnorm(10),
    x2 = runif(10)
  )
  
  # Test with identity link (no transformation)
  formula <- "0.3"
  size <- "1"
  link <- "identity"
  
  result <- simstudy:::.getBinaryMean(
    dtSim = dtSim,
    formula = formula,
    size = size,
    link = link,
    n = 10
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_length(result, 2)
  
  # Check probability values
  p <- result[[1]]
  expect_length(p, 10)
  expect_true(all(p == 0.3))  # Identity link, so should be unchanged
  
  # Check size values
  size_result <- result[[2]]
  expect_length(size_result, 10)
  expect_true(all(size_result == 1))
})

test_that(".getBinaryMean works with logit link", {
  skip_on_cran()
  
  dtSim <- data.table(
    id = 1:5,
    x = c(-2, -1, 0, 1, 2)
  )
  
  # Test with logit link
  formula <- "x"  # Use the x column values
  size <- "1"
  link <- "logit"
  
  result <- simstudy:::.getBinaryMean(
    dtSim = dtSim,
    formula = formula,
    size = size,
    link = link,
    n = 5
  )
  
  p <- result[[1]][,1]
  
  # Logit transformation: p = 1 / (1 + exp(-x))
  expected_p <- 1 / (1 + exp(-dtSim$x))
  expect_equal(p, expected_p, tolerance = 1e-4)
  
  # Check that probabilities are between 0 and 1
  expect_true(all(p >= 0 & p <= 1))
  
  # Check specific values
  expect_equal(p[3], 0.5, tolerance = 1e-10)  # logit(0) = 0.5
})

test_that(".getBinaryMean works with log link", {
  skip_on_cran()
  
  dtSim <- data.table(
    id = 1:4,
    x = c(-2, -1, 0, 0.5)
  )
  
  # Test with log link
  formula <- "x"
  size <- "1"
  link <- "log"
  
  result <- simstudy:::.getBinaryMean(
    dtSim = dtSim,
    formula = formula,
    size = size,
    link = link,
    n = 4
  )
  
  p <- result[[1]][,1]
  
  # Log transformation: p = exp(x)
  expected_p <- exp(dtSim$x)
  expect_equal(p, expected_p, tolerance = 1e-4)
  
  # Check specific values
  expect_equal(p[3], 1, tolerance = 1e-10)  # exp(0) = 1
  expect_equal(p[4], exp(0.5), tolerance = 1e-10)
})

### .gencat

test_that(".gencat works with logit link", {
  skip_on_cran()
  
  dtSim <- data.table(id = 1:100, x = rnorm(100))
  
  # Test with logit link
  # Using log-odds: log(0.3/0.5) ≈ -0.51, log(0.2/0.5) ≈ -0.92
  formula <- "-0.51;-0.92"  # Will be transformed via logit
  variance <- 0
  link <- "logit"
  
  result <- simstudy:::.gencat(
    n = 100,
    formula = formula,
    variance = variance,
    link = link,
    dtSim = dtSim,
    envir = parent.frame()
  )
  
  expect_length(result, 100)
  expect_true(all(result %in% c(1, 2, 3)))
  
  # With logit link, the probabilities are transformed
  # Check that we get reasonable distribution
  expect_true(length(unique(result)) == 3)
})

### gendeterm with log and logit links

test_that(".gendeterm works with log and logit link", {
  skip_on_cran()

  
  dtSim <- data.table(id = 1:100, x = rnorm(100))
  
  # Test with logit link
  # Using log link
  formula <- "-0.4 * x"  # Will be transformed via logit
  link <- "logit"
  
  result <- simstudy:::.gendeterm(
    n = 100,
    formula = formula,
    link = link,
    dtSim = dtSim,
    envir = parent.frame()
  )
  
  expect_length(result, 100)
  expect_equal(1/(1 + exp(0.4 * dtSim$x)), result[,1], tolerance = 1e-4)
  
  # Test with log link
  # Using log link
  formula <- "-0.35 * x"  # Will be transformed via log
  link <- "log"
  
  result <- simstudy:::.gendeterm(
    n = 100,
    formula = formula,
    link = link,
    dtSim = dtSim,
    envir = parent.frame()
  )
  
  expect_length(result, 100)
  expect_equal(exp(-0.35 * dtSim$x), result[,1], tolerance = 1e-4)

})

### genexp with log links

test_that(".gendeterm works with log", {
  skip_on_cran()
  
  N <- 100000
  dtSim <- data.table(id = 1:N, x = rnorm(N))
  
  
  # Test with log link
  # Using log link
  formula <- "1.3 * x"  # Will be transformed via log
  link <- "log"
  
  result <- simstudy:::.genexp(
    n = N,
    formula = formula,
    link = link,
    dtSim = dtSim,
    envir = parent.frame()
  )
  
  expect_length(result, N)
  expect_equal(mean(1/exp(1.3 * dtSim$x)), mean(result), tolerance = 1e-1)
})

### .genmixture throws error when probabilities don't sum to 1

test_that(".genmixture throws error when probabilities don't sum to 1", {
  skip_on_cran()
  
 def <- 
   defData(varname = "x", formula = 0, variance = 1) |>
   defData(varname = "y", formula = 4, variance = 1) |>
   defData(varname = "m", formula = "x|0.5 + y|0.6", dist = "mixture")
   
 expect_error(genData(5, def),
    "Probabilities in mixture formula.*have to sum to 1"
 )
})

### .genpoisson log link

test_that("..genpoisson works with log link", {
  skip_on_cran()
  
  def <- 
    defData(varname = "x", formula = 0, variance = 2) |>
    defData(varname = "p", formula = "1.5*x", link = "log", dist = "poisson")
  
  dd <- genData(100000, def)
  
  expect_equal(mean(exp(dd$x * 1.5)), mean(dd$p), tolerance = 1e-2)

})

## negative binomial

test_that("..gennegbinom works", {
  skip_on_cran()
  
  mu <- runif(1, .5, 2)
  
  def <- 
    defData(varname = "x", formula = "..mu", variance = 2, dist = "gamma") |>
    defData(varname = "nb", formula = "x" , variance = 1, dist = "negBinomial")
  
  dd <- genData(100000, def)
  expect_equal(dd[, mean(x)],  dd[, mean(nb)], tolerance = 1e-1)
  
  ## Log link
  
  mu <- runif(1, -.5, .5)
  
  def <- 
    defData(varname = "x", formula = "..mu", variance = 2, dist = "normal") |>
    defData(varname = "nb", formula = "x" , variance = 1, link = "log", dist = "negBinomial")
  
  dd <- genData(100000, def)
  expect_equal(dd[, mean(exp(x))],  dd[, mean(nb)], tolerance = 1e-1)
  
  
})

