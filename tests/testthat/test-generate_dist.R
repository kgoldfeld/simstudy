# .gencat ----
test_that(".gencat throws errors", {
  skip_on_cran()
  expect_error(.gencat(
    n = n,
    formula = "1;",
    variance = NULL,
    link = "identity",
    envir = emptyenv()
  ), "two probabilities")
  expect_error(.gencat(
    n = n,
    formula = "1; ",
    variance = NULL,
    link = "identity",
    envir = emptyenv()
  ), "two probabilities")
  expect_error(.gencat(
    n = 10,
    formula = ".5;.5",
    variance = "a;",
    link = "identity",
    envir = emptyenv()
  ), class = "simstudy::lengthMismatch")
})

test_that("categorical data is generated as expected.", {
  skip_on_cran()
  expect_type(.gencat(
    n = 10,
    formula = genCatFormula(n = 3),
    variance = "a;b;c",
    link = "identity",
    envir = emptyenv()
  ), "character")
  expect_type(.gencat(
    n = 10,
    formula = genCatFormula(n = 3),
    variance = "a;2;c",
    link = "identity",
    envir = emptyenv()
  ), "character")
  expect_true(is.numeric(.gencat(
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
  expect_error(.genunif(n, "test;test2", dterr, environment()), "Length mismatch")
  expect_length(.genunif(n, "test;test2", dt, environment()), n)
  expect_true(all(!is.na(.genunif(n, "test;test2", dt, environment()))))
  expect_length(.genunif(n, "1.3;100.2", dt, environment()), n)
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

  expect_length(.genUnifInt(n, "test;test2", dt, environment()), n)
  expect_true(all(!is.na(.genUnifInt(n, "test;test2", dt, environment()))))
  expect_length(.genUnifInt(n, "1;100", dt, environment()), n)
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
      expect_silent(.genUnifInt(x$n, x$range, NULL, environment()))
    }
  )

  expect_error(.genUnifInt(3, "1.1;2.4", NULL, environment()), "must be integer")
})

# .genmixture ----
test_that("mixtures are generated correctly", {
  skip_on_cran()
  def <- defData(varname = "a", formula = 5)
  def <- defData(def,
    varname = "blksize",
    formula = "..sizes[1] | .5 + ..sizes[2] * a/10 | .5", dist = "mixture"
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
  
  expect_silent(.genAssign(dt, balanced = "identity", strata = 0, grpName = "rx", ratio = "1;1"))
  expect_silent(.genAssign(dt, balanced = "identity", strata = "grp", grpName = "rx", ratio = "1;1"))
  expect_silent(.genAssign(dt, balanced = "identity", strata = "grp", grpName = "rx", ratio = 4))
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
