# .gencat ----
test_that(".gencat throws errors", {
  expect_error(.gencat(n, "1;", "identity", NULL, environment()), "two probabilities")
  expect_error(.gencat(n, "1; ", "identity", NULL, environment()), "two probabilities")
})

# .genunif ----
test_that("unif data is generated as expected.", {
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
      expect_silent(.genunif(x$n, x$range, NULL, environment()))
    }
  )

  expect_warning(.genunif(3, "1;1", NULL, environment()), "are equal")
  expect_error(.genunif(3, "", NULL, environment()), "format")
  expect_error(.genunif(3, "1;2;3", NULL, environment()), "format")
  expect_error(.genunif(3, "2;1", NULL, environment()), "'max' < 'min'")
})

# .genUnifInt ----
test_that("unifInt data is generated as expected.", {
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
