# addPariods
test_that("addPeriods works", {
  skip_on_cran()
  
  tdef <- defData(varname = "T", dist = "binary", formula = 0.5)
  tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
  tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 + 5 + 5 * T", variance = 1)
  tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y0 + 10 + 5 * T", variance = 1)

  n <- ceiling(runif(1, 10, 20))
  dtTrial <- genData(n, tdef)
  
  p <- ceiling(runif(1, 3, 8))
  dtTime <- addPeriods(
    dtTrial,
    nPeriods = p, idvars = "id"
  )
  
  expect_equal(nrow(dtTime), n*p)
  
  expect_silent(
    addPeriods(dtTrial,
      nPeriods = 3, idvars = "id",
      timevars = c("Y0", "Y1", "Y2"), timevarName = "Y",
      periodVec = c(0, 3, 5)
    )
  )
  
  expect_warning(
    addPeriods(dtTrial,
               nPeriods = 2, idvars = "id",
               timevars = c("Y0", "Y1", "Y2"), timevarName = "Y"
    )
  )
  
  testthat::expect_silent(
    addPeriods(dtTrial,
               nPeriods = 3, idvars = "id",
               timevars = c("Y0", "Y1", "Y2"), 
               timevarName = "Y"
    )
  )
  
  def <- defData(varname = "xbase", dist = "normal", formula = 20, variance = 3)
  def <- defData(def, varname = "nCount", dist = "noZeroPoisson", formula = 6)
  def <- defData(def, varname = "mInterval", dist = "gamma", formula = 30, variance = .01)
  def <- defData(def, varname = "vInterval", dist = "nonrandom", formula = .07)

  dt <- genData(50, def)
  expect_silent(addPeriods(dt))
  
  
  def <- defData(varname = "xbase", dist = "normal", formula = 20, variance = 3)
  def <- defData(def, varname = "nCount", dist = "noZeroPoisson", formula = 6)
  def <- defData(def, varname = "mInterval", dist = "gamma", formula = 30, variance = .01)

  dt <- genData(50, def)
  expect_silent(addPeriods(dt))
  
  def <- defData(varname = "xbase", dist = "normal", formula = 20, variance = 3)
  
  dt <- genData(50, def)
  expect_error(addPeriods(dt))
  
})

# .addStrataCode ----
test_that("strata codes are added as expected.", {
  skip_on_cran()
  def <- defData(varname = "male", dist = "binary", formula = .5, id = "cid")
  def <- defData(def, varname = "over65", dist = "binary", formula = "-1.7 + .8*male", link = "logit")
  def <- defData(def, varname = "baseDBP", dist = "normal", formula = 70, variance = 40)

  data <- genData(330, def)

  expect_equal(range(.addStrataCode(data, "male")$.stratum), c(1, 2))
  expect_equal(range(.addStrataCode(data, c("male", "over65"))$.stratum), c(1, 4))
  expect_equal(.addStrataCode(data, "male")[, .SD, .SDcols = !".stratum"], data)
  expect_error(.addStrataCode(data, ""))
})

# .stratSamp ----
test_that("stratified samples are drawn correctly.", {
  skip_on_cran()
  expect_length(.stratSamp(1, 2), 1)
  expect_length(.stratSamp(2, 4), 2)
  expect_length(.stratSamp(50, 3), 50)
  expect_gte(table(.stratSamp(148, 2, c(1, 2)))[1], 49)
  expect_gte(table(.stratSamp(148, 2, c(1, 2)))[2], 98)
  expect_true(all(table(.stratSamp(150, 2, c(1, 2))) == c(50, 100)))
  expect_equal(range(.stratSamp(50, 3)), c(1, 3))
})
