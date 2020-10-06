# .addStrataCode ----
test_that("strata codes are added as expected.", {
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
  expect_length(.stratSamp(1, 2), 1)
  expect_length(.stratSamp(2, 4), 2)
  expect_length(.stratSamp(50, 3), 50)
  expect_gte(table(.stratSamp(148, 2, c(1, 2)))[1], 49)
  expect_gte(table(.stratSamp(148, 2, c(1, 2)))[2], 98)
  expect_true(all(table(.stratSamp(150, 2, c(1, 2))) == c(50, 100)))
  expect_equal(range(.stratSamp(50, 3)), c(1, 3))
})
