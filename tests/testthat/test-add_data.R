# addCondition ----
test_that("addCondition throws errors.", {
  expect_error(addCondition(), class = "simstudy::missingArgument")
  expect_error(addCondition("a"), class = "simstudy::missingArgument")
  expect_error(addCondition(data.frame(), data.frame(), "a"), class = "simstudy::wrongClass")
})

test_that("addCondition works.", {
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
test_that("addColumns throws erros.", {
  expect_error(addColumns(), class = "simstudy::missingArgument")
  expect_error(addColumns("a"), class = "simstudy::missingArgument")
  expect_error(addColumns(data.frame(), data.frame()), class = "simstudy::wrongClass")
})

test_that("addColumns works.", {
  def <- defData(varname = "x", formula = "1;10", dist = "uniformInt")
  dt <- genData(100, def)
  def2 <- defDataAdd(varname = "y", formula = "2.3 * (1/x)", dist = "normal")

  expect_silent(addColumns(def2, dt))
})

test_that("defRepeatAdd works", {
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
  expect_error(defRepeatAdd(prefix = "b", formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument")
  expect_error(defRepeatAdd(nVars = 8, formula = 5, variance = 3, dist = "normal"),
    class = "simstudy::missingArgument")
  expect_error(defRepeatAdd(nVars = 8, prefix = "b", variance = 3, dist = "normal"),
    class = "simstudy::missingArgument")
})
