library(testthat)
library(simstudy)
library(data.table)

# genSurv -----

test_that("genSurv runs OK", {
  skip_on_cran()
  
  dS <- defSurv(varname = "event_1", formula = "-10", shape = .3)
  dS <- defSurv(dS, "event_2", "-6.5", shape = .4)
  dS <- defSurv(dS, "event_3", "-7", shape = .5)
  
  dd <- genData(1000)
  
  expect_equal(genSurv(dd, dS)[, .(c(
    median(event_1),
    median(event_2),
    median(event_3)
  ))][, V1],
  c(18, 11.6, 27.5),
  tolerance = .1
  )
  
  d1 <- defData(varname = "x", formula = .5, dist = "binary")
  
  dS <- defSurv(varname = "time", formula = "-14.6 - .5*x", shape = .35, transition = )
  dS <- defSurv(dS, varname = "time", formula = "-14.6 - 1.5*x", shape = .35, transition = 150)
  
  dd <- genData(1000, d1)
  expect_equal(genSurv(dd, dS)[x == 1, median(time)], 213, tolerance = 20)
})

test_that("genSurv throws errors", {
  skip_on_cran()
  dS <- defSurv(varname = "event_1", formula = "-10", shape = .3)
  dS <- defSurv(dS, "event_2", "-6.5", shape = .4)
  dS <- defSurv(dS, "event_3", "-7", shape = .5)
  
  dd <- genData(5)
  expect_error(genSurv(dd, dS, timeName = "event_1", keepEvents = TRUE),
               "Variable event_1 previously defined!")
  expect_error(genSurv(dd, dS, timeName = "event_1", censorName = "censor"),
               "Variable censor not previously defined!")
})

test_that("addCompRisk works", {
  skip_on_cran()
  dS <- defSurv(varname = "event_1", formula = "-10", shape = .3)
  dS <- defSurv(dS, "event_2", "-6.5", shape = .4)
  
  dd <- genData(1000)
  dd <- genSurv(dd, dS)
  
  expect_equal(addCompRisk(dd, c("event_1", "event_2"), "time")[, mean(event)],
               1.8,
               tolerance = .1
  )
  
  expect_equivalent(
    names(addCompRisk(dd, c("event_1", "event_2"), "time")[, table(event)]),
    c("1", "2")
  )
  
  expect_equivalent(
    names(addCompRisk(dd, c("event_1", "event_2"), "time", "event_2")[, table(event)]),
    c("0", "1")
  )
})

test_that("genSurv handles different shape and scale definitions with warnings", {
  skip_on_cran()
  dS <- defSurv(varname = "time", formula = "-14.6 - .5*x", shape = .35, scale = 50, transition = 0)
  dS <- defSurv(dS, varname = "time", formula = "-14.6 - 1.5*x", shape = .45, scale = 50, transition = 150)
  
  d1 <- defData(varname = "x", formula = .5, dist = "binary")
  dd <- genData(100, d1)
  
  expect_warning(genSurv(dd, dS), "Shape definitions over periods are different. Only first definition will be used.")
  
  dS <- defSurv(varname = "time", formula = "-14.6 - .5*x", shape = .45, scale = 60, transition = 0)
  dS <- defSurv(dS, varname = "time", formula = "-14.6 - 1.5*x", shape = .45, scale = 50, transition = 150)
  
  d1 <- defData(varname = "x", formula = .5, dist = "binary")
  dd <- genData(100, d1)
  
  expect_warning(genSurv(dd, dS), "Scale definitions over periods are different. Only first definition will be used.")
})

test_that("genSurv works with default parameters", {
  skip_on_cran()
  dS <- defSurv(varname = "time", formula = "-14.6 - .5*x", shape = .35)
  d1 <- defData(varname = "x", formula = .5, dist = "binary")
  dd <- genData(100, d1)
  
  result <- genSurv(dd, dS)
  expect_true("time" %in% names(result))
  expect_equal(nrow(result), 100)
})

test_that("genSurv handles competing risks correctly", {
  skip_on_cran()
  dS <- defSurv(varname = "died", formula = "-10", shape = .3)
  dS <- defSurv(dS, "censor", "-6.5", shape = .4)
  
  dd <- genData(500)
  dd <- genSurv(dd, dS, timeName = "time", censorName = "censor", 
                eventName = "event", typeName = "type", keepEvents = TRUE)
  
  expect_true("time" %in% names(dd))
  expect_true("censor" %in% names(dd))
  expect_true("event" %in% names(dd))
  expect_true("type" %in% names(dd))
  expect_equal(nrow(dd), 500)
})
