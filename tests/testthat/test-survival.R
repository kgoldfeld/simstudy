test_that("defSurv kicks out transition error", {
  skip_on_cran()
  expect_error(defSurv(varname = "censor", formula = "-7", shape = 0.55, transition = 150))
})

test_that("genSurv runs OK", {
  skip_on_cran()
  dS <- defSurv(varname = "event_1", formula = "-10", shape = 0.3)
  dS <- defSurv(dS, "event_2", "-6.5", shape = 0.4)
  dS <- defSurv(dS, "event_3", "-7", shape = 0.5)

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

  dS <- defSurv(varname = "time", formula = "-14.6 - .5*x", shape = .35, transition = 0)
  dS <- defSurv(dS, varname = "time", formula = "-14.6 - 1.5*x", shape = .35, transition = 150)

  dd <- genData(1000, d1)
  expect_equal(genSurv(dd, dS)[x == 1, mean(time)], 213, tolerance = .1)
})

test_that("genSurv throws errors", {
  skip_on_cran()
  dS <- defSurv(varname = "event_1", formula = "-10", shape = 0.3)
  dS <- defSurv(dS, "event_2", "-6.5", shape = 0.4)
  dS <- defSurv(dS, "event_3", "-7", shape = 0.5)

  dd <- genData(5)
  expect_error(genSurv(dd, dS, timeName = "event_1", keepEvents = TRUE))
  expect_error(genSurv(dd, dS, timeName = "event_1", censorName = "censor"))
})

test_that("addCmpRisk works", {
  skip_on_cran()
  dS <- defSurv(varname = "event_1", formula = "-10", shape = 0.3)
  dS <- defSurv(dS, "event_2", "-6.5", shape = 0.4)

  dd <- genData(5000)
  dd <- genSurv(dd, dS)

  expect_equal(addCompRisk(dd, c("event_1", "event_2"), "time")[, mean(event)],
    1.8,
    tolerance = 0.1
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
