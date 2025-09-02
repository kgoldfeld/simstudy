library(testthat)
library(simstudy)
library(data.table)

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

  expect_equal(range(simstudy:::.addStrataCode(data, "male")$.stratum), c(1, 2))
  expect_equal(range(simstudy:::.addStrataCode(data, c("male", "over65"))$.stratum), c(1, 4))
  expect_equal(simstudy:::.addStrataCode(data, "male")[, .SD, .SDcols = !".stratum"], data)
  expect_error(simstudy:::.addStrataCode(data, ""))
})

# simstudy:::.stratSamp ----
test_that("stratified samples are drawn correctly.", {
  skip_on_cran()
  expect_length(simstudy:::.stratSamp(1, 2), 1)
  expect_length(simstudy:::.stratSamp(2, 4), 2)
  expect_length(simstudy:::.stratSamp(50, 3), 50)
  expect_gte(table(simstudy:::.stratSamp(148, 2, c(1, 2)))[1], 49)
  expect_gte(table(simstudy:::.stratSamp(148, 2, c(1, 2)))[2], 98)
  expect_true(all(table(simstudy:::.stratSamp(150, 2, c(1, 2))) == c(50, 100)))
  expect_equal(range(simstudy:::.stratSamp(50, 3)), c(1, 3))
})

# genCluster <----

# Mock function to simulate the behavior of mergeData used within genCluster
mergeData <- function(dtClust, dt, cLevelVar) {
  merge(dtClust, dt, by = cLevelVar, all = TRUE)
}

test_that("genCluster handles missing arguments", {
  skip_on_cran()
  
  dtClust <- data.table(idSchool = 1:3, nClasses = c(2, 3, 4))
  
  expect_error(genCluster(), "argument 'dtClust' is missing")
  expect_error(genCluster(dtClust), "argument 'cLevelVar' is missing")
  expect_error(genCluster(dtClust, "idSchool"), "argument 'numIndsVar' is missing")
  expect_error(genCluster(dtClust, "idSchool", "nClasses"), "argument 'level1ID' is missing")
})

test_that("genCluster generates data with correct dimensions when numIndsVar is a column name", {
  skip_on_cran()
  
  nS <- max(3, rpois(1, 11))
  nC <- rpois(nS, 15)
  dtClust <- data.table(idSchool = 1:nS, nClasses = nC)
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass")
  
  expect_equal(nrow(result), sum(dtClust$nClasses))
  expect_equal(ncol(result), 3)
})

test_that("genCluster generates data with correct dimensions when numIndsVar is a single integer", {
  skip_on_cran()
  
  nS <- max(3, rpois(1, 11))
  nC <- rpois(1, 15)
  dtClust <- data.table(idSchool = 1:nS, nClasses = nC)
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass")
  
  expect_equal(nrow(result), nC * nrow(dtClust))
  expect_equal(ncol(result), 3)
})

test_that("genCluster includes all level 2 data columns when allLevel2 is TRUE", {
  
  skip_on_cran()
  
  nSchools <- max(3, rpois(1, 6))
  nClasses <- sample(2:10, size = nSchools, replace = TRUE)
  extraVar <- sample(LETTERS, nSchools, replace = FALSE)
  
  dtClust <- data.table(idSchool = 1:3, nClasses = c(2, 3, 4), extraCol = c("A", "B", "C"))
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass", allLevel2 = TRUE)

  expect_true("extraCol" %in% names(result))
})

test_that("genCluster includes only level 1 and 2 ids when allLevel2 is FALSE", {
  
  skip_on_cran()
  
  nSchools <- max(3, rpois(1, 6))
  nClasses <- sample(2:10, size = nSchools, replace = TRUE)
  extraVar <- sample(LETTERS, nSchools, replace = FALSE)
  
  dtClust <- data.table(idSchool = 1:nSchools, nClasses = nClasses, extraCol = extraVar)
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass", allLevel2 = FALSE)

  expect_false("extraCol" %in% names(result))
  expect_true(all(c("idSchool", "idClass") %in% names(result)))
})

test_that("genCluster generates unique level 1 ids", {
  skip_on_cran()
  
  nSchools <- max(3, rpois(1, 6))
  nClasses <- sample(2:10, size = nSchools, replace = TRUE)

  dtClust <- data.table(idSchool = 1:nSchools, nClasses = nClasses)
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass")

  expect_equal(length(unique(result$idClass)), nrow(result))
})

test_that("genCluster handles single row data table", {
  skip_on_cran()
  
  nClasses <- max(3, rpois(1, 25))
  dtClust <- data.table(idSchool = 1, nClasses = nClasses)
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass")

  expect_equal(nrow(result), nClasses)
  expect_equal(ncol(result), 3)
})

test_that("genCluster handles multiple clusters with single observation each", {
  
  skip_on_cran()
  
  nClasses <- max(3, rpois(1, 25))
  dtClust <- data.table(idSchool = 1:nClasses, nClasses = rep(1, nClasses))
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass")

  expect_equal(nrow(result), nClasses)
  expect_equal(ncol(result), 3)
})

test_that("genCluster generates correct number of observations per cluster", {
  
  skip_on_cran()
  
  nSchools <- max(3, rpois(1, 6))
  nClasses <- sample(2:10, size = nSchools, replace = TRUE)
  
  dtClust <- data.table(idSchool = 1:nSchools, nClasses = nClasses)
  result <- genCluster(dtClust, "idSchool", "nClasses", "idClass")

  for (i in 1:nrow(dtClust)) {
    expect_equal(nrow(result[idSchool == dtClust$idSchool[i]]), dtClust$nClasses[i])
  }
})

test_that("genCluster accepts numeric value for numIndsVar", {
  
  skip_on_cran()
  
  nSchools <- max(3, rpois(1, 6))
  N <- max(3, rpois(1, 12))

  dtClust <- data.table(idSchool = 1:nSchools)
  result <- genCluster(dtClust, cLevelVar = "idSchool", numIndsVar = N, "idClass")
  
  for (i in 1:nrow(dtClust)) {
    expect_equal(nrow(result[idSchool == dtClust$idSchool[i]]), N)
  }
})

# genNthEvent <-----

test_that("genNthEvent returns a data.table with correct columns", {
  
  skip_on_cran()
  
  defD <- 
    defData(varname = "effect", formula = 0 , variance = 1, dist = "normal")
    
  defE <- defDataAdd(
    varname = "event", formula = "-2.5 + .3*period + effect",
    dist = "binary", link = "logit"
  )
  
  d <- genData(100, defD)
  d <- addPeriods(d, 10)
  result <- genNthEvent(d, defEvent = defE, nEvents = 3)
  
  expect_true(is.data.table(result))
  expect_true(all(c("id", "period", "event") %in% names(result)))
})

test_that("genNthEvent stops generating events after the nth event", {
  
  skip_on_cran()
  
  nEvents <- max(3, rpois(1, 5))
  
  defD <- defData(
    varname = "effect", formula = 0, variance = 1,
    dist = "normal"
  )
  defE <- defDataAdd(
    varname = "event", formula = "-2.5 + .3*period + effect",
    dist = "binary", link = "logit"
  )

  d <- genData(100, defD)
  d <- addPeriods(d, sample(10:20,1))
  result <- genNthEvent(d, defEvent = defE, nEvents = nEvents)
  
  # Make sure no one has more than maximum number of events

  event_counts <- result[, .(N=sum(event)), keyby = id][N > nEvents]
  expect_equal(nrow(event_counts), 0 )
  
  # Make sure no one has records after they hit threshold
  
  result[, cumsum := cumsum(event), keyby = id]
  max_events <- result[cumsum == nEvents]
  expect_equal(nrow(max_events), length(max_events[, unique(id)]) )
  
})

test_that("genNthEvent handles cases where no events occur", {
  
  skip_on_cran()
  
  nEvents <- max(3, rpois(1, 5))
  nPeriods <- max(5, rpois(1, 15))
  
  defD <- defData(
    varname = "effect", formula = 0 , variance = 1,
    dist = "normal"
  )
  defE <- defDataAdd(
    varname = "event", formula = 0, dist = "binary"
  )

  d <- genData(100, defD)
  d <- addPeriods(d, nPeriods)
  result <- genNthEvent(d, defEvent = defE, nEvents = nEvents)

  expect_true(all(result$events == 0))
  expect_true(all(result[, .N, keyby = id][, N] == nPeriods))
})

test_that("genNthEvent handles cases where fewer than n events occur", {
  
  skip_on_cran()
  
  nEvents <- max(3, rpois(1, 5))
  nPeriods <- nEvents + 2
  
  defD <- defData(
    varname = "effect", formula = 0, variance = 1,
    dist = "normal"
  )
  defE <- defDataAdd(
    varname = "event", formula = "-2.5 + .3*period + effect",
    dist = "binary", link = "logit"
  )

  d <- genData(100, defD)
  d <- addPeriods(d, nPeriods)
  result <- genNthEvent(d, defEvent = defE, nEvents = nEvents)

  event_counts <- result[, .(N = sum(event)), by = id]
  expect_true(nrow(event_counts[N < nEvents]) > 0)
})

test_that("genNthEvent handles custom period and id names", {
  
  skip_on_cran()
  
  defD <- defData(
    varname = "effect", formula = 0, variance = 1,
    dist = "normal"
  )
  defE <- defDataAdd(
    varname = "died", formula = "-2.5 + .3*time + effect",
    dist = "binary", link = "logit"
  )

  d <- genData(100, defD, id = "custom_id")
  d <- addPeriods(d, nPeriods = 10, idvars = "custom_id", perName = "time")
  
  result <- genNthEvent(d, defEvent = defE, nEvents = 3, perName = "time", id = "custom_id")

  expect_true(is.data.table(result))
  expect_true(all(c("custom_id", "time", "died") %in% names(result)))
})

# trtStepWedge <-------

test_that("trtStepWedge returns error with missing data.table argument", {
  skip_on_cran()
  
  expect_error(trtStepWedge(clustID = "cluster", nWaves = 3, lenWaves = 3, startPer = 2),
               "Data table argument is missing")
})

test_that("trtStepWedge returns error with duplicate group name", {
  
  skip_on_cran()
  
  defc <- 
    defData(
      varname = "ceffect", formula = 0, variance = .10,
      dist = "normal", id = "cluster"
    ) |>
    defData(varname = "m", formula = 10, dist = "nonrandom") |>
    defData(varname = "rx", formula = .3, dist = "binary")
  
  dc <- genData(12, defc)
  dp <- addPeriods(dc, 12, "cluster")
  expect_error(trtStepWedge(dp, clustID = "cluster", nWaves = 3, lenWaves = 3, startPer = 2),
               "Group name has previously been defined in data table")

})

test_that("trtStepWedge returns error with wrong period name", {
  
  skip_on_cran()
  
  defc <- 
    defData(
      varname = "ceffect", formula = 0, variance = .10,
      dist = "normal", id = "cluster"
    ) |>
    defData(varname = "m", formula = 10, dist = "nonrandom") 
  
  dc <- genData(12, defc)
  dp <- addPeriods(dc, 12, "cluster")
  expect_error(
    trtStepWedge(dp, clustID = "cluster", nWaves = 3, lenWaves = 3, startPer = 2, perName = "p1"),
               "Period name has not been defined in data table")
  
})

test_that("trtStepWedge returns a data.table with correct columns", {
  
  skip_on_cran()
  
  defc <- 
    defData(
      varname = "ceffect", formula = 0, variance = .10,
      dist = "normal", id = "cluster"
    ) |>
    defData(varname = "m", formula = 10, dist = "nonrandom")
  
  dc <- genData(12, defc)
  dp <- addPeriods(dc, 12, "cluster")
  result <- trtStepWedge(dp, "cluster", nWaves = 3, lenWaves = 3, startPer = 2)
  
  expect_true(is.data.table(result))
  expect_true(all(c("cluster", "period", "rx") %in% names(result)))
})

test_that("trtStepWedge assigns treatment correctly for a simple case", {
  
  skip_on_cran()
  
  W <- max(2, rpois(1,4))
  L <- max(2, rpois(1,4))
  S <- max(1, rpois(1,3))
  E <- rpois(1, 2)
  
  K <- max(2, rpois(1, 4)) 
  nClust <- W * K
  nPer <- W * L + S + E
  
  defc <- 
    defData(
      varname = "ceffect", formula = 0, variance = 0.10,
      dist = "normal", id = "cluster"
    ) |>
    defData(varname = "m", formula = 10, dist = "nonrandom")
  
  dc <- genData(nClust, defc)
  dp <- addPeriods(dc, nPer, "cluster")
  result <- trtStepWedge(dp, "cluster", nWaves = W, lenWaves = L, startPer = S)
  
  first_rx_period <- result[rx == 1, .SD[1], by = cluster]
  wave_data <- first_rx_period[, .N, keyby = startTrt]
  
  expect_true(all(wave_data[, startTrt - shift(startTrt, type = "lag")][-1] == L))
  expect_true(all(wave_data[, N] == K))
  expect_equal(nrow(wave_data), W)
  expect_equal(wave_data[1, startTrt], S)
  
})

test_that("trtStepWedge handles cases where the number of clusters is not divisible by the number of waves", {
  
  skip_on_cran()
  
  W <- max(2, rpois(1,4))
  L <- max(2, rpois(1,4))
  S <- max(1, rpois(1,3))
  E <- rpois(1, 2)
  
  K <- max(2, rpois(1, 4)) 
  nClust <- W * K + 1 
  nPer <- W * L + S + E
  
  defc <- defData(
    varname = "ceffect", formula = 0, variance = .10,
    dist = "normal", id = "cluster"
  )
  defc <- defData(defc, "m", formula = 10, dist = "nonrandom")

  dc <- genData(nClust, defc)
  dp <- addPeriods(dc, nPer, "cluster")

  expect_error(trtStepWedge(dp, "cluster", nWaves = W, lenWaves = L, startPer = S),
    glue::glue("Cannot create equal size waves with {nClust} clusters and {W} waves."))
})

test_that("trtStepWedge handles cases where the number of periods is insufficient for the design", {
  
  skip_on_cran()
  
  W <- max(2, rpois(1,4))
  L <- max(2, rpois(1,4))
  S <- max(1, rpois(1,3))
  E <- rpois(1, 2)
  
  K <- max(2, rpois(1, 4)) 
  nClust <- W * K 
  nPer <- W + S 
  
  defc <- defData(
    varname = "ceffect", formula = 0, variance = .10,
    dist = "normal", id = "cluster"
  )
  defc <- defData(defc, "m", formula = 10, dist = "nonrandom")
  
  dc <- genData(nClust, defc)
  dp <- addPeriods(dc, nPer, "cluster")

  expect_error(trtStepWedge(dp, "cluster", nWaves = W, lenWaves = L, startPer = S),
    glue::glue("Design requires {S + (W - 1) * L + 1} periods but only {nPer} generated."))
})

test_that("trtStepWedge handles the lag period correctly", {
  
  skip_on_cran()
  
  W <- max(2, rpois(1,4))
  L <- max(2, rpois(1,4))
  S <- max(1, rpois(1,3))
  E <- rpois(1, 2)
  LAG <- max(2, rpois(1,4))
  
  K <- max(2, rpois(1, 4)) 
  
  nClust <- W * K
  nPer <- W * (L+LAG) + S + E
  
  defc <- 
    defData(
      varname = "ceffect", formula = 0, variance = 0.10,
      dist = "normal", id = "cluster"
    ) |>
    defData(varname = "m", formula = 10, dist = "nonrandom")
  
  dc <- genData(nClust, defc)
  dp <- addPeriods(dc, nPer, "cluster")
  result <- trtStepWedge(dp, "cluster", nWaves = W, lenWaves = L, startPer = S, lag = LAG)
  
  start_periods <- merge(
    result[xr == 1, .SD[1, .(period, startTrt)], keyby = cluster],
    result[rx == 1, .SD[1, .(lagPeriod = period, lagStart = startTrt)], keyby = cluster],
    by = "cluster"
  )
  
  expect_true(all(start_periods[, period] == start_periods[, startTrt]))
  expect_true(all(start_periods[, lagStart] == start_periods[, startTrt]))
  expect_true(all(start_periods[, lagPeriod - lagStart] == LAG))
  expect_equal(nrow(start_periods), nClust)
  
  waves <- start_periods[, .N, keyby = .(startTrt, lagPeriod)]
  expect_equal(nrow(waves), W)
  expect_true(waves[, all(N == K)])
  expect_true(waves[, all( (lagPeriod - startTrt) == LAG)])
  expect_true(all(waves[, startTrt - shift(startTrt, type = "lag")][-1] == L))
  expect_true(all(waves[, lagPeriod - shift(lagPeriod, type = "lag")][-1] == L))
  
})

test_that("trtStepWedge handles lag period error correctly", {
  
  skip_on_cran()
  
  W <- max(3, rpois(1,5))
  L <- max(1, rpois(1,4))
  S <- max(1, rpois(1,3))
  E <- rpois(1, 2)
  LAG <- max(3, rpois(1,4))
  
  K <- max(2, rpois(1, 4)) 
  
  nClust <- W * K
  nPer <- W * L - 1
  
  defc <- 
    defData(
      varname = "ceffect", formula = 0, variance = 0.10,
      dist = "normal", id = "cluster"
    ) |>
    defData(varname = "m", formula = 10, dist = "nonrandom")
  
  dc <- genData(nClust, defc)
  dp <- addPeriods(dc, nPer, "cluster")
  expect_error(trtStepWedge(dp, "cluster", nWaves = W, lenWaves = L, startPer = S, lag = LAG),
    glue::glue("Design requires {(S + (W - 1) * (L + LAG) + 1)} periods but only {nPer} generated.")
  )
  
  
})

# trtAssign <-----

test_that("Returns correct number of rows and new group column", {
  
  skip_on_cran()
  
  N <- max(100, rpois(1, 200))
  
  dt <- genData(N)
  result <- trtAssign(dt, nTrt = 3, balanced = TRUE)
  expect_equal(nrow(result), N)
  expect_true("trtGrp" %in% names(result))
  expect_type(result$trtGrp, "integer")
})

test_that("Balanced assignment without stratification distributes approximately evenly", {

  skip_on_cran()
  
  G <- max(3, rpois(1, 3))
  dt <- genData(max(100, rpois(1, 300)))
  result <- trtAssign(dt, nTrt = G, balanced = TRUE)
  dist <- result[, .N, by = trtGrp]
  dist[, p := N/sum(N)]
  
  x <-abs(dist[, p - mean(p)])
  expect_true(all(x < 0.05)) 
})

test_that("Balanced assignment with stratification respects strata", {
  
  skip_on_cran()
  
  G <- max(3, rpois(1, 3))
  dt <- genData(max(200, rpois(1, 400)))
  def <- defData(varname = "male", formula = rbeta(1, 10, 10), dist = "binary")
  dt <- genData(300, def)
  result <- trtAssign(dt, nTrt = 2, strata = "male", balanced = TRUE)
  dist <- result[, .N, keyby = .(male, trtGrp)]
  dist[, p:= N/sum(N), keyby = male]
  x <-abs(dist[, p - mean(p)])
  expect_true(all(x < 0.05)) 
})

test_that("Group name is respected when grpName is specified", {
  
  skip_on_cran()
  
  dt <- genData(100)
  result <- trtAssign(dt, nTrt = 3, balanced = TRUE, grpName = "arm")
  expect_true("arm" %in% names(result))
  expect_false("trtGrp" %in% names(result))
})

test_that("balanced assignment honors ratio", {
  
  skip_on_cran()
  
  G <- max(3, rpois(1, 4))
  ratio <- pmax(1, rpois(G, 2))
  dt <- genData(1000)
  
  result <- trtAssign(dt, nTrt = G, balanced = TRUE, ratio = ratio, grpName = "arm")
  prop <- result[, .(p =.N/1000), keyby = arm][, p]
  x <- abs(prop - ratio/sum(ratio))
  expect_true(all(x < 0.01)) 
})

### This generates some errors!

test_that("Unbalanced assignment honors ratio", {

  skip_on_cran()

  G <- max(3, rpois(1, 4))
  ratio <- pmax(1, rpois(G, 1))
  dt <- genData(1000)

  result <- trtAssign(dt, nTrt = G, balanced = FALSE, ratio = ratio, grpName = "arm")
  prop <- result[, .(p =.N/1000), keyby = arm][, p]
  x <- abs(prop - ratio/sum(ratio))
  expect_true(all(x < 0.05))
  
})

test_that("Binary group coding returns 0/1", {
  
  skip_on_cran()
  
  dt <- genData(100)
  result <- trtAssign(dt, nTrt = 2, balanced = TRUE)
  expect_true(all(result$trtGrp %in% c(0, 1)))
})

test_that("Throws error when grpName already exists", {
  
  skip_on_cran()
  
  dt <- genData(100)
  dt[, trtGrp := 1]
  expect_error(trtAssign(dt, nTrt = 2, balanced = TRUE), "Variable trtGrp previously defined!")
})

test_that("Throws error when ratio length does not match nTrt", {
  
  skip_on_cran()
  
  nTrt <- max(3, rpois(1, 3))
  dif <- max(1, rpois(1,2))
  ratio <- rpois(nTrt + dif, 2)
  dt <- genData(100)
  expect_error(trtAssign(dt, nTrt = 3, balanced = TRUE, ratio = ratio), 
      glue::glue("nTrt not equal to {length(ratio)}!"))
})

test_that("Single formula returns 0/1 group when balanced = TRUE", {
  
  skip_on_cran()
  
  def <- defData(varname = "x", dist = "binary", formula = 0.5)
  dt <- genData(100, def)
  
  dt_out <- trtAssign(dt, nTrt = 2, grpName = "trt")
  expect_true(all(dt_out$trt %in% c(0, 1)))
})

test_that("Single formula returns 0/1 group when balanced = FALSE", {
  
  skip_on_cran()
  
  def <- defData(varname = "x", dist = "binary", formula = 0.5)
  dt <- genData(100, def)
  
  dt_out <- trtAssign(dt, nTrt = 2, balance = FALSE, grpName = "trt")
  expect_true(all(dt_out$trt %in% c(0, 1)))
})

# trtObserve <-----

test_that("Function returns expected output columns", {
  
  skip_on_cran()
  
  def <- defData(varname = "x1", dist = "binary", formula = .5)
  def <- defData(def, varname = "x2", dist = "binary", formula = "-1 + 2 * x1", link = "logit")
  dt <- genData(100, def)
  
  formulas <- c("-1 + x1 + x2", "0 + x1 - x2")
  dt_out <- trtObserve(dt, formulas, logit.link = TRUE, grpName = "group")
  
  expect_true("group" %in% colnames(dt_out))
  expect_type(dt_out$group, "integer")
  expect_true(all(dt_out$group %in% 1:3))  # since there are 2 formulas, 3 groups
  
})

test_that("Output changes when using identity vs. logit link", {
  
  skip_on_cran()
  
  def <- defData(varname = "x1", dist = "binary", formula = .5)
  dt <- genData(1000, def)
  formulas <- c("0.2 + 0.1 * x1", "0.5 - 0.2 * x1")
  
  out_logit <- trtObserve(dt, formulas, logit.link = TRUE, grpName = "grp")
  out_identity <- trtObserve(dt, formulas, logit.link = FALSE, grpName = "grp")

  prop_logit <- prop.table(table(out_logit$grp))
  prop_identity <- prop.table(table(out_identity$grp)) 
  
  # Check that the two group assignment distributions are not too similar
  # We'll check that the absolute difference in group proportions is sufficiently large
  diff <- abs(prop_logit - prop_identity[names(prop_logit)])  # align by group labels
  expect_true(any(diff > 0.05))  # Threshold can be adjusted
})

test_that("Group name already in dt triggers error", {
  
  skip_on_cran()
  
  def <- defData(varname = "x1", dist = "binary", formula = .5)
  dt <- genData(100, def)
  dt[, exposure := 1]  # preexisting column
  
  formulas <- c("x1")
  expect_error(trtObserve(dt, formulas, grpName = "exposure"),
               "Group name has previously been defined in data table")
})

test_that("Missing data.table triggers error", {
  
  skip_on_cran()
  
  expect_error(trtObserve(formulas = c("1")), "Data table argument is missing")
  
})

test_that("Single formula returns 0/1 group", {
  
  skip_on_cran()
  
  def <- defData(varname = "x", dist = "binary", formula = 0.5)
  dt <- genData(100, def)
  
  dt_out <- trtObserve(dt, formulas = "0.5 * x", logit.link = FALSE, grpName = "trt")
  expect_true(all(dt_out$trt %in% c(0, 1)))
})

test_that("Rough distribution of group assignments for balanced formulas", {
  
  skip_on_cran()
  
  G <- max(3, rpois(1, 5))
  
  def <- defData(varname = "x", dist = "binary", formula = .5)
  dt <- genData(3000, def)
  formulas <- rep(1/G, G)  # logit of 0 = probability 0.5 before normalization
  
  dt_out <- trtObserve(dt, formulas = formulas, logit.link = FALSE, grpName = "grp")
  
  prop_table <- prop.table(table(dt_out$grp))
  expect_true(all(abs(prop_table - 1/G) < 0.025))  # roughly equal
  
})

 
