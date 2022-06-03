# defMiss ----
test_that("defMiss produces correct output", {
  varname <- "test"
  formula <- ".3"
  logit.link <- FALSE
  baseline <- TRUE
  monotonic <- FALSE
  
  def <- data.table::data.table(
    varname, formula, logit.link,
    baseline, monotonic
  )
  
  expect_equal(def, defMiss(
    NULL, varname, formula, logit.link,
    baseline, monotonic
  ))
  
  expect_equal(rbind(def, def), defMiss(
    def, varname, formula,
    logit.link, baseline, monotonic
  ))
})

# genMiss ----
test_that("genMiss works", {
  # not repeated
  def1 <- defData(varname = "m", dist = "binary", formula = .5)
  def1 <- defData(def1, "u", dist = "binary", formula = .5)
  def1 <- defData(def1, "x1", dist="normal", formula = "20*m + 20*u", variance = 2)
  def1 <- defData(def1, "x2", dist="normal", formula = "20*m + 20*u", variance = 2)
  def1 <- defData(def1, "x3", dist="normal", formula = "20*m + 20*u", variance = 2)
  
  dtAct <- genData(5000, def1)
  
  hardProbForm <- runif(1)
  form1val0 <- runif(1)
  form1val1 <- runif(1)
  form1 <- paste0(form1val0, "*(1-m) + m*", form1val1)
  
  defM <- defMiss(varname = "x1", formula = hardProbForm, logit.link = FALSE)
  defM <- defMiss(defM, varname = "x2", formula = form1, logit.link = FALSE)
  defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed
  
  missMat <- genMiss(dtAct, defM, idvars = "id")
  
  ## check probabilistic column has correct distribution
  hardProbAct <- mean(missMat[, x1])
  hPDiff <- abs(hardProbAct - hardProbForm)
  
  expect_true(hPDiff < 0.02)
  
  ## check dependent column 1
  #check probs when m is 0/1, u is 0/1
  
  mIs0 <- dtAct[, m == 0]
  mIs1 <- dtAct[, m == 1]

  missMat0 <- missMat[mIs0]
  missMat1 <- missMat[mIs1]
  
  avg0 <- mean(missMat0[, x2])
  avg1 <- mean(missMat1[, x2])
  
  diff0 <- abs(form1val0 -avg0)
  diff1 <- abs(form1val1 -avg1)
  
  expect_true(diff0 < 0.02 && diff1 < 0.02)
  
  ## check empty column
  expect_true(all(missMat[, u] == 1))
  
  # repeated (not worth looking at if !baseline and !mono)
  ## includes lags
  
  ## baseline
  #if missing at baseline, missing at all other perios
  idNum <- 500
  
  dtAct <- genData(idNum, def1)
  dtAct <- trtObserve(dtAct, formulas = .5, logit.link = FALSE, grpName = "rx")
  
  defLong <- defDataAdd(varname = "y", dist = "normal", formula = "10 + period*2 + 2 * rx", variance = 2)
  
  dtTime <- addPeriods(dtAct, nPeriods = 4)
  dtTime <- addColumns(defLong, dtTime)
  
  defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
  defMlong <- defMiss(defMlong,varname = "y", formula = "-1.5 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = FALSE)
  
  missMatLong <- genMiss(dtTime, defMlong, idvars = c("id","rx"), repeated = TRUE, periodvar = "period")
  
  casevec <- NULL
  for(i in 1:idNum) {
    tempMM <- missMatLong[id == i]
    if(tempMM[1, x1] == 1) {
      casevec <- c(casevec, all(tempMM[, x1] == 1))
    }
  }
  
  expect_true(all(casevec == TRUE))
  
  ### monotonic
  #same as baseline stipulation
  defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
  defMlong <- defMiss(defMlong,varname = "y", formula = "-1.8 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = TRUE)
  
  missMatLong <- genMiss(dtTime, defMlong, idvars = c("id","rx"), repeated = TRUE, periodvar = "period")
  
  casevec <- NULL
  for(i in 1:idNum) {
    tempMM <- missMatLong[id == i]
    # TODO is there a cleaner way to do this?
    if(tempMM[1, x1] == 1) {
      casevec <- c(casevec, all(tempMM[, x1] == 1))
    } else if(tempMM[2, x1] == 1) {
      casevec <- c(casevec, (all(tempMM[-1, x1] == 1) && tempMM[1, x1] == 0))
    } else if(tempMM[3, x1] == 1) {
      casevec <- c(casevec, (all(tempMM[-(1:2), x1] == 1) && tempMM[1:2, x1] == 0))
    } else if(tempMM[4, x1] == 1) {
      casevec <- c(casevec, (all(tempMM[-(1:3), x1] == 1) && tempMM[1:3, x1] == 0))
    }
  }
  
  expect_true(all(casevec == TRUE))
  
  ## includes lags
  
  
})

# .checkLags ----
test_that("LAG() usage is detected correctly.", {
  hasLag <- c("a + 5 | LAG(3) - 4", " 4 + 3 ", "LAG(x)")
  noLag <- c("a +5", "3 + 1", "log(3) + b")
  
  expect_true(.checkLags(hasLag))
  expect_true(.checkLags(hasLag[1]))
  
  expect_false(.checkLags(noLag))
  expect_false(.checkLags(noLag[1]))
})

# .addLags ----
test_that(".addLags throws errors.", {
  ok <- "1 * LAG(rx)"
  doubleErr <- "LAG(rx) + LAG(rx)*2"
  notFound <- "1 + LAG(NO)"
  data <- genData(200)
  data <- trtAssign(data, grpName = "rx")
  dataLong <- addPeriods(data, nPeriods = 5)
  reservedName <- copy(dataLong)
  reservedName$.rx1 <- reservedName$rx
  
  expect_error(.addLags(data, ok), "not longitudinal")
  expect_error(.addLags(dataLong, doubleErr), "Repeated lag term")
  expect_error(.addLags(dataLong, notFound), "not in data table")
  expect_error(.addLags(reservedName, ok), "do not use")
})

test_that("LAGS are added as expected.", {
  data <- genData(200)
  dataLong <- addPeriods(data, nPeriods = 5)
  dataLong$rx <- sample(c(0, 1), 1000, replace = TRUE)
  dataLong$tx <- sample(c(0, 1), 1000, replace = TRUE)
  
  dataAfter <- copy(dataLong)
  
  # manual version of data.table::shift to find possible issues in case of
  # changes in data.table. data.table version:
  # dataAfter[, .rx1 := shift(.SD[, rx], n = 1L, fill = 0), by = id] #nolint
  rx1 <- vector(mode = "numeric", length = 1000)
  tx1 <- vector(mode = "numeric", length = 1000)
  for (i in 1:200) {
    rx1[((i - 1) * 5 + 1):((i - 1) * 5 + 5)] <- c(0, dataAfter[id == i, rx][1:4])
    tx1[((i - 1) * 5 + 1):((i - 1) * 5 + 5)] <- c(0, dataAfter[id == i, tx][1:4])
  }
  
  dataAfter$.rx1 <- rx1
  dataAfter$.tx1 <- tx1
  setkey(dataAfter, "timeID")
  setindex(dataAfter, NULL)
  
  noLAG <- " 1 * rx"
  origForm <- c("-2 + 1.5 * LAG(rx)", "-2.1 + 1.3 * LAG(tx)")
  lagNames <- c(".rx1", ".tx1")
  lagForms <- c("-2 + 1.5 * .rx1", "-2.1 + 1.3 * .tx1")
  
  expect_equal(.addLags(dataLong, c(origForm, noLAG)), list(dataAfter, c(lagForms, noLAG), lagNames))
})

# .genMissDataMat ----

test_that(".genMissDataMat works", {
  
  
  
})
