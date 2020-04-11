context("Pseudo-random data generation")

test_that("Formula is parsed correctly", {
  n <- 15
  err <- "Failed to parse"
  link <- "no"
  expect_error(.genPseudoSeq(n, ";", link), err)
  expect_error(.genPseudoSeq(n, "", link), err)
  expect_error(.genPseudoSeq(n, "abc;3", link), err)
  expect_error(.genPseudoSeq(n, "1+2+3;a", link), err)
  expect_error(.genPseudoSeq(n, "1+b+3;3", link), err)
  expect_vector(.genPseudoSeq(6, "1+2+3;2", link), size = 6)
  
  expect_error(.genPseudoRandom(n, ";", link), err)
  expect_error(.genPseudoRandom(n, "", link), err)
  expect_error(.genPseudoRandom(n, "abc;3", link), err)
  expect_error(.genPseudoRandom(n, "1+2+3;a", link), err)
  expect_error(.genPseudoRandom(n, "1+b+3;3", link), err)
  expect_vector(.genPseudoRandom(6, "1+2+3;2", link), size = 6)
  expect_true(!any(is.na(.genPseudoRandom(6, "1+2+3;2", link))))
})

test_that("data is generated correctly", {
  err <- "Length mis"
  link <- "no"
  
  expect_vector(.genPseudoSeq(6, "1+2+3;2", link), size = 6)
  expect_length(unique(table(.genPseudoSeq(6, "1+2+3;2", "aa"))), 1)
  expect_vector(.genPseudoSeq(13, "1+2+3;2", "fill"), size = 13)
  expect_vector(.genPseudoSeq(4, "1+2+3;2", "fill"), size = 4)
  expect_error(.genPseudoSeq(13, "1+2+3;2", link), err)
  expect_error(.genPseudoSeq(4, "1+2+3;2", link), err)
  
  expect_vector(.genPseudoRandom(6, "1+2+3;2", link), size = 6)
  expect_vector(.genPseudoRandom(13, "1+2+3;2", "fill"), size = 13)
  expect_vector(.genPseudoRandom(4, "1+2+3;2", "fill"), size = 4)
  expect_error(.genPseudoRandom(13, "1+2+3;2", link), err)
  expect_error(.genPseudoRandom(4, "1+2+3;2", link), err)
})

test_that("User workflow functions correctly",{
  def <- defData(varname = "age", dist = "uniformInt", formula = "20;35")
  def <- defData(def, varname = "stim", dist = "pseudorandom", formula = "1+4+6+8;10")
  
  def2 <- defData(varname = "age", dist = "uniformInt", formula = "20;35")
  def2 <- defData(def2, varname = "stim", dist = "pseudorandom", formula = "1+4+6+8;10", link = "fill")
  
  def3 <- defData(varname = "age", dist = "uniformInt", formula = "20;35")
  
  
  expect_error(genData(35,def),"Length mis")
  expect_error(defData(def3, varname = "stim", dist = "pseudorandom", formula = "1,4,6,8;10", link = "fill"),"Equation")
  expect_true(!any(is.na(genData(40,def))))
  expect_length(unique(table(genData(40,def)$stim)),1)
  expect_true(!any(is.na(genData(35,def2))))
  expect_length(unique(table(genData(35,def2)$stim)),2)
  
})
