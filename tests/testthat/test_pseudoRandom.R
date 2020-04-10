context("Pseudo-random data generation")

test_that("Formula is parsed correctly", {
  n <- 15
  err <- "Failed to parse"
  link <- "no"
  expect_error(.genPseudoSeq(n, ";", link), err)
  expect_error(.genPseudoSeq(n, "", link), err)
  expect_error(.genPseudoSeq(n, "abc;3", link), err)
  expect_error(.genPseudoSeq(n, "1,2,3;a", link), err)
  expect_error(.genPseudoSeq(n, "1,b,3;3", link), err)
  expect_vector(.genPseudoSeq(6, "1,2,3;2", link), size = 6)
  
  expect_error(.genPseudoRandom(n, ";", link), err)
  expect_error(.genPseudoRandom(n, "", link), err)
  expect_error(.genPseudoRandom(n, "abc;3", link), err)
  expect_error(.genPseudoRandom(n, "1,2,3;a", link), err)
  expect_error(.genPseudoRandom(n, "1,b,3;3", link), err)
  expect_vector(.genPseudoRandom(6, "1,2,3;2", link), size = 6)
})

test_that("data is generated correctly", {
  err <- "Length mis"
  link <- "no"
  
  expect_vector(.genPseudoSeq(6, "1,2,3;2", link), size = 6)
  expect_length(unique(table(.genPseudoSeq(6, "1,2,3;2", "aa")), 1))
  expect_vector(.genPseudoSeq(13, "1,2,3;2", "fill"), size = 13)
  expect_vector(.genPseudoSeq(4, "1,2,3;2", "fill"), size = 4)
  expect_error(.genPseudoSeq(13, "1,2,3;2", link), err)
  expect_error(.genPseudoSeq(4, "1,2,3;2", link), err)
  
  expect_vector(.genPseudoRandom(6, "1,2,3;2", link), size = 6)
  expect_vector(.genPseudoRandom(13, "1,2,3;2", "fill"), size = 13)
  expect_vector(.genPseudoRandom(4, "1,2,3;2", "fill"), size = 4)
  expect_error(.genPseudoRandom(13, "1,2,3;2", link), err)
  expect_error(.genPseudoRandom(4, "1,2,3;2", link), err)
})