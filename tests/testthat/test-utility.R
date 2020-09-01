test_that("dotVars are parsed correctly", {
  extVar1 <- 23
  extVar2 <- 42
  res <- list(..extVar1 = 23, ..extVar2 = 42)

  expect_equal(.parseDotVars("a + ..extVar1 | b + ..extVar2"), res)
  expect_equal(.parseDotVars(c("a + ..extVar1", "b + ..extVar2")), res)
  expect_equal(length(.parseDotVars("a + b")), 0)

  expect_error(.parseDotVars("..extVar12"))
})

test_that("variables from different environments are parsed correctly", {
  extVar3 <- 7
  env1 <- new.env()
  env2 <- new.env(parent = env1)
  env1$extVar1 <- 23
  env2$extVar2 <- 42
  res <- list(..extVar1 = 23, ..extVar2 = 42, ..extVar3 = 7)

  expect_equal(.parseDotVars("a + ..extVar1 | b + ..extVar2 * ..extVar3"), res)
  expect_equal(.parseDotVars(c("a + ..extVar1 * ..extVar2", "b + ..extVar3")), res)

  with(
    env2, {
      expect_equal(.parseDotVars("a + ..extVar1 | b + ..extVar2 * ..extVar3"), res)
      expect_equal(.parseDotVars(c("a + ..extVar1 * ..extVar2", "b + ..extVar3")), res)
    }
  )
})