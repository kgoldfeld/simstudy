test_that("assertNotMissing works.", {
  skip_on_cran()
  
  testFunc <- function(x, y, z) {
    assertNotMissing(x = missing(x), y = missing(y), z = missing(z))
  }
  expect_silent(testFunc(1, 2, 3))
  expect_error(testFunc(), regexp = "and z", class = "simstudy::missingArgument")
  expect_error(testFunc(3, 2), regexp = "z", class = "simstudy::missingArgument")
})

test_that("assertNotEqual works.", {
  skip_on_cran()
  
  expect_silent(assertNotEqual(x = 1, y = 1, val = 2))
  expect_error(assertNotEqual(x = 1, val = 1, class = "simstudy::equal"))
})

test_that("assertAtLeast works.", {
  skip_on_cran()
  
  expect_silent(assertAtLeast(x = 2,  minVal = 2))
  expect_error(assertAtLeast(x = 1, minVal = 2), class = "simstudy::minError")
})

test_that("assertNotInVector works.", {
  skip_on_cran()
  
  expect_silent(assertNotInVector(var = 4, vec = c(1, 2, 3)))
  expect_error(assertNotInVector(var = 2, vec = c(1, 2, 3)), class = "simstudy::alreadyInVector")
})

test_that("assertAscending works.", {
  skip_on_cran()
  
  expect_silent(assertAscending(vec = c(1, 3, 5)))
  expect_error(assertAscending(vec = c(3, 1, 5)), class = "simstudy::wrongOrder")
})

test_that("assertDescending works.", {
  skip_on_cran()
  
  expect_silent(assertDescending(vec = c(5, 3, 1)))
  expect_error(assertDescending(vec = c(3, 1, 5)), class = "simstudy::wrongOrder")
})

test_that("assertPositive works.", {
  skip_on_cran()
  
  expect_silent(assertPositive(vec = c(1, 2, 1)))
  expect_error(assertPositive(vec = c(3, 1, 0)), class = "simstudy::wrongSign")
})

test_that("assertProbability works.", {
  skip_on_cran()
  
  p <- runif(5, 0, 1)
  expect_silent(assertProbability(vec = p))
  n <- rnorm(5, 0, 25)
  expect_error(assertProbability(vec = n), class = "simstudy::probError")
})

test_that("assertLengthEqual works.", {
  skip_on_cran()
  
  expect_error(assertLengthEqual(x = 5, y = c(1, 3)), class = "simstudy::lengthMismatch")
  expect_error(assertLengthEqual(x = 5, y = c(1, 3), z = list(a = 1, b = 2)), class = "simstudy::lengthMismatch")
  expect_silent(assertLengthEqual(y = c(1, 3), z = list(a = 1, b = 2), a = data.table(a = 1:3, b = 3:5)))
  expect_error(assertLengthEqual(x = 5), class = "simpleError")
})

test_that("assertEqual works.", {
  skip_on_cran()
  
  expect_error(assertEqual(x = 5, val = 6), class = "simstudy::notEqual")
  expect_error(assertEqual(x = 5, y = 6, val = 5), class = "simstudy::notEqual")
  expect_error(assertEqual(x = "one", val = "two"), class = "simstudy::notEqual")
  expect_silent(assertEqual(x = "three", y = "three", val = "three"))
})

test_that("assertLength works", {
  skip_on_cran()
  
  expect_error(assertLength(x = 5, y = c(1, 3), z = list(a = 3, b = 4), length = 2),
    class = "simstudy::lengthMismatch"
  )
  expect_silent(assertLength(x = 1, z = "b", length = 1))
})

test_that("assertAtLeastLength works", {
  skip_on_cran()
  
  expect_error(assertAtLeastLength(x = c("3", "4"), length = 3),
    class = "simstudy::lengthMismatch"
  )
  expect_silent(assertAtLeastLength(x = c("3", "4", "5"), length = 2))
})



test_that("assertClass works.", {
  skip_on_cran()
  
  expect_error(assertClass(x = c(1, 2, 3), y = "b", class = "data.frame"), class = "simstudy::wrongClass")
  expect_silent(assertClass(x = c(1, 2, 3), class = "numeric"))
})

test_that("assertType works.", {
  skip_on_cran()
  
  expect_error(assertType(vec = c(1.1, 2.2, 3.3), list = list(a = 1:10, b = "a"), type = "double"),
    class = "simstudy::wrongType"
  )
  expect_error(assertType(a = data.frame(a = rnorm(10), b = letters[1:10]), b = matrix(1:25, 5), type = "double"),
    regexp = "a and b",
    class = "simstudy::wrongType"
  )
  expect_error(assertType(a = list(1, 2, 3), type = "list"), class = "simstudy::wrongType")
  expect_silent(assertType(a = list(1, 2, 3), type = "list", deep = FALSE))
})

test_that("assertNumeric works.", {
  skip_on_cran()
  
  expect_error(assertNumeric(
    a = c(1, 2, 3), b = list(a = 1, b = 2.2, c = "d"),
    c = data.frame(a = 1:10, b = (1:10) * 1.1)
  ),
  regexp = "b", class = "simstudy::wrongType"
  )
  expect_silent(assertNumeric(
    a = c(1, 2, 3), b = list(a = 1, b = 2.2),
    c = data.frame(a = 1:10, b = (1:10) * 1.1),
    d = 5
  ))
})

test_that("assertNumericMatrix works.", {  
  skip_on_cran()

  a <- matrix(rnorm(16), nrow = 4)
  b <- a
  d <- rnorm(3)
  b[3, 4] <- sample(letters, 1)
  expect_error(assertNumericMatrix(b = b),
    class = "simstudy::wrongType")
  expect_error(assertNumericMatrix(d = d),
               class = "simstudy::wrongType")
  expect_silent(assertNumericMatrix(a = a))
})

test_that("assertInteger works.", {
  skip_on_cran()
  
  expect_error(assertInteger(
    a = c(1, 2, 3), b = list(a = 1, b = 2.2),
    c = data.frame(a = 1:10, b = (1:10) * 1.1),
    d = 1.1
  ),
  regexp = "c and d", class = "simstudy::wrongType"
  )

  expect_silent(assertInteger(
    a = c(1, 2, 3), b = list(a = 1, b = 2),
    c = data.frame(a = 1:10, b = 1:10),
    d = 1
  ))
})

test_that("assertFactor works.", {
  skip_on_cran()
  
  expect_error(assertFactor(
    a = "two", b = 123.456,
    c = as.factor(c(1, 2, 3)),
    d = as.factor(1.1), e = as.factor("one")
  ),
  regexp = "a and b", class = "simstudy::wrongType"
  )

  expect_silent(assertFactor(
    a = as.factor("two"), b = as.factor(123.456),
    c = as.factor(c(1, 2, 3)),
    d = as.factor(1.1), e = as.factor("one")
  ))
})

test_that("assertValue works.", {
  skip_on_cran()
  
  expect_error(assertValue(a = NULL, b = NA, c = character(0)),
    regexp = "b and c",
    class = "simstudy::noValue"
  )

  expect_silent(assertValue(a = "", b = 0, c = 3))
})

test_that("assertUnique works.", {
  skip_on_cran()
  
  expect_error(assertUnique(a = c(1, 2, 3, 2), b = list(a = 1:3, b = 1:3)),
    regexp = "a and b",
    class = "simstudy::uniqueValue"
  )
  expect_error(assertUnique(a = data.frame()), class = "simpleError")
  expect_silent(assertUnique(a = c(1, 2, 3), b = list(a = 1:3), c = glue("test {1:3}")))
})

test_that("assertInDataTable works.", {
  skip_on_cran()
  
  dt <- data.table(a = 1:10)
  expect_error(assertInDataTable(vars = c("a", "b"), dt),
    regexp = "b",
    class = "simstudy::notDefined"
  )
  expect_silent(assertInDataTable("a", dt))
})

test_that("assertInDataTable works.", {
  skip_on_cran()
  
  dt <- data.table(a = 1:10)
  expect_error(assertNotInDataTable(vars = c("a", "b"), dt),
    regexp = "a",
    class = "simstudy::alreadyDefined"
  )
  expect_silent(assertNotInDataTable("b", dt))
})

test_that("ensureLength works.", {
  skip_on_cran()
  
  expect_length(ensureLength(a = c(1, 2, 3), n = 3), 3)
  expect_length(ensureLength(a = "5", n = 5), 5)
  expect_length(ensureLength(a = list(a = 5), n = 5), 5)
  expect_error(ensureLength(a = c(1, 2, 3), n = 5), class = "simstudy::lengthMismatch")
})

test_that("ensureMatrix works", {
  skip_on_cran()
  
  expect_error(ensureMatrix(data.frame("a")), class = "simpleError")
  expect_is(ensureMatrix(c(1, 2, 3, 4)), "matrix")
  expect_is(ensureMatrix(matrix(1:25, 5)), "matrix")
})

test_that("assertPositiveSemiDefinite works.", {
  skip_on_cran()
  
  notPosDef <- matrix(rep(-.5, 25), 5, 5)
  diag(notPosDef) <- 1
  posDef <- matrix(rep(-.2, 25), 5, 5)
  diag(posDef) <- 1

  expect_silent(assertPositiveSemiDefinite(mat = posDef))
  expect_error(assertPositiveSemiDefinite(mat = notPosDef), class = "simstudy::notPositiveSemiDefinite")
})

test_that("assertOption works", {
  skip_on_cran()
  
  expect_silent(assertOption(opt = 2, options = c(1, 2, 3, 5)))
  expect_error(assertOption(opt = FALSE, options = c(1, 2, 3)), class = "simstudy::optionInvalid")
  expect_error(assertOption(opt = FALSE, options = TRUE), class = "simstudy::optionInvalid")
  expect_error(assertOption(opt = 5, options = 1:4), class = "simstudy::optionInvalid")
  expect_error(assertOption(opt = "in", options = c("a", "b", "c")), class = "simstudy::optionInvalid")
})

test_that("ensureOption works", {
  skip_on_cran()
  
  expect_equal(suppressWarnings(
    ensureOption(opt = "b", options = letters[4:7], default = "d"),
    classes = "simstudy::optionInvalid"
  ), "d")

  expect_equal(suppressWarnings(
    ensureOption(opt = "e", options = letters[4:7], default = "d"),
    classes = "simstudy::optionInvalid"
  ), "e")

  expect_equal(suppressWarnings(
    ensureOption(opt = 1, options = 2:4, default = 2),
    classes = "simstudy::optionInvalid"
  ), 2)
})

test_that("assertInRange works", {
  skip_on_cran()
  
  expect_error(assertInRange(a = 1, b = 2, range = c(2, 3)), class = "simstudy::valueError")
  expect_error(assertInRange(a = -100, b = 2, range = c(2, Inf), maxCheck = "<"), class = "simstudy::valueError")
  expect_silent(assertInRange(a = 1:5, range = c(0, 100)))
})

test_that("dots2args works.", {
  skip_on_cran()
  
  expect_error(dots2argNames(), class = "simpleError")
  expect_error(dots2argNames(a = 3, 2), class = "simpleError")
  expect_silent(dots2argNames(a = 3, b = 4))
  expect_equal(dots2argNames(a = 3, b = 4), list(args = list(a = 3, b = 4), names = c("a", "b")))
})
