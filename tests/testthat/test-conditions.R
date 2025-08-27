library(testthat)
library(simstudy)
library(data.table)

test_that("conditions have correct class.", {
  skip_on_cran()
  expect_error(stop(condition(c("error", "custom_Error"), "This is a custom error")),
    class = c("error", "custom_Error")
  )
  expect_warning(warning(simstudy:::condition(c("warning", "custom_warning"), "This is a custom warning")),
    class = c("warning", "custom_warning")
  )
  expect_message(message(simstudy:::condition(c("message", "custom_message"), "This is a custom message")),
    class = c("message", "custom_message")
  )
})

test_that("pluralization works.", {
  skip_on_cran()
  expect_error(simstudy:::argMissingError("arg1"), "argument is missing",
    class = "simstudy::missingArgument"
  )
  expect_error(simstudy:::argMissingError(c("arg1", "arg2")), "arguments are missing",
    class = "simstudy::missingArgument"
  )
})
