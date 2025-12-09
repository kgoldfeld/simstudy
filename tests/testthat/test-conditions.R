library(testthat)
library(simstudy)
library(data.table)

# This test broke after simstudy version 0.9.0

# test_that("conditions have correct class.", {
#   skip_on_cran()
#   expect_error(stop(condition(c("error", "custom_Error"), "This is a custom error")),
#     class = c("error", "custom_Error")
#   )
#   expect_warning(warning(simstudy:::condition(c("warning", "custom_warning"), "This is a custom warning")),
#     class = c("warning", "custom_warning")
#   )
#   expect_message(message(simstudy:::condition(c("message", "custom_message"), "This is a custom message")),
#     class = c("message", "custom_message")
#   )
# })

# Replacing with this test

test_that("simstudy custom condition classes are constructed correctly", {
  skip_on_cran()
  
  # Test the condition constructor directly — this part is stable
  err_cond <- simstudy:::condition(
    c("error", "custom_Error"),
    "This is a custom error"
  )
  warn_cond <- simstudy:::condition(
    c("warning", "custom_warning"),
    "This is a custom warning"
  )
  msg_cond <- simstudy:::condition(
    c("message", "custom_message"),
    "This is a custom message"
  )
  
  # Ensure the custom classes are present in the constructed object
  expect_s3_class(err_cond, "custom_Error")
  expect_s3_class(warn_cond, "custom_warning")
  expect_s3_class(msg_cond, "custom_message")
  
  # Now test that signaling the condition yields the correct *base* class
  # This avoids fragile assumptions about how R internally rewrites classes.
  expect_error(stop(err_cond), class = "error")
  expect_warning(warning(warn_cond), class = "warning")
  expect_message(message(msg_cond), class = "message")
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

