test_that("Blocks are collapsed as expected.", {
  nums <- 1:3
  num <- 23
  expect_equal(
    glueCollapse("Collapse block one: { nums *} but not { num }."),
    "Collapse block one: 1, 2 and 3 but not 23."
  )
  expect_equal(
    glueCollapse("Collapse block one: { nums *} but not { num }.", last = " und "),
    "Collapse block one: 1, 2 und 3 but not 23."
  )
  expect_length(glueCollapse("Collapse block one: { nums *} but not { num }."), 1)
  expect_length(glueCollapse("Collapse block one: { nums } but not { num }."), 3)
    })

test_that("numbers are formated as expected.", {
  nums <- c(1.23, 0.556, 1/3)
  ints <- c(1, 2, 3)
  expect_equal(glueFmt("{nums:.2f}"), as.character(round(nums, 2)))
  expect_equal(glueFmt("{ints:.2f}"), c("1.00", "2.00", "3.00"))
  expect_equal(glueFmt("{5}"), "5")
})

test_that("numbers are collapsed and formated correctly.", {
  ints <- c(1, 2, 3)
  expect_equal(glueFmtC("{ints:02d}"), "01, 02 and 03")
  expect_equal(glueFmtC("{2:.1f}"), "2.0")
  expect_equal(glueFmtC("{2}"), "2")
})