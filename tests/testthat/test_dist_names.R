
test_that("Distribution names are correct", {
  expect_error(defCondition(condition="x==1",formula="5+2*y",variance=1,dist="ormal"))
  expect_error(defCondition(condition="x==1",formula="5+2*y",variance=1,dist="Normal"))
  expect_error(defCondition(condition="x==1",formula="5+2*y",variance=1,dist="noraml"))
  expect_error(defData(varname="x", dist="Normal", formula=3, variance=2))

  expect_true(validDistNames("normal"))
  expect_is(defData(varname="x", dist="normal", formula=3, variance=2), "data.table")
})
