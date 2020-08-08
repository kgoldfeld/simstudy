test_that("data is generated as expected", {
  n <- 20
  def <- defData(varname = "test",formula = .3,dist  = "nonrandom")
  def <- defData(def,varname = "test2",formula = .7,dist  = "nonrandom")
  def2 <- defData(def,varname = "cat",formula = "test2;.4",dist  = "categorical")
  def3 <- defData(def,varname = "cat",formula = "test2;.2",dist  = "categorical")
  def <- defData(def,varname = "cat",formula = "test;test2",dist  = "categorical")
  
  expect_silent(genData(n,def))
  expect_warning(genData(n,def2),"will be normalized")
  expect_warning(genData(n,def3),"Adding category")
  expect_error(.gencat(n,"1;","identity",NULL),"two probabilities")
  expect_error(.gencat(n,"1; ","identity",NULL),"two probabilities")
})

# ok for now, but needs more tests.