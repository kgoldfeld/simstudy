test_that("data is generated as expected.",{
  n <- 20
  def <- defData(varname = "test",formula = 5,dist  = "nonrandom")
  def <- defData(def,varname = "test2",formula = "test + 3",dist  = "normal")
  
  dt <- genData(n,def)
  dt$test2 <- ceiling(dt$test2)
  
  expect_length(.genUnifInt(n,"test;test2",dt),n)
  expect_true(all(!is.na(.genUnifInt(n,"test;test2",dt))))
  expect_length(.genUnifInt(n,"1;100",dt),n)
})

test_that("'uniformInt' formula checked correctly", {
  forall(generate(for (x in list(
    range =  gen_uniformInt_range(),
    n = gen.int(40)
  ))x)
  ,  function(x)
    expect_silent(.genUnifInt(x$n,x$range,NULL)))
  
  expect_error(.genUnifInt(3,"1.1;2.4",NULL), "must be integer")
})