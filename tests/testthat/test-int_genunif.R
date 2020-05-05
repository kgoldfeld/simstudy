test_that("data is generated as expected.",{
  n <- 20
  def <- defData(varname = "test",formula = 5,dist  = "nonrandom")
  def <- defData(def,varname = "test2",formula = "test + 3",dist  = "normal")

  dt <- genData(n,def)
  dterr <- genData(n-5,def)
  expect_error(.genunif(n,"test;test2",dterr),"Length mismatch")
  expect_length(.genunif(n,"test;test2",dt),n)
  expect_true(all(!is.na(.genunif(n,"test;test2",dt))))
  expect_length(.genunif(n,"1.3;100.2",dt),n)
})

test_that("'uniform' formula checked correctly", {
  forall(generate(for (x in list(
    range =  gen_uniform_range(),
    n = gen.int(40)
  ))x)
  , function(x)
    expect_silent(.genunif(x$n,x$range,NULL)))
  
  expect_warning(.genunif(3,"1;1",NULL), "are equal")
  expect_error(.genunif(3,"",NULL), "format")
  expect_error(.genunif(3,"1;2;3",NULL), "format")
  expect_error(.genunif(3,"2;1",NULL), "'max' < 'min'")
})


