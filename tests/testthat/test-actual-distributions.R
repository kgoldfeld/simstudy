test_that("beta distributed data are generated correctly", {
  skip_on_cran()
  
  p1 <- runif(1, .1, .9)
  v1 <- p1*(1-p1)/2
  
  p2 <- runif(1, .1, .9)
  v2 <- p2*(1-p2)/2
  
  logitp2 <- log(p2 / (1-p2))
  def <- defData(varname = "b1", formula = "..p1", variance = 1, dist = "beta")
  def <- defData(def, varname = "b2", formula = "..logitp2", variance = 1, 
                 dist = "beta", link="logit")
  
  dd <- genData(n = 1000, dtDefs = def)
  diff <- dd[, .(abs(mean(b1) - p1), abs(mean(b2) - p2)) ]
  expect_true(all(diff < 0.05))
  
  diffv <- dd[, .(abs(var(b1) - v1), abs(var(b2) - v2)) ]
  expect_true(all(diffv < 0.02))
})

test_that("gamma distributed data are generated correctly", {
  skip_on_cran()
  
  u1 <- runif(1, 5, 15)
  u2 <- runif(1, 5, 15)
  logu2 <- log(u2)
 
  def <- defData(varname = "g1", formula = "..u1", variance = 2, dist = "gamma")
  def <- defData(def, varname = "g2", formula = "..logu2", 
                 variance = 2, dist = "gamma", link = "log")
  dd <- genData(n = 5000, dtDefs = def)
  
  diff <- dd[, .(abs(mean(g1) - u1), abs(mean(g2) - u2)) ]
  expect_true(all(diff < 1))
  
  diffv <- dd[, .(abs(var(g1)/ (u1^2*2)), var(g2)/(u2^2*2)) ]
  expect_true(all(abs(diffv - 1) < 0.25))
})

