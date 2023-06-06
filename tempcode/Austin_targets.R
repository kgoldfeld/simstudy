
getBeta0 <- function(target.prop, def, int.low = -10, int.high = 10, tolerance = .00001) {

  outcome.function <- function(b0, def){
    
    dd <- genData(100000, def)
    
    dd[, new_l := l + b0]
    dd[, p := 1/(1 + exp(-new_l))]
    dd[, Y := rbinom(1, 1, p), keyby = id]
    
    mean(dd$Y)
    
    
  }
  
  while(abs(int.high - int.low) > tolerance){
    
    int.mid <- (int.low + int.high)/2
    
    outcome.prev <- outcome.function(b0 = int.mid, def)
    
    if (outcome.prev < target.prop) {
      int.low <- int.mid
    } else {
      int.high <- int.mid
    }
    
  }
  
  return(int.mid)
  
}

####

d1 <- defData(varname = "x1", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x2", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x3", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x4", formula = 0, variance = 1)
d1 <- defData(d1, varname = "b1", formula = 0.5, dist = "binary")
d1 <- defData(d1, varname = "b2", formula = 0.5, dist = "binary")
d1 <- defData(d1, varname = "b3", formula = 0.5, dist = "binary")
d1 <- defData(d1, varname = "l", 
  formula = "log(1.2) * x1 + log(1.1) * x2 + log(1.2) * x3 + log(1.1) * x4 + 
             log(1.2) * b1 + log(1.1) * b2 + log(1.2) * b3", 
  dist = "nonrandom")

beta0 <- getBeta0(0.55, d1)

d1 <- defData(d1, "Y.r", formula = "l + ..beta0", dist = "binary", link = "logit")

genData(1000000, d1)[, mean(Y.r)]
