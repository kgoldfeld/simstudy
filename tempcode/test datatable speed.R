library(data.table)
library(simstudy)

dd <- data.table(id = 1:100000)

dd[, y := rbinom(.N, 1, .5)]
dd[, y := rbinom(1, 1, .5), keyby = id]

d1 <- defData(varname = "x1", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x2", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x3", formula = 0, variance = 1)
d1 <- defData(d1, varname = "x4", formula = 0, variance = 1)

genData(1000000, d1)


