library(glue)
library(data.table)
library(simstudy)
library(stringr)

set.seed(28229)

sim.melt <- function(dd, vars, baseline=NULL) {
  
  dm <- melt(
    dd, 
    id.vars = c("id", baseline), 
    measure = patterns(paste0("^", vars)), 
    value.name = vars,
    variable.name = "period"
  )    
  
  dm[, period := as.numeric(period) - 1]
  dm[]
  
}

###

seqDef <- function(dd, t.start, t.end) {
  
  nn <- function(value) {
    if (value >= 0) {
      return(value)
    } else {
      return("0*0")
    }
  }
  
  dd[, .n := .I]
  rows <- (str_detect(dd$varname, "<*>") | str_detect(dd$formula, "<*>")) 
  d..1 <- dd[rows]
       
  d..1 <- d..1[, .(t = c(t.start:t.end),
                   varname = str_replace_all(varname, c("<" = "<nn(", ">" = ")>")),
                   formula = str_replace_all(formula, c("<" = "<nn(", ">" = ")>")),
                   variance,
                   dist,
                   link,
                   .n
  ), 
  by = .(row.names(d..1))]
  
  d..1 <- d..1[, .(t,
                   varname = glue(varname, t = t, .open = "<", .close = ">"),
                   formula = glue(formula, t = t, .open = "<", .close = ">"),
                   variance,
                   dist,
                   link,
                   .n
  ),
  by = .(row.names(d..1))]
  
  setkey(d..1, "t")  
  d..1[, `:=`(row.names = NULL, t = NULL)]
  d..1[, `:=`(.n = min(.n), seq = .I)]
  
  class(d..1$varname) <- "character"
  class(d..1$formula) <- "character"
  
  # probably should check rows
  
  d..0 <- dd[!rows]  
  d..0[, seq:=0]
  dd <- rbind(d..0, d..1)
  setkey(dd, .n, seq)
  dd[, `:=`(.n = NULL, seq = NULL)]
  
  dd[]
}

#--- Example 1

d1 <- defDataAdd(varname = "y0", formula = 0, variance = 2)
d1 <- defDataAdd(d1, varname = "x0", formula = 0, variance = 4)
d1 <- defDataAdd(d1, varname = "y<t>", formula = ".5*x<t-1> + .8*y<t-1>", variance = 2)
d1 <- defDataAdd(d1, varname ="x<t>", formula = ".6*x<t-1> + 0.4*x<t-2>", variance = 4)

d1 <- seqDef(d1, 1, 30)

dd <- genData(n = 10)
dd <- addColumns(d1, dd)
dd

# Convert to long format

dm <- sim.melt(dd, c("y", "x"))
setkey(dm, "id")
dm

ggplot(data = dm, aes(x = period, y = y, group = id)) +
  geom_line()

ggplot(data = dm, aes(x = period, y = x, group = id)) +
  geom_line()

#--- Example 2 - Marginal Structural Model

d1 <- defDataAdd(varname = "U", formula = 0, variance = 1)
d1 <- defDataAdd(d1, varname = "L0", formula = "-1 + U", dist = "binary", link="logit")
d1 <- defDataAdd(d1, varname = "A0", formula = "-1 + 0.3*L0", dist = "binary", link = "logit")
d1 <- defDataAdd(d1, varname = "L<t>", formula = "-1 - 0.5*A<t-1> + 0.5*L<t-1> + U", dist = "binary", link="logit")
d1 <- defDataAdd(d1, varname = "A<t>", formula = "-1 + 2*A<t-1>+ 0.3*L<t-1>", dist = "binary", link = "logit")
d1 <- defDataAdd(d1, varname = "Y", formula = "2*U + 5*A0 + 3*A1 + 2*A2 + 1*A3", variance = 4)

d1 <- seqDef(d1, 1, 3)
dd <- genData(n = 1000)
dd <- addColumns(d1, dd)

# Fitting MSM

getWeight <- function(predA0, actA0, predA1, actA1,
                      predA2, actA2, predA3, actA3) {
  predActA0 <- actA0*predA0 + (1-actA0)*(1-predA0)
  predActA1 <- actA1*predA1 + (1-actA1)*(1-predA1)
  predActA2 <- actA2*predA2 + (1-actA2)*(1-predA2)
  predActA3 <- actA3*predA3 + (1-actA3)*(1-predA3)
  
  p <- predActA0 * predActA1 * predActA2 * predActA3
  return(1/p)
}

fitA0 <- glm(A0 ~ L0, data = dd, family=binomial)
fitA1 <- glm(A1 ~ L0 + A0 + L1, data = dd, family=binomial)
fitA2 <- glm(A2 ~ L0 + A0 + L1 + A1 + L2, data = dd, family=binomial)
fitA3 <- glm(A3 ~ L0 + A0 + L1 + A1 + L2 + A2 + L3, data = dd, family=binomial)

dd[, predA0 := predict(fitA0, type = "response")]
dd[, predA1 := predict(fitA1, type = "response")]
dd[, predA2 := predict(fitA2, type = "response")]
dd[, predA3 := predict(fitA3, type = "response")]

dd[, wgt := getWeight(predA0, A0, predA1, A1, 
                      predA2, A2, predA3, A3)]

broom::tidy(lm(Y ~ A0 + A1 + A2 + A3, weights = wgt, data = dd))


