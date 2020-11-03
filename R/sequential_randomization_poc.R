library(glue)
library(data.table)
library(simstudy)
library(stringr)

sim.melt <- function(dd, vars, baseline) {
  
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
  
  d..1 <- d..1[, .(varname = gsub("<", "<nn(", varname),
                   formula = gsub("<", "<nn(", formula),
                   variance,
                   dist,
                   link,
                   .n
  ), 
  by = .(row.names(d..1))]
  
  d..1 <- d..1[, .(varname = gsub(">", ")>", varname),
                   formula = gsub(">", ")>", formula),
                   variance,
                   dist,
                   link,
                   .n
  ), 
  by = .(row.names(d..1))]
  
  d..1 <- d..1[, .(t = c(t.start:t.end), 
                   varname,
                   formula,
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
  d..1 <- d..1[, -c(1, 2)]
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

###

library(glue)
library(data.table)
library(simstudy)

d1 <- defDataAdd(varname = "y0", formula = 0, variance = 2)
d1 <- defDataAdd(d1, varname = "x0", formula = 0, variance = 4)
d1 <- defDataAdd(d1, varname = "y<t>", formula = ".5*x<t-1> + .8*y<t-1>", variance = 2)
d1 <- defDataAdd(d1, varname ="x<t>", formula = ".6*x<t-1> + 0.4*x<t-2>", variance = 4)

d1 <- seqDef(d1, 1, 30)

dd <- genData(n = 10)
dd <- addColumns(d1, dd)

###


dm <- sim.melt(dd, c("y", "x"))
setkey(dm, "id")
dm

ggplot(data = dm, aes(x = period, y = y, group = id)) +
  geom_line()

ggplot(data = dm, aes(x = period, y = x, group = id)) +
  geom_line()

d1 <- defDataAdd(varname = "U", formula = 0, variance = 1)
d1 <- defDataAdd(d1, varname = "L0", formula = "-2 + U", dist = "binary", link="logit")
d1 <- defDataAdd(d1, varname = "A0", formula = "-1 + 0.3*L0", dist = "binary", link = "logit")
d1 <- defDataAdd(d1, varname = "L<t>", formula = "-2 - A<t-1> + 0.5*L<t-1> + U", dist = "binary", link="logit")
d1 <- defDataAdd(d1, varname = "A<t>", formula = "-1 + 1*A<t-1>+ 0.3*L<t-1>", dist = "binary", link = "logit")
d1 <- defDataAdd(d1, varname = "Y", formula = "2*U + 3*A8", variance = 4)

d1 <- seqDef(d1, 1, 8)

dd <- genData(n = 10)
dd <- addColumns(d1, dd)

dm <- sim.melt(dd, c("L", "A"), baseline = c("Y", "U"))
setkey(dm, "id")
dm

