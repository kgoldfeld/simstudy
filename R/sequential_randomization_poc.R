library(glue)
library(data.table)
library(simstudy)

sim.melt <- function(dd, vars) {
  
  dm <- melt(
    dd, 
    id.vars = "id", 
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
  
  rows <- unique(c(grep("<*>", dd$varname), grep("<*>", dd$formula)))
  d..1 <- dd[rows]
  
  d..1 <- d..1[, .(varname = gsub("<", "<nn(", varname),
                   formula = gsub("<", "<nn(", formula),
                   variance,
                   dist,
                   link
  ), 
  by = .(row.names(d..1))]
  
  d..1 <- d..1[, .(varname = gsub(">", ")>", varname),
                   formula = gsub(">", ")>", formula),
                   variance,
                   dist,
                   link
  ), 
  by = .(row.names(d..1))]
  
  d..1 <- d..1[, .(t = c(t.start:t.end), 
                   varname,
                   formula,
                   variance,
                   dist,
                   link
  ), 
  by = .(row.names(d..1))]
  
  
  d..1 <- d..1[, .(t,
                   varname = glue(varname, t = t, .open = "<", .close = ">"),
                   formula = glue(formula, t = t, .open = "<", .close = ">"),
                   variance,
                   dist,
                   link
  ), 
  by = .(row.names(d..1))]
  
  setkey(d..1, "t")  
  d..1 <- d..1[, -c(1, 2)]
  
  class(d..1$varname) <- "character"
  class(d..1$formula) <- "character"
  
  # probably should check rows
  
  d..0 <- dd[-rows,]  
  rbind(d..0, d..1)
  
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
