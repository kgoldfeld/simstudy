## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)

plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

ggtheme <- function(panelback = "white") {
  
  ggplot2::theme(
    panel.background = element_rect(fill = panelback),
    panel.grid = element_blank(),
    axis.ticks =  element_line(colour = "black"),
    panel.spacing =unit(0.25, "lines"),  # requires package grid
    panel.border = element_rect(fill = NA, colour="gray90"), 
    plot.title = element_text(size = 8,vjust=.5,hjust=0),
    axis.text = element_text(size=8),
    axis.title = element_text(size = 8)
  )  
  
}

## ---- tidy=TRUE---------------------------------------------------------------
# specifying a specific correlation matrix C
C <- matrix(c(1,.7,.2, .7, 1, .8, .2, .8, 1),nrow = 3)
C

set.seed(282726)

# generate 3 correlated variables with different location and scale for each field
dt <- genCorData(1000, mu=c(4,12,3), sigma = c(1,2,3), corMatrix=C)
dt

# estimate correlation matrix
dt[,round(cor(cbind(V1, V2, V3)),1)]

# estimate standard deviation
dt[,round(sqrt(diag(var(cbind(V1, V2, V3)))),1)]

## ---- tidy=TRUE---------------------------------------------------------------
# generate 3 correlated variables with different location but same standard deviation
# and compound symmetry (cs) correlation matrix with correlation coefficient = 0.4.
# Other correlation matrix structures are "independent" ("ind") and "auto-regressive" ("ar1").

dt <- genCorData(1000, mu=c(4,12,3), sigma = 3, rho = .4, corstr = "cs", 
                cnames=c("x0","x1","x2"))
dt

# estimate correlation matrix
dt[,round(cor(cbind(x0, x1, x2)),1)]

# estimate standard deviation
dt[,round(sqrt(diag(var(cbind(x0, x1, x2)))),1)]

## ---- tidy = TRUE-------------------------------------------------------------

# define and generate the original data set
def <- defData(varname = "x", dist = "normal", formula = 0, variance = 1, id = "cid")
dt <- genData(1000, def)

# add new correlate fields a0 and a1 to "dt"
dt <- addCorData(dt, idname="cid", mu=c(0,0), sigma = c(2,.2), rho = -0.2, 
                 corstr = "cs", cnames=c("a0","a1"))

dt

# estimate correlation matrix
dt[,round(cor(cbind(a0, a1)),1)]

# estimate standard deviation
dt[,round(sqrt(diag(var(cbind(a0, a1)))),1)]

## -----------------------------------------------------------------------------
l <- c(8, 10, 12) # lambda for each new variable

dx <- genCorGen(1000, nvars = 3, params1 = l, dist = "poisson", rho = .3, corstr = "cs", wide = TRUE)
dx
round(cor(as.matrix(dx[, .(V1, V2, V3)])), 2)

## -----------------------------------------------------------------------------
genCorGen(1000, nvars = 3, params1 = c(.3, .5, .7), dist = "binary", rho = .8, corstr = "cs", wide = TRUE)

## -----------------------------------------------------------------------------
dx <- genCorGen(1000, nvars = 3, params1 = l, params2 = c(1,1,1), dist = "gamma", rho = .7, corstr = "cs", wide = TRUE, cnames="a, b, c")
dx
round(cor(as.matrix(dx[, .(a, b, c)])), 2)

## -----------------------------------------------------------------------------
dx <- genCorGen(1000, nvars = 3, params1 = l, params2 = c(1,1,1), dist = "gamma", rho = .7, corstr = "cs", wide = FALSE, cnames="NewCol")
dx

## -----------------------------------------------------------------------------
def <- defData(varname = "xbase", formula = 5, variance = .2, dist = "gamma", id = "cid")
def <- defData(def, varname = "lambda", formula = ".5 + .1*xbase", dist="nonrandom", link = "log")
def <- defData(def, varname = "p", formula = "-2 + .3*xbase", dist="nonrandom", link = "logit")
def <- defData(def, varname = "gammaMu", formula = ".5 + .2*xbase", dist="nonrandom", link = "log")
def <- defData(def, varname = "gammaDis", formula = 1, dist="nonrandom")

dt <- genData(10000, def)
dt

## -----------------------------------------------------------------------------

dtX1 <- addCorGen(dtOld = dt, idvar = "cid", nvars = 3, rho = .1, corstr = "cs",
                    dist = "poisson", param1 = "lambda", cnames = "a, b, c")
dtX1

## -----------------------------------------------------------------------------
dtX2 <- addCorGen(dtOld = dt, idvar = "cid", nvars = 4, rho = .4, corstr = "ar1",
                    dist = "binary", param1 = "p")
dtX2

## -----------------------------------------------------------------------------
dtX3 <- addCorGen(dtOld = dt, idvar = "cid", nvars = 4, rho = .4, corstr = "cs",
                  dist = "gamma", param1 = "gammaMu", param2 = "gammaDis")
dtX3

## -----------------------------------------------------------------------------
def <- defData(varname = "xbase", formula = 5, variance = .4, dist = "gamma", id = "cid")
def <- defData(def, "nperiods", formula = 3, dist = "noZeroPoisson")

def2 <- defDataAdd(varname = "lambda", formula = ".5+.5*period + .1*xbase", dist="nonrandom", link = "log")

dt <- genData(1000, def)

dtLong <- addPeriods(dt, idvars = "cid", nPeriods = 3)
dtLong <- addColumns(def2, dtLong)

dtLong

### Generate the data 

dtX3 <- addCorGen(dtOld = dtLong, idvar = "cid", nvars = 3, rho = .6, corstr = "cs",
                  dist = "poisson", param1 = "lambda", cnames = "NewPois")
dtX3

## -----------------------------------------------------------------------------
geefit <- gee(NewPois ~ period + xbase, data = dtX3, id = cid, family = poisson, corstr = "exchangeable")
round(summary(geefit)$working.correlation, 2)


