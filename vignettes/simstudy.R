## ---- echo = FALSE, message = FALSE-------------------------------------------

library(simstudy)
library(data.table)
library(ggplot2)
library(knitr)
library(data.table)

options(digits = 3)

opts_chunk$set(tidy.opts=list(width.cutoff=55), tidy=TRUE)

plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

ggtheme <- function(panelback = "white") {
  
  ggplot2::theme(
    panel.background = element_rect(fill = panelback),
    panel.grid = element_blank(),
    axis.ticks =  element_line(colour = "black"),
    panel.spacing =unit(0.25, "lines"),  # requires package grid
    panel.border = element_rect(fill = NA, colour="grey90"), 
    plot.title = element_text(size = 8,vjust=.5,hjust=0),
    axis.text = element_text(size=8),
    axis.title = element_text(size = 8)
  )  
  
}

## ----  echo=FALSE-------------------------------------------------------------
def <- defData(varname="age", dist="normal", formula=10, variance = 2)
def <- defData(def, varname="female", dist="binary", 
    formula="-2 + age * 0.1", link = "logit")
def <- defData(def,varname="visits", dist="poisson", 
    formula="1.5 - 0.2 * age + 0.5 * female", link="log")

knitr::kable(def)

## -----------------------------------------------------------------------------
def <- defData(varname="age", dist="normal", formula=10, variance = 2)
def <- defData(def, varname="female", dist="binary", 
  formula="-2 + age * 0.1", link = "logit")
def <- defData(def,varname="visits", dist="poisson", 
  formula="1.5 - 0.2 * age + 0.5 * female", link="log")

## -----------------------------------------------------------------------------
set.seed(87261)

dd <- genData(1000, def)
dd

## -----------------------------------------------------------------------------
genData(1000)

## -----------------------------------------------------------------------------
study1 <- trtAssign(dd , n=3, balanced = TRUE, strata = c("female"), grpName = "rx")
study1

study1[, .N, keyby = .(female, rx)]

## -----------------------------------------------------------------------------
def <- defData(varname = "age", dist = "normal", formula=10, variance = 2)
def <- defData(def, varname="female", dist="binary", 
  formula="-2 + age * 0.1", link = "logit")
def <- defData(def,varname="visits", dist="poisson", 
  formula="1.5 - 0.2 * age + 0.5 * female", link="log")

## -----------------------------------------------------------------------------
myinv <- function(x) {
  1/x
}

def <- defData(varname = "age", formula=10, variance = 2, dist = "normal")
def <- defData(def, varname="loginvage", formula="log(myinv(age))", 
  variance = 0.1, dist="normal")

genData(5, def)

## -----------------------------------------------------------------------------
def10 <- updateDef(def, changevar = "loginvage", newformula = "log10(myinv(age))")
def10

genData(5, def10)

## -----------------------------------------------------------------------------
age_effect <- 3

def <- defData(varname = "age", formula=10, variance = 2, dist = "normal")
def <- defData(def, varname="agemult", 
  formula="age * ..age_effect", dist="nonrandom")

def

genData(2, def)

## -----------------------------------------------------------------------------
age_effects <- c(0, 5, 10)
list_of_data <- list()

for (i in seq_along(age_effects)) {
  age_effect <- age_effects[i]
  list_of_data[[i]] <- genData(2, def)  
}

list_of_data

## ----  echo=FALSE-------------------------------------------------------------
d <- list()
d[[1]] <- data.table("beta", "mean", "both", "-", "dispersion", "X", "-", "X") 
d[[2]] <- data.table("binary", "probability", "both", "-", "-", "X", "-", "X") 
d[[3]] <- data.table("binomial", "probability", "both", "-", "# of trials", "X", "-", "X")
d[[4]] <- data.table("categorical", "probability", "string", " p_1;p_2;...;p_n", "-", "X", "-", "-")
d[[5]] <- data.table("exponential", "mean", "both", "-", "-", "X", "X", "-")
d[[6]] <- data.table("gamma", "mean", "both", "-", "dispersion", "X", "X", "-")
d[[7]] <- data.table("mixture", "formula", "string", "x_1 | p_1 + ... + x_n | p_n", "-", "X", "-", "-")
d[[8]] <- data.table("negBinomial", "mean", "both", "-", "dispersion", "X", "X", "-")
d[[9]] <- data.table("nonrandom", "formula", "both", "-", "-", "X", "-", "-")
d[[10]] <- data.table("normal", "mean", "both", "-", "variance", "X", "-", "-")
d[[11]] <- data.table("noZeroPoisson", "mean", "both", "-", "-", "X", "X", "-")
d[[12]] <- data.table("poisson", "mean", "both", "-", "-", "X", "X", "-")
d[[13]] <- data.table("uniform", "range", "string", "from ; to", "-", "X", "-", "-")
d[[14]] <- data.table("uniformInt", "range", "string", "from ; to", "-", "X", "-", "-")


d <- rbindlist(d)
setnames(d, c("name", "formula", "string/value", "format", "variance", "identity", "log", "logit"))
knitr::kable(d, align = "lllllccc")

## -----------------------------------------------------------------------------
d1 <- defData(varname = "x1", formula = 0, variance = 1, dist = "normal")
d1 <- defData(d1, varname = "x2", formula = 0.5, dist = "binary")

d2 <- defDataAdd(varname = "y", formula = "-2 + 0.5*x1 + 0.5*x2 + 1*rx", 
                 dist = "binary", link = "logit")

dd <- genData(5, d1)
dd <- trtAssign(dd, nTrt = 2, grpName = "rx")
dd

dd <- addColumns(d2, dd)
dd

## -----------------------------------------------------------------------------
d <- defData(varname = "x", formula = 0, variance = 9, dist = "normal")

dc <- defCondition(condition = "x <= -2", formula = "4 + 3*x", variance = 2, 
                   dist = "normal")
dc <- defCondition(dc, condition = "x > -2 & x <= 2", formula = "0 + 1*x", variance = 4, 
                   dist = "normal")
dc <- defCondition(dc, condition = "x > 2", formula = "-5 + 4*x", variance = 3, 
                   dist = "normal")

dd <- genData(1000, d)
dd <- addCondition(dc, dd, newvar = "y")

## ---- fig.width = 5, fig.height = 3, echo=FALSE, message=FALSE----------------
ggplot(data = dd, aes(y = y, x = x)) +
  geom_point(color = " grey60", size = .5) +
  geom_smooth(se = FALSE, size = .5) +
  ggtheme("grey90")

