## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)
odds <- function (p)  p/(1 - p) # TODO temporary remove when added to package
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


## -----------------------------------------------------------------------------
defs <- defData(varname = "x", formula = 0, variance = 3, dist = "normal")
defs <- defData(defs, varname = "y", formula = "2 + 3*x", variance = 1, dist = "normal")
defs <- defData(defs, varname = "z", formula = "4 + 3*x - 2*y", variance = 1, dist = "normal")

defs

## -----------------------------------------------------------------------------
defs <- updateDef(dtDefs = defs, changevar = "y", newformula = "x + 5", newvariance = 2)
defs

## -----------------------------------------------------------------------------
defs <- updateDef(dtDefs = defs, changevar = "z", newdist = "poisson", newlink = "log")
defs

## -----------------------------------------------------------------------------
defs <- updateDef(dtDefs = defs, changevar = "z", remove = TRUE)
defs

## -----------------------------------------------------------------------------
def <- defData(varname = "x", formula = 0, 
  variance = 5, dist = "normal")
def <- defData(def, varname = "y", formula = "..B0 + ..B1 * x", 
  variance = "..sigma2", dist = "normal")

def

## -----------------------------------------------------------------------------
B0 <- 4;
B1 <- 2;
sigma2 <- 9

set.seed(716251)

dd <- genData(100, def)

fit <- summary(lm(y ~ x, data = dd))

coef(fit)
fit$sigma

## -----------------------------------------------------------------------------
sigma2 <- 16

dd <- genData(100, def)
fit <- summary(lm(y ~ x, data = dd))

coef(fit)
fit$sigma

## ---- fig.width = 5-----------------------------------------------------------
sigma2s <- c(1, 2, 6, 9)

gen_data <- function(sigma2, d) {
  dd <- genData(200, d)
  dd$sigma2 <- sigma2
  dd
}

dd_4 <- lapply(sigma2s, function(s) gen_data(s, def))
dd_4 <- rbindlist(dd_4)

ggplot(data = dd_4, aes(x = x, y = y)) +
  geom_point(size = .5, color = "grey30") +
  facet_wrap(sigma2 ~ .) +
  theme(panel.grid = element_blank())

## -----------------------------------------------------------------------------
defblk <- defData(varname = "blksize", 
   formula = "..sizes[1] | .5 + ..sizes[2] | .5", dist = "mixture")

defblk

## -----------------------------------------------------------------------------
sizes <- c(2, 4)
genData(1000, defblk)

