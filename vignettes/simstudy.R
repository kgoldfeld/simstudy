## ---- echo = FALSE, message = FALSE--------------------------------------

library(simstudy)
library(ggplot2)
library(grid)
library(gridExtra)
library(knitr)

set.seed(33333)

opts_chunk$set(tidy.opts=list(width.cutoff=75), tidy=TRUE)

plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

# 

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

splotfunc <- function(dt, ptitle) {

  dtplot <- dt[,.N,keyby=.(male, over65, rxGrp)][, .(rxGrp, grp = male * 2 + over65 * 1, N)]
  ggplot(dtplot, aes(factor(grp), N)) +
    geom_bar(aes(fill = factor(rxGrp)), alpha=.8, position = "dodge", stat="identity") +
    scale_fill_manual(values = plotcolors) +
    ggtitle(ptitle) +
    theme(legend.position = "none") +
    ggtheme() +
    xlab("Strata") +
    ylim(0,80)
}

aplotfunc <- function(dt, ptitle) {

  dtplot <- dt[,.N,keyby=.(rxGrp)]
  ggplot(dtplot, aes(factor(rxGrp), N)) +
    geom_bar(aes(fill = factor(rxGrp)), alpha=.8, position="dodge", stat="identity", width=.5) +
    scale_fill_manual(values = plotcolors) +
    ggtitle(ptitle) +
    theme(legend.position = "none") +
    ggtheme() +
    xlab("Treatment group") +
    ylim(0,150)
}


## ----  echo=FALSE--------------------------------------------------------
def <- defData(varname = "nr", dist = "nonrandom", formula=7, id = "idnum")
def <- defData(def,varname="x1", dist="uniform", formula="10;20")
def <- defData(def,varname="y1", formula="nr + x1 * 2", variance=8)
def <- defData(def,varname="y2", dist="poisson", formula="nr - 0.2 * x1",link="log")
def <- defData(def, varname = "xnb", dist = "negBinomial" , formula="nr - 0.2 * x1", variance = 0.05, link = "log")
def <- defData(def,varname="xCat",formula = "0.3;0.2;0.5", dist="categorical")
def <- defData(def,varname="g1", dist="gamma", formula = "5+xCat", variance = 1, link = "log")
def <- defData(def,varname="b1", dist="beta", formula = "1+0.3*xCat", variance = 1, link = "logit")
def <- defData(def, varname = "a1", dist = "binary" , formula="-3 + xCat", link="logit")
def <- defData(def, varname = "a2", dist = "binomial" , formula="-3 + xCat", variance = 100, link="logit")

knitr::kable(def)

## ---- tidy = TRUE--------------------------------------------------------
def <- defData(varname = "nr", dist = "nonrandom", formula=7, id = "idnum")
def <- defData(def,varname="x1",dist="uniform",formula="10;20")
def <- defData(def,varname="y1",formula="nr + x1 * 2",variance=8)
def <- defData(def,varname="y2",dist="poisson",formula="nr - 0.2 * x1",link="log")
def <- defData(def, varname = "xnb", dist = "negBinomial" , formula="nr - 0.2 * x1", variance = 0.05, link = "log")
def <- defData(def,varname="xCat",formula = "0.3;0.2;0.5",dist="categorical")
def <- defData(def,varname="g1", dist="gamma", formula = "5+xCat", variance = 1, link = "log")
def <- defData(def,varname="b1", dist="beta", formula = "1+0.3*xCat", variance = 1, link = "logit")
def <- defData(def, varname = "a1", dist = "binary" , formula="-3 + xCat", link="logit")
def <- defData(def, varname = "a2", dist = "binomial" , formula="-3 + xCat", variance = 100, link="logit") 

## ---- tidy = TRUE--------------------------------------------------------
dt <- genData(1000, def)
dt

## ---- tidy = TRUE--------------------------------------------------------
addef <- defDataAdd(varname = "zExtra", dist = "normal", formula = '3 + y1', 
                 variance = 2)

dt <- addColumns(addef, dt)
dt

## ---- tidy = TRUE--------------------------------------------------------
def <- defData(varname = "male", dist = "binary", formula = .5 , id="cid")
def <- defData(def, varname = "over65", dist = "binary", formula = "-1.7 + .8*male", link="logit")
def <- defData(def, varname = "baseDBP", dist = "normal", formula = 70, variance = 40)

dtstudy <- genData(330, def)

## ---- tidy = TRUE--------------------------------------------------------
study1 <- trtAssign(dtstudy , n=3, balanced = TRUE, strata = c("male","over65"), grpName = "rxGrp")

study1


## ---- tidy = TRUE--------------------------------------------------------
study2 <- trtAssign(dtstudy , n=3, balanced = TRUE, grpName = "rxGrp")

## ---- tidy = TRUE--------------------------------------------------------
study3 <- trtAssign(dtstudy , n=3, balanced = FALSE, grpName = "rxGrp")

