## ---- echo = FALSE, message=FALSE---------------------------------------------

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


## -----------------------------------------------------------------------------
def <- defData(varname = "male", dist = "binary", 
               formula = .5 , id="cid")
def <- defData(def, varname = "over65", dist = "binary", 
               formula = "-1.7 + .8*male", link="logit")
def <- defData(def, varname = "baseDBP", dist = "normal", 
               formula = 70, variance = 40)

dtstudy <- genData(330, def)

## ---- tidy = TRUE-------------------------------------------------------------
study1 <- trtAssign(dtstudy , n=3, balanced = TRUE, strata = c("male","over65"), grpName = "rxGrp")

study1

## ---- tidy = TRUE-------------------------------------------------------------
study2 <- trtAssign(dtstudy , n=3, balanced = TRUE, grpName = "rxGrp")

## ---- tidy = TRUE-------------------------------------------------------------
study3 <- trtAssign(dtstudy , n=3, balanced = FALSE, grpName = "rxGrp")

## ---- tidy = TRUE, echo = FALSE, fig.width = 4, fig.height = 6----------------
p1 <- splotfunc(study1, "Balanced within strata")
p1a <- aplotfunc(study1, "")

p2 <- splotfunc(study2, "Balanced without strata")
p2a <- aplotfunc(study2, "")

p3 <- splotfunc(study3, "Random allocation")
p3a <- aplotfunc(study3, "")

grid.arrange(p1, p1a, p2, p2a, p3, p3a, ncol=2)

## ---- tidy = TRUE-------------------------------------------------------------
formula1 <- c("-2 + 2*male - .5*over65", "-1 + 2*male + .5*over65")
dtExp <- trtObserve(dtstudy, formulas = formula1, logit.link = TRUE, grpName = "exposure")

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 2.5------------
dtplot1 <- dtExp[,.N,keyby=.(male,exposure)]
p1 <- ggplot(data = dtplot1, aes(x=factor(male), y=N)) +
  geom_bar(aes(fill=factor(exposure)), alpha = .8, stat="identity", position = "dodge") +
  ggtheme() +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_text(size = 8)) +
  ylim(0, 150) +
  scale_fill_manual(values = plotcolors, name = "Exposure") +
  scale_x_discrete(breaks = c(0, 1), labels = c("Female", "Male")) +
  ylab("Number exposed")+
  ggtitle("Gender")

dtplot2 <- dtExp[,.N,keyby=.(over65,exposure)]
p2 <- ggplot(data = dtplot2, aes(x=factor(over65), y=N)) +
  geom_bar(aes(fill=factor(exposure)), alpha = .8, stat="identity", position = "dodge") +
  ggtheme() +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_text(size = 8)) +
  ylim(0, 150) +
  scale_fill_manual(values = plotcolors, name = "Exposure") +
  scale_x_discrete(breaks = c(0, 1), labels = c("65 or younger", "Over 65")) +
  ylab("Number exposed") +
  ggtitle("Age")

grid.arrange(p1,p2,nrow=1)


## ---- tidy = TRUE-------------------------------------------------------------
formula2 <- c(.35, .45)

dtExp2 <- trtObserve(dtstudy, formulas = formula2, logit.link = FALSE, grpName = "exposure")

## ---- tidy = TRUE, echo = FALSE, fig.width = 6.5, fig.height = 2.5------------
dtplot1a <- dtExp2[,.N,keyby=.(male,exposure)]
p1a <- ggplot(data = dtplot1a, aes(x=factor(male), y=N)) +
  geom_bar(aes(fill=factor(exposure)), alpha = .8, stat="identity", position = "dodge") +
  ggtheme() +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_text(size = 8)) +
  ylim(0, 150) +
  scale_fill_manual(values = plotcolors, name = "Exposure") +
  scale_x_discrete(breaks = c(0, 1), labels = c("Female", "Male")) +
  ylab("Number exposed")+
  ggtitle("Gender")

dtplot2a <- dtExp2[,.N,keyby=.(over65,exposure)]
p2a <- ggplot(data = dtplot2a, aes(x=factor(over65), y=N)) +
  geom_bar(aes(fill=factor(exposure)), alpha = .8, stat="identity", position = "dodge") +
  ggtheme() +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_text(size = 8)) +
  ylim(0, 150) +
  scale_fill_manual(values = plotcolors, name = "Exposure") +
  scale_x_discrete(breaks = c(0, 1), labels = c("65 or younger", "Over 65")) +
  ylab("Number exposed") +
  ggtitle("Age")

grid.arrange(p1a,p2a,nrow=1)

## -----------------------------------------------------------------------------
library(simstudy)
library(ggplot2)

defc <- defData(varname = "ceffect", formula = 0, variance = 0.20, 
                dist = "normal", id = "cluster")
defc <- defData(defc, "m", formula = 15, dist = "nonrandom")

defa <- defDataAdd(varname = "Y", 
                   formula = "0 + ceffect + 0.1*period + trt*1.5", 
                   variance = 1.75, dist = "normal")

## -----------------------------------------------------------------------------
set.seed(608477)

dc <- genData(30, defc)
dp <- addPeriods(dc, 24, "cluster", timevarName = "t")
dp <- trtStepWedge(dp, "cluster", nWaves = 5, lenWaves = 4, 
          startPer = 4, grpName = "trt")

dd <- genCluster(dp, cLevelVar = "timeID", "m", "id")
dd <- addColumns(defa, dd)

dd

## -----------------------------------------------------------------------------
dSum <- dd[, .(Y = mean(Y)), keyby = .(cluster, period, trt, startTrt)]

ggplot(data = dSum, 
    aes(x = period, y = Y, group = interaction(cluster, trt))) +
  geom_line(aes(color = factor(trt))) +
  facet_grid(factor(startTrt, labels = c(1 : 5)) ~ .) +
  scale_x_continuous(breaks = seq(0, 23, by = 4), name = "week") +
  scale_color_manual(values = c("#b8cce4", "#4e81ba")) +
  theme(panel.grid = element_blank(),
        legend.position = "none") 

