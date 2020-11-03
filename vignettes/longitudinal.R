## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)

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


## ---- tidy = TRUE-------------------------------------------------------------
tdef <- defData(varname = "T", dist="binary", formula = 0.5)
tdef <- defData(tdef, varname = "Y0", dist = "normal", formula = 10, variance = 1)
tdef <- defData(tdef, varname = "Y1", dist = "normal", formula = "Y0 + 5 + 5 * T", variance = 1)
tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y0 + 10 + 5 * T", variance = 1)

set.seed (483726)

dtTrial <- genData( 500, tdef)
dtTrial

## ---- tidy = TRUE-------------------------------------------------------------
dtTime <- addPeriods(dtTrial, nPeriods = 3, idvars = "id", timevars = c("Y0", "Y1", "Y2"), timevarName = "Y")
dtTime

## ---- tidy = TRUE, echo = FALSE, fig.width = 6, fig.height = 3----------------

avg <- dtTime[,.(Y=mean(Y)), keyby = .(T, period)]

ggplot(data = dtTime, aes(x = factor(period), y = Y)) +
  geom_jitter(aes(color=factor(T)), size = .5, alpha = .8, width = .25) +
  geom_line(data=avg, aes(x = factor(period), y = Y, group = T, color= factor(T)), size=1) +
  xlab("Period") +
  scale_color_manual(values = plotcolors[c(3,1)], 
                     labels = c("Ctrl", "Trt")) +
  theme(legend.title=element_blank()) +
  ggtheme("grey90") +
  theme(legend.key=element_rect(fill=NA))

## ---- tidy = TRUE-------------------------------------------------------------
def <- defData(varname = "xbase", dist = "normal", formula = 20, variance = 3)
def <- defData(def,varname = "nCount", dist = "noZeroPoisson", formula = 6)
def <- defData(def, varname = "mInterval", dist = "gamma", formula = 30, variance = .01)
def <- defData(def, varname = "vInterval", dist = "nonrandom", formula = .07)

dt <- genData(200, def)
dt[id %in% c(8,121)]                # View individuals 8 and 121

## ---- tidy = TRUE-------------------------------------------------------------
dtPeriod <- addPeriods(dt)
dtPeriod[id %in% c(8,121)]  # View individuals 8 and 121 only

## ---- tidy = TRUE-------------------------------------------------------------
def2 <- defDataAdd(varname = "Y", dist = "normal", formula = "15 + .1 * time", variance = 5)
dtPeriod <- addColumns(def2, dtPeriod)

## ---- tidy = TRUE, echo = FALSE, fig.width = 6, fig.height = 3----------------

sampledID <- sample(1:nrow(dt), 5)
dtSample <- dtPeriod[id %in% sampledID]

ggplot(data = dtSample, aes(x = time, y = Y, group=id)) +
  geom_point(aes(color = factor(id))) +
  geom_line(aes(color = factor(id))) +
  xlab("Day") +
  scale_color_manual(values = cbbPalette) +
  theme(legend.position = "none") +
  ggtheme("grey90")

