## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)

ggmissing <- function(dtPlot,varSelect=NULL,varLevel=NULL, idvar = "id",
                      periodvar = "period", missvar,
                      pcolor="#738e75", title = NULL) {

  dtP <- copy(dtPlot)

  if (! is.null(varSelect)) dtP <- dtP[eval(parse(text=varSelect)) == varLevel]

  xp <- ggplot(data=dtP, aes(y = factor(eval(parse(text=idvar))),
                             x = eval(parse(text=periodvar)))) +
    geom_tile(aes(fill=factor(eval(parse(text=missvar)))),
                  color="white") +
    ggtheme()+
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          legend.position="none",
          plot.title=element_text(size=8)
    ) +
    scale_fill_manual(values=c("grey80",pcolor))

  if (is.null(title)) {
    return(xp)
  } else {
    return(xp + ggtitle(title))
  }
}
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
def1 <- defData(varname = "m", dist = "binary", formula = .5)
def1 <- defData(def1, "u", dist = "binary", formula = .5)
def1 <- defData(def1, "x1", dist="normal", formula = "20*m + 20*u", variance = 2)
def1 <- defData(def1, "x2", dist="normal", formula = "20*m + 20*u", variance = 2)
def1 <- defData(def1, "x3", dist="normal", formula = "20*m + 20*u", variance = 2)

dtAct <- genData(1000, def1)

## ---- tidy = TRUE-------------------------------------------------------------
defM <- defMiss(varname = "x1", formula = .15, logit.link = FALSE)
defM <- defMiss(defM, varname = "x2", formula = ".05 + m * 0.25", logit.link = FALSE)
defM <- defMiss(defM, varname = "x3", formula = ".05 + u * 0.25", logit.link = FALSE)
defM <- defMiss(defM, varname = "u", formula = 1, logit.link = FALSE) # not observed

set.seed(283726)

missMat <- genMiss(dtAct, defM, idvars = "id")
dtObs <- genObs(dtAct, missMat, idvars = "id")

## ---- tidy = TRUE-------------------------------------------------------------
missMat
dtObs

## ----tidy=TRUE----------------------------------------------------------------
# Two functions to calculate means and compare them

rmean <- function(var, digits = 1) {
  round(mean(var, na.rm=TRUE), digits)
}

showDif <- function(dt1, dt2, rowName = c("Actual", "Observed", "Difference")) {
  dt <- data.frame(rbind(dt1, dt2, dt1 - dt2))
  rownames(dt) <- rowName
  return(dt)
}

# data.table functionality to estimate means for each data set

meanAct <- dtAct[,.(x1 = rmean(x1), x2 = rmean(x2), x3 = rmean(x3))]
meanObs <- dtObs[,.(x1 = rmean(x1), x2 = rmean(x2), x3 = rmean(x3))]

showDif(meanAct, meanObs)

## ----tidy=TRUE----------------------------------------------------------------
meanActm <- dtAct[,.(x1 = rmean(x1), x2 = rmean(x2), x3 = rmean(x3)), keyby = m]
meanObsm <- dtObs[,.(x1 = rmean(x1), x2 = rmean(x2), x3 = rmean(x3)), keyby = m]

## ---- tidy = TRUE-------------------------------------------------------------
# compare observed and actual when m = 0

showDif(meanActm[m==0, .(x1, x2, x3)], meanObsm[m==0, .(x1, x2, x3)])

# compare observed and actual when m = 1

showDif(meanActm[m==1, .(x1, x2, x3)], meanObsm[m==1, .(x1, x2, x3)])

## ---- tidy = TRUE-------------------------------------------------------------

# use baseline definitions from the previous example

dtAct <- genData(120, def1)
dtAct <- trtObserve(dtAct, formulas = .5, logit.link = FALSE, grpName = "rx")

# add longitudinal data

defLong <- defDataAdd(varname = "y", dist = "normal", formula = "10 + period*2 + 2 * rx", variance = 2)

dtTime <- addPeriods(dtAct, nPeriods = 4)
dtTime <- addColumns(defLong, dtTime)

## ---- tidy = TRUE-------------------------------------------------------------

# missingness for y is not monotonic

defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
defMlong <- defMiss(defMlong,varname = "y", formula = "-1.5 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = FALSE)

missMatLong <- genMiss(dtTime, defMlong, idvars = c("id","rx"), repeated = TRUE, periodvar = "period")

## ----tidy=TRUE, echo=FALSE, fig.width = 7, fig.height = 6---------------------
xp10 <- ggmissing(missMatLong, varSelect="rx", varLevel = 0, idvar = "id",
                 periodvar = "period", missvar="x1", pcolor="#1C5974",
                 title = "x1: baseline (control)")

xp11 <- ggmissing(missMatLong, varSelect="rx", varLevel = 1, idvar = "id",
                 periodvar = "period", missvar="x1", pcolor="#B84226",
                 title = "x1: baseline (exposed)")

xp20 <- ggmissing(missMatLong, varSelect="rx", varLevel = 0, idvar = "id",
                 periodvar = "period", missvar="y", pcolor="#1C5974",
                 title = "y: not monotonic (control)")

xp21 <- ggmissing(missMatLong, varSelect="rx", varLevel = 1, idvar = "id",
                 periodvar = "period", missvar="y", pcolor="#B84226",
                 title = "y: not monotonic (exposed)")

grid.arrange(xp10, xp20, xp11, xp21,
             nrow = 2,
             bottom = textGrob("Periods", gp = gpar(cex=.8)) #,
#             left = textGrob("ID", gp = gpar(cex = .8), rot = 90)
)



## ----tidy=TRUE----------------------------------------------------------------
# missingness for y is monotonic

defMlong <- defMiss(varname = "x1", formula = .20, baseline = TRUE)
defMlong <- defMiss(defMlong,varname = "y", formula = "-1.8 - 1.5 * rx + .25*period", logit.link = TRUE, baseline = FALSE, monotonic = TRUE)

missMatLong <- genMiss(dtTime, defMlong, idvars = c("id","rx"), repeated = TRUE, periodvar = "period")

## ----tidy=TRUE, echo=FALSE, fig.width = 7, fig.height = 6---------------------
xp10 <- ggmissing(missMatLong, varSelect="rx", varLevel = 0, idvar = "id",
                 periodvar = "period", missvar="x1", pcolor="#1C5974",
                 title = "x1: baseline (control)")

xp11 <- ggmissing(missMatLong, varSelect="rx", varLevel = 1, idvar = "id",
                 periodvar = "period", missvar="x1", pcolor="#B84226",
                 title = "x1: baseline (exposed)")

xp20 <- ggmissing(missMatLong, varSelect="rx", varLevel = 0, idvar = "id",
                 periodvar = "period", missvar="y", pcolor="#1C5974",
                 title = "y: monotonic (control)")

xp21 <- ggmissing(missMatLong, varSelect="rx", varLevel = 1, idvar = "id",
                 periodvar = "period", missvar="y", pcolor="#B84226",
                 title = "y: monotonic (exposed)")

grid.arrange(xp10, xp20, xp11, xp21,
             nrow = 2,
             bottom = textGrob("Periods", gp = gpar(cex=.8)) #,
#             left = textGrob("ID", gp = gpar(cex = .8), rot = 90)
)

