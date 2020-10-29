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


## ----options, echo = FALSE----------------------------------------------------
options(digits = 2)

## ----threshold, fig.width = 5.25, fig.height = 3.5, echo = FALSE--------------
# preliminary libraries and plotting defaults

library(ggplot2)
library(data.table)

my_theme <- function() {
  theme(panel.background = element_rect(fill = "grey90"), 
        panel.grid = element_blank(), 
        axis.ticks = element_line(colour = "black"), 
        panel.spacing = unit(0.25, "lines"), 
        plot.title = element_text(size = 12, vjust = 0.5, hjust = 0), 
        panel.border = element_rect(fill = NA, colour = "gray90"))
}

# create data points density curve 

x <- seq(-6, 6, length = 1000)
pdf <- dlogis(x, location = 0, scale = 1)
dt <- data.table(x, pdf)

# set thresholds for Group A

thresholdA <- c(-2.1, -0.3, 1.4, 3.6)

pdf <- dlogis(thresholdA)
grpA <- data.table(threshold = thresholdA, pdf)
aBreaks <- c(-6, grpA$threshold, 6)

# plot density with cutpoints

dt[, grpA := cut(x, breaks = aBreaks, labels = F, include.lowest = TRUE)]

p1 <- ggplot(data = dt, aes(x = x, y = pdf)) +
  geom_line() +
  geom_area(aes(x = x, y = pdf, group = grpA, fill = factor(grpA))) +
  geom_hline(yintercept = 0, color = "grey50") +
  annotate("text", x = -5, y = .28, label = "unexposed", size = 5) +
  scale_fill_manual(values = c("#d0d7d1", "#bbc5bc", "#a6b3a7", "#91a192", "#7c8f7d"),
                    labels = c("strongly disagree", "disagree", "neutral", "agree", "strongly agree"),
                    name = "Frequency") +
  scale_x_continuous(breaks = thresholdA) +
  scale_y_continuous(limits = c(0, 0.3), name = "Density") +
  my_theme() +
  theme(legend.position = c(.85, .7),
        legend.background = element_rect(fill = "grey90"),
        legend.key = element_rect(color = "grey90"))

p1

## ----plotB, fig.width = 5.25, fig.height = 3.5, echo = FALSE------------------

pA= plogis(c(thresholdA, Inf)) - plogis(c(-Inf, thresholdA))
probs <- data.frame(pA)
rownames(probs) <- c("P(Resp = 1)", "P(Resp = 2)", 
                     "P(Resp = 3)", "P(Resp = 4)", "P(Resp = 5)")

probA <- data.frame(
           cprob = plogis(thresholdA), 
           codds = plogis(thresholdA)/(1-plogis(thresholdA)),
           lcodds = log(plogis(thresholdA)/(1-plogis(thresholdA)))
)
rownames(probA) <- c("P(Grp < 2)", "P(Grp < 3)", "P(Grp < 4)", "P(Grp < 5)")

thresholdB <- thresholdA + 1.1

pdf <- dlogis(thresholdB)
grpB <- data.table(threshold = thresholdB, pdf)
bBreaks <- c(-6, grpB$threshold, 6)

pB = plogis(c(thresholdB, Inf)) - plogis(c(-Inf, thresholdB))
probs <- data.frame(pA, pB)
rownames(probs) <- c("P(Resp = 1)", "P(Resp = 2)", 
                     "P(Resp = 3)", "P(Resp = 4)", "P(Resp = 5)")


# Plot density for group B

dt[, grpB := cut(x, breaks = bBreaks, labels = F, include.lowest = TRUE)]

p2 <- ggplot(data = dt, aes(x = x, y = pdf)) +
  geom_line() +
  geom_area(aes(x = x, y = pdf, group = grpB, fill = factor(grpB))) +
  geom_hline(yintercept = 0, color = "grey5") +
  geom_segment(data=grpA, 
               aes(x=threshold, xend = threshold, y=0, yend=pdf), 
               size = 0.3, lty = 2, color = "#857284") +
  annotate("text", x = -5, y = .28, label = "exposed", size = 5) +
  scale_fill_manual(values = c("#d0d7d1", "#bbc5bc", "#a6b3a7", "#91a192", "#7c8f7d"),
                    name = "Frequency") +
  scale_x_continuous(breaks = thresholdB) +
  scale_y_continuous(limits = c(0.0, 0.3), name = "Density") +
  my_theme() +
  theme(legend.position = "none")

p2

## ----acuts--------------------------------------------------------------------
baseprobs <- c(0.11, 0.33, 0.36, 0.17, 0.03)

defA <- defDataAdd(varname = "z", formula = "-1.1*exposed", dist = "nonrandom")

set.seed(130)

dT <- genData(25000)
dT <- trtAssign(dT, grpName = "exposed")
dT <- addColumns(defA, dT)

dT <- genOrdCat(dT, adjVar = "z", baseprobs, catVar = "r")

## ----ordinal------------------------------------------------------------------
library(ordinal)
clmFit <- clm(r ~ exposed, data = dT)
summary(clmFit)

## -----------------------------------------------------------------------------
(logOdds.unexp <- log(odds(cumsum(dT[exposed == 0, prop.table(table(r))])))[1:4])

## -----------------------------------------------------------------------------
(logOdds.expos <- log(odds(cumsum(dT[exposed == 1, prop.table(table(r))])))[1:4])

## -----------------------------------------------------------------------------
logOdds.expos - logOdds.unexp

## -----------------------------------------------------------------------------
baseprobs <- matrix(c(0.2, 0.1, 0.7,
                      0.7, 0.2, 0.1,
                      0.5, 0.2, 0.3,
                      0.4, 0.2, 0.4,
                      0.6, 0.2, 0.2), 
                    nrow = 5, byrow = TRUE)

# generate the data

set.seed(333)                     
dT <- genData(10000)

dX <- genOrdCat(dT, adjVar = NULL, baseprobs = baseprobs, 
                   prefix = "q", rho = 0.15, corstr = "cs", asFactor = FALSE)

## -----------------------------------------------------------------------------
round(dX[, cor(cbind(q1, q2, q3, q4, q5))], 2)

## -----------------------------------------------------------------------------
dM <- melt(dX, id.vars = "id")
dProp <- dM[ , prop.table(table(value)), by = variable]
dProp[, response := rep(seq(3), 5)]

# observed probabilities
dcast(dProp, variable ~ response, value.var = "V1", fill = 0)

# specified probabilites
baseprobs

## -----------------------------------------------------------------------------
dX <- genOrdCat(dT, adjVar = NULL, baseprobs = baseprobs, 
                   prefix = "q", rho = 0.40, corstr = "ar1", asFactor = FALSE)

# correlation
round(dX[, cor(cbind(q1, q2, q3, q4, q5))], 2)

dM <- melt(dX, id.vars = "id")
dProp <- dM[ , prop.table(table(value)), by = variable]
dProp[, response := rep(seq(3), 5)]

# probabilities
dcast(dProp, variable ~ response, value.var = "V1", fill = 0)

