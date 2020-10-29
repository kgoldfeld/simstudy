## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(splines)
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

## ---- fig.width = 6, fig.height = 2.5-----------------------------------------
knots <- c(0.25, 0.50, 0.75)
viewBasis(knots, degree = 2)

knots <- c(0.20, 0.40, 0.60, 0.80)
viewBasis(knots, degree = 3)


## ---- fig.width = 6, fig.height = 2.5-----------------------------------------
knots <- c(0.25, 0.5, 0.75)

# number of elements in theta: length(knots) + degree + 1
theta1 = c(0.1, 0.8, 0.4, 0.9, 0.2, 1.0) 

viewSplines(knots, degree = 2, theta1)


theta2 = matrix(c(0.1, 0.2, 0.4, 0.9, 0.2, 0.3, 0.6, 
                  0.1, 0.3, 0.3, 0.8, 1.0, 0.9, 0.4, 
                  0.1, 0.9, 0.8, 0.2, 0.1, 0.6, 0.1),
               ncol = 3)

theta2

viewSplines(knots, degree = 3, theta2)

## -----------------------------------------------------------------------------
ddef <- defData(varname = "age", formula = "20;60", dist = "uniform")

theta1 = c(0.1, 0.8, 0.6, 0.4, 0.6, 0.9, 0.9)
knots <- c(0.25, 0.5, 0.75)

## ---- fig.width = 6, fig.height = 2.5-----------------------------------------
viewSplines(knots = knots, theta = theta1, degree = 3)

## -----------------------------------------------------------------------------
set.seed(234)

dt <- genData(1000, ddef)
dt <- genSpline(dt = dt, newvar = "weight",
                predictor = "age", theta = theta1,
                knots = knots, degree = 3,
                newrange = "90;160",
                noise.var = 64)

## ---- fig.width = 6, fig.height = 3, message = FALSE--------------------------
ggplot(data = dt, aes(x=age, y=weight)) +
  geom_point(color = "grey65", size = 0.75) +
  geom_smooth(se=FALSE, color="red", size = 1, method = "auto") +
  geom_vline(xintercept = quantile(dt$age, knots)) +
  theme(panel.grid.minor = element_blank())

## ---- fig.width = 6, fig.height = 3-------------------------------------------

# normalize age for best basis functions
dt[, nage := (age - min(age))/(max(age) - min(age))] 

# fit a cubic spline
lmfit3 <- lm(weight ~ bs(x = nage, knots = knots, degree = 3, intercept = TRUE) - 1, data = dt)

# fit a quadtratic spline
lmfit2 <- lm(weight ~ bs(x = nage, knots = knots, degree = 2), data = dt)

# fit a  linear spline
lmfit1 <- lm(weight ~ bs(x = nage, knots = knots, degree = 1), data = dt)

# add predicted values for plotting
dt[, pred.3deg := predict(lmfit3)]
dt[, pred.2deg := predict(lmfit2)]
dt[, pred.1deg := predict(lmfit1)]

ggplot(data = dt, aes(x=age, y=weight)) +
  geom_point(color = "grey65", size = 0.75) +
  geom_line(aes(x=age, y = pred.3deg), color = "#1B9E77", size = 1) +
  geom_line(aes(x=age, y = pred.2deg), color = "#D95F02", size = 1) +
  geom_line(aes(x=age, y = pred.1deg), color = "#7570B3", size = 1) +
  geom_vline(xintercept = quantile(dt$age, knots)) +
  theme(panel.grid.minor = element_blank())

