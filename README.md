---
output: github_document
---


<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[![R build status](https://github.com/assignUser/simstudy/workflows/R-CMD-check/badge.svg)](https://github.com/assignUser/simstudy/actions)
<!-- badges: end -->

The simstudy package is collection of functions that allow users to generate simulated data sets in order to explore modeling techniques or better understand data generating processes. The user specifies a set of relationships between covariates, and generates data based on these specifications. The final data sets can represent data from randomized control trials, repeated measure (longitudinal) designs, and cluster randomized trials. Missingness can be generated using various mechanisms (MCAR, MAR, NMAR).

Here is some simple sample code, much more in the vignette:


```r
library(simstudy)

def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1)
dt <- genData(250, def)

dt <- trtAssign(dt, nTrt = 4, grpName = "grp", balanced = TRUE)

dt
```

```
##       id         x        y grp
##   1:   1 10.443681 9.138509   1
##   2:   2  8.319111 5.242411   3
##   3:   3  8.733083 7.069943   1
##   4:   4  9.178951 8.803296   1
##   5:   5 12.757580 9.982477   1
##  ---                           
## 246: 246 10.394314 7.554547   3
## 247: 247  8.791271 6.998441   2
## 248: 248  9.034011 6.667073   2
## 249: 249 10.152148 6.397955   2
## 250: 250 12.175421 8.860795   1
```
