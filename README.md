---
title: "README"
output: github_document
---

<!-- badges: start -->
[![R build status](https://github.com/assignUser/simstudy/workflows/R-CMD-check/badge.svg)](https://github.com/assignUser/simstudy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/assignUser/simstudy/branch/main/graph/badge.svg)](https://codecov.io/gh/assignUser/simstudy)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

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
##   1:   1  9.718610 8.548122   3
##   2:   2 10.133943 8.834960   3
##   3:   3  7.992035 7.614741   2
##   4:   4  8.785684 6.225638   4
##   5:   5 11.314316 8.550809   2
##  ---                           
## 246: 246  9.755718 9.064451   4
## 247: 247 11.367846 8.628514   3
## 248: 248 11.353467 7.353261   2
## 249: 249 12.650672 9.361326   4
## 250: 250 11.755328 9.446549   3
```
