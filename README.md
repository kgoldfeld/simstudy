---
output: github_document
---

<!-- badges: start -->
[![R build status](https://github.com/assignUser/simstudy/workflows/R-CMD-check/badge.svg)](https://github.com/assignUser/simstudy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![Codecov test coverage](https://codecov.io/gh/assignUser/simstudy/branch/master/graph/badge.svg)](https://codecov.io/gh/assignUser/simstudy?branch=master)
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
##       id         x         y grp
##   1:   1 10.576770  8.330868   4
##   2:   2 10.778353  8.924251   4
##   3:   3  7.834482  6.561576   2
##   4:   4 12.940744 11.016495   3
##   5:   5 12.511204  8.636431   1
##  ---                            
## 246: 246 10.906759  8.572105   3
## 247: 247 10.571323  8.244524   1
## 248: 248 11.118148 10.209973   4
## 249: 249  8.376581  5.995201   3
## 250: 250 10.251902  8.606679   3
```
