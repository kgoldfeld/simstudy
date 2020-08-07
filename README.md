---
output: github_document
---

<!-- badges: start -->
[![R build status](https://github.com/assignUser/simstudy/workflows/R-CMD-check/badge.svg)](https://github.com/assignUser/simstudy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![Codecov test coverage](https://codecov.io/gh/assignUser/simstudy/branch/master/graph/badge.svg)](https://codecov.io/gh/assignUser/simstudy?branch=main)
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
##   1:   1 10.963917 8.589558   3
##   2:   2  8.363271 7.671193   3
##   3:   3  8.853794 8.184564   2
##   4:   4  7.857379 6.333022   1
##   5:   5 12.132598 8.736175   3
##  ---                           
## 246: 246  9.084634 7.860628   4
## 247: 247  8.826237 5.551464   3
## 248: 248  7.021703 4.862885   1
## 249: 249  9.472174 9.367608   3
## 250: 250 10.667318 7.101749   2
```
