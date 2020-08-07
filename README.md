---
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
##   1:   1 10.728427 8.698919   1
##   2:   2 11.000987 8.574932   4
##   3:   3  9.616239 7.409083   2
##   4:   4  8.847424 5.956734   3
##   5:   5 11.033262 7.815748   2
##  ---                           
## 246: 246  7.996506 6.377561   1
## 247: 247 13.470463 9.139281   2
## 248: 248  8.046128 8.377411   1
## 249: 249 11.308003 9.486817   4
## 250: 250  8.627176 6.875442   1
```
