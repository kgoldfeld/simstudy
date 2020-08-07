---
output: github_document
---


<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[![R build status](https://github.com/assignUser/simstudy/workflows/R-CMD-check/badge.svg)](https://github.com/assignUser/simstudy/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
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
##   1:   1  9.631016 7.846761   1
##   2:   2  7.881361 6.024021   2
##   3:   3 13.026409 9.014371   1
##   4:   4 11.498174 8.876936   4
##   5:   5  7.260292 6.537616   4
##  ---                           
## 246: 246 10.283806 6.215614   2
## 247: 247 10.296753 7.855150   4
## 248: 248 10.399282 8.780247   1
## 249: 249  9.459950 7.799998   2
## 250: 250  9.061005 7.089787   1
```
