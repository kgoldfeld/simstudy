
<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=restructure)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/assignUser/simstudy/branch/main/graph/badge.svg)](https://codecov.io/gh/assignUser/simstudy)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

test

The simstudy package is collection of functions that allow users to
generate simulated data sets in order to explore modeling techniques or
better understand data generating processes. The user specifies a set of
relationships between covariates, and generates data based on these
specifications. The final data sets can represent data from randomized
control trials, repeated measure (longitudinal) designs, and cluster
randomized trials. Missingness can be generated using various mechanisms
(MCAR, MAR, NMAR).

Here is some simple sample code, much more in the vignette:

``` r
library(simstudy)

def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1)
dt <- genData(250, def)

dt <- trtAssign(dt, nTrt = 4, grpName = "grp", balanced = TRUE)

dt
```

    ##       id         x         y grp
    ##   1:   1  9.100623  6.354316   1
    ##   2:   2 11.276444  8.491086   3
    ##   3:   3 11.709242  9.248228   4
    ##   4:   4  7.971723  7.608986   2
    ##   5:   5 11.084042  9.410333   4
    ##  ---                            
    ## 246: 246 11.759686  8.786624   4
    ## 247: 247  9.253809  7.315656   4
    ## 248: 248 11.411634 10.058268   4
    ## 249: 249 11.814660  6.739552   4
    ## 250: 250 10.495308  7.104637   1
