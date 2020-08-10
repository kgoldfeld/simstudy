simstudy
================

<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/kgoldfeld/simstudy/branch/main/graph/badge.svg)](https://codecov.io/gh/kgoldfeld/simstudy)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

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
    ##   1:   1  9.443780  7.938595   1
    ##   2:   2  9.133914  6.863542   1
    ##   3:   3 11.274323  9.019781   3
    ##   4:   4 10.335448  7.835146   3
    ##   5:   5  9.708638  7.370022   1
    ##  ---                            
    ## 246: 246  9.721158  7.920153   4
    ## 247: 247  9.519108  9.439130   3
    ## 248: 248 13.939843 10.300459   4
    ## 249: 249 11.222627  9.627471   2
    ## 250: 250  9.071751  8.051840   3
