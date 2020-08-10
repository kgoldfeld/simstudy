
<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=restructure)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/kgoldfeld/simstudy/branch/restructure/graph/badge.svg)](https://codecov.io/gh/kgoldfeld/simstudy)
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
    ##   1:   1  9.477244  7.811216   2
    ##   2:   2  9.860280  7.181101   4
    ##   3:   3  9.668771  7.164853   4
    ##   4:   4  8.961609  5.534077   4
    ##   5:   5 10.546951  8.634364   3
    ##  ---                            
    ## 246: 246 10.500862 10.440874   4
    ## 247: 247 11.820150  8.935285   3
    ## 248: 248 10.211233  7.572336   2
    ## 249: 249  8.287996  7.323070   4
    ## 250: 250 11.379392  8.736977   3
