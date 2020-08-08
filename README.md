
<!-- badges: start -->

[![R build
status](https://github.com/assignUser/simstudy/workflows/R-CMD-check/badge.svg)](https://github.com/assignUser/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/assignUser/simstudy/branch/main/graph/badge.svg)](https://codecov.io/gh/assignUser/simstudy)
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
    ##   1:   1 10.270203  8.529158   1
    ##   2:   2 10.328287  7.518664   3
    ##   3:   3 10.637073  6.877337   4
    ##   4:   4  9.320375  7.222970   3
    ##   5:   5 10.563545  7.834908   1
    ##  ---                            
    ## 246: 246 13.466478 10.417064   4
    ## 247: 247  8.064638  5.248006   2
    ## 248: 248 10.955293  9.191688   2
    ## 249: 249 11.022278  9.101368   2
    ## 250: 250  9.732747  6.881608   3
