
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

    ##       id         x        y grp
    ##   1:   1  7.845677 7.968414   3
    ##   2:   2  9.155164 8.611664   4
    ##   3:   3 10.023274 7.111683   3
    ##   4:   4  8.165585 6.955434   1
    ##   5:   5 11.127551 7.844974   1
    ##  ---                           
    ## 246: 246  7.395211 6.215873   2
    ## 247: 247  8.673591 6.307510   4
    ## 248: 248 10.046994 8.585955   3
    ## 249: 249  8.362917 6.913074   3
    ## 250: 250  9.608058 9.713480   1
