
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
    ##   1:   1  9.695586 7.368589   2
    ##   2:   2 12.032679 7.976080   2
    ##   3:   3 10.696851 9.596919   2
    ##   4:   4 11.934678 6.884811   2
    ##   5:   5  7.914016 6.699555   3
    ##  ---                           
    ## 246: 246  9.748985 7.197879   3
    ## 247: 247  7.653961 5.453690   3
    ## 248: 248 11.300205 8.852895   3
    ## 249: 249 10.298133 6.576789   3
    ## 250: 250 10.058696 8.421488   2
