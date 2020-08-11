
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simstudy

<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=restructure)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/kgoldfeld/simstudy/branch/restructure/graph/badge.svg)](https://codecov.io/gh/kgoldfeld/simstudy)
<!-- badges: end -->

The simstudy package is collection of functions that allow users to
generate simulated data sets in order to explore modeling techniques or
better understand data generating processes. The user specifies a set of
relationships between covariates, and generates data based on these
specifications. The final data sets can represent data from randomized
control trials, repeated measure (longitudinal) designs, and cluster
randomized trials. Missingness can be generated using various mechanisms
(MCAR, MAR, NMAR).

## Installation

You can install the released version of simstudy from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("simstudy")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kgoldfeld/simstudy")
```

## Example

Here is some simple sample code, much more in the vignette:

``` r
library(simstudy)

def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1)
dt <- genData(250, def)

dt <- trtAssign(dt, nTrt = 4, grpName = "grp", balanced = TRUE)

dt
#>       id         x        y grp
#>   1:   1  8.891680 7.811615   1
#>   2:   2  9.671047 6.875817   4
#>   3:   3  9.565093 7.013342   3
#>   4:   4  9.768469 8.845951   2
#>   5:   5 10.592527 8.729015   2
#>  ---                           
#> 246: 246  9.167103 7.331931   4
#> 247: 247 11.651576 9.455624   3
#> 248: 248  9.845113 7.639048   4
#> 249: 249 11.266620 8.468850   1
#> 250: 250 11.775456 9.316336   3
```
